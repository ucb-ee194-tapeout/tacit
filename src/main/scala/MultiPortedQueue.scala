package tacit

import chisel3._
import chisel3.util._
import chisel3.experimental.requireIsChiselType
import freechips.rocketchip.trace._

import org.chipsalliance.cde.config.Parameters

/**
 * A queue that allows multiple inputs to be enqueued per cycle and a multiple outputs to be dequeued.
 * Does not need banking as this is flip-flop based.
 */
class MultiPortedRegQueue[T <: Data](
  val gen: T,
  val numEntries: Int,
  val numInputs: Int
) extends Module {
  requireIsChiselType(gen)
  val io = IO(new Bundle {
    val enqs = Flipped(Vec(numInputs, Decoupled(gen)))
    val deq = Decoupled(gen)
    val stall_enq = Output(Bool())
    val count = Output(UInt(log2Ceil(numEntries).W))
  })

  requireIsChiselType(gen)

  val ram = Reg(Vec(numEntries, gen))

  // head and tail are masks, not pointers
  val head = RegInit(1.U(numEntries.W)) // deq
  val tail = RegInit(1.U(numEntries.W)) // enq
  val maybe_full = RegInit(false.B)

  val enq_mask = io.enqs.map(_.valid)

  val enq_count = io.enqs.map(_.fire.asUInt).reduce(_ +& _)

  def rotateLeft(in: UInt, k: Int) = {
    val n = in.getWidth
    Cat(in(n-k-1,0), in(n-1, n-k))
  }

  // explicitly excluding the tail case, as the final element hitting head is ok
  val might_hit_head = (1 until numInputs).map(k => rotateLeft(tail, k) & head).reduce(_|_).orR
  // is the tail at the head?
  val ptr_match = (tail & head).orR

  val do_enq = !(ptr_match && maybe_full || might_hit_head)
  io.enqs.map(_.ready := do_enq)
  io.stall_enq := !do_enq

  // Generate one-hot write indices
  val enq_idxs = Wire(Vec(numInputs, UInt(numEntries.W)))

  def inc_mask(mask: UInt): UInt = {
    val n = mask.getWidth
    Cat(mask(n-2,0), mask(n-1))
  }

  var enq_idx = tail // notice that this is mutable
  for (i <- 0 until numInputs) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(enq_mask(i), inc_mask(enq_idx), enq_idx)
  }

  // write to the RAM
  for (i <- 0 until numInputs) {
    for (j <- 0 until numEntries) {
      when (do_enq && enq_mask(i) && enq_idxs(i)(j)) {
        ram(j) := io.enqs(i).bits
      }
    }
  }

  // dequeue logic
  val head_hit_tail = (head & tail).orR
  val empty = head_hit_tail && !maybe_full

  val do_deq = io.deq.ready && !empty

  when (do_enq) {
    tail := enq_idx
    when (enq_mask.reduce(_||_)) {
      maybe_full := true.B
    }
  }

  when (do_deq) {
    head := inc_mask(head)
    maybe_full := false.B
  }

  io.deq.bits := Mux1H(head, ram)
  io.deq.valid := !empty

  // Convert one-hot masks to binary indices for count calculation
  val tail_idx = OHToUInt(tail)
  val head_idx = OHToUInt(head)
  val ptr_diff = tail_idx - head_idx

  if (isPow2(numEntries)) {
    io.count := Mux(maybe_full && ptr_match, numEntries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(
      ptr_match,
      Mux(maybe_full, numEntries.asUInt, 0.U),
      Mux(head_idx > tail_idx, numEntries.asUInt + ptr_diff, ptr_diff)
    )
  }
}

class MultiPortedQueue[T <: Data](
  val gen: T,
  val numEntries: Int,
  val numInputs: Int,
  val useSramQueue: Boolean = false
) extends Module {
  requireIsChiselType(gen)
  val io = IO(new Bundle {
    val enqs = Flipped(Vec(numInputs, Decoupled(gen)))
    val deq = Decoupled(gen)
    val stall_enq = Output(Bool())
    val count = Output(UInt(log2Ceil(numEntries).W))
  })

  if (!useSramQueue) {
    val queue = Module(new MultiPortedRegQueue(gen, numEntries, numInputs))
    queue.io.enqs <> io.enqs
    io.deq <> queue.io.deq
    io.stall_enq := queue.io.stall_enq
    io.count := queue.io.count
  } else {
    require(numEntries % numInputs == 0, "SRAM queue requires numEntries divisible by numInputs")
    val queue = Module(new MultiPortedSRAMQueue(gen, numEntries / numInputs, numInputs))
    queue.io.enqs <> io.enqs
    io.deq <> queue.io.deq
    io.stall_enq := queue.io.full

    val countReg = RegInit(0.U(log2Ceil(numEntries + 1).W))
    val enqCount = PopCount(io.enqs.map(_.fire))
    val deqCount = io.deq.fire.asUInt
    countReg := countReg + enqCount - deqCount
    io.count := countReg
  }
}

object MultiPortedQueue {
  private val defaultUseSramQueue = false

  def apply[T <: Data](gen: T, numEntries: Int, numInputs: Int): MultiPortedQueue[T] = {
    Module(new MultiPortedQueue(gen, numEntries, numInputs, useSramQueue = defaultUseSramQueue))
  }

  def apply[T <: Data](gen: T, numEntries: Int, numInputs: Int, useSramQueue: Boolean): MultiPortedQueue[T] = {
    Module(new MultiPortedQueue(gen, numEntries, numInputs, useSramQueue = useSramQueue))
  }
}

/* A queue that allows multiple inputs to be enqueued per cycle and a single output to be dequeued.
  All or nothing enqueue, ordered dequeue
*/
class MultiPortedSRAMQueue[T <: Data](
  val gen: T,
  val depth: Int,
  val numInputs: Int,
) extends Module {

  requireIsChiselType(gen)

  val io = IO(new Bundle {
    val enqs = Flipped(Vec(numInputs, Decoupled(gen)))
    val deq = Decoupled(gen)
    val full = Output(Bool()) // essentially enq ready, but deduplicated
  })

  require(depth > 0)
  require(numInputs > 0)

  val maxEntries = depth * numInputs
  val ptrWidth = log2Ceil(maxEntries)
  val countWidth = log2Ceil(maxEntries + 1)

  def wrapAdd(ptr: UInt, inc: UInt): UInt = {
    val sum = ptr +& inc
    Mux(sum >= maxEntries.U, (sum - maxEntries.U)(ptrWidth - 1, 0), sum(ptrWidth - 1, 0))
  }

  private val bankIdxWidth = log2Ceil(numInputs)
  def bankOf(ptr: UInt): UInt = {
    if (numInputs == 1) 0.U(1.W) else (ptr % numInputs.U)(bankIdxWidth - 1, 0)
  }
  def rowOf(ptr: UInt): UInt = ptr / numInputs.U

  val banks = Seq.fill(numInputs)(SyncReadMem(depth, gen, SyncReadMem.WriteFirst))

  val enqPtr = RegInit(0.U(ptrWidth.W))
  val deqPtr = RegInit(0.U(ptrWidth.W))
  val entries = RegInit(0.U(countWidth.W))

  val frontData = Reg(gen)
  val frontValid = RegInit(false.B)

  val readPending = RegInit(false.B)
  val readPendingBank = RegInit(0.U(bankIdxWidth.W))

  // Match the register queue policy: all inputs share one ready bit and enqueue is all-or-nothing
  // with capacity reserved for a full numInputs-wide enqueue.
  val canAccept = entries <= (maxEntries - numInputs).U
  io.enqs.foreach(_.ready := canAccept)
  io.full := !canAccept

  val enqFireVec = io.enqs.map(enq => enq.valid && canAccept)
  val enqCount = PopCount(enqFireVec)
  val doEnq = enqCount =/= 0.U
  val deqValid = frontValid || readPending
  val doDeq = io.deq.ready && deqValid

  val writeEnable = WireInit(VecInit(Seq.fill(numInputs)(false.B)))
  val writeAddr = Wire(Vec(numInputs, UInt(log2Ceil(depth).W)))
  val writeData = Wire(Vec(numInputs, gen))
  for (i <- 0 until numInputs) {
    writeAddr(i) := 0.U
    writeData(i) := DontCare
  }

  for (i <- 0 until numInputs) {
    when (enqFireVec(i)) {
      val enqRank = PopCount(enqFireVec.take(i))
      val writePtr = wrapAdd(enqPtr, enqRank)
      val writeBank = bankOf(writePtr)
      writeEnable(writeBank) := true.B
      writeAddr(writeBank) := rowOf(writePtr)
      writeData(writeBank) := io.enqs(i).bits
    }
  }

  val nextDeqPtr = Mux(doDeq, wrapAdd(deqPtr, 1.U), deqPtr)
  val entriesAfter = entries + enqCount - doDeq.asUInt
  val frontValidAfterDeq = frontValid && !doDeq
  val issueFrontRead = entriesAfter =/= 0.U && !frontValidAfterDeq

  val readReqBank = bankOf(nextDeqPtr)
  val readReqRow = rowOf(nextDeqPtr)

  val bankReadData = Wire(Vec(numInputs, gen))
  for (i <- 0 until numInputs) {
    when (writeEnable(i)) {
      banks(i).write(writeAddr(i), writeData(i))
    }
    val doRead = issueFrontRead && readReqBank === i.U
    bankReadData(i) := banks(i).read(readReqRow, doRead)
  }

  val readRespData = Mux1H(UIntToOH(readPendingBank, numInputs), bankReadData)
  val deqBits = Mux(frontValid, frontData, readRespData)

  val consumedFromReadPending = doDeq && !frontValid && readPending
  when (readPending && !consumedFromReadPending) {
    frontData := readRespData
  }

  val frontValidNext = (frontValid && !doDeq) || (readPending && !consumedFromReadPending)
  frontValid := frontValidNext

  when (doDeq) {
    deqPtr := wrapAdd(deqPtr, 1.U)
  }

  when (doEnq) {
    enqPtr := wrapAdd(enqPtr, enqCount)
  }

  entries := entriesAfter

  when (issueFrontRead) {
    readPending := true.B
    readPendingBank := readReqBank
  } .otherwise {
    readPending := false.B
  }

  io.deq.bits := deqBits
  io.deq.valid := deqValid
}
