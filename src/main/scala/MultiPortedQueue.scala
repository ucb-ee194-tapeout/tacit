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
    val reg_queue = Module(new MultiPortedRegQueue(gen, numEntries, numInputs))
    reg_queue.io.enqs <> io.enqs
    io.deq <> reg_queue.io.deq
    io.stall_enq := reg_queue.io.stall_enq
    io.count := reg_queue.io.count
  } else {
    require(numEntries % numInputs == 0, "SRAM queue requires numEntries divisible by numInputs")
    val sram_queue = Module(new MultiPortedSRAMQueue(gen, numEntries / numInputs, numInputs))
    sram_queue.io.enqs <> io.enqs
    io.deq <> sram_queue.io.deq
    io.stall_enq := sram_queue.io.full

    val count_reg = RegInit(0.U(log2Ceil(numEntries + 1).W))
    val enq_count = PopCount(io.enqs.map(_.fire))
    val deq_count = io.deq.fire.asUInt
    count_reg := count_reg + enq_count - deq_count
    io.count := count_reg
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

  val max_entries = depth * numInputs
  val ptr_width = log2Ceil(max_entries)
  val count_width = log2Ceil(max_entries + 1)

  def wrapAdd(ptr: UInt, inc: UInt): UInt = {
    val sum = ptr +& inc
    Mux(sum >= max_entries.U, (sum - max_entries.U)(ptr_width - 1, 0), sum(ptr_width - 1, 0))
  }

  private val bank_idx_width = log2Ceil(numInputs)
  def bankOf(ptr: UInt): UInt = {
    if (numInputs == 1) 0.U(1.W) else (ptr % numInputs.U)(bank_idx_width - 1, 0)
  }
  def rowOf(ptr: UInt): UInt = ptr / numInputs.U

  val banks = Seq.fill(numInputs)(SyncReadMem(depth, gen, SyncReadMem.WriteFirst))

  val enq_ptr = RegInit(0.U(ptr_width.W))
  val deq_ptr = RegInit(0.U(ptr_width.W))
  val entries = RegInit(0.U(count_width.W))

  val front_data = Reg(gen)
  val front_valid = RegInit(false.B)

  val read_pending = RegInit(false.B)
  val read_pending_bank = RegInit(0.U(bank_idx_width.W))

  // Match the register queue policy: all inputs share one ready bit and enqueue is all-or-nothing
  // with capacity reserved for a full numInputs-wide enqueue.
  val can_accept = entries <= (max_entries - numInputs).U
  io.enqs.foreach(_.ready := can_accept)
  io.full := !can_accept

  val enq_fire_vec = io.enqs.map(enq => enq.valid && can_accept)
  val enq_count = PopCount(enq_fire_vec)
  val do_enq = enq_count =/= 0.U
  val deq_valid = front_valid || read_pending
  val do_deq = io.deq.ready && deq_valid

  val write_enable = WireInit(VecInit(Seq.fill(numInputs)(false.B)))
  val write_addr = Wire(Vec(numInputs, UInt(log2Ceil(depth).W)))
  val write_data = Wire(Vec(numInputs, gen))
  for (i <- 0 until numInputs) {
    write_addr(i) := 0.U
    write_data(i) := DontCare
  }

  for (i <- 0 until numInputs) {
    when (enq_fire_vec(i)) {
      val enq_rank = PopCount(enq_fire_vec.take(i))
      val write_ptr = wrapAdd(enq_ptr, enq_rank)
      val write_bank = bankOf(write_ptr)
      write_enable(write_bank) := true.B
      write_addr(write_bank) := rowOf(write_ptr)
      write_data(write_bank) := io.enqs(i).bits
    }
  }

  val next_deq_ptr = Mux(do_deq, wrapAdd(deq_ptr, 1.U), deq_ptr)
  val entries_after = entries + enq_count - do_deq.asUInt
  val front_valid_after_deq = front_valid && !do_deq
  val issue_front_read = entries_after =/= 0.U && !front_valid_after_deq

  val read_req_bank = bankOf(next_deq_ptr)
  val read_req_row = rowOf(next_deq_ptr)

  val bank_read_data = Wire(Vec(numInputs, gen))
  for (i <- 0 until numInputs) {
    when (write_enable(i)) {
      banks(i).write(write_addr(i), write_data(i))
    }
    val do_read = issue_front_read && read_req_bank === i.U
    bank_read_data(i) := banks(i).read(read_req_row, do_read)
  }

  val read_resp_data = Mux1H(UIntToOH(read_pending_bank, numInputs), bank_read_data)
  val deq_bits = Mux(front_valid, front_data, read_resp_data)

  val consumed_from_read_pending = do_deq && !front_valid && read_pending
  when (read_pending && !consumed_from_read_pending) {
    front_data := read_resp_data
  }

  val front_valid_next = (front_valid && !do_deq) || (read_pending && !consumed_from_read_pending)
  front_valid := front_valid_next

  when (do_deq) {
    deq_ptr := wrapAdd(deq_ptr, 1.U)
  }

  when (do_enq) {
    enq_ptr := wrapAdd(enq_ptr, enq_count)
  }

  entries := entries_after

  when (issue_front_read) {
    read_pending := true.B
    read_pending_bank := read_req_bank
  } .otherwise {
    read_pending := false.B
  }

  io.deq.bits := deq_bits
  io.deq.valid := deq_valid
}
