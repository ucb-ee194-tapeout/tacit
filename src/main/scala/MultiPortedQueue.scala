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

  val might_hit_head = (1 until numInputs+1).map(k => rotateLeft(tail, k) & head).reduce(_|_).orR
  // is the tail at the head?
  val at_head = (tail & head).orR

  val do_enq = !(at_head && maybe_full || might_hit_head)
  io.enqs.map(_.ready := do_enq)

  // Generate one-hot write indices
  val enq_idxs = Wire(Vec(numInputs, UInt(numEntries.W)))

  def inc_mask(mask: UInt): UInt = {
    val n = mask.getWidth
    Cat(mask(n-2,0), mask(n-1))
  }

  var enq_idx = tail
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

  def max_entries = depth * numInputs

  // points to the idx in the SRAM, disregarding which bank it is in
  val enq_index = RegInit(0.U(log2Ceil(depth).W))      // which row in each bank
  val enq_bank = RegInit(0.U(log2Ceil(numInputs).W))   // which bank to start from
  val deq_index = RegInit(0.U(log2Ceil(depth).W))
  val deq_bank = RegInit(0.U(log2Ceil(numInputs).W))

  val maybe_full = RegInit(false.B)
  val ptr_match = enq_index === deq_index
  val deq_ptr = deq_index * numInputs.U + deq_bank
  val enq_ptr = enq_index * numInputs.U + enq_bank
  val ptr_diff = enq_ptr - deq_ptr

  val do_enq = io.enqs.map(_.fire).reduce(_ || _)
  val do_deq = io.deq.fire

  // number of enqs in this cycle
  val enq_count = io.enqs.map(_.fire.asUInt).reduce(_ +& _)

  when (do_enq) {
    val next_bank = enq_bank + enq_count
    // wrap around if we overflow the number of banks
    when (next_bank >= numInputs.U) {
      enq_bank := next_bank - numInputs.U
      enq_index := Mux(enq_index === (depth - 1).U, 0.U, enq_index + 1.U)
    }.otherwise {
      // otherwise, just increment the bank
      enq_bank := next_bank
    }
  }

  when (do_deq) {
    when (deq_bank === (numInputs - 1).U) {
      deq_bank := 0.U
      deq_index := deq_index + 1.U
    }.otherwise {
      deq_bank := deq_bank + 1.U
    }
  }

  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  // flow control only happens at the io level
  // once enq is accepted, it is not possible to backpressure anymore
  val enq_bank_n = Wire(Vec(numInputs, Valid(gen)))
  val deq_bank_n = Wire(Vec(numInputs, Valid(gen)))

  for (i <- 0 until numInputs) {
    // default, no enq at all
    enq_bank_n(i).valid := false.B
    enq_bank_n(i).bits := DontCare
    // default, no deq at all
    deq_bank_n(i).valid := false.B
    deq_bank_n(i).bits := DontCare
    when (io.enqs(i).fire) {
      // first, pack enqs - counting among inputs, what is my 0-indexed enq rank?
      val enq_rank = PopCount(io.enqs.slice(0, i).map(_.fire)) - 1.U
      // swizzle the enq to the correct bank
      val enq_bank_idx = (enq_bank + enq_rank) % numInputs.U
      enq_bank_n(enq_bank_idx).valid := io.enqs(i).fire
      enq_bank_n(enq_bank_idx).bits := io.enqs(i).bits
    }
  }

  // instantiate SRAM banks
  for (i <- 0 until numInputs) {
    val ram = SyncReadMem(depth, gen, SyncReadMem.WriteFirst)
    when (enq_bank_n(i).valid) {
      val true_enq_index = Mux(i.U + enq_bank >= numInputs.U, Mux(enq_index === (depth - 1).U, 0.U, enq_index + 1.U), enq_index)
      ram.write(true_enq_index, enq_bank_n(i).bits)
      deq_bank_n(i).bits := ram.read(deq_index)
    }
  }

  val entries = Mux(
    ptr_match,
    Mux(maybe_full, max_entries.asUInt, 0.U),
    Mux(deq_ptr > enq_ptr, max_entries.asUInt + ptr_diff, ptr_diff)
  )
  // do not account for banks. we conservatively say full when any bank is full
  val full = entries + enq_count >= max_entries.asUInt
  val empty = entries === 0.U

  io.deq.bits := deq_bank_n(deq_bank).bits
  io.deq.valid := !empty
  io.full := full
  io.enqs.map(_.ready := !full)
}