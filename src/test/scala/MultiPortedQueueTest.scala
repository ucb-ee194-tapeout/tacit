package tacit

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec

class MultiPortedQueueTest extends AnyFlatSpec {
  behavior of "MultiPortedRegQueue"
  it should "do something" in {
    simulate(new MultiPortedRegQueue(UInt(8.W), numEntries = 4, numInputs = 2)) { c =>
      // reset
      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)
      c.io.enqs.map(_.valid).foreach(_.poke(false.B))
      c.io.enqs.map(_.ready).foreach(_.expect(true.B))
      // enq 42 to input 0
      c.io.enqs(0).valid.poke(true.B)
      c.io.enqs(0).bits.poke(42.U)
      c.io.enqs(0).ready.expect(true.B)
      // enq 43 to input 1
      c.io.enqs(1).valid.poke(true.B)
      c.io.enqs(1).bits.poke(43.U)
      c.io.enqs(1).ready.expect(true.B)
      c.clock.step()
      c.io.enqs(0).valid.poke(true.B)
      c.io.enqs(0).bits.poke(44.U)
      c.io.enqs(0).ready.expect(true.B)
      // enq 43 to input 1
      c.io.enqs(1).valid.poke(true.B)
      c.io.enqs(1).bits.poke(45.U)
      c.io.enqs(1).ready.expect(true.B)
      c.clock.step()
      c.io.enqs(0).valid.poke(false.B)
      c.io.enqs(1).valid.poke(false.B)
      c.io.enqs.map(_.ready).foreach(_.expect(false.B))
      c.clock.step()
      // deq
      c.io.deq.ready.poke(true.B)
      c.io.deq.valid.expect(true.B)
      c.io.deq.bits.expect(42.U)
      c.clock.step()
      c.io.enqs.map(_.ready).foreach(_.expect(false.B))
      // deq
      c.io.deq.ready.poke(true.B)
      c.io.deq.valid.expect(true.B)
      c.io.deq.bits.expect(43.U)
      c.clock.step()
      // deq
      c.io.deq.ready.poke(true.B)
      // c.io.deq.valid.expect(false.B)
      c.io.enqs.map(_.ready).foreach(_.expect(true.B))
      c.clock.step()
    }
  }
}