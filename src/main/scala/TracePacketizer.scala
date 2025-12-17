package tacit

import chisel3._
import chisel3.util._
import freechips.rocketchip.trace.TraceCoreParams

// slice packets into bytes TODO: is this efficient?
class TracePacketizer(val coreParams: TraceCoreParams) extends Module with MetaDataWidthHelper {

  val io = IO(new Bundle {
    val target_addr = Flipped(Decoupled(Vec(addrMaxNumBytes, UInt(8.W))))
    val trap_addr = Flipped(Decoupled(Vec(addrMaxNumBytes, UInt(8.W))))
    val time = Flipped(Decoupled(Vec(timeMaxNumBytes, UInt(8.W))))
    val byte = Flipped(Decoupled(UInt(8.W)))
    val prv = Flipped(Decoupled(UInt(8.W)))
    val ctx = Flipped(Decoupled(Vec(ctxMaxNumBytes, UInt(8.W))))
    val metadata = Flipped(Decoupled(new MetaDataBundle(coreParams)))
    val out = Decoupled(UInt(8.W))
  })

  val pIdle :: pComp :: pFull :: Nil = Enum(3)
  val state = RegInit(pIdle)

  val trap_addr_num_bytes = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val trap_addr_index = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val target_addr_num_bytes = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val target_addr_index = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val time_num_bytes = Reg(UInt(log2Ceil(timeMaxNumBytes).W))
  val time_index = Reg(UInt(log2Ceil(timeMaxNumBytes).W))
  val prv_num_bytes = Reg(UInt(1.W))
  val prv_index = Reg(UInt(1.W))
  val ctx_num_bytes = Reg(UInt(log2Ceil(ctxMaxNumBytes).W))
  val ctx_index = Reg(UInt(log2Ceil(ctxMaxNumBytes).W))
  val header_num_bytes = Reg(UInt(1.W))
  val header_index = Reg(UInt(1.W))
  
  // default values
  io.out.valid := false.B
  io.metadata.ready := false.B
  io.target_addr.ready := false.B
  io.trap_addr.ready := false.B
  io.time.ready := false.B
  io.prv.ready := false.B
  io.ctx.ready := false.B
  io.byte.ready := false.B
  io.out.bits := 0.U

  def prep_next_state(): Unit = {
    trap_addr_index := 0.U
    trap_addr_num_bytes := Mux(io.metadata.fire, io.metadata.bits.trap_addr, 0.U)
    target_addr_index := 0.U
    target_addr_num_bytes := Mux(io.metadata.fire, io.metadata.bits.target_addr, 0.U)
    time_index := 0.U
    time_num_bytes := Mux(io.metadata.fire, io.metadata.bits.time, 0.U)
    prv_index := 0.U
    prv_num_bytes := Mux(io.metadata.fire, io.metadata.bits.prv, 0.U)
    header_index := 0.U
    header_num_bytes := Mux(io.metadata.fire, ~io.metadata.bits.is_compressed, 0.U)
    state := Mux(io.metadata.fire, 
      Mux(io.metadata.bits.is_compressed.asBool, pComp, pFull),
      pIdle
    )
  }
  
  switch (state) {
    is (pIdle) {
      io.metadata.ready := true.B
      when (io.metadata.fire) {
        trap_addr_num_bytes := io.metadata.bits.trap_addr
        trap_addr_index := 0.U
        target_addr_num_bytes := io.metadata.bits.target_addr
        target_addr_index := 0.U
        time_num_bytes := io.metadata.bits.time
        time_index := 0.U
        header_num_bytes := ~io.metadata.bits.is_compressed
        header_index := 0.U
        prv_num_bytes := io.metadata.bits.prv
        prv_index := 0.U
        ctx_num_bytes := io.metadata.bits.ctx
        ctx_index := 0.U
        state := Mux(io.metadata.bits.is_compressed.asBool, pComp, pFull)
      }
    }
    is (pComp) {
      // transmit a byte from byte buffer
      io.byte.ready := io.out.ready
      io.out.valid := io.byte.valid
      io.out.bits := io.byte.bits
      when (io.byte.fire) {
        // metadata runs ahead by 1 cycle for performance optimization
        io.metadata.ready := true.B
        prep_next_state()
      }
    }
    is (pFull) {
      // header, addr, time
      io.out.valid := true.B
      when (header_num_bytes > 0.U && header_index < header_num_bytes) {
        io.out.bits := io.byte.bits
        io.out.valid := io.byte.valid
        header_index := header_index + io.out.fire
      } .elsewhen (prv_num_bytes > 0.U && prv_index < prv_num_bytes) {
        io.out.bits := io.prv.bits
        io.out.valid := io.prv.valid
        prv_index := prv_index + io.out.fire
      } .elsewhen (ctx_num_bytes > 0.U && ctx_index < ctx_num_bytes) {
        io.out.bits := io.ctx.bits(ctx_index)
        io.out.valid := io.ctx.valid
        ctx_index := ctx_index + io.out.fire
      } .elsewhen (trap_addr_num_bytes > 0.U && trap_addr_index < trap_addr_num_bytes) {
        io.out.bits := io.trap_addr.bits(trap_addr_index)
        io.out.valid := io.trap_addr.valid
        trap_addr_index := trap_addr_index + io.out.fire
      } .elsewhen (target_addr_num_bytes > 0.U && target_addr_index < target_addr_num_bytes) {
        io.out.bits := io.target_addr.bits(target_addr_index)
        io.out.valid := io.target_addr.valid
        target_addr_index := target_addr_index + io.out.fire
      } .elsewhen (time_num_bytes > 0.U && time_index < time_num_bytes) {
        io.out.bits := io.time.bits(time_index)
        io.out.valid := io.time.valid
        time_index := time_index + io.out.fire
      } .otherwise {
        // FIXME: delay for 1 cycle
        io.out.valid := false.B
        // release buffers
        io.byte.ready := true.B
        // conditional depletion of buffers, based on whether we included it in the packet
        io.target_addr.ready := target_addr_num_bytes =/= 0.U
        io.trap_addr.ready := trap_addr_num_bytes =/= 0.U
        io.time.ready := time_num_bytes =/= 0.U
        io.prv.ready := prv_num_bytes =/= 0.U
        io.ctx.ready := ctx_num_bytes =/= 0.U
        io.metadata.ready := true.B
        prep_next_state()
      }
    }
  }
}

class TraceMaskedPacketizer(val coreParams: TraceCoreParams) extends Module with MetaDataWidthHelper {
  val io = IO(new Bundle {
    val message = Flipped(Decoupled(new MessagePacketBundle(coreParams)))
    val byte = Flipped(Decoupled(UInt(8.W)))
    val metadata = Flipped(Decoupled(new MetaDataBundle(coreParams)))
    val out = Decoupled(UInt(8.W)) // FIXME: can we do better?
  })

  val pIdle :: pComp :: pFull :: Nil = Enum(3)
  val state = RegInit(pIdle)

  val metadata_reg = Reg(new MetaDataBundle(coreParams))

  io.out.valid := false.B
  io.metadata.ready := false.B
  io.message.ready := false.B
  io.byte.ready := false.B
  io.out.bits := 0.U

  def inRange(n: UInt, upper: Int, lower: Int): Bool = {
    n >= lower.U && n < upper.U
  }

  def prep_next_state(): Unit = {
    io.metadata.ready := true.B
    state := Mux(io.metadata.fire, 
      Mux(io.metadata.bits.is_compressed.asBool, pComp, pFull),
      pIdle
    )
  }

  io.byte.ready := false.B
  io.message.ready := false.B
  io.metadata.ready := false.B
  io.out.valid := false.B
  io.out.bits := DontCare

  switch (state) {
    is (pIdle) {
      io.metadata.ready := true.B
      when (io.metadata.fire) {
        metadata_reg := io.metadata.bits
        state := Mux(io.metadata.bits.is_compressed.asBool, pComp, pFull)
      }
    }
    is (pComp) {
      io.byte.ready := io.out.ready
      io.out.valid := io.byte.valid
      io.out.bits := io.byte.bits
      when (io.byte.fire) {
        io.metadata.ready := true.B
        state := pFull
        prep_next_state()
      }
    }
    is (pFull) {
      io.out.valid := true.B
      io.message.ready := true.B
      when (metadata_reg.asUInt =/= 0.U) {
        val idx = PriorityEncoder(metadata_reg.asUInt)
        when (idx.asUInt === 0.U) {
          io.out.bits := io.byte.bits
        } .elsewhen (inRange(idx, 1+1, 1)) {
          io.out.bits := io.message.bits.prv
        } .elsewhen (inRange(idx, 2+ctxMaxNumBytes, 2)) {
          io.out.bits := io.message.bits.ctx(idx - 2.U)
        } .elsewhen (inRange(idx, 2+ctxMaxNumBytes+addrMaxNumBytes, 2+ctxMaxNumBytes)) {
          io.out.bits := io.message.bits.trap_addr(idx - 2.U - ctxMaxNumBytes.U)
        } .elsewhen (inRange(idx, 2+ctxMaxNumBytes+addrMaxNumBytes+addrMaxNumBytes, 2+ctxMaxNumBytes+addrMaxNumBytes)) {
          io.out.bits := io.message.bits.target_addr(idx - 2.U - ctxMaxNumBytes.U - addrMaxNumBytes.U)
        } .elsewhen (inRange(idx, 2+ctxMaxNumBytes+addrMaxNumBytes+addrMaxNumBytes+timeMaxNumBytes, 2+ctxMaxNumBytes+addrMaxNumBytes+addrMaxNumBytes)) {
          io.out.bits := io.message.bits.time(idx - 2.U - ctxMaxNumBytes.U - addrMaxNumBytes.U - addrMaxNumBytes.U)
        } .otherwise {
          io.out.bits := 0.U
        }
      } .otherwise {
        io.byte.ready := true.B // dequeue the header byte
        io.message.ready := true.B // dequeue the message
        prep_next_state()
      }
    }
  }

}