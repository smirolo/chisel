/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.junit.rules.TemporaryFolder
import org.junit.Assert._
import org.junit.Ignore
import org.junit.Before
import org.junit.After
import org.junit.Test

import Chisel._


/** This testsuite checks all methods in the Bits class.
*/
class DelaySuite extends AssertionsForJUnit {

  val tmpdir = new TemporaryFolder();

  @Before def initialize() {
    tmpdir.create()
  }

  @After def done() {
    tmpdir.delete()
  }

  def assertFile( filename: String, content: String ) {
    val source = scala.io.Source.fromFile(filename, "utf-8")
    val lines = source.mkString
    source.close()
    assert(lines === content)
  }

  /** Uninitialized register, update on each clock. */
  @Test def testRegNoInitUpdate() {
    println("\ntestRegNoInitUpdate ...")
    try {
    class RegNoInitUpdate extends Module {
      val io = new Bundle() {
        val out = UInt(OUTPUT, 32)
      }
      // XXX BE CAREFUL UInt(32) will create a literal 0x20 6 bits wide.
      val res = Reg(UInt(width=32))
      res := res + UInt(1)
      io.out := res
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RegNoInitUpdate()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_RegNoInitUpdate_1.v",
"""module DelaySuite_RegNoInitUpdate_1(
    input clk,
    output [31:0] io_out
);

  reg [31:0] res;
  wire [31:0] T0;

  assign io_out = res;
  assign T0 = res + 1'h1/* 1*/;

  always @(posedge clk) begin
    res <= T0;
  end
endmodule

""")
    } catch {
      case expt => expt.printStackTrace()
    }
  }

  /** Initialized register, update on each clock. */
  @Test def testRegInitUpdate() {
    println("\ntestRegInitUpdate ...")
    class RegInitUpdate extends Module {
      val io = new Bundle() {
        val out = UInt(OUTPUT, 32)
      }
      // XXX Should the width be 32-bit here?
      val res = Reg(init=UInt(0))
      res := res + UInt(1)
      io.out := res
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RegInitUpdate()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_RegInitUpdate_1.v",
"""module DelaySuite_RegInitUpdate_1(input clk, input reset,
    output[31:0]  io_out
);

  reg res;
  wire T0;

  assign io_out = res;
  assign T0 = res + 1'h1/* 1*/;

  always @(posedge clk) begin
    res <= reset ? 1'h0/* 0*/ : T0;
  end
endmodule

""")
  }

  /** Initialized register, conditional update. */
  @Test def testRegInitCondUpdate() {
    println("\ntestRegInitCondUpdate ...")
    class RegInitCondUpdate extends Module {
      val io = new Bundle() {
        val in = Bool(INPUT)
        val out = UInt(OUTPUT, 32)
      }
      val res = Reg(init=UInt(0))
      when( io.in ) {
        res := res + UInt(1)
      }
      io.out := res
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RegInitCondUpdate()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_RegInitCondUpdate_1.v",
"""module DelaySuite_RegInitCondUpdate_1(input clk, input reset,
    input  io_in,
    output[31:0]  io_out
);

  reg res;
  wire T0;
  wire T1;

  assign io_out = res;
  assign T0 = io_in ? T1 : res;
  assign T1 = res + 1'h1/* 1*/;

  always @(posedge clk) begin
    res <= reset ? 1'h0/* 0*/ : T0;
  end
endmodule

""")
  }

  /** Uninitialized sram, one read on each clock. */
  @Test def testMemRead() {
    class MemReadModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(32), 8)
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MemReadModule()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_MemReadModule_1.v",
"""
""")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadWrite() {
    class ReadWriteModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(32), 8)
      mem(io.addr) := mem(io.addr) + UInt(1)
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ReadWriteModule()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_ReadWriteModule_1.v",
"""
""")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondWrite() {
    class ReadCondWriteModule extends Module {
      val io = new Bundle() {
        val enable = Bool(INPUT)
        val addr = UInt(INPUT)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(32), 8)
      when( io.enable ) {
        mem(io.addr) := mem(io.addr) + UInt(1)
      }
      .otherwise {
        mem(io.addr) := mem(io.addr + UInt(4))
      }
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ReadCondWriteModule()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_ReadCondWriteModule_1.v",
"""
""")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondMaskedWrite() {
    class ReadCondMaskedWrite extends Module {
      val io = new Bundle() {
        val enable = Bool(INPUT)
        val addr = UInt(INPUT)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(32), 8)
      when( io.enable ) {
        mem.write(io.addr, mem(io.addr), UInt(0xff00))
      }
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ReadCondMaskedWrite()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_ReadCondMaskedWrite_1.v",
"""
""")
  }

  /** Initialized ROM. */
  @Test def testROM() {
    class ROMModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT)
        val out = UInt(OUTPUT)
      }
      val rom = ROM(UInt(1) :: UInt(2) :: UInt(3) :: Nil)
      io.out := rom(io.addr)
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ROMModule()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_ROMModule_1.v",
"""
""")
  }
}
