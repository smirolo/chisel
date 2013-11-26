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
"""module DelaySuite_RegNoInitUpdate_1(input clk,
    output[31:0] io_out
);

  reg[31:0] res;
  wire[31:0] T0;
  wire[31:0] T1;

  assign io_out = res;
  assign T0 = res + T1;
  assign T1 = {31'h0/* 0*/, 1'h1/* 1*/};

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
    chiselMain(Array[String]("--v"),
//      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RegInitUpdate()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_RegInitUpdate_1.v",
"""module DelaySuite_RegInitUpdate_1(input clk, input reset,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg[0:0] res;
  wire T1;

  assign io_out = T0;
  assign T0 = {31'h0/* 0*/, res};
  assign T1 = res + 1'h1/* 1*/;

  always @(posedge clk) begin
    res <= reset ? 1'h0/* 0*/ : T1;
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
    output[31:0] io_out
);

  wire[31:0] T0;
  reg[0:0] res;
  wire T1;

  assign io_out = T0;
  assign T0 = {31'h0/* 0*/, res};
  assign T1 = res + 1'h1/* 1*/;

  always @(posedge clk) begin
    if(reset) begin
      res <= 1'h0/* 0*/;
    end else if(io_in) begin
      res <= T1;
    end
  end
endmodule

""")
  }

  /** Uninitialized sram, one read on each clock. */
  @Test def testMemRead() {
    class MemReadModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(width=32), 8)
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MemReadModule()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_MemReadModule_1.v",
"""module DelaySuite_MemReadModule_1(input clk, input reset,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[2:0] T1;

  assign io_out = T0;
  assign T0 = mem[T1];
  assign T1 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
  end
endmodule

""")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadWrite() {
    class ReadWriteModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(width=32), 8)
      mem(io.addr) := mem(io.addr) + UInt(1)
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ReadWriteModule()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_ReadWriteModule_1.v",
"""module DelaySuite_ReadWriteModule_1(input clk, input reset,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[31:0] T3;
  wire[31:0] T4;
  wire[2:0] T5;
  wire[2:0] T6;

  assign io_out = T0;
  assign T0 = mem[T6];
  assign T2 = T3;
  assign T3 = T0 + T4;
  assign T4 = {31'h0/* 0*/, 1'h1/* 1*/};
  assign T5 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T6 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (1'h1/* 1*/)
      mem[T5] <= T2;
  end
endmodule

""")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondWrite() {
    class ReadCondWriteModule extends Module {
      val io = new Bundle() {
        val enable = Bool(INPUT)
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(width=32), 8)
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
"""module DelaySuite_ReadCondWriteModule_1(input clk, input reset,
    input  io_enable,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[31:0] T3;
  wire[2:0] T4;
  wire[31:0] T5;
  wire[31:0] T6;
  wire T7;
  wire[2:0] T8;
  wire[31:0] T9;
  wire[31:0] T10;
  wire[31:0] T11;
  wire[31:0] T12;
  wire[2:0] T13;
  wire[2:0] T14;

  assign io_out = T0;
  assign T0 = mem[T14];
  assign T2 = T3;
  assign T3 = mem[T4];
  assign T4 = T5[2'h2/* 2*/:1'h0/* 0*/];
  assign T5 = io_addr + T6;
  assign T6 = {29'h0/* 0*/, 3'h4/* 4*/};
  assign T7 = ! io_enable;
  assign T8 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T10 = T11;
  assign T11 = T0 + T12;
  assign T12 = {31'h0/* 0*/, 1'h1/* 1*/};
  assign T13 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T14 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (T7)
      mem[T8] <= T2;
    if (io_enable)
      mem[T13] <= T10;
  end
endmodule

""")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondMaskedWrite() {
    class ReadCondMaskedWrite extends Module {
      val io = new Bundle() {
        val enable = Bool(INPUT)
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(UInt(width=32), 8)
      when( io.enable ) {
        mem.write(io.addr, mem(io.addr), UInt(0xff00))
      }
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ReadCondMaskedWrite()))
    assertFile(tmpdir.getRoot() + "/DelaySuite_ReadCondMaskedWrite_1.v",
"""module DelaySuite_ReadCondMaskedWrite_1(input clk, input reset,
    input  io_enable,
    input [31:0] io_addr,
    output[31:0] io_out
);

  wire[31:0] T0;
  reg [31:0] mem [7:0];
  wire[31:0] T1;
  wire[31:0] T2;
  wire[15:0] T3;
  wire[31:0] T4;
  wire[2:0] T5;
  wire[2:0] T6;

  assign io_out = T0;
  assign T0 = mem[T6];
  assign T2 = {16'h0/* 0*/, T3};
  assign T3 = 16'hff00/* 65280*/;
  assign T4 = T0;
  assign T5 = io_addr[2'h2/* 2*/:1'h0/* 0*/];
  assign T6 = io_addr[2'h2/* 2*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if (io_enable && T2[0])
      mem[T5][0] <= T4[0];
    if (io_enable && T2[1])
      mem[T5][1] <= T4[1];
    if (io_enable && T2[2])
      mem[T5][2] <= T4[2];
    if (io_enable && T2[3])
      mem[T5][3] <= T4[3];
    if (io_enable && T2[4])
      mem[T5][4] <= T4[4];
    if (io_enable && T2[5])
      mem[T5][5] <= T4[5];
    if (io_enable && T2[6])
      mem[T5][6] <= T4[6];
    if (io_enable && T2[7])
      mem[T5][7] <= T4[7];
    if (io_enable && T2[8])
      mem[T5][8] <= T4[8];
    if (io_enable && T2[9])
      mem[T5][9] <= T4[9];
    if (io_enable && T2[10])
      mem[T5][10] <= T4[10];
    if (io_enable && T2[11])
      mem[T5][11] <= T4[11];
    if (io_enable && T2[12])
      mem[T5][12] <= T4[12];
    if (io_enable && T2[13])
      mem[T5][13] <= T4[13];
    if (io_enable && T2[14])
      mem[T5][14] <= T4[14];
    if (io_enable && T2[15])
      mem[T5][15] <= T4[15];
    if (io_enable && T2[16])
      mem[T5][16] <= T4[16];
    if (io_enable && T2[17])
      mem[T5][17] <= T4[17];
    if (io_enable && T2[18])
      mem[T5][18] <= T4[18];
    if (io_enable && T2[19])
      mem[T5][19] <= T4[19];
    if (io_enable && T2[20])
      mem[T5][20] <= T4[20];
    if (io_enable && T2[21])
      mem[T5][21] <= T4[21];
    if (io_enable && T2[22])
      mem[T5][22] <= T4[22];
    if (io_enable && T2[23])
      mem[T5][23] <= T4[23];
    if (io_enable && T2[24])
      mem[T5][24] <= T4[24];
    if (io_enable && T2[25])
      mem[T5][25] <= T4[25];
    if (io_enable && T2[26])
      mem[T5][26] <= T4[26];
    if (io_enable && T2[27])
      mem[T5][27] <= T4[27];
    if (io_enable && T2[28])
      mem[T5][28] <= T4[28];
    if (io_enable && T2[29])
      mem[T5][29] <= T4[29];
    if (io_enable && T2[30])
      mem[T5][30] <= T4[30];
    if (io_enable && T2[31])
      mem[T5][31] <= T4[31];
  end
endmodule

""")
  }

  /** Initialized ROM.

    XXX Skip object ROM not implemented.

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
  }*/
}
