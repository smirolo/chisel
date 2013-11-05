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

import scala.collection.mutable.ArrayBuffer
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore


import Chisel._


/** This testsuite checks all methods in the Bits class.
*/
class DelaySuite extends AssertionsForJUnit {

  /** Uninitialized register, update on each clock. */
  @Test def testRegNoInitUpdate() {
    println("\ntestRegNoInitUpdate ...")
    val res = Reg(UInt(32))
    res := res + UInt(1)
  }

  /** Initialized register, update on each clock. */
  @Test def testRegInitUpdate() {
    println("\ntestRegInitUpdate ...")
    val res = Reg(init=UInt(0))
    res := res + UInt(1)
  }

  /** Initialized register, conditional update. */
  @Test def testRegInitCondUpdate() {
    println("\ntestRegInitCondUpdate ...")
    val in = Bool(INPUT)
    val res = Reg(init=UInt(0))
    when( in ) {
      res := res + UInt(1)
    }
  }

  /** Uninitialized sram, one read on each clock. */
  @Test def testMemRead() {
    val addr = UInt(INPUT)
    val data = UInt(OUTPUT)
    val mem= Mem(UInt(32), 8)
    data := mem(addr)
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadWrite() {
    val addr = UInt(INPUT)
    val mem= Mem(UInt(32), 8)
    mem(addr) := mem(addr) + UInt(1)
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondWrite() {
    val enable = Bool(INPUT)
    val addr = UInt(INPUT)
    val mem= Mem(UInt(32), 8)
    when( enable ) {
      mem(addr) := mem(addr) + UInt(1)
    }
    .otherwise {
      mem(addr) := mem(addr + UInt(4))
    }
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondMaskedWrite() {
    val enable = Bool(INPUT)
    val addr = UInt(INPUT)
    val mem= Mem(UInt(32), 8)
    when( enable ) {
      mem.write(addr, mem(addr), UInt(0xff00))
    }
  }

  /** Initialized ROM. */
  @Test def testROM() {
    val addr = UInt(INPUT)
    val rom = ROM(UInt(1) :: UInt(2) :: UInt(3) :: Nil)
    val out = rom(addr)
  }

}
