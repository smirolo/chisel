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

package Chisel

import scala.collection.mutable.ArrayBuffer

/** Represents nodes which are holding state (registers and srams) */
abstract class Delay extends Node {

// XXX should be here instead of Node:  var clock: Clock = null

}

/** Storage nodes represented as registers. */
class RegDelay(nextNode: Node, initNode: Node = null,
               val depth: Int = 1) extends Delay {

  this.inputs.append(nextNode)
  if( initNode != null ) this.inputs.append(initNode)

  def next: Node = if( this.inputs.length > 0 ) this.inputs(0) else null
  def init: Node  = if( this.inputs.length > 1 ) this.inputs(1) else null

  def enableSignal: Node = inputs(enableIndex);

  def inferWidth(): Width = new WidthOf(0)

  var enableIndex = 0;
  var isReset = false
  var isEnable = false;

  override def assigned: Boolean = (next != null)

  override def rvalue( value: Node ): Node = {
    if( inputs.length > 0 ) {
      inputs(0) = value
    } else {
      inputs.append(value)
    }
    this
  }

}

/** Storage nodes represented as srams. */
class MemDelay(val depth: Int = 1, isInlineP: Boolean = false)
    extends Delay {

  val writes = ArrayBuffer[MemWrite]()
  val seqreads = ArrayBuffer[MemSeqRead]()
  val reads = ArrayBuffer[MemRead]()
  val readwrites = ArrayBuffer[MemReadWrite]()

  def inferWidth(): Width = new FixedWidth(1 * depth) // XXX compute width.

  override def isInVCD = false

  def isInline = isInlineP

//XXX  def isInline = Module.isInlineMem || !reads.isEmpty

  def ports: ArrayBuffer[_ <: MemAccess] =
    (writes ++ reads ++ seqreads ++ readwrites)
}


/** Reference to a location inside a state-holding register or sram.
  */
class MemReference(mem: MemDelay, addr: Node) extends Node {

  override def inferWidth(): Width = new FixedWidth(log2Up(mem.depth))

  override def lvalue(): Node = {
    new MemRead(this.mem, this.addr)
  }

  override def rvalue( value: Node ): Node = {
    new MemWrite(this.mem, this.addr, value)
  }

}


/** Base class for memory ports (read, write, read/write)
  */
abstract class MemAccess(memi: MemDelay, addri: Node) extends Node {
  def mem: MemDelay = inputs(0).asInstanceOf[MemDelay]
  def addr: Node = inputs(1)
  def cond: Node

  inputs += memi
  inputs += addri

  override def inferWidth(): Width = new FixedWidth(log2Up(mem.depth))

  var referenced = false
  def used = referenced
  def getPortType: String
}


/** Memory Read Port
  */
class MemRead(mem: MemDelay, addr: Node) extends MemAccess(mem, addr) {

  override def cond = Literal(1)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "cread"
}


/** Memory Sequential Read Port
  */
class MemSeqRead(mem: MemDelay, addri: Node) extends MemAccess(mem, addri) {

  val addrReg = addri.asInstanceOf[RegDelay]
  override def cond = if (addrReg.isEnable) addrReg.enableSignal else Bool(true).node
  override def isReg = true
  override def addr = if(inputs.length > 2) inputs(2) else null

/* XXX deprecated
  override def forceMatchingWidths = {
    val forced = addrReg.next.matchWidth(log2Up(mem.n))
    inputs += forced
    assert(addr == forced)
  }
 */

  inputs += mem
//XXX  inferWidth = fixWidth(mem.data.getWidth)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "read"
  override def isRamWriteInput(n: Node) = addrReg.isEnable && addrReg.enableSignal == n || addr == n
}


/** Memory Write Port
  */
class MemWrite(mem: MemDelay, addri: Node, datai: Node, condi: Node = null, maski: Node = null) extends MemAccess(mem, addri) {
  inputs += condi
  override def cond = inputs(1)
  clock = mem.clock

  if (datai != null) {
    def wrap(x: Node) = { // prevent Verilog syntax errors when indexing constants
      x
    }
    inputs += wrap(datai)
    if (maski != null) {
      inputs += wrap(maski)
    }
  }

/* XXX deprecated
  override def forceMatchingWidths = {
    val w = mem.width
    super.forceMatchingWidths
    if(inputs.length >= 3 && inputs(2).width != w) inputs(2) = inputs(2).matchWidth(w)
    if(inputs.length >= 4 && inputs(3).width != w) inputs(3) = inputs(3).matchWidth(w)
  }
 */

  var pairedRead: MemSeqRead = null

  def emitRWEnable(r: MemSeqRead) = {
    def getProducts(x: Node): List[Node] = {
      if (x.isInstanceOf[LogicalAndOp]) {
        List(x) ++ getProducts(x.inputs(0)) ++ getProducts(x.inputs(1))
      } else {
        List(x)
      }
    }
    def isNegOf(x: Node, y: Node) = (
      x.isInstanceOf[LogicalNegOp] && x.inputs(0) == y)

    val wp = getProducts(cond)
    val rp = getProducts(r.cond)
    wp.find(wc => rp.exists(rc => isNegOf(rc, wc) || isNegOf(wc, rc)))
  }
  def data = inputs(2)
  def mask = inputs(3)
  def isMasked = inputs.length > 3
  override def toString: String = mem + "[" + addr + "] = " + data + " COND " + cond
  override def getPortType: String = if (isMasked) "mwrite" else "write"
  override def isRamWriteInput(n: Node) = inputs.contains(n)
}


/** Memory Read/Write Port
  */
class MemReadWrite(val read: MemSeqRead, val write: MemWrite) extends MemAccess(read.mem, null)
{
  override def cond = throw new Exception("")
  override def getPortType = if (write.isMasked) "mrw" else "rw"
}

