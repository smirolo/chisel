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

/** posedge update signal for delays. This is a clock  */
class Update(srci: Update = null, val mul: Int = 1, val div: Int = 1)
    extends Node {

  inferWidth = new FixedWidth(1)
  if (srci != null) this.inputs.append(srci)

  def src: Update = if( inputs.length > 0 ) inputs(0).asInstanceOf[Update] else null

}


/** Represents nodes which are holding state (registers and srams) */
abstract class Delay(clockNode: Update) extends Node {

  this.inputs.append(clockNode)

  def clock: Update = this.inputs(0).asInstanceOf[Update]

  def reset: Node

}

/** Storage nodes represented as registers. */
class RegDelay(clockNode: Update, nextNode: Node,
  initNode: Node = null, resetNode: Node = null,
  val depth: Int = 1) extends Delay(clockNode) {

  inferWidth = new WidthOf(0)

  val CLOCK_NEXT = 0
  val CLOCK_NEXT_INIT = 1
  val CLOCK_NEXT_INIT_RESET = 2

  this.inputs.append(nextNode)
  if( initNode != null ) this.inputs.append(initNode)
  if( resetNode != null ) this.inputs.append(resetNode)

  def next: Node = this.inputs(1)
  def init: Node  = if( this.inputs.length > 2 ) this.inputs(2) else null

  override def reset: Node  = if( this.inputs.length > 3 ) this.inputs(3) else null

  def isReset: Boolean = (reset != null)

  override def assigned: Node = next

  override def rvalue( value: Node ): Node = {
    if( inputs.length > 0 ) {
      inputs(0) = value
    } else {
      inputs.append(value)
    }
    this
  }

  def enable(): Node = {
    next match {
      case mux: MuxOp => mux.enable
      case _ => null
    }
  }

}

/** Storage nodes represented as srams. */
class MemDelay(clockNode: Update, resetNode: Node = null,
  val depth: Int = 1, isInlineP: Boolean = false) extends Delay(clockNode) {

  inferWidth = new FixedWidth(1 * depth) // XXX compute width.

  this.inputs.append(resetNode)

  override def reset: Node  = this.inputs(1)

  override def isInVCD = false

  def isInline = isInlineP

//XXX  def isInline = Module.isInlineMem || !reads.isEmpty

/* XXX
  val writes = ArrayBuffer[MemWrite]()
  val seqreads = ArrayBuffer[MemSeqRead]()
  val reads = ArrayBuffer[MemRead]()
  val readwrites = ArrayBuffer[MemReadWrite]()
 */

  val ports = new ArrayBuffer[MemAccess]()

  def writes(): Seq[MemWrite] = ports.filter(_.isInstanceOf[MemWrite]).map(
    _.asInstanceOf[MemWrite])
}


class ROMemDelay(inits: Seq[Node],
  clockNode: Update, resetNode: Node = null,
  depth: Int = 1, isInlineP: Boolean = false)
    extends MemDelay(clockNode, resetNode, depth, isInlineP) {

  this.inputs ++= inits
}


/** Reference to a location inside a state-holding register or sram.
  */
class MemReference(memN: MemDelay, addrN: Node) extends Node {

  inferWidth = new FixedWidth(log2Up(memN.depth))
  this.inputs.append(memN)
  this.inputs.append(addrN)

  def mem = this.inputs(0).asInstanceOf[MemDelay]
  def addr = this.inputs(1)

  override def rvalue( value: Node ): Node = {
    new MemWrite(this.mem, this.addr, value)
  }

}


/** Base class for memory ports (read, write, read/write)
  */
abstract class MemAccess(memi: MemDelay, addri: Node) extends Node {

  inputs += memi
  inputs += addri
  inferWidth = new FixedWidth(log2Up(mem.depth))
  memi.ports.append(this)

  def mem: MemDelay = inputs(0).asInstanceOf[MemDelay]
  def addr: Node = inputs(1)
  def cond: Node

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

  override def cond: Node = {
    val enable = addrReg.enable
    if( enable != null ) enable else Literal(1)
  }

  override def addr = if(inputs.length > 2) inputs(2) else null

/* XXX deprecated
  override def forceMatchingWidths = {
    val forced = addrReg.next.matchWidth(log2Up(mem.n))
    inputs += forced
    assert(addr == forced)
  }
 */

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "read"

/* XXX What is isInObject and isUsedByRam?
  override def isRamWriteInput(n: Node) = {
    addrReg.isEnable && addrReg.enableSignal == n || addr == n
  }
 */
}


/** Memory Write Port
  */
class MemWrite(mem: MemDelay, addri: Node, datai: Node,
  condi: Node = null, maski: Node = null) extends MemAccess(mem, addri) {

  this.inputs += datai
  this.inputs += (if( condi != null ) condi else Literal(1))
  if( maski != null ) this.inputs += maski

//XXX cannot override mutable variable  override def clock = mem.clock

  override def cond = inputs(3)

  def data = inputs(2)

  def mask = inputs(4)

  def isMasked: Boolean = inputs.length > 4


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

