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

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import java.io.PrintStream


/** *Node* defines the root class of the class hierarchy for
  a [Composite Pattern](http://en.wikipedia.org/wiki/Composite_pattern).

  A digital logic graph is encoded as adjacency graph where instances
  of *Node* describe vertices and *inputs*, *consumers* member fields
  are used to traverse the directed graph respectively backward (from
  output to input) and forward (from input to output).
  */
abstract class Node extends nameable {

  /* The Strongly Connected Component this vertex belongs to. */
  var sccId: Int = 0

  /** XXX used in Module.visitNodes */
  var walked = false;

  /* Assigned in Binding and Mod.reset */
  var component: Module = Module.getComponent();
  var visitDepth = 0;
  def componentOf: Module = if (Module.isEmittingComponents && component != null) component else Module.topComponent
  var width = -1;
  var index = -1;
  var isFixedWidth = false;
  val consumers = new ArrayBuffer[Node]; // mods that consume one of my outputs
  val inputs = new ArrayBuffer[Node];

  /** XXX deprecated: */
  var isClkInput = false;

/* XXX deprecated?
  var stack: Array[StackTraceElement] = null;
 */

  var isScanArg = false
  var isPrintArg = false
  var prune = false
  var driveRand = false
  //XXX move to Delay
  var clock: Clock = null

  //XXX used in Verilog code generator:
  var isSigned: Boolean = false

  def assigned: Boolean = false

  def inferWidth(): Width

  def nameIt( path: String ): this.type = {
    if( !named ) {
      /* If the name was set explicitely through *setName*,
       we don't override it. */
      name = path;
    }
    this
  }

  def clearlyEquals(x: Node): Boolean = this == x

/* XXX deprecated
  // TODO: MOVE TO WIRE
  def assign(src: Node) {
    if (inputs.length > 0) {
      inputs(0) = src;
    } else {
      inputs += src;
    }
  }
 */
  def asDirectionless(): this.type = this

  def asInput(): this.type = this

  def asOutput(): this.type = this

  def flip(): this.type = this

  private var _isIo = false
  def isIo = _isIo
  def isIo_=(isIo: Boolean) = _isIo = isIo
  def isReg: Boolean = false;
  def isUsedByRam: Boolean = {
    for (c <- consumers)
      if (c.isRamWriteInput(this)) {
        return true;
      }
    return false;
  }
  def isRamWriteInput(i: Node): Boolean = false;

  def isInObject: Boolean =
    (isIo && (Module.isIoDebug || component == Module.topComponent)) ||
    Module.topComponent.debugs.contains(this) ||
    isReg || isUsedByRam || Module.isDebug || isPrintArg || isScanArg;

  def isInVCD: Boolean = width > 0 &&
    ((isIo && isInObject) || isReg || (Module.isDebug && !name.isEmpty))


  /** Returns the lvalue associated with the node */
  def lvalue(): Node = this


  /** Assign and returns the rvalue associated with the node */
  def rvalue( value: Node ): Node = {
    ChiselError.error("cannot assign to wire net")
    this
  }

  /** Prints all members of a node and recursively its inputs up to a certain
    depth level. This method is purely used for debugging. */
  def printTree(writer: PrintStream, depth: Int = 4, indent: String = ""): Unit = {
    if (depth < 1) return;
    writer.println(indent + getClass + " width=" + width + " #inputs=" + inputs.length);
    writer.println("sccId: " + sccId)
    writer.println("component: " + component)
    writer.println("depth: " + depth)
    writer.println("width: " + width)
    writer.println("index: " + index)
    writer.println("isFixedWidth: " + isFixedWidth)
    writer.println("consumers.length: " + consumers.length)
    writer.println("isClkInput: " + isClkInput)
    writer.println("isScanArg: " + isScanArg)
    writer.println("isPrintArg: " + isPrintArg)
    for (in <- inputs) {
      if (in == null) {
        writer.println("null");
      } else {
        in.printTree(writer, depth-1, indent + "  ");
      }
    }
  }

  def setName(n: String) {
    name = n
    named = true;
  }

  def setIsClkInput {};


  def addConsumers() {
    for ((i, off) <- inputs.zipWithIndex) {
      /* By construction we should not end-up with null inputs. */
      assert(i != null, ChiselError.error("input " + off
        + " of " + inputs.length + " for node " + this + " is null"))
      if(!i.consumers.contains(this)) {
        i.consumers += this;
      }
    }
  }

/** XXX good idea. deprecated?
  def extract (widths: Array[Int]): List[UInt] = {
    var res: List[UInt] = Nil;
    var off = 0;
    for (w <- widths) {
      res  = UInt(this)(off + w - 1, off) :: res;
      off += w;
    }
    res.reverse
  }

  def extract (b: Bundle): List[Node] = {
    var res: List[Node] = Nil;
    var off = 0;
    for ((n, io) <- b.flatten) {
      if (io.dir == OUTPUT) {
        val w = io.width;
        res  = Extract(UInt(this), UInt(off + w - 1), UInt(off)).node :: res;
        off += w;
      }
    }
    res.reverse
  }
  */

  override def toString(): String = {
    var sep = ""
    val str = new StringBuilder
    str.append(uniqueName + " = " + getClass.getName + "(")
    for( inp <- inputs ) {
      if( inp != null ) {
        str.append(sep + inp.uniqueName + "/*" + inp.getClass.getName
          + " in " + (if (inp.component != null) inp.component.getClass.getName
          else "?") + "*/")
      } else {
        str.append(sep + "null")
      }
      sep = ", "
    }
    str.append(")")
    str.toString
  }

  def uniqueName(): String = {
    if( !name.isEmpty ) name else ("T" + hashCode.toString)
  }
}
