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
import Node._
import ChiselError._

/* backward compatibility */
object Bits {
  def apply(x: Int): UInt = UInt(x);
  def apply(x: Int, width: Int): UInt = UInt(x, width);
  def apply(x: String): UInt = UInt(x);
  def apply(x: String, width: Int): UInt = UInt(x, width);

  def apply(dir: IODirection = null, width: Int = -1): UInt = UInt(dir, width);
}


/** Base class for built-in Chisel types Bits and SInt. */
abstract class Bits(node: Node = new CondAssign()) extends Data(node) {

  Module.ioMap += ((this, Module.ioCount));
  Module.ioCount += 1;

  var dir: IODirection = null;
  /** width as inputed by the developper. */
  var width: Int = -1;

  def create(dir: IODirection, width: Int) {
    this.dir = dir;
    this.width = width;
  }

  /** Returns ``true`` when this Bits instance is bound to a ``Node``
    that generates a constant signal.
    */
  def isConst: Boolean = {
    node != null && node.isInstanceOf[Literal]
  }


  /** bind src as an input to node XXX Use BindingNode */
  def assign(src: Node) {
  }


  override def procAssign(src: Node) {
    if( node.isInstanceOf[CondAssign] ) {
      node.asInstanceOf[CondAssign].append(Node.genCond(), src)
    } else {
      ChiselError.error("reassignment to Node");
    }
  }

  //code generation stuff

  override def flatten: Array[(String, Bits)] = Array((name, this));

  override def toString: String = {
    // XXX We cannot print the width here as it would computed the infered
    // width, hence change the computations. It might be possible to print
    // width_ but it seems to also have some underlying computations associated
    // to it.
    var str = (
      "/*" + (if (name != null && !name.isEmpty) name else "?")
        + (if (component != null) (" in " + component) else "") + "*/ "
        + getClass.getName + "("
        + (if (dir == INPUT) "INPUT, "
        else if (dir == OUTPUT) "OUTPUT, " else "")
        + "width=" + width)
    str = str + "))"
    str
  }

  override def flip(): this.type = {
    assert(dir != null,
      ChiselError.error("Can't flip something that doesn't have a direction"))
    if (dir == INPUT) {
      dir = OUTPUT
    } else if(dir == OUTPUT) {
      dir = INPUT
    }
    this
  }

  override def asDirectionless(): this.type = {
    dir = null
    this
  }

  override def asInput(): this.type = {
    dir = INPUT
    this
  }

  override def asOutput(): this.type = {
    dir = OUTPUT
    this
  }

  override def isDirectionless: Boolean = {
    return dir == null
  }

  /* The <> operator bulk connects interfaces of opposite direction between
   sibling modules or interfaces of same direction between parent/child modules.

   Bulk connections connect leaf ports using pathname matching. Connections
   are only made if one of the ports is non-null, allowing users to repeatedly
   bulk-connect partially filled interfaces.

   After all connections are made and the circuit is being elaborated,
   Chisel warns users if ports have other than exactly one connection to them.
  */
  override def <>(right: Data): Unit = {
    right match {
      case other: Bits => this <> other;
      case _ => super.<>(right)
    }
  }

  def <>(other: Bits) {
    if (dir == INPUT) {
          if (other.dir == OUTPUT) {
            // input - output connections
            if(this.component == other.component) {
              // passthrough
              other assign this.node
            } else if (this.component.parent == other.component.parent) {
              // producer - consumer
              if(other.node.inputs.length > 0 ) {
                this assign other.node // only do assignment if output has stuff connected to it
              }
            } else {
              ChiselError.error({"Undefined connections between " + this + " and " + other})
            }
          } else if (other.dir == INPUT) {
            // input <> input conections
            if(this.component == other.component.parent) { // parent <> child
              other assign this.node
            } else if(this.component.parent == other.component) { //child <> parent
              this assign other.node
            } else {
              ChiselError.error({"Can't connect Input " + this + " Input " + other})
            }
          } else {
            // io <> wire
            if(this.component == other.component) { //internal wire
              other assign this.node
            } else if(this.component.parent == other.component) { //external wire
              this assign other.node
            } else {
              ChiselError.error({"Connecting Input " + this + " to " + other})
            }
          }
    } else if (dir == OUTPUT) {
          if (other.dir == INPUT) { // input - output connections
            if (this.component == other.component ) {
              // passthrough
              this assign other.node;
            } else if (this.component.parent == other.component.parent ) {
              // producer - consumer
              if(this.node.inputs.length > 0) {
                // only do connection if I have stuff connected to me
                other assign this.node;
              }
            } else {
              ChiselError.error({"Undefined connection between " + this + " and " + other})
            }
          } else if (other.dir == OUTPUT) { // output <> output connections
            if(this.component == other.component.parent) { // parent <> child
              if(other.node.inputs.length > 0) {
                // only do connection if child is assigning to that output
                this assign other.node
              }
            } else if (this.component.parent == other.component) { // child <> parent
              if(this.node.inputs.length > 0) {
                other assign this.node // only do connection if child (me) is assinging that output
              }
            } else {
              ChiselError.error({"Connecting Output " + this + " to Output " + other})
            }
          } else { // io <> wire
            if(this.component == other.component) { //output <> wire
              this assign other.node
            } else if(this.component.parent == other.component) {
              ChiselError.error({"Connecting Ouptut " + this + " to an external wire " + other})
            } else {
              ChiselError.error({"Connecting Output " + this + " to IO without direction " + other})
            }
          }
    } else {
          if (other.dir == INPUT) { // wire <> input
            if(this.component == other.component) {
              this assign other.node
            } else if(this.component == other.component.parent) {
              other assign this.node
            } else {
              ChiselError.error({"Undefined connection between wire " + this + " and input " + other})
            }
          } else if (other.dir == OUTPUT) { //wire <> output
            if(this.component == other.component) { // internal wire
              other assign this.node
            } else if(this.component == other.component.parent) { // external wire
              this assign other.node
            } else {
              ChiselError.error({"Undefined connection between wire " + this + " and output " + other})
            }
          } else {
            this assign other.node
          }
    }
  }

/*XXX
  override def setIsClkInput {
    isClkInput = true
    this assign clk
  }
 */
  override def clone: this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    res.width = this.width;
    res.dir = this.dir;
    res.node = this.node;
    res
  }

/*
  override def maxNum: BigInt = {
    if (inputs.length == 0) {
      width;
    } else if (inputs(0).isLit) {
      inputs(0).value
    } else if (inputs(0).litOf != null) {
      inputs(0).litOf.value
    } else if (inputs.length == 1 && inputs(0) != null) {
      inputs(0).maxNum
    } else {
      super.maxNum
    }
  }

  override def forceMatchingWidths {
    if(inputs.length == 1 && inputs(0).width != width) {
      inputs(0) = inputs(0).matchWidth(width)
    }
  }
 */

  /** Assignment operator.

    The assignment operator can be called multiple times
   */
  def :=(src: Bits): Unit = {
    this procAssign src.node;
  }


  // bitwise operators
  // =================

  /** Extract a single Bool at index *bit*.
    */
  final def apply(bit: Int): Bool = Extract(this, UInt(bit))
  final def apply(bit: UInt): Bool = Extract(this, bit)

  /** Extract a range of bits */
  final def apply(hi: Int, lo: Int): UInt = Extract(this, UInt(hi), UInt(lo))
  final def apply(hi: UInt, lo: UInt): UInt = Extract(this, hi, lo)

/** can't define apply(range: (UInt, UInt)) because it gets same
  signature after type erasure. */
  final def apply(range: (Int, Int)): UInt = this(range._1, range._2)

  // to support implicit convestions
  override def ===(right: Data): Bool = {
    right match {
      case bits: Bits => EqlOp(this, bits)
      case _ => this === right.toBits
    }
  }

  def unary_~(): UInt = BitwiseRev(this)
  def andR(): Bool = ReduceAndOp(this)
  def orR(): Bool = ReduceOrOp(this)
  def xorR(): Bool = ReduceXorOp(this)
  def != (right: Bits): Bool = NeqOp(this, right)
  def & (right: Bits): this.type = AndOp(this, right)
  def | (right: Bits): this.type = OrOp(this, right)
  def ^ (right: Bits): this.type = XorOp(this, right)

  def ##(right: Bits): UInt = CatOp(this, right)
}


