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

/** *Data* is part of the *Node* Composite Pattern class hierarchy.
  It is the root of the type system which includes composites (Bundle, Vec)
  and atomic types (UInt, SInt, etc.).

  Instances of Data are meant to help with construction and correctness
  of a logic graph. They will trimmed out of the graph before a *Backend*
  generates target code.

  Instances of Data are used to transport data types (UInt, SInt, etc)
  while executing the Scala program. These data types are used to create
  Nodes through dynamic dispatch.
  */
abstract class Data(var node: Node = null) extends nameable {

  /* Component this AST Data Type belongs to. We use it
   in the <> operator to bind nodes. */
  val component: Module = Module.getComponent();

  def nameIt(name: String) {}

  def getWidth(): Int = node.getWidth()

  // 2 following: Interface required by Vec:
  def ===(right: Data): Bool = {
    if(this.getClass != right.getClass) {
      ChiselError.error("=== not defined on " + this.getClass
        + " and " + right.getClass);
    }
    Bool()
  }

  def toBits(): UInt = {
    ChiselError.error("toBits not defined on " + this.getClass)
    UInt()
  }

/*
  def toBool(): Bool = {
    if(this.getWidth > 1) {
      throw new Exception("multi bit signal " + this + " converted to Bool");
    }
    if(this.getWidth == -1) {
      throw new Exception("unable to automatically convert " + this + " to Bool, convert manually instead")
    }
    chiselCast(this){Bool()};
  }

  // Interface required by Cat:
  def ##[T <: Data](right: T): this.type = {
    throw new Exception("## not defined on " + this.getClass + " and " + right.getClass)
  }

  def apply(name: String): Data = null
 */

  def flatten: Array[(String, Bits)] = Array[(String, Bits)]();

  /** Flips the direction (*dir*) of instances derived from INPUT
    to OUTPUT or OUTPUT to INPUT respectively for Bits
    and recursively for Bundle/VecT.

    Returns this instance with its exact type.
    */
  def flip(): this.type = this;

  /** Sets the direction (*dir*) of instances derived from Bits to INPUT
    or recursively sets members of Bundle/Vec to INPUT.

    Returns this instance with its exact type.
    */
  def asInput(): this.type = this;

  /** Sets the direction (*dir*) of instances derived from Bits to OUTPUT
    or recursively sets members of Bundle/Vec to OUTPUT.

    Returns this instance with its exact type.
    */
  def asOutput(): this.type

  def asDirectionless(): this.type

  def isDirectionless: Boolean = true;

  def procAssign(src: Node): Unit = {
    if(this.getClass != src.getClass) {
      ChiselError.error("procAssign not defined on " + this.getClass
        + " and " + src.getClass);
    }
  }

  def :=(data: Data): Unit = {
    if(this.getClass != data.getClass) {
      ChiselError.error(":= not defined on " + this.getClass
        + " and " + data.getClass);
    }
// XXX fix later:    this procAssign data.node;
  }

  override def clone(): this.type = {
    try {
      val constructor = this.getClass.getConstructors.head
      val res = constructor.newInstance(Array.fill(constructor.getParameterTypes.size)(null):_*)
      res.asInstanceOf[this.type]
    } catch {
      case npe: java.lang.reflect.InvocationTargetException if npe.getCause.isInstanceOf[java.lang.NullPointerException] =>
        throwException("Parameterized Bundle " + this.getClass + " needs clone method. You are probably using an anonymous Bundle object that captures external state and hence is un-cloneable", npe)
      case e: java.lang.Exception =>
        throwException("Parameterized Bundle " + this.getClass + " needs clone method", e)
    }
  }

/*
  def setWidth(w: Int) {
    this.width = w;
  }
 */

  def ^^(src: Data): Unit = {}
  def <>(src: Data): Unit = {}
}

abstract class CompositeData extends Data {
}
