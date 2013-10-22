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

trait IODirection;

object INPUT extends IODirection;

object OUTPUT extends IODirection;

object NODIRECTION extends IODirection;


// used for component to component connections
object Binding {

  def apply(m: Node, c: Module, ioComp: Module): Node = {
    if (Module.isEmittingComponents) {
      val res = c.findBinding(m);
      if (res == null) {
        val res = new IOBound()
        res.inputs.append(m)
        res.component = c;
/* XXX rewrite Binding
        c.nodes += res
        res.init("", widthOf(0), m);
        res.infer;
        c.bindings += res;
 */
        res
      } else {
        res;
      }
    } else {
      m
    }
  }
}

/** Pass through binding

  This kind of node is used to connect nodes accross module boundaries.
*/
class IOBound(var dir: IODirection = NODIRECTION,
              widthP: Int = -1,
              opand: Node = null)
    extends UnaryOp(opand) {

  width = widthP

  override def asDirectionless(): this.type = {
    dir = NODIRECTION
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

  def bind( target: IOBound ): this.type = {
    inputs.clear()
    inputs.append(target)
    this
  }

  override def flip(): this.type = {
    if (dir == INPUT) {
      dir = OUTPUT
    } else if(dir == OUTPUT) {
      dir = INPUT
    }
    this
  }

  def isDirectionless: Boolean = {
    return dir == NODIRECTION
  }

  def target: Node = inputs(0)

  def inferWidth(): Width = new WidthOf(0)

  override def toString: String = {
/* XXX
(if (dir == INPUT) "INPUT, "
        else if (dir == OUTPUT) "OUTPUT, " else "")
 */
    "IOBound(" + dir + "," + width + "," + (
      if( inputs.length > 0 ) target else "") + ")"
  }
}
