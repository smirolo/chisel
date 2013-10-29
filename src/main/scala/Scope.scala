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

import scala.collection.mutable.Stack


/** The programming model to construct a circuit is equivalent
  to a recursive descent parser from a top ``Module``.

  As such, scopes are pushed and popped along the way.
  */
class Scope {

  /** defines the Scope of conditions set through *when* calls.
    Each tuple contains the condition typed condition node and
    a boolean that indicates if the condition is part of
    an otherwise clause. */
  val conds = new Stack[(Bool, Boolean)]();

  def genCond(): Node = conds.top._1.node;

  def topCond: Bool = conds.top._1

  /** Returns true if the condition can be used as a default value. */
  def isDefaultCond(): Boolean = {
    /* true is the conditional at the top of the stack is bound
     to an 'always true' variable and false otherwise. */
    genCond() match {
      case lit: Literal => lit.value == 1
      case _ =>
        /* We only have Bool(true) and an otherwise condition on the stack. */
        conds.length == 2 && conds.top._2
    }
  }

  /** defines the Scope of Bits set by *switch* call and used for *is* */
  val keys = new Stack[Bits]();

  val clocks = new Stack[Clock]();

  def clk: Clock = clocks.top

  conds.push((Bool(true), true));
  clocks.push((new Clock).nameIt("clk"))
}
