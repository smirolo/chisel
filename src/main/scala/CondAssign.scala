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

/** This Node represents conditional assignments.

  It is a little special in that it can only be fuly evaluated
  at the elaboration phase.
  */
class CondAssign extends Node {
  // XXX Shouldn't be Node
  def inferWidth(): Width = new FixedWidth(1)

  var updates = new collection.mutable.ListBuffer[(Node, Node)];

  private def genMuxes(default: Node, others: Seq[(Node, Node)]): Unit = {
    val update = others.foldLeft(default)((v, u) => new MuxOp(u._1, u._2, v))
    if (inputs.isEmpty) inputs += update else inputs(0) = update
  }

  def genMuxes(): Unit = {
    if (inputs.length == 0 || inputs(0) == null) {
      ChiselError.error({"NO UPDATES ON " + this}, this.line)
    } else {
      val (topCond, topValue) = updates.head
      val (lastCond, lastValue) = updates.last
      if (!topCond.boundToTrue && !lastCond.canBeUsedAsDefault) {
        ChiselError.error(
          {"NO DEFAULT SPECIFIED FOR WIRE: " + this + " in component " + this.component.getClass},
          this.line)
      } else {
        if (topCond.boundToTrue)
          genMuxes(topValue, updates.toList.tail)
        else if (lastCond.canBeUsedAsDefault)
          genMuxes(lastValue, updates)
      }
    }
  }

  /** Add another update path. */
  def append( cond: Node, value: Node ) {
    updates += ((cond, value))
  }

  Module.procs += this;
}
