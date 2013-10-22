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
import scala.math._


object MuxLookup {
  def apply[S <: UInt, T <: Bits](key: S, default: T, mapping: Seq[(S, T)])
    (implicit m: Manifest[T]): T
  = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }

}

object MuxCase {
  def apply[T <: Bits](default: T, mapping: Seq[(Bool, T)])
    (implicit m: Manifest[T]): T = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
}


object Mux {
  def apply[T <: Data](t: Bool, c: T, a: T)(implicit m: Manifest[T]): T = {
    val op =
      if( t.node.isInstanceOf[Literal] ) {
        if( t.node.asInstanceOf[Literal].value == 0 ) a.node else c.node
      } else if( c.node.isInstanceOf[Literal] && a.node.isInstanceOf[Literal]) {
        if (c.node.asInstanceOf[Literal].value
          == a.node.asInstanceOf[Literal].value) {
          c.node
        } else if (c.node.asInstanceOf[Literal].value == 1
          && a.asInstanceOf[Literal].value == 0) {
          /* special case where we can use the cond itself. */
          if(c.node.width == 1 && a.node.width == 1) {
            t.node
          } else {
            new CatOp(new FillOp(Literal(0,1),
              max(c.node.width-1, a.node.width-1)), t.node)
          }
        } else if (c.node.asInstanceOf[Literal].value == 0
          && a.node.asInstanceOf[Literal].value == 1) {
          /* special case where we can use the cond itself. */
          if(c.node.width == 1 && a.node.width == 1) {
            new BitwiseRevOp(t.node)
          } else {
            new CatOp(new FillOp(Literal(0,1),
              max(c.node.width-1, a.node.width-1)),
              new BitwiseRevOp(t.node))
          }
        } else {
          new MuxOp(t.node, c.node, a.node)
        }
      } else if (a.node.isInstanceOf[MuxOp]
        && c.node.clearlyEquals(a.node.inputs(1))) {
        new MuxOp(new LogicalOrOp(
          t.node, a.node.inputs(0)), c.node, a.node.inputs(2))
      } else {
        new MuxOp(t.node, c.node, a.node)
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}



object isLessThan {

  def distFromData(x: java.lang.Class[_]): Int = {
    var xClass = x
    var xCnt = 0
    while(xClass.toString != "class Chisel.Data") {
      xClass = xClass.getSuperclass
      xCnt += 1
    }
    xCnt
  }

  def checkCommonSuperclass(x: java.lang.Class[_], y: java.lang.Class[_]) {
  }

  def apply(x: java.lang.Class[_], y: java.lang.Class[_]): Boolean = {
    checkCommonSuperclass(x, y)
    distFromData(x) > distFromData(y)
  }
}
