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

import Reg._
import ChiselError._
import scala.reflect._

object Reg {

/* XXX
  def regMaxWidth(m: Node) =
    if (isInGetWidth) {
      throw new Exception("getWidth was called on a Register or on an object connected in some way to a Register that has a statically uninferrable width")
    } else {
      maxWidth(m)
    }

  // Rule: If no width is specified, use max width. Otherwise, use the specified width.
  def regWidth(w: Int) =
    if(w <= 0) {
      regMaxWidth _ ;
    } else {
      fixWidth(w)
    }
 */
  /** Rule: if r is using an inferred width, then don't enforce a width. If it is using a user inferred
    width, set the the width

    XXX Can't specify return type. There is a conflict. It is either
    (Node) => (Int) or Int depending which execution path you believe.
    
  def regWidth(r: Node) = {
    val rLit = r.litOf
    if (rLit != null && rLit.hasInferredWidth) {
      regMaxWidth _
    } else {
      fixWidth(r.getWidth)
    }
  }
*/
  def validateGen[T <: Data](gen: => T) {
//XXX        throwException("Invalid Type Specifier for Reg")
  }

  /** *type_out* defines the data type of the register when it is read.
    *update* and *reset* define the update and reset values
    respectively.
    */
  def apply[T <: Data](outType: T = null, next: T = null, init: T = null,
    clock: Clock = Module.scope.clock, reset: Bool = Module.scope.reset): T = {
    var mType = outType
    if(mType == null) {
      mType = next
    }
    if(mType == null) {
      mType = init
    }
    if(mType == null) {
      throw new Exception("cannot infer type of Reg.")
    }

    val gen = mType.clone
    validateGen(gen)

    val d: Array[(String, Bits)] =
      if(next == null) {
        gen.flatten.map{case(x, y) => (x -> null)}
      } else {
        next.flatten
      }

    // asOutput flip the direction and returns this.
    val res = gen.asOutput
    if(init != null) {
      for((((res_n, res_i), (data_n, data_i)), (rval_n, rval_i))
        <- res.flatten zip d zip init.flatten) {
        assert(rval_i.getWidth > 0,
          {ChiselError.error("Negative width to wire " + res_i)})
        val reg = new RegDelay(
          clock.node.asInstanceOf[Update],
          if( data_i != null ) data_i.node else null,
          if( rval_i != null ) rval_i.node else null,
          reset.node)
        reg.inferWidth = res_i.node.inferWidth
        res_i.node = reg
      }
    } else {
      for(((res_n, res_i), (data_n, data_i)) <- res.flatten zip d) {
        val reg = new RegDelay(
          clock.node.asInstanceOf[Update],
          if( data_i != null ) data_i.node else null,
          null,
          reset.node)
        reg.inferWidth = res_i.node.inferWidth
        res_i.node = reg
      }
    }
    res
  }

  /* Without this method, the scala compiler is not happy
   when we declare registers as Reg(signal). */
  def apply[T <: Data](outType: T): T = Reg[T](outType, null.asInstanceOf[T], null.asInstanceOf[T])
}


object RegNext {

  def apply[T <: Data](next: T): T = Reg[T](next, next, null.asInstanceOf[T])

  def apply[T <: Data](next: T, init: T): T = Reg[T](next, next, init)

}

object RegInit {

  def apply[T <: Data](init: T): T = Reg[T](init, null.asInstanceOf[T], init)

}

