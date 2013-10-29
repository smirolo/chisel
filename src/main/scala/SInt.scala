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
import ChiselError._

object SInt {

  def apply(x: Int): SInt = SInt(Literal(x))
  def apply(x: Int, width: Int): SInt = SInt(Literal(x, width))

  def apply(dir: IODirection = null, width: Int = -1): SInt = {
    val res = new SInt()
    res.node = new IOBound(dir, width)
    res
  }

  def apply(node: Node): SInt = {
    val res = new SInt()
    res.node = node
    res
  }
}

class SInt extends Bits {

  type T = SInt;

/* XXX deprecated
  override def matchWidth(w: Int): Node = {
    if (w > this.width) {
      val topBit = Extract(this, this.width-1); topBit.infer
      val fill = NodeFill(w - this.width, topBit); fill.infer
      val res = CatOp(fill, this); res.infer
      res
    } else if (w < this.width) {
      val res = Extract(this, w-1,0); res.infer
      res
    } else {
      this
    }
  }
 */

  /** casting from UInt followed by assignment. */
  def :=(src: UInt): Unit = this := src.zext;

  def gen[T <: Bits](): T = SInt().asInstanceOf[T];

  // arithmetic operators
  def unary_-(): SInt = SignRev(this)
  def unary_!(): Bool = LogicalNeg(this)
  def << (right: UInt): SInt = LeftShift(this, right)
  def >> (right: UInt): SInt = RightShift(this, right)
// XXX deprecated  def ?  (b: SInt): SInt = newBinaryOp(b, "?");

  // order operators
  def >  (right: SInt): Bool = GtrS(this, right)
  def <  (right: SInt): Bool = LtnS(this, right)
  def <= (right: SInt): Bool = LteS(this, right)
  def >= (right: SInt): Bool = GteS(this, right)
  def !=  (right: UInt): Bool = this != right.zext;
  def >   (right: UInt): Bool = this > SInt(right.zext.node);
  def <   (right: UInt): Bool = this < SInt(right.zext.node);
  def >=  (right: UInt): Bool = this >= SInt(right.zext.node);
  def <=  (right: UInt): Bool = this <= SInt(right.zext.node);

  override def ===(right: Data): Bool = {
    right match {
      case right: UInt => UInt(this.node) === right.zext;
      case _ => super.===(right)
    }
  }

  //SInt to SInt arithmetic
  def +  (right: SInt): SInt = Add(this, right)
  def *  (right: SInt): SInt = MulS(this, right)
  def /  (right: SInt): SInt = DivS(this, right)
  def %  (right: SInt): SInt = RemS(this, right)
  def -  (right: SInt): SInt = Sub(this, right)

  //SInt to UInt arithmetic
  def +   (right: UInt): SInt = this + right.zext;
  def -   (right: UInt): SInt = this - right.zext;
  def *   (right: UInt): SInt = MulSU(this, right.zext)
  def /   (right: UInt): SInt = DivSU(this, right.zext)
  def %   (right: UInt): SInt = DivSU(this, right.zext)
  def abs: UInt = Mux(this < SInt(0), UInt((-this).node), UInt(this.node))
}


object SignRev {
  def apply(opand: Bits): SInt = {
    SInt(
      if( opand.isConst ) {
        Literal(-opand.node.asInstanceOf[Literal].value, opand.node.width)
      } else {
        new SignRevOp(opand.node)
      })
  }
}

object DivS {
  def apply[T <: SInt]( left: SInt, right: SInt): SInt = {
      SInt(new DivSOp(left.node, right.node))
  }
}

object DivSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new DivSUOp(left.node, right.node)
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object MulS {
  def apply[T <: SInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new MulSOp(left.node, right.node)
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object MulSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new MulSUOp(left.node, right.node)
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object RemS {
  def apply[T <: SInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemSOp(left.node, right.node)
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object RemSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new RemSUOp(left.node, right.node)
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object GteS {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Gte(left, right)
  }
}

object GtrS {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Gtr(left, right)
  }
}

object LteS {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Lte(left, right)
  }
}

object LtnS {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Ltn(left, right)
  }
}
