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
import scala.math.max
import scala.reflect.Manifest
import Node._
import Literal._


/** Base class for nodes that appear as operators in the generated graph.
  */
abstract class Op extends Node {
  val opSlug = "und"

  override def toString: String = {
    val res = new StringBuilder
    res.append(name + " = " + opSlug + "(")
    var sep = ""
    for( inp <- inputs ) {
      res.append(sep + inp.name)
      sep = ","
    }
    res.append(")")
    res.toString
  }
}


/** Base class for nodes that appear as unary operators in the generated graph.
  */
abstract class UnaryOp(opandNode: Node) extends Op {
  this.inputs.append(opandNode)

  def opand: Node = this.inputs(0)
}

/** Bitwise reverse
*/
class BitwiseRevOp(opand: Node) extends UnaryOp(opand) {
  override val opSlug = "~"

  def inferWidth(): Width = new WidthOf(0)
}

object BitwiseRev {
  def apply(opand: Bits): UInt = {
    UInt(
      if( opand.isConst ) {
        Literal((-opand.node.asInstanceOf[Literal].value - 1)
          & ((BigInt(1) << opand.node.width) - 1),
          opand.node.width)
      } else {
        new BitwiseRevOp(opand.node)
      })
  }
}


/** Logical Negation
*/
class LogicalNegOp(opand: Node) extends UnaryOp(opand) {
  override val opSlug = "!"

  override def inferWidth(): Width = new FixedWidth(1)
}

object LogicalNeg {
  def apply( opand: Bits): Bool = {
    Bool(
      if( opand.isConst ) {
        if( opand.node.asInstanceOf[Literal].value == 0) Literal(1)
        else Literal(0)
      } else {
        new LogicalNegOp(opand.node)
      })
  }
}

/** Sign reversal for integers
*/
class SignRevOp(opand: Node) extends UnaryOp(opand) {
  override val opSlug = "-"

  override def inferWidth(): Width = new WidthOf(0)
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


/** Base class for binary operators
  */
abstract class BinaryOp(left: Node, right: Node) extends Op {
  this.inputs.append(left)
  this.inputs.append(right)
}

class LeftShiftOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "<<"

  override def inferWidth(): Width = new lshWidthOf(0, right)
}

object LeftShiftOp {
  def apply[T <: Bits](left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          << right.node.asInstanceOf[Literal].value.toInt,
          left.node.width + right.node.width)
      } else {
        new LeftShiftOp(left.node, right.node)
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


class RightShiftOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = ">>"

  override def inferWidth(): Width = new rshWidthOf(0, right)
}


class RightShiftSOp(left: Node, right: Node) extends RightShiftOp(left, right) {
}

object RightShift {
  def apply[T <: Bits](left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        if( left.isInstanceOf[UInt] ) {
          Literal(left.node.asInstanceOf[Literal].value
            >> right.node.asInstanceOf[Literal].value.toInt,
            left.node.width - right.node.width)
        } else {
          /* XXX BigInt signed right shift? */
          Literal(left.node.asInstanceOf[Literal].value
            >> right.node.asInstanceOf[Literal].value.toInt,
            left.node.width - right.node.width)
        }
      } else {
        if( left.isInstanceOf[UInt] ) {
          new RightShiftOp(left.node, right.node)
        } else {
          new RightShiftSOp(left.node, right.node)
        }
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class AddOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "+"

  def inferWidth(): Width = new maxWidth()
}

object AddOp {
  def apply[T <: Bits]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          + right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width) + 1) // XXX does not always need carry.
      } else {
        new AddOp(left.node, right.node)
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class AndOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "&"

  def inferWidth(): Width = new maxWidth()
}

object AndOp {
  def apply[T <: Bits](left: Bits, right: Bits)(implicit m: Manifest[T]): T = {
    val op = if( left.isConst && right.isConst ) {
      Literal(left.node.asInstanceOf[Literal].value
        & right.node.asInstanceOf[Literal].value,
        max(left.node.width, right.node.width))
    } else {
      new AndOp(left.node, right.node)
    }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


class ExtractOp(opand: Node, hiBit: Node, loBit: Node) extends Op {
  override val opSlug = "extract"

  override def inferWidth(): Width = new WidthOf(0)

  this.inputs.append(opand)
  this.inputs.append(hiBit)
  this.inputs.append(loBit)

  def hi: Node = this.inputs(1)
  def lo: Node = this.inputs(2)

  override def toString: String =
    (inputs(0) + "(" +  (if (hi == lo) "" else (", " + hi)) + ", " + lo + ")")
}

object Extract {

  def apply(opand: Bits, bit: UInt): Bool = Bool(apply(opand, bit, bit).node)

  // extract bit range
  def apply(opand: Bits, hi: Bits, lo: Bits): UInt = {
    UInt(
      if( opand.isConst && hi.isConst && lo.isConst ) {
        val w = if (opand.node.width == -1) (
          hi.node.asInstanceOf[Literal].value.toInt
            - lo.node.asInstanceOf[Literal].value.toInt + 1) else opand.node.width;
        Literal((opand.node.asInstanceOf[Literal].value
          >> lo.node.asInstanceOf[Literal].value.toInt)
          & ((BigInt(1) << w) - BigInt(1)), w)
      } else if( opand.isConst ) {
        val rsh = new RightShiftOp(opand.node, lo.node)
        val hiMinusLoPlus1 = new AddOp(
          new SubOp(hi.node, lo.node), Literal(1))
        val mask = new SubOp(
          new LeftShiftOp(Literal(1), hiMinusLoPlus1), Literal(1))
        new AndOp(rsh, mask)
      } else {
        new ExtractOp(opand.node, hi.node, lo.node)
      })
  }
}


class FillOp(opand: Node, val n: Int) extends UnaryOp(opand) {
  override val opSlug = "fill"

  override def inferWidth(): Width = new WidthOf(0)

  override def toString: String = name + "(" + inputs(0) + ", " + n + ")";
}


object Fill {

  def apply(n: Int, opand: Bits): UInt = apply(opand, n)

  def apply(opand: Bits, n: Int): UInt = {
    UInt(
      if( opand.isConst ) {
        var c = BigInt(0)
        val w = opand.node.width
        val a = opand.node.asInstanceOf[Literal].value
        for (i <- 0 until n)
          c = (c << w) | a
        Literal(c, n * w)
      } else if( n == 1 ) {
        opand.node
      } else {
        new FillOp(opand.node, n)
      })
  }
}

class DivOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "/"

  def inferWidth(): Width = new WidthOf(0)
}

object Div {
  def apply( left: UInt, right: UInt): UInt = {
    UInt(new DivOp(left.node, right.node))
  }
}

class DivSOp(left: Node, right: Node) extends DivOp(left, right) {
  override val opSlug = "s/s"
}

object DivSOp {
  def apply[T <: SInt]( left: SInt, right: SInt): SInt = {
      SInt(new DivSOp(left.node, right.node))
  }
}

class DivSUOp(left: Node, right: Node) extends DivOp(left, right) {
  override val opSlug = "s/u"
}


object DivSUOp {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new DivSUOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class DivUSOp(left: Node, right: Node) extends DivOp(left, right) {
  override val opSlug = "s/s"

  override def inferWidth(): Width = new WidthOf(0, -1)
}

object DivUS {
  def apply[T <: SInt]( left: UInt, right: T)(implicit m: Manifest[T]): T = {
    val op = new DivUSOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class Log2Op(opand: Node, val nbits: Int) extends UnaryOp(opand) {
  override val opSlug = "log2"

  def inferWidth(): Width = new FixedWidth(nbits)
}

object Log2 {
  def apply (opand: Bits, n: Int): UInt = {
    UInt(new Log2Op(opand.node, sizeof(n-1)))
  }
}


class MulOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "*"

  def inferWidth(): Width = new SumWidth()
}

object MulOp {
  def apply[T <: UInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new MulOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class MulSOp(left: Node, right: Node) extends MulOp(left, right) {
  override val opSlug = "s*s"
}

object MulSOp {
  def apply[T <: SInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new MulSOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class MulSUOp(left: Node, right: Node) extends MulOp(left, right) {
  override val opSlug = "s*s"

  override def inferWidth(): Width = new SumWidth(-1)
}

object MulSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new MulSUOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


class MuxOp(cond: Node, thenNode: Node, elseNode: Node ) extends Op {
  override val opSlug = "Mux";

  override def inferWidth(): Width = new WidthOf(0)

  Module.muxes += this;

  def ::(a: Node): MuxOp = { inputs(2) = a; this }

}


class RemOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "%"

  override def inferWidth(): Width = new minWidth()
}

object Rem {
  def apply[T <: UInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class RemSOp(left: Node, right: Node) extends RemOp(left, right) {
  override val opSlug = "s%s"
}

object RemSOp {
  def apply[T <: SInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemSOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class RemSUOp(left: Node, right: Node) extends RemOp(left, right) {
  override val opSlug = "s%u"

  override def inferWidth(): Width = new RemWidthOf(0, 1)
}

object RemSUOp {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new RemSUOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class RemUSOp(left: Node, right: Node) extends RemOp(left, right) {
  override val opSlug = "u%s"

  override def inferWidth(): Width = new RemWidthOf(1, 0)
}

object RemUS {
  def apply[T <: SInt]( left: UInt, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemUSOp(left.node, right.node)
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class OrOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "|"

  override def inferWidth(): Width = new maxWidth()
}

object OrOp {
  def apply[T <: Bits](left: Bits, right: Bits)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          | right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width))
      } else {
        new OrOp(left.node, right.node)
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

/** Substraction operator
  */
class SubOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "-"

  override def inferWidth(): Width = new maxWidth()
}

object SubOp {
  def apply[T <: Bits]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          - right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width) + 1) // XXX unnecessary carry.
      } else {
        new SubOp(left.node, right.node)
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

class XorOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "^"

  override def inferWidth(): Width = new maxWidth()
}

object XorOp {
  def apply[T <: Bits](left: Bits, right: Bits)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          ^ right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width))
      } else {
        new XorOp(left.node, right.node)
      }
    val result = m.erasure.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

/** Bus concatenation operator
  */
class CatOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "##"

  override def inferWidth(): Width = new SumWidth()
}


object CatOp {
  def apply(left: Bits, right: Bits): UInt = {
    UInt(
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value << right.node.width
          | right.node.asInstanceOf[Literal].value,
          left.node.width + right.node.width)
      } else {
        new CatOp(left.node, right.node)
      })
  }
}


class LogicalOp(left: Node, right: Node) extends BinaryOp(left, right) {

  override def inferWidth(): Width = new maxToFixedWidth(1)
}

class EqlOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "=="
}

object EqlOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          == right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new EqlOp(left.node, right.node)
      })
  }
}


class NeqOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "!="
}

object NeqOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          != right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new NeqOp(left.node, right.node)
      })
  }
}


class GteOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = ">="
}

object GteOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          >= right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new GteOp(left.node, right.node)
      })
  }
}

class GteSOp(left: Node, right: Node) extends GteOp(left, right) {
}

object GteSOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    GteOp(left, right)
  }
}


class GtrOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = ">"
}

object GtrOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          > right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new GtrOp(left.node, right.node)
    })
  }
}

class GtrSOp(left: Node, right: Node) extends GtrOp(left, right) {
}

object GtrSOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    GtrOp(left, right)
  }
}

class LtnOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "<"
}

object LtnOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          < right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new LtnOp(left.node, right.node)
      })
  }
}

class LtnSOp(left: Node, right: Node) extends LtnOp(left, right) {
}

object LtnSOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    LtnOp(left, right)
  }
}


class LteOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "<="
}

object LteOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(if( left.isConst && right.isConst ) {
      Literal(if (left.node.asInstanceOf[Literal].value
        <= right.node.asInstanceOf[Literal].value) 1 else 0)
    } else {
      new LteOp(left.node, right.node)
    })
  }
}

class LteSOp(left: Node, right: Node) extends LteOp(left, right) {
}

object LteSOp {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    LteOp(left, right)
  }
}

class LogicalOrOp(left: Node, right: Node) extends  LogicalOp(left, right) {
  override val opSlug = "||"
}

object LogicalOrOp {
  def apply( left: Bits, right: Bits): Bool = {
    Bool(
      if (left.isConst) {
        if( left.node.asInstanceOf[Literal].value > 0 ) {
          left.node // alias to true
        } else {
          right.node
        }
      } else if( right.isConst ) {
        if( right.node.asInstanceOf[Literal].value > 0 ) {
          right.node // alias to true
        } else {
          left.node
        }
      } else {
        new LogicalOrOp(left.node, right.node)
      })
  }
}


class LogicalAndOp(left: Node, right: Node) extends  LogicalOp(left, right) {
  override val opSlug = "&&"
}

object LogicalAndOp {
  def apply( left: Bits, right: Bits): Bool = {
    if(Module.searchAndMap
      && Module.chiselAndMap.contains((left, right))) {
      Module.chiselAndMap((left, right))
    }
    val op = {
      if (left.isConst) {
        if( left.node.asInstanceOf[Literal].value > 0 ) {
          right.node
        } else {
          left.node // alias to false
        }
      } else if( right.isConst ) {
        if( right.node.asInstanceOf[Literal].value > 0 ) {
          left.node
        } else {
          right.node // alias to true
        }
      } else {
        new LogicalAndOp(left.node, right.node)
      }
    }
    val result = Bool(op)
    if(Module.searchAndMap && !Module.chiselAndMap.contains((left, right))) {
      Module.chiselAndMap += ((left, right) -> result)
    }
    result
  }
}


class ReduceOp(opand: Node) extends UnaryOp(opand) {
  override def inferWidth(): Width = new FixedWidth(1)
}

class ReduceAndOp(opand: Node) extends ReduceOp(opand) {
  override val opSlug = "&"
}

object ReduceAndOp {
  def apply[T <: Bits](opand: T): Bool = {
    val op = new ReduceAndOp(opand.node)
    Bool(op)
  }
}

object andR {
    def apply(x: Bits): Bool = ReduceAndOp(x)
}

class ReduceOrOp(opand: Node) extends ReduceOp(opand) {
  override val opSlug = "|"
}

object ReduceOrOp {
  def apply[T <: Bits](opand: T): Bool = {
    val op = new ReduceOrOp(opand.node)
    Bool(op)
  }
}

object orR {
    def apply(x: Bits): Bool = ReduceOrOp(x)
}


class ReduceXorOp(opand: Node) extends ReduceOp(opand) {
  override val opSlug = "^"
}

object ReduceXorOp {
  def apply[T <: Bits](opand: T): Bool = {
    val op = new ReduceXorOp(opand.node)
    Bool(op)
  }
}

object xorR {
    def apply(x: Bits): Bool = ReduceXorOp(x)
}



// XXX      case "?"   => Multiplex(x, y, null);


