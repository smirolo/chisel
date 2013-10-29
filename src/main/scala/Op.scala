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

/** Base class for nodes that appear as operators in the generated graph.
  */
abstract class Op extends Node {
  val opSlug = "und"

  override def toString: String = {
    val res = new StringBuilder
    res.append(name + " = " + opSlug + "(")
    var sep = ""
    for( inp <- inputs ) {
      if( inp != null ) {
        res.append(sep + inp.name)
      } else {
        res.append(sep + "(null)")
      }
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


/** Logical Negation
*/
class LogicalNegOp(opand: Node) extends UnaryOp(opand) {
  override val opSlug = "!"

  override def inferWidth(): Width = new FixedWidth(1)
}


/** Sign reversal for integers
*/
class SignRevOp(opand: Node) extends UnaryOp(opand) {
  override val opSlug = "-"

  override def inferWidth(): Width = new WidthOf(0)
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


class RightShiftOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = ">>"

  override def inferWidth(): Width = new rshWidthOf(0, right)
}


class RightShiftSOp(left: Node, right: Node) extends RightShiftOp(left, right) {
}


class AddOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "+"

  def inferWidth(): Width = new maxWidth()
}


class AndOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "&"

  def inferWidth(): Width = new maxWidth()
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


class FillOp(opand: Node, val n: Int) extends UnaryOp(opand) {
  override val opSlug = "fill"

  override def inferWidth(): Width = new WidthOf(0)

  override def toString: String = name + "(" + inputs(0) + ", " + n + ")";
}


class DivOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "/"

  def inferWidth(): Width = new WidthOf(0)
}


class DivSOp(left: Node, right: Node) extends DivOp(left, right) {
  override val opSlug = "s/s"
}


class DivSUOp(left: Node, right: Node) extends DivOp(left, right) {
  override val opSlug = "s/u"
}


class DivUSOp(left: Node, right: Node) extends DivOp(left, right) {
  override val opSlug = "s/s"

  override def inferWidth(): Width = new WidthOf(0, -1)
}


class Log2Op(opand: Node, val nbits: Int) extends UnaryOp(opand) {
  override val opSlug = "log2"

  def inferWidth(): Width = new FixedWidth(nbits)
}


class MulOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "*"

  def inferWidth(): Width = new SumWidth()
}


class MulSOp(left: Node, right: Node) extends MulOp(left, right) {
  override val opSlug = "s*s"
}


class MulSUOp(left: Node, right: Node) extends MulOp(left, right) {
  override val opSlug = "s*s"

  override def inferWidth(): Width = new SumWidth(-1)
}


class MuxOp(cond: Node, thenNode: Node, elseNode: Node ) extends Op {
  override val opSlug = "Mux";

  inputs.append(cond)
  inputs.append(thenNode)
  inputs.append(elseNode)

  override def inferWidth(): Width = new WidthOf(0)

  Module.muxes += this;

  def ::(a: Node): MuxOp = { inputs(2) = a; this }

}


class RemOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "%"

  override def inferWidth(): Width = new minWidth()
}


class RemSOp(left: Node, right: Node) extends RemOp(left, right) {
  override val opSlug = "s%s"
}


class RemSUOp(left: Node, right: Node) extends RemOp(left, right) {
  override val opSlug = "s%u"

  override def inferWidth(): Width = new RemWidthOf(0, 1)
}


class RemUSOp(left: Node, right: Node) extends RemOp(left, right) {
  override val opSlug = "u%s"

  override def inferWidth(): Width = new RemWidthOf(1, 0)
}


class OrOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "|"

  override def inferWidth(): Width = new maxWidth()
}


/** Substraction operator
  */
class SubOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "-"

  override def inferWidth(): Width = new maxWidth()
}


class XorOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "^"

  override def inferWidth(): Width = new maxWidth()
}


/** Bus concatenation operator
  */
class CatOp(left: Node, right: Node) extends BinaryOp(left, right) {
  override val opSlug = "##"

  override def inferWidth(): Width = new SumWidth()
}


class LogicalOp(left: Node, right: Node) extends BinaryOp(left, right) {

  override def inferWidth(): Width = new maxToFixedWidth(1)
}

class EqlOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "=="
}


class NeqOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "!="
}


class GteOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = ">="
}


class GteSOp(left: Node, right: Node) extends GteOp(left, right) {
}


class GtrOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = ">"
}


class GtrSOp(left: Node, right: Node) extends GtrOp(left, right) {
}


class LtnOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "<"
}


class LtnSOp(left: Node, right: Node) extends LtnOp(left, right) {
}


class LteOp(left: Node, right: Node) extends LogicalOp(left, right) {
  override val opSlug = "<="
}


class LteSOp(left: Node, right: Node) extends LteOp(left, right) {
}

class LogicalAndOp(left: Node, right: Node) extends  LogicalOp(left, right) {
  override val opSlug = "&&"
}

class LogicalOrOp(left: Node, right: Node) extends  LogicalOp(left, right) {
  override val opSlug = "||"
}


class ReduceOp(opand: Node) extends UnaryOp(opand) {
  override def inferWidth(): Width = new FixedWidth(1)
}

class ReduceAndOp(opand: Node) extends ReduceOp(opand) {
  override val opSlug = "&"
}


class ReduceOrOp(opand: Node) extends ReduceOp(opand) {
  override val opSlug = "|"
}


class ReduceXorOp(opand: Node) extends ReduceOp(opand) {
  override val opSlug = "^"
}


// XXX      case "?"   => Multiplex(x, y, null);


