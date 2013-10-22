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


/** Base class for width inference algorithms.
  */
abstract class Width {

  def forward(node: Node): Boolean
  def backward(node: Node): Boolean

}

/** Fixed width
  */
class FixedWidth(width: Int) extends Width {

  override def forward(node: Node): Boolean = {
    assert(width > 0);
    node.width = width
    false
  }

  override def backward(node: Node): Boolean = {
    assert(width > 0);
    node.width = width
    false
  }
}


/** Width of an input specified by index
  */
class WidthOf(index: Int, offset: Int = 0) extends Width {

  override def forward(node: Node): Boolean = {
    assert( node.inputs.length > 0 )
    node.width = node.inputs(index).width - offset
    false
  }

  override def backward(node: Node): Boolean = {
    // XXX only one output so far.
    node.inputs(0).width = node.width
    false
  }
}


/** Maximum width of all inputs
  */
class maxWidth extends Width {

  override def forward(node: Node): Boolean = {
    var width = 0
    for (i <- node.inputs)
      if (!(i == null || i == node)) {
        width = width.max(i.width)
      }
    node.width = width
    false
  }

  override def backward(node: Node): Boolean = {
    // XXX match widths
    if (node.inputs(0).width != node.width) node.inputs(0).width = node.width
    if (node.inputs(1).width != node.width) node.inputs(1).width = node.width
    false
  }
}

class maxToFixedWidth(width: Int) extends Width {

  override def forward(node: Node): Boolean = {
    node.width = width
    false
  }

  override def backward(node: Node): Boolean = {
    // XXX match widths.
    val w = scala.math.max(node.inputs(0).width, node.inputs(1).width)
    if (node.inputs(0).width != w) node.inputs(0).width = w
    if (node.inputs(1).width != w) node.inputs(1).width = w
    false
  }
}


/** Width of an input specified by index
  */

class minWidth extends Width {

  override def forward(node: Node): Boolean = {
    node.width = node.inputs.map(_.width).min
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }
}


class maxWidthPlusOne extends Width {

  override def forward(node: Node): Boolean = {
    node.width = node.inputs.map(_.width).max + 1
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }
}


class SumWidth(offset: Int = 0) extends Width {

  override def forward(node: Node): Boolean = {
    var res = 0;
    for (i <- node.inputs)
      res = res + i.width;
    node.width = res + offset
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }

}


class lshWidthOf(i: Int, n: Node) extends Width {

  override def forward(node: Node): Boolean = {
    node.width = node.inputs(i).width + (1 << n.width)
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }
}


class rshWidthOf(i: Int, n: Node) extends Width {

  override def forward(node: Node): Boolean = {
    node.width = node.inputs(i).width - (1 << n.width)
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }
}


class RemWidthOf(first: Int, second: Int) extends Width {

  override def forward(node: Node): Boolean = {
    node.width = node.inputs(first).width.min(node.inputs(second).width - 1)
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }
}


class PrintfWidth(format: String, formats: String) extends Width {
  override def forward(node: Node): Boolean = {
    val argLength = formats.zip(node.inputs).map{
      case (a,b) => node.asInstanceOf[PrintfBase].lengths(a)(b.width)
    }.sum
    8*(format.length - 2*formats.length + argLength)
    false
  }

  override def backward(node: Node): Boolean = {
    false
  }
}
