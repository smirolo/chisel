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

class NotImplementedException extends Exception {
}

/** Base class for width inference algorithms.
  */
abstract class Width {

  /** Sets the width of a *node* based on its inputs.

   This method is called in forward topological traversal
   of the graph from inputs to outputs.
   */
  def forward(node: Node): Boolean

  /** Sets the width of a *node* based on its outputs.

   This method is called in backward topological traversal
   of the graph from outputs to inputs.
   */
  def backward(node: Node): Boolean

}

/** Fixed width of *width* bits.
  */
class FixedWidth(width: Int) extends Width {

  assert(width > 0);

  override def forward(node: Node): Boolean = {
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }
}


/** Width of an input labeled *index* + fixed *offset*
  */
class WidthOf(index: Int, offset: Int = 0) extends Width {

  override def forward(node: Node): Boolean = {
    /* IO nodes might or might not be connected. */
    val width = (if ( node.inputs.length > index )
      node.inputs(index).width + offset else node.width)
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }
}


/** Maximum width of all inputs + fixed *offset*
  */
class maxWidth(offset: Int = 0) extends Width {

  override def forward(node: Node): Boolean = {
/*XXX
    var width = 0
    for (i <- node.inputs)
      if (!(i == null || i == node)) {
        width = width.max(i.width)
      }
 */
    val width = node.inputs.map(_.width).max + offset
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
    // XXX match widths
    if (node.inputs(0).width != node.width) node.inputs(0).width = node.width
    if (node.inputs(1).width != node.width) node.inputs(1).width = node.width
    false
  }
}

/** Maximum width of all inputs saturated to a fixed *width*.
  */
class maxToFixedWidth(width: Int) extends Width {

  override def forward(node: Node): Boolean = {
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    // XXX match widths.
    val w = scala.math.max(node.inputs(0).width, node.inputs(1).width)
    if (node.inputs(0).width != w) node.inputs(0).width = w
    if (node.inputs(1).width != w) node.inputs(1).width = w
    false
  }
}


/** Minimum width of all inputs - fixed *offset*
  */
class minWidth(offset: Int = 0) extends Width {

  override def forward(node: Node): Boolean = {
    val width = node.inputs.map(_.width).min - offset
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }
}


/** Sum of the widths of all inputs + fixed *offset*
  */
class SumWidth(offset: Int = 0) extends Width {

  override def forward(node: Node): Boolean = {
    var width = offset;
    for (i <- node.inputs)
      width = width + i.width;
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }

}


/** XXX Left shift width
  */
class lshWidthOf(index: Int, n: Node) extends Width {

  override def forward(node: Node): Boolean = {
    val width = node.inputs(index).width + (1 << n.width)
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }
}


/** XXX Right shift width
  */
class rshWidthOf(index: Int, n: Node) extends Width {

  override def forward(node: Node): Boolean = {
    val width = node.inputs(index).width - (1 << n.width)
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }
}


/** XXX Right shift width
  */
class RemWidthOf(first: Int, second: Int) extends Width {

  override def forward(node: Node): Boolean = {
    val width = node.inputs(first).width.min(node.inputs(second).width - 1)
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }
}


/** XXX Right shift width
  */
class PrintfWidth(format: String, formats: String) extends Width {

  override def forward(node: Node): Boolean = {
    val argLength = formats.zip(node.inputs).map{
      case (a,b) => node.asInstanceOf[PrintfBase].lengths(a)(b.width)
    }.sum
    val width = 8*(format.length - 2*formats.length + argLength)
    val update = (node.width != width)
    if( update ) node.width = width
    update
  }

  override def backward(node: Node): Boolean = {
    throw new NotImplementedException
  }
}
