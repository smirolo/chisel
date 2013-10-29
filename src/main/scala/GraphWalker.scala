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

import scala.math._
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class CircleError extends Exception {
}


class GraphVisitor {

  def start( node: Node ): Unit = {}

  def backEdge( source: Node, target: Node ): Unit = {}

  def finish( node: Node ): Unit = {}
}



class NoCircleGraphVisitor extends GraphVisitor {

  override def backEdge( source: Node, target: Node ): Unit = { throw new CircleError() }

}


object GraphWalker {

  def anyNode(node: Node): Boolean = {
    /* Assignments are using Muxes with dangling pointers
     to construct a tree rooted with a default value. */
    node != null
  }

  /** Breath first traversal from a set of *roots*. The *visitor* will
    be applied to all vertices after the subtree rooted at it was traversed.
    */
  def breadthFirst(roots: Seq[Node], visitor: GraphVisitor,
    nodeFilter: (Node) => Boolean = anyNode ) {
    val queue = new scala.collection.mutable.Queue[Node]
    val visited = new HashSet[Node]

    for( root <- roots ) {
      queue.enqueue(root)
    }

    while( !queue.isEmpty ) {
      val top = queue.dequeue
      visited += top
      visitor.start(top)
      for( inp <- top.inputs ) {
        if( nodeFilter(inp) ) {
          if( !visited.contains(inp) ) {
            queue.dequeueFirst(_ == inp)
            queue.enqueue(inp)
          } else {
            // back edge
            visitor.backEdge(top, inp)
          }
        }
      }
      visitor.finish(top)
    }
  }


  /** Depth first traversal from a set of *roots*. The *visitor* will
    be applied to all vertices after the subtree rooted at it was traversed.
    */
  def depthFirst(roots: Seq[Node], visitor: GraphVisitor,
    nodeFilter: (Node) => Boolean = anyNode ) {

    val grey = 1
    val black = 2
    val color = new HashMap[Node, Int]

    def recursiveDepthFirst(root: Node, visitor: GraphVisitor,
      nodeFilter: (Node) => Boolean = anyNode ) {
      visitor.start(root)
      color.put(root, grey)
      for( inp <- root.inputs ) {
        if( nodeFilter(inp) ) {
          if( !color.contains(inp) ) {
            // unexplored "white" edge
            recursiveDepthFirst(inp, visitor, nodeFilter)
          } else if ( color(inp) == grey ) {
            // back edge
            visitor.backEdge(root, inp)
          } else {
            // forward or cross edge.
          }
        }
      }
      color.put(root, black)
      visitor.finish(root)
    }

    // Loop over all nodes of the graph to deal with disconnected components.
    for( node <- roots ) {
      if( !color.contains(node) ) {
        recursiveDepthFirst(node, visitor, nodeFilter)
      }
    }
  }


  /** Associate a strongly connected component id to each node in nodes,
    the set of all vertices in the graph.

    On return nodes will be partitionned into strongly connected components.
    The SCC a ``Node`` belongs to can be queried through its sccId field.
    */
  def tarjan(nodes: Seq[Node], visitor: (Node) => Unit,
    nodeFilter: (Node) => Boolean = anyNode ) {
    var sccId = 0
    var sccIndex = 0
    val stack = new Stack[Node]
    val index = new HashMap[Node, Int]
    val lowlink = new HashMap[Node, Int]

    def tarjanSCC(node: Node) {
      sccIndex += 1
      index.put(node, sccIndex)
      lowlink.put(node, sccIndex)
      stack.push(node)

      for( inp <- node.inputs ) {
        if( nodeFilter(inp) ) {
          if( !index.contains(inp) ) {
            // Successor has not yet been visited.
            tarjanSCC(inp)
            lowlink.put(node, min(lowlink(node), lowlink(inp)))
          } else if( stack.contains(inp) ) {
            lowlink.put(node, min(lowlink(node), index(inp)))
          }
        }
      }

      // If node is a root node, pop the stack and generate an SCC
      if(lowlink(node) == index(node)) {
        var top: Node = null
        sccId += 1
        do {
          top = stack.pop()
          top.sccId = sccId
//          println("XXX [tarjan:" + sccId + "] visit " + top)
          visitor(top)
        } while( !(node == top) )
      }
    }

    // Loop over all nodes of the graph to deal with disconnected components.
    for( node <- nodes ) {
      if( !index.contains(node) ) {
        tarjanSCC(node)
      }
    }
  }
}
