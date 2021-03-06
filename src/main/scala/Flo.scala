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
import scala.collection.mutable.ArrayBuffer
import scala.math._
import java.io.File;
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import Node._
import Reg._
import ChiselError._
import Literal._
import Module._
import scala.collection.mutable.HashSet

class FloBackend extends Backend {
  val keywords = new HashSet[String]();
  var isRnd = false

  override def emitDec(node: Node): String =
    emitRef(node) + " = "

  override def emitTmp(node: Node): String =
    emitRef(node)

  override def emitRef(node: Node): String = {
    if (node.litOf == null) {
      node match {
        case x: Literal =>
          "" + x.value

        case x: Binding =>
          emitRef(x.inputs(0))

        case x: Bits =>
          if (!node.isInObject && node.inputs.length == 1) emitRef(node.inputs(0)) else super.emitRef(node)

        case _ =>
          super.emitRef(node)
      }
    } else {
      "" + node.litOf.value
    }
  }

  def emit(node: Node): String = {
    node match {
      case x: Mux =>
        emitDec(x) + "mux " + emitRef(x.inputs(0)) + " " + emitRef(x.inputs(1)) + " " + emitRef(x.inputs(2)) + "\n"

      case o: Op =>
        emitDec(o) +
        (if (o.inputs.length == 1) {
          o.op match {
            case "~" => "not/" + node.inputs(0).width + " " + emitRef(node.inputs(0))
            case "!" => "not/" + node.inputs(0).width + " " + emitRef(node.inputs(0))
            case "-" => {
              "neg/" + node.inputs(0).width + " " + emitRef(node.inputs(0))
            }
          }
         } else {
           o.op match {
             case "<"  => "lt/"   + node.inputs(0).width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case ">=" => "gte/"  + node.inputs(0).width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "<=" => "gte/"  + node.inputs(0).width + " " + emitRef(node.inputs(1)) + " " + emitRef(node.inputs(0))
             case ">"  => "lt/"   + node.inputs(0).width + " " + emitRef(node.inputs(1)) + " " + emitRef(node.inputs(0))
             case "+"  => "add/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "-"  => "sub/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "*"  => "mul/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "!"  => "not/" + node.width + " " + emitRef(node.inputs(0))
             case "<<" => "lsh/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case ">>" => "rsh/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "##" => "cat/" + node.inputs(1).width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "|"  => "or "  + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "||" => "or "  + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "&"  => "and " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "&&" => "and " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "^"  => "xor " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "==" => "eq "  + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "!=" => "neq " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
           }
         }) + "\n"

      case x: Extract =>
        if (node.inputs.length == 2)
          emitDec(node) + "rsh/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1)) + "\n"     
        else
          emitDec(node) + "rsh/" + node.width + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(2)) + "\n"

      case x: Fill =>
        emitDec(x) + "fill/" + node.width + " " + emitRef(node.inputs(0)) + "\n"

      case x: Bits =>
        if (x.inputs.length == 1) {
          // println("NAME " + x.name + " DIR " + x.dir + " COMP " + x.componentOf + " TOP-COMP " + topComponent)
          if (x.dir == OUTPUT && x.componentOf == topComponent && x.consumers.length == 0)
            emitDec(x) + (if (isRnd) "eat" else ("out/" + x.width))  + " " + emitRef(x.inputs(0)) + "\n"
          else if (node.isInObject && node.litOf == null) {
            if (x.inputs(0).isInstanceOf[Literal])
              println("LIT IN OBJECT NAME " + x.name + " " + x.consumers.length + " CONSUMERS " + x.consumers(0))
            emitDec(x) + "mov " + emitRef(x.inputs(0)) + "\n"
          } else
            ""
          // println("--> NO CONSUMERS " + x + " = " + x.consumers.length);
          // ""
        } else
          emitDec(x) + (if (x.name == "reset") "rst" else ((if (isRnd) "rnd/" else "in/")) + x.width) + "\n"

      case m: Mem[_] =>
        emitDec(m) + "mem " + m.n + "\n"
        // emitDec(m) + "mem " + m.n + "\n" + trueAll(emitRef(m) + "__is_all_read", m.reads)

      case m: MemRead =>
        emitDec(m) + "ld " + emitRef(m.mem) + " " + emitRef(m.addr) + "\n" // emitRef(m.mem)
        // val w = r.mem.writes(0);
        // emitRef(r) + "__is_read = and " + emitRef(w) + "__write " + emitRef(r.cond) + "\n" +
        // emitDec(r) + "rd " + emitRef(r) + "__is_read" + " " + emitRef(r.mem) + " " + emitRef(r.addr) + "\n" 

      case m: MemWrite =>
        if (m.inputs.length == 2) 
          return ""
        emitDec(m) + "st " + emitRef(m.mem) + " " + emitRef(m.addr) + " " + emitRef(m.data) + "\n"
        // emitRef(w) + "__is_wr" + " = and " + emitRef(w.mem) + "__is_all_read" + " " + emitRef(w.cond) + "\n" + 
        // emitDec(w) + "wr " + emitRef(w) + "__is_wr" + " " + emitRef(w.mem) + " " + emitRef(w.addr) + " " + emitRef(w.data) + "\n" +
        // emitRef(w) + "__write0 = reg 1 " + emitRef(w) + "\n" +
        // emitRef(w) + "__write = or 1 " + emitRef(w) + "__write0" + "\n"       

      case x: Reg => // TODO: need resetData treatment
        (if (x.isReset) 
          (emitRef(x) + "__update = mux " + emitRef(x.inputs.last) + " " + emitRef(x.init) + " " + emitRef(x.next) + "\n") 
         else "") +
        emitDec(x) + "reg " + (if (x.isEnable) emitRef(x.enableSignal) else "1") + " " + 
          (if (x.isReset) (emitRef(x) + "__update") else emitRef(x.next)) + "\n"

      case x: Log2 => // TODO: log2 instruction?
        emitDec(x) + "log2/" + x.width + " " + emitRef(x.inputs(0)) + "\n"

      case c: Cat =>
        emitDec(c) + "cat/" + c.inputs(1).width + " " + emitRef(c.inputs(0)) + " " + emitRef(c.inputs(1)) + "\n"
        
      case l: Literal =>
        ""
      case _ =>
        println("NO EMITTER FOR " + node)
        ""
    }
  }

  def renameNodes(c: Module, nodes: Seq[Node]) = {
    for (m <- nodes) {
      m match {
        case l: Literal => ;
        case any        =>
          if (m.name != "" && !(m == c.reset) && !(m.component == null)) {
            // only modify name if it is not the reset signal or not in top component
            if(m.name != "reset" || !(m.component == c)) {
              m.name = m.component.getPathName + "__" + m.name;
            }
          }
      }
    }
  }

  override def elaborate(c: Module): Unit = {
    super.elaborate(c)

    for (cc <- Module.components) {
      if (!(cc == c)) {
        c.mods       ++= cc.mods;
        c.debugs     ++= cc.debugs;
      }
    }
    c.findConsumers();
    c.verifyAllMuxes;
    ChiselError.checkpoint()

    c.collectNodes(c);
    c.findOrdering(); // search from roots  -- create omods
    renameNodes(c, c.omods);
    if (Module.isReportDims) {
      val (numNodes, maxWidth, maxDepth) = c.findGraphDims();
      ChiselError.info("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    // Write the generated code to the output file
    val out = createOutputFile(c.name + ".flo");
    for (m <- c.omods)
      out.write(emit(m));
    out.close();
  }

}
