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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.BufferProxy
import scala.collection.mutable.Stack
import scala.math._
import Vec._

object VecUIntToOH
{
  def apply(in: UInt, width: Int): UInt =
  {
    if(Module.chiselOneHotMap.contains((in, width))) {
      Module.chiselOneHotMap((in, width))
    } else {
      val out = UInt(1, width)
      val res = (out << in)(width-1,0)
      Module.chiselOneHotMap += ((in, width) -> res)
      res
    }
  }
}

object VecMux {
  def apply(addr: UInt, elts: Seq[Data]): Data = {
    def doit(elts: Seq[Data], pos: Int): Data = {
      if (elts.length == 1) {
        elts(0)
      } else {
        val newElts = (0 until elts.length/2).map(i => Mux(addr(pos), elts(2*i + 1), elts(2*i)))
        doit(newElts ++ elts.slice(elts.length/2*2, elts.length), pos + 1)
      }
    }
    doit(elts, 0)
  }
}

object Vec {

  /** Returns a new *Vec* from a sequence of *Data* nodes.
    */
  def apply[T <: Data](elts: Seq[T]): Vec[T] = {
    val res = new Vec[T](i => elts.head.clone)
    elts.zipWithIndex.foreach{ case (e,i) => res += e }
    res
  }

  /** Returns a new *Vec* from the contatenation of a *Data* node
    and a sequence of *Data* nodes.
    */
  def apply[T <: Data](elt0: T, elts: T*): Vec[T] =
    apply(elt0 +: elts.toSeq)

  /** Returns an array that contains the results of some element computation
    a number of times.

    Note that this means that elem is computed a total of n times.
    */
  def fill[T <: Data](n: Int)(gen: => T): Vec[T] = {
    Vec.tabulate(n){ i => gen }
  }

  def getEnable(onehot: UInt, i: Int): Bool = {
    var enable: Bool = null
      if(Module.chiselOneHotBitMap.contains(onehot, i)){
        enable = Module.chiselOneHotBitMap(onehot, i)
      } else {
        enable = onehot(i)
        Module.chiselOneHotBitMap += ((onehot, i) -> enable)
      }
    enable
  }

  /** Returns an array containing values of a given function over
    a range of integer values starting from 0.
    */
  def tabulate[T <: Data](n: Int)(gen: (Int) => T): Vec[T] = {
    val res = new Vec[T](gen);
    var i = 0
    while (i < n) {
      res += gen(i)
      i += 1
    }
    res
  }

  def tabulate[T <: Data](n1: Int, n2: Int)(f: (Int, Int) => T): Vec[Vec[T]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

}

abstract class VecProc extends Node {
  var addr: UInt = null
  var elms: ArrayBuffer[Bits] = null

  def genMuxes(default: Node) {}

  def procAssign(src: Node) {
    val onehot = VecUIntToOH(addr, elms.length)
    Module.searchAndMap = true
    for(i <- 0 until elms.length){
      when (getEnable(onehot, i)) {
/* XXX Really what is .comp?
        if(elms(i).comp != null) {
          elms(i).comp procAssign src
        } else {
          elms(i) procAssign src
        }
 */
      }
    }
    Module.searchAndMap = false
  }
}

/* XXX should be a no-argument constructor */
class Vec[T <: Data](val gen: (Int) => T) extends AggregateData[Int]
    with Cloneable with BufferProxy[T] {
  val self = new ArrayBuffer[T]
  val readPortCache = new HashMap[UInt, T]
  var sortedElementsCache: ArrayBuffer[ArrayBuffer[Data]] = null
  var flattenedVec: Node = null

  override def items(): Seq[(Int, Data)] = {
    self.zipWithIndex.map(tuple => (tuple._2, tuple._1))
  }


  override def apply(idx: Int): T = {
    self(idx)
  }

  def sortedElements: ArrayBuffer[ArrayBuffer[Data]] = {
    if (sortedElementsCache == null) {
      sortedElementsCache = new ArrayBuffer[ArrayBuffer[Data]]

      // create buckets for each elm in data type
      for(i <- 0 until this(0).flatten.length)
        sortedElementsCache += new ArrayBuffer[Data]

      // fill out buckets
      for(elm <- this) {
        for(((n, io), i) <- elm.flatten zip elm.flatten.indices) {
          //val bits = io.toBits
          //bits.comp = io.comp
          sortedElementsCache(i) += io.asInstanceOf[Data]
        }
      }
    }
    sortedElementsCache
  }

  def apply(index: UInt): T = { self(0) } // XXX
/* XXX This should really be in the Mem
    = read(ind)

  def write(addr: UInt, data: T) {
    if(data.isInstanceOf[Node]){

      val onehot = VecUIntToOH(addr, length)
      Module.searchAndMap = true
      for(i <- 0 until length){
        when (getEnable(onehot, i)) {
          this(i).comp procAssign data.toNode
        }
      }
      Module.searchAndMap = false
    }
  }

  def read(addr: UInt): T = {
    if(readPortCache.contains(addr)) {
      return readPortCache(addr)
    }

    val res = this(0).clone
    val iaddr = UInt(width=log2Up(length))
    iaddr.inputs += addr
    for(((n, io), sortedElm) <- res.flatten zip sortedElements) {
// XXX what's the point of this?      io assign VecMux(iaddr, sortedElm)

      // setup the comp for writes
      val io_comp = new VecProc()
      io_comp.addr = iaddr
      io_comp.elms = sortedElm.asInstanceOf[ArrayBuffer[Bits]] // XXX ?
      // XXX io.comp = io_comp
    }
    readPortCache += (addr -> res)
    res
  }
 */

  override def flatten: Array[(String, Bits)] = {
    val res = new ArrayBuffer[(String, Bits)]
    for (elm <- self)
      res ++= elm.flatten
    res.toArray
  }

  override def fromBits( bits: Bits ): this.type = {
    // XXX implement correctly
    this
  }

  override def <>(src: Data) {
    src match {
      case other: Vec[T] => {
        for((b, o) <- self zip other.self)
          b <> o
      }
    }
  }

  def <>(src: Iterable[T]) {
    for((b, e) <- self zip src)
      b <> e;
  }

  def :=[T <: Data](src: Iterable[T]): Unit = {
    if( this.size != src.size ) {
      ChiselError.error("Can't wire together Vecs of mismatched lengths")
    } else {
      for( (me, other) <- this zip src ) {
        me := other
      }
    }
  }

  override def :=(src: Data): Unit = {
    src match {
      case uint: UInt =>
        for(i <- 0 until length)
          this(i) := uint(i)
      case vec: Vec[_] => {
        /* We would prefer to match for Vec[Data] but that's impossible
         because of JVM constraints which lead to type erasure. */
        val vecdata = vec.asInstanceOf[Vec[Data]]
        this := vecdata.asInstanceOf[Iterable[Data]]
      }
      case _ =>
        super.:=(src)
    }
  }

  override def nameIt (path: String): this.type = {
    if( !named
      && (name.isEmpty
        || (!path.isEmpty && name != path)) ) {
      val prevPrefix = if (name.length > 0) name + "_" else ""
      name = path
      val prefix = if (name.length > 0) name + "_" else ""
      for( (elm, i) <- self.zipWithIndex ) {
        val prevElmPrefix = prevPrefix + i
        val suffix = if( elm.name.startsWith(prevElmPrefix) ) {
          /* XXX Cludgy! We remove the previous prefix and regenerate
          the _elm_ name with a new prefix. */
          elm.name.substring(prevElmPrefix.length)
        } else {
          elm.name
        }
        elm.nameIt(prefix + i + suffix)
      }
    } else {
      /* We are trying to rename a Vec that has a fixed name. */
    }
    this
  }

  override def clone(): this.type = {
    val res = Vec.tabulate(size)(gen);
    res.asInstanceOf[this.type]
  }

  override def toBits(): UInt = {
    val reversed = this.reverse.map(_.toBits)
    Cat(reversed.head, reversed.tail: _*)
  }


  override def toString: String = {
    var sep = ""
    val str = new StringBuilder
    str.append("[")
    for( (key, value) <- items ) {
      str.append(sep + value)
      sep = ", "
    }
    str.append("]")
    str.toString
  }


  def forall(p: T => Bool): Bool = (this map p).fold(Bool(true))(_&&_)
  def exists(p: T => Bool): Bool = (this map p).fold(Bool(false))(_||_)
  def contains(x: Bits): Bool = this.exists(_ === x)
  def count(p: T => Bool): UInt = PopCount(this map p)

  private def indexWhereHelper(p: T => Bool) = this map p zip (0 until size).map(i => UInt(i))
  def indexWhere(p: T => Bool): UInt = PriorityMux(indexWhereHelper(p))
  def lastIndexWhere(p: T => Bool): UInt = PriorityMux(indexWhereHelper(p).reverse)
}
