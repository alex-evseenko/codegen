/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
 * 
 * 23-JAN-2015
 * 
 * Riga, Latvia.
 * 
 */
package com.adal.codegen

import scala.collection.mutable.ArrayBuffer
import com.adal.codegen.Code._
import com.adal.codegen.types.java._


/**
 * @author Alex Evseenko
 *
 */
object CleanMethod {
  def apply(name: Symbol, params: Type*) = new CleanMethod(name, params: _*)
  def apply(name: Symbol) = new CleanMethod(name)
}

class CleanMethod(override val name: Symbol, types: Type*) extends Callable {
  if (!checkUnitArg)
    throw new IllegalArgumentException(name+": wrong formal params list.")
  // add imports declarations for params
  types.filter(!_.typeOf.isPrimitive).map(t => this <~ Import(t.typeOf.qName))

  override val params = if (types.isEmpty) Seq(Parameter(JavaVoid)) else Parameter(types)

  def this(name: Symbol) = this(name, JavaVoid)

  override def holder: String =
    throw new IllegalStateException(s"""Clean method $name cannot have an implementation.""")
}


object Method {
  def apply(name: Symbol, params: Parameter*) = new Method(name, params: _*)
  def apply(name: String, params: Parameter*) = new Method(name, params: _*)
}

class Method(name: Symbol, _params: Parameter*)
  extends CleanMethod(name, Type(_params): _*) with SectionedCode {
  // define default name of params
// FIXME formals.size == 0
if (_params.size > 1)
  for (i <- 0 to (_params.take(_params.size - 1)).size-1) {
    if (_params(i).name == null) {
      _params(i).name = Symbol("arg" + (i+1))
    }
  }

  override val params = if (_params.isEmpty) Seq(Parameter(JavaVoid)) else _params

  protected var modifiersChain =  collection.mutable.ListBuffer[Modifier]()
  private val _dependentMethods = ArrayBuffer[Method]()

  def this(name: String, args: Parameter*) = this(Symbol(name), args: _*)


  def apply(lc: LCode) = {
    this += lc

    this
  }

  override def <~(i: Import) = {
    super.<~(i)
    this
  }

  override def <~(xs: List[Import]) = {
    super.<~(xs)
    this
  }

  def decl: String =
    s"""$qualifiers ${typeOf.sName} ${sName}(${formals.map(p => p.typeOf.sName +" "+ p.sName).mkString(", ")})"""

  override def holder =
s"""
$decl {
  $code
}
"""

  def ::(m: Modifier) = {
    modifiersChain += m
    this
  }

  protected def qualifiers = modifiersChain.reverse.map(m => {
    m.value.name + (if (m == Override) "\n" else "")
  }).mkString(" ")

  /**
   * Add the method to passed class.
   */
  def ~>(c: Class) = c += this
}

object Constructor {
  def apply(owner: Class, params: Parameter*) = new Constructor(owner, params: _*)
}

class Constructor(owner: Class, _params: Parameter*) extends Method(owner.id, _params: _*) {
  override def decl: String =
    s"""$qualifiers ${sName}(${params.map(p => p.typeOf.sName +" "+ p.sName).mkString(", ")})"""
}
