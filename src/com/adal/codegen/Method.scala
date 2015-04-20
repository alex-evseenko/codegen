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
  formals.filter(!_.typeOf.isPrimitive).map(p => this <~ Import(p.typeOf.qName))

  override val params = if (types.isEmpty) Seq(Parameter(JavaVoid)) else Parameter(types)

  def this(name: Symbol) = this(name, JavaVoid)

  override def holder: String =
    throw new IllegalStateException(s"""Clean method $name cannot have an implementation.""")
}


abstract class Visibility(val value: String) {
  override def toString = value
}

object Public extends Visibility("public")
object Protected extends Visibility("protected")
object Default extends Visibility("")
object Private extends Visibility("private")


object Method {
  def apply(name: Symbol, params: Parameter*) = new Method(name, params: _*)
  def apply(name: String, params: Parameter*) = new Method(name, params: _*)
}

class Method(name: Symbol, _params: Parameter*)
  extends CleanMethod(name, Type(_params): _*) with SectionedCode {
  // define default name of params
  for (i <- 0 to formals.size-1) {
    if (formals(i).name == null) {
      formals(i).name = Symbol("arg" + (i+1))
    }
  }

  override val params = if (_params.isEmpty) Seq(Parameter(JavaVoid)) else _params

  private var visibility: Visibility = Default
  private val _dependentMethods = ArrayBuffer[Method]()

  def this(name: String, args: Parameter*) = this(Symbol(name), args: _*)


  def apply(lc: LCode) = {
    this += lc

    this
  }

  def dependentMethodsList: List[Method] =
    _dependentMethods.foldLeft(List[Method]())((a, m) => a:::List(m):::m.dependentMethodsList)

  def decl: String =
    s"""$visibility ${typeOf.sName} ${sName}(${formals.map(p => p.typeOf.sName +" "+ p.sName).mkString(", ")})"""

  override def holder =
s"""
$decl {
  $code
}
"""

  def ::(v: Visibility) = {
    visibility = v
    this
  }

  /**
   * Add the method to passed class.
   */
  def ~>(c: Class) = c += this
}

