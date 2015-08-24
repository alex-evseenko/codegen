/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
 * 
 */
package com.adal.codegen

import scala.collection.mutable.ArrayBuffer
import com.adal.codegen.Code._

case class Modifier(val value: Symbol) {

  def ::(modifier: ClassModifier) = {
    this
  }

  override def toString = value.name
}

object $public extends Modifier('public)
object $protected extends Modifier('protected)
object $default extends Modifier(Symbol(""))
object $private extends Modifier('private)
object $static extends Modifier('static)
object $final extends Modifier('final)

/**
 * Single property of type imp()
 * 
 * @author Alex Evseenko
 *
 */
object Property {

  def apply(name: String, imp: Import) = new Property(name, imp)

  def apply(name: String, imp: String) = new Property(name, Import(imp))

  def apply(name: Symbol, typ: Type) = new Property(name, typ)
}


class Property(val sym: Symbol, override val typeOf: Type) extends Value { self =>
  private val modifiersChain = collection.mutable.ListBuffer[Modifier]()
  $private::self

  def this(name: String, imp: Import) = this(Symbol(name), new Type(Symbol(imp.pkgName), Symbol(imp())))

  private def qualifiers = modifiersChain.reverse.map(_.value.name).mkString(" ")

  def ::(modifier: Modifier) = {
    modifiersChain += modifier
    this
  }

  def name = sym.name

  def fieldsList = typeOf.fieldsList
  def fields(name: Symbol) = typeOf.fields(name)
  def methodsList = typeOf.methodsList
  def methods(name: Symbol) = typeOf.methods(name)

  /**
   * Property declaration as a class field.
   */
  def decl = s"""$qualifiers ${typeOf.sName} $name;"""

// FIXME exclude it as Android-specific
  def init = s"""$name = (${typeOf.sName}) findViewById(R.id.$name);"""

  override def code = code"""$name"""

  override def toString = name
}
