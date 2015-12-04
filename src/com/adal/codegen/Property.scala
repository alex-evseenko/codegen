/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
 * 
 */
package com.adal.codegen

import scala.collection.mutable.ArrayBuffer
import com.adal.codegen.Code._

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

  def apply(name: Symbol, typ: Type, initVal : Value) = new Property(name, typ, Some(initVal))

  def apply(name: Symbol, typ: Type, init: Code) = new Property(name, typ, None, Some(init))

  def apply(name: Symbol, typ: Type, initVal : Value, init: Code) = new Property(name, typ, Some(initVal), Some(init))
}


class Property(val sym: Symbol, override val typeOf: Type,
               val initVal: Option[Value] = None,
               val init: Option[Code] = None) extends Value {
  if (initVal.isDefined && initVal.get.typeOf != typeOf) {
    throw new IllegalArgumentException(s"Property $name initialized by different type.")
  }
  private val modifiersChain = collection.mutable.ListBuffer[Modifier]()

  def this(name: String, imp: Import) = this(Symbol(name), new Type(Symbol(imp.pkgName), Symbol(imp())))

  protected def qualifiers = modifiersChain.reverse.map(_.value.name).mkString(" ")

  def ::(modifier: Modifier) = {
    modifiersChain += modifier
    this
  }

  def name = sym.name

  def fieldsList = typeOf.fields
  def fields(name: Symbol) = typeOf.fields(name)
  def methodsList = typeOf.methods
  def methods(name: Symbol) = typeOf.methods(name)

  /**
   * Property declaration as a class field.
   */
  def decl = s"""$qualifiers ${typeOf.sName} $name${if (initVal.isDefined) " = "+ ~initVal.get else ""};"""

  override def code = code"""$name"""

  override def toString = name
}


object Const {
  def apply(name: Symbol, typ: Type, initVal : Value) = new Const(name, typ, initVal)

  def apply(name: Symbol, initVal : Value) = new Const(name, initVal)
}

class Const(sym: Symbol, typeOf: Type, value: Value) extends Property(sym, typeOf, Some(value)) {
  Final::Static::this

  def this(name: Symbol, initVal : Value) = this(name, initVal.typeOf, initVal)
}
