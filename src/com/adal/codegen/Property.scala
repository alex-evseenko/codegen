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
}


class Property(val sym: Symbol, override val typeOf: Type) extends Value {
// FIXME
//  this <~ Import(typeOf)

  def this(name: String, imp: Import) = this(Symbol(name), new Type(Symbol(imp.pkgName), Symbol(imp())))

  def name = sym.name

//  def listenersList = typeOf.cleanMethodsList.filter(!_.params.isEmpty)

// FIXME
//  def listeners = listenersList.foldLeft("")((r, listener) => r+(listener())+";"+Code.CRLF)

  def fieldsList = typeOf.fieldsList
  def fields(name: Symbol) = typeOf.fields(name)
  def methodsList = typeOf.methodsList
  def methods(name: Symbol) = typeOf.methods(name)

  /**
   * Property declaration as a class field.
   */
  def decl = s"""private ${typeOf.sName} $name;"""

// FIXME exclude it as Android-specific
  def init = s"""$name = (${typeOf.sName}) findViewById(R.id.$name);"""

  override def code = code"""$name"""

  override def toString = name
}
