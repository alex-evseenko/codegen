/**
 * 05-MAR-2014
 * 
 * Kiev, Ukraine.
 * 
 */
package com.adal.codegen

import scala.collection.mutable.ArrayBuffer

/**
 * @author Alex Evseenko
 *
 */
@Deprecated
object Param {
  def apply(name: String, value: String) = new Const(name, value)

  def apply(name: String, method: Method) = new Expr(name, method)

  def apply(name: String, prop: Property) = new Prop(name, prop)

  def apply(name: String, imp: Import) = new Anonymous(name, imp)
}


@Deprecated
abstract class Param(val name: String) {
  val importsList = ArrayBuffer[Import]()
  val methodsList = ArrayBuffer[Method]()
  val dependentMethodsList = ArrayBuffer[Method]()


  def value: String

  override def toString = value
}

case class Const(override val name: String, val const: String) extends Param(name) {
  override def value = const
}

case class Expr(override val name: String, val method: Method) extends Param(name) {
//  dependentMethodsList += method

  override def value = ""//method.caller
}

case class Prop(override val name: String, val p: Property) extends Param(name) {
//  importsList ++= p.importsList

  override def value = p.name
}

case class Anonymous(override val name: String, val imp: Import) extends Param(name) {
  importsList += imp

  override def value =
s"""new ${imp()}() {
${methodsList.foldLeft("")((a, m) => a + m.holder + Code.CRLF)}
}"""

  def <~(m: Method) = {
    importsList ++= m.importsList
    methodsList += m
    dependentMethodsList ++= m.dependentMethodsList
    this
  }

}

