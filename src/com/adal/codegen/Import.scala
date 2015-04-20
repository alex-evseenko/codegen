/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
 * 
 */
package com.adal.codegen

/**
 * Import declaration.
 * 
 * @author Alex Evseenko
 *
 */
@Deprecated
object Import {
  def apply(imp: String) = new Import(imp)
  def apply(typ: Type) = new Import(typ)
}

@Deprecated
class Import(val imp: String) {

  def this(typ: Type) = this(typ.qName)


  /**
   * Return a type corresponding to import (i.e. class w/o package name).
   */
  def apply() =
    if (imp.lastIndexOf('.') > -1) imp.substring(imp.lastIndexOf('.')+1) else imp

  def pkgName = if (imp.lastIndexOf('.') > -1) imp.substring(0, imp.lastIndexOf('.')) else ""

  override def equals(that: Any): Boolean =
    that.isInstanceOf[Import] && imp.equals(that.asInstanceOf[Import].imp)

  override def hashCode: Int = imp.hashCode()

  override def toString = "import "+imp+";"

}
