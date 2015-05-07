/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
 * 
 * 06-FEB-2015
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
class Interface(override val pkg: Option[Symbol], override val name: Symbol)
  extends Type(pkg, name) with Code {

  def this(typ: Type) = this(typ.pkg, typ.name)

//  def cleanMethods = cleanMethodsList.foldLeft("")((a, m) => a+ ~m +";"+ Code.CRLF)

  override def holder  =
s"""
  public interface ${this.sName} {
  cleanMethods
}
"""

}

object Class {
  def apply(pkg: Symbol, name: Symbol) = new Class(Some(pkg), name, JavaLangObject)
  def apply(pkg: Symbol, name: Symbol, base: Type) = new Class(Some(pkg), name, base)
}

class Class(pkg: Option[Symbol], name: Symbol, val base: Type)
  extends Type(pkg, name) with SectionedCode {

//  this <~ Import(base)

// FIXME it's semanticaly the same as 
  private val _props = collection.mutable.Set[Property]()

  def this(pkg: Symbol, name: Symbol, base: Type) = this(Some(pkg), name, base)

  def this(pkg: Symbol, name: Symbol) = this(Some(pkg), name, JavaLangObject)


  def propsList = _props.toList

  def propsDecl = _props.foldLeft("")((a, p) => a+p.decl+Code.CRLF)

  def propsInit = _props.foldLeft("")((a, p) => a+p.init+Code.CRLF)

  def +=(p: Property) = {
    _props += p
//    this <~ p.importsList.toList

    this
  }

  override def +=(m: Callable) = {
    super.+=(m)

    this
  }

  def apply(args: Value*) =
    new VEval(this, code"""new $sName()""")

  override def holder =
s"""
${if (pkg.isDefined) "package "+pkgName+";" else ""}

$imports

public class $sName${if (base.name != 'Object) " extends "+base.sName} {
  $propsDecl
  ${methodsList.foldLeft("")((a, m) => a + ~m + CRLF)}
}
"""

}


class AnonymousClass(base: Type) extends Class(base.pkg, base.name, base) {
  // copy a base type methods to this with an implementation
  base.methodsList.foreach(m => this += Public::Method(m.name, m.params: _*))

  def apply() = new VEval(this, this)

  override def holder =
s"""
new $sName() {
  ${methodsList.foldLeft("")((a, m) => a + ~m + CRLF)}
}
"""
  
}

