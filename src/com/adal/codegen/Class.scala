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
import com.adal.codegen.types.java._

/**
 * @author Alex Evseenko
 *
 */
class Interface(pkg: Option[Symbol], id: Symbol)
  extends Type(pkg, id) with Code {

  def this(typ: Type) = this(typ.pkg, typ.id)

//  def cleanMethods = cleanMethodsList.foldLeft("")((a, m) => a+ ~m +";"+ Code.CRLF)

  override def holder  =
s"""
  public interface ${this.sName} {
  cleanMethods
}
"""

}

object Class {
  def apply(pkg: Symbol, name: Symbol) = new Class(Some(pkg), name)
  def apply(pkg: Symbol, name: Symbol, base: Type) = new Class(Some(pkg), name, base)
  def apply(name: Symbol) = new Class(None, name)
  def apply(name: Symbol, base: Type) = new Class(None, name, base)
  def apply(pkg: String, name: Symbol) = new Class(Some(Symbol(pkg)), name)
  def apply(pkg: String, name: Symbol, base: Type) = new Class(Some(Symbol(pkg)), name, base)
}

class Class(pkg: Option[Symbol], name: Symbol, val base: Type = JavaLangObject)
  extends Type(pkg, name) with SectionedCode {

  if (!base.isPrimitive) {
    this <~ Import(base)
  }

  this += 'PropsDecl -> $"""$propsDecl"""


  private val modifiersChain = collection.mutable.ListBuffer[Modifier]()
  private val _nestedClasses = collection.mutable.Set[InnerClass]()
// FIXMY use instead Type#fieldsList
  private val _props = collection.mutable.Set[Property]()

  override def imports: List[Import] = (imps.toList ::: nestedClasses.flatMap(_.imports)).distinct

  def className = sName + ".java"

  def propsList = _props.toList

  def propsDecl = _props.foldLeft("")((a, p) => a+p.decl+Code.CRLF)

  def propsInit = _props.foldLeft("")((a, p) => a+p.init+Code.CRLF)

  def ::(modifier: Modifier) = {
    modifiersChain += modifier
    this
  }

  protected def qualifiers = modifiersChain.reverse.map(_.value.name).mkString(" ")

  def +=(p: Property) = {
    _props += p
    if (!p.typeOf.isPrimitive) {
      this <~ Import(p.typeOf)
    }
    this
  }

  def +=(inner: InnerClass) {
    _nestedClasses += inner
  }

  def nestedClasses = _nestedClasses.toList

  override def +=(m: Callable) = {
    super.+=(m)
    this <~ m.imports

    this
  }

  def apply(args: Value*) =
    new VEval(this, code"""new $sName()""")

  override def holder =
s"""
${if (pkg.isDefined) "package "+pkgName+";" else ""}

${imports.foldLeft("")((a, i) => a+i+Code.CRLF)}

$qualifiers class $sName${if (base.id != 'Object) " extends "+base.sName else ""} {
  ${~this('PropsDecl).get}
  ${nestedClasses.foldLeft("")((a, nested) => a + ~nested + CRLF)}
  ${methods.foldLeft("")((a, m) => a + ~m + CRLF)}
}
"""

}


class AnonymousClass(base: Type) extends Class(base.pkg, base.id, base) {
  // copy a base type methods to this with an implementation
  base.methods.foreach(m => this += Public::Method(m.name, m.params: _*))

  def apply() = new VEval(this, this)

  override def holder =
s"""
new $sName() {
  ${methods.foldLeft("")((a, m) => a + ~m + CRLF)}
}
"""
  
}


object InnerClass {
  def apply(outerClass: Class, name: Symbol, base: Type = JavaLangObject) = new InnerClass(outerClass, name, base)
}

class InnerClass(val outerClass: Class, name: Symbol, base: Type = JavaLangObject) extends
  Class(None, name, base) {

  outerClass += this


  override def holder =
s"""
$qualifiers class $sName${if (base.id != 'Object) " extends "+base.sName else ""} {
  ${~this('PropsDecl).get}
  ${nestedClasses.foldLeft("")((a, nested) => a + ~nested + CRLF)}
  ${methods.foldLeft("")((a, m) => a + ~m + CRLF)}
}
"""

}

object NestedClass {
  def apply(outerClass: Class, name: Symbol, base: Type = JavaLangObject) = new NestedClass(outerClass, name, base)
}

class NestedClass(outerClass: Class, name: Symbol, base: Type = JavaLangObject) extends
  InnerClass(outerClass, name, base) {

  Static::this
}
