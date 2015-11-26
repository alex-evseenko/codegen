/**
 * 28-FEB-2014
 *
 * Kiev, Ukraine.
 *
 */
package com.adal.codegen

import com.adal.codegen.Code._
import com.adal.codegen.types.java._


/**
 * @author Alex Evseenko
 *
 */
object Type {
  def apply(params: Seq[Parameter]): Seq[Type] =
    if (params.isEmpty) {
      Seq()
    } else {
      params.map(_.typeOf)
    }

  def apply(pkg: Option[Symbol], name: Symbol): Type = new Type(pkg, name)

  def apply(pkg: Symbol, name: Symbol): Type = new Type(pkg, name)

  def apply(name: Symbol): Type = new Type(name)
}

class Type(val pkg: Option[Symbol], val id: Symbol) {
  private val fieldsList = collection.mutable.Set[Parameter]()
  private val callableList = collection.mutable.Set[Callable]()

  def this(pkg: Symbol, name: Symbol) = this(Some(pkg), name)
  def this(name: Symbol) = this(None, name)


  def fields: List[Parameter] = fieldsList.toList
  def fields(name: Symbol): Option[Parameter] = fieldsList.find(_.name == name)
  def methods: List[Callable] = callableList.toList
  def methods(name: Symbol, args: Value*): Option[Callable] = callableList.find(cll => cll.name == name && cll.theSameAs(args: _*))

  protected def +=(f: Parameter) = {
    fieldsList += f

    this
  }

  protected def +=(m: Callable) = {
    callableList += m

    this
  }

  def apply(id: Symbol, args: Value*): Value =
    if (methods(id, args: _*).isDefined) {
      methods(id, args: _*).get(args: _*)
    } else if (fields(id).isDefined) {
      VEval(fields(id).get.typeOf, code"${fields(id).get.sName}")
    } else {
      throw new IllegalArgumentException(s"Type $sName neither contain method nor field ${id.name}")
    }

  def pkgName: String = if (!isPrimitive) pkg.get.name else ""
  def sName: String = id.name
  def qName: String = (if ("" == pkgName) "" else pkgName + '.') + sName
  def isPrimitive: Boolean = pkg.isEmpty || pkg.get == Symbol("java.lang")
  def importDecl: Option[String] = if (isPrimitive) None else Some(s"import $qName;")

  override def equals(other: Any): Boolean = {
    other match {
      case that: com.adal.codegen.Type => pkg == that.pkg && id == that.id
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val prime = 41
    prime * (prime + pkg.hashCode) + id.hashCode
  }

  override def toString: String = qName
}


/**
 * Any fragment of code with associated list of imports.
 *
 * @author Alex Evseenko
 *
 */
object Code {
  val CRLF = System.getProperty("line.separator") // "\r\n" for Win

  implicit def exprParamWrapper(expr: Parameter): VParam = VParam(expr)
  implicit def exprBooleanWrapper(expr: Boolean): VBool = new VBool(expr)
  implicit def exprIntWrapper(expr: Int): VInt = new VInt(expr)
  implicit def exprLongWrapper(expr: Long): VLong = new VLong(expr)
  implicit def exprFloatWrapper(expr: Float): VFloat = new VFloat(expr)
  implicit def exprDoubleWrapper(expr: Double): VDouble = new VDouble(expr)
  implicit def exprStringWrapper(expr: String): VStr = new VStr(expr)
  implicit def typeToParam(typ: Type): Parameter = new Parameter(typ)
  implicit def entryToParam(entry: (Symbol, Type)): Parameter = new Parameter(entry)

  implicit class CodeHelper(val sc: StringContext) extends AnyVal {

    def code(args: Any*): Code = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }

      new Code {
        override def holder = buf.toString
      }
    }

  }

  type LCode = () => Code

  implicit def valueToLCode(v: Value): LCode = () => v.code
  implicit def entryToLCode(entry: (Symbol, Value)): (Symbol, LCode) = entry._1 -> {() => entry._2.code}
  implicit def bynameToNoarg[a](a: => a): () => a = () => a
  implicit def lcodeToCode(lc: LCode): Code = lc()
  implicit def vevalToVint(eval: VEval): VInt = eval.typeOf match {
    case JavaInt => VInt(eval.code)
    case _ => throw new IllegalArgumentException("Cannot convert to VInt: " + eval)
  }

  implicit class LambdaCodeHelper(val sc: StringContext) extends AnyVal {

    def $(args: (() => Any)*): LCode = () => {
      val unpacked = args.map(a => a())

      new Code {
        override def holder = scala.StringContext(sc.parts: _*).s(unpacked: _*)
      }
    }
  }

}


object EmptyCode extends Code {
  override def holder: String = ""
}

/**
 * Presents a code fragment with its dependencies.
 * It can be defined as evaluation using StringContext:
 *
 * code"int i = ${prop.sName};"
 *
 * or as a lambda -- a postponed evaluation which would be important to make all
 *  initializations done before the string interpolation:
 *
 * $"int i = ${prop.sName};"
 *
 * or as an overriden method:
 *
 * override def holder = s"int i = ${prop.sName}"
 *
 * the `prop` here might not be defined at the moment of declaration of the holder
 * but must be defined in moment when it's actually called.
 */
trait Code {

  /**
   * List of imports that this code referenced to.
   */
  protected val imps = collection.mutable.Set[Import]()

  /**
   * Holds the code.
   */
  protected def holder: String

  def unary_~ = holder

  def imports: List[Import] = imps.toList

  /**
   * Adds the passed import to this code,
   * the same as importsList += i but in form of builder operation.
   */
  def <~(i: Import): Code = {
    imps += i
    this
  }

  def <~(xs: List[Import]): Code = {
    imps ++= xs
    this
  }

  /**
   * Returns a new code fragment combines both.
   */
  def ++(c: Code): Code =
    new Code {
      this <~ c.imports ::: Code.this.imports

      override def holder = Code.this.holder + c.holder
    }

  override def equals(that: Any): Boolean =
    that.isInstanceOf[Code] &&
    holder == (that.asInstanceOf[Code]).holder

  override def hashCode: Int = holder.hashCode()

  override def toString: String = holder
}


/**
 * Consolidate several fragments of code into one piece of code (i.e. methods body,
 * class body, etc.).
 *
 * Allows dynamic creation of classes/methods depending on external conditions.
 * String interpolation of code fragments is postponed till calling holder.
 */
trait SectionedCode extends Code {
  protected val scs = collection.mutable.Map[Symbol, List[LCode]]()
  this += 'Code

  protected override def holder = code

  def sections: List[(Symbol, List[LCode])] = scs.toList

  def code: String =
    scs.values.foldLeft("")((a, llc) => a + ~toCode(llc))

  def apply(secName: Symbol): Option[Code] =
    if (scs contains secName) {
      Some(toCode(scs(secName)))
    } else {
      None
    }

  private def toCode(llc: List[LCode]): Code =
    llc.foldLeft(code"")((c, lc) => c ++ lc())

  def +=(secName: Symbol): SectionedCode = {
    if (this(secName).isEmpty) {
      scs += secName -> List()
    }

    this
  }

  def +=(entry: (Symbol, LCode)): SectionedCode = {
    if (this(entry._1).isEmpty) {
      scs += entry._1 -> List(entry._2)
    } else {
      scs(entry._1) = scs(entry._1) :+ entry._2
    }

    this
  }

  def +=(lc: LCode): SectionedCode =
    this += 'Code -> lc

}


import xml._

class XmlCode(val rootTag: String) extends SectionedCode {
  val printer = new PrettyPrinter(80, 2)

  protected override def holder =
s"""
<?xml version="1.0" encoding="utf-8"?>
${printer.formatNodes(!this)}
"""

  def unary_!(): scala.xml.Elem = {
    val innerTags = if (code == "") Text("") else XML.loadString( code )
    Elem(null, rootTag, Null, TopScope, true, innerTags)
  }
}


trait Value {
  def typeOf: Type
  def code: Code

  def unary_~ = ~code

  def apply(id: Symbol, args: Value*): VEval = {
    def eval(typ: Type, c: Code) = typ match {
      case JavaInt => VInt(c)
      case _ => VEval(typ, c)
    }

    val method = typeOf.methods(id, args: _*)
    if (method.isDefined) {
      eval(method.get.typeOf, code ++ code"." ++ method.get(args:_*).code)
    } else {
      val field = typeOf.fields(id)
      if (field.isDefined) {
        eval(field.get.typeOf, code ++ code".${field.get.sName}")
      } else {
        throw new IllegalArgumentException(s"Type $typeOf doesn't contain field ${id.name}")
      }
    }
  }

  override def equals(that: Any): Boolean =
    that.isInstanceOf[Value] &&
    typeOf == (that.asInstanceOf[Value]).typeOf &&
    code == (that.asInstanceOf[Value]).code

  override def toString: String = ~code + ": " + typeOf
}

/**
 * Formal parameter
 */
object Parameter {
  def apply(types: Seq[Type]): Seq[Parameter] =
    if (types.isEmpty) {
      Seq()
    } else {
      types.map(p => Parameter(p))
    }

  def apply(name: Symbol, typeOf: Type): Parameter = new Parameter(name, typeOf)
  def apply(entry: (Symbol, Type)): Parameter = new Parameter(entry)
  def apply(typ: Type): Parameter = new Parameter(typ)
}

class Parameter(var name: Symbol, val typeOf: Type) {
  def this(entry: (Symbol, Type)) = this(entry._1, entry._2)
  def this(typ: Type) = this(null, typ)

  def sName: String = if (name != null) name.name else ""

  override def toString: String = (name -> typeOf).toString
}


/**
 * Callable returns a new value of type which the last arg is.
 */
trait Callable extends SectionedCode {
  val name: Symbol

  def sName: String = name.name

  /**
   * Formal parameters lile P1, P2...Pn-1, R,
   * where R -- return type of the callable.
   */
  val params: Seq[Parameter]

  def formals: Seq[Parameter] =
    if (params == null || params.isEmpty) {
      Seq()
    } else {
      params.take(params.size - 1)
    }

  def typeOf: Type = params.last.typeOf

  protected def checkUnitArg =
    !formals.exists(_.typeOf == JavaVoid)

  protected def checkParams =
    if (params == null || params.isEmpty) {
      throw new IllegalStateException("At least one (return) param must be defined.")
    }

  protected def checkArgsSize(actuals: Seq[Value]) =
    actuals.size == formals.size

  protected def checkArgs(actuals: Seq[Value]): Option[Value] = {
    for {i <- 0 to actuals.size - 1} {
      if (params(i).typeOf != actuals(i).typeOf) {
        return Some(actuals(i))
      }
    }

    None
  }

  /**
   * Apply actual parameters (arguments)
   */
  def apply(args: Value*): Value = {
    if (!checkArgsSize(args)) {
      throw new IllegalArgumentException(s"$signature: wrong amount of arguments ${1 + args.size}")
    }

    val p = checkArgs(args)

    if (p.isDefined) {
      throw new IllegalArgumentException(s"$signature: actual args list don't match formal params list ${p.get}")
    }

    VEval(typeOf, code"$sName(${args.map(~_.code).mkString(", ")})")
  }

  def signature: String = s"${typeOf.sName} $sName(${formals.map(_.typeOf.sName).mkString(", ")})"

  def theSameAs(args: Value*): Boolean =
    checkArgs(args.toSeq) == None

  override def equals(that: Any): Boolean =
    that.isInstanceOf[Callable] &&
    signature == (that.asInstanceOf[Callable]).signature

  override def hashCode: Int = signature.hashCode()

  override def toString: String = signature
}


object VEval {
  def apply(typ: Type, c: Code): VEval = new VEval(typ, c)
}

class VEval(typ: Type, c: Code) extends Value {
  override def typeOf: Type = typ
  override def code: Code = c
  protected def op2[T <: VEval](op: Code, r: T): Code = code ++ op ++ r.code
}

case class VParam(expr: Parameter) extends VEval(expr.typeOf, $"${expr.sName}")

object VBool {
  def apply(expr: Boolean): VBool = new VBool(expr)
}

case class VBool(c: Code) extends VEval(JavaBoolean, c) {
  def this(expr: Boolean) = this($"$expr")

  def unary_! = VBool(code"!(" ++ code ++ code")")
  def &&~(r: VBool) = VBool(op2(code" && ", r))
  def ||~(r: VBool) = VBool(op2(code" || ", r))
  def ==~(r: VBool) = VBool(op2(code" == ", r))
  def !=~(r: VBool) = VBool(op2(code" != ", r))
}

abstract class VNumeric[T <: Type](typ: Type, c: Code) extends VEval(typ, c) {
  protected def create(c: Code): VNumeric[T]

  def -~(r: VNumeric[T]): VNumeric[T] = create(op2(code" - ", r))
  def +~(r: VNumeric[T]): VNumeric[T] = create(op2(code" + ", r))
  def *~(r: VNumeric[T]): VNumeric[T] = create(op2(code" * ", r))
  def /~(r: VNumeric[T]): VNumeric[T] = create(op2(code" / ", r))
  def %~(r: VNumeric[T]): VNumeric[T] = create(op2(code" % ", r))
  def >>~(r: VNumeric[T]) = VBool(op2(code" > ", r))
  def >>=~(r: VNumeric[T]) = VBool(op2(code" >= ", r))
  def <<~(r: VNumeric[T]) = VBool(op2(code" < ", r))
  def <=~(r: VNumeric[T]) = VBool(op2(code" <= ", r))
}

case class VInt(c: Code) extends VNumeric(JavaInt, c) {
  def this(expr: Int) = this($"$expr")

  override protected def create(c: Code) = VInt(c)
}

case class VLong(c: Code) extends VNumeric(JavaLong, c) {
  def this(expr: Long) = this($"${expr}l")

  override protected def create(c: Code) = VLong(c)
}

case class VFloat(c: Code) extends VNumeric(JavaFloat, c) {
  def this(expr: Float) = this($"${expr}f")

  override protected def create(c: Code) = VFloat(c)
}

case class VDouble(c: Code) extends VNumeric(JavaDouble, c) {
  def this(expr: Double) = this($"$expr")

  override protected def create(c: Code) = VDouble(c)
}

case class VStr(c: Code) extends VEval(JavaLangString, c) {
  def this(expr: String) = this($""""$expr"""")

  def +~(r: VStr): VStr = VStr(op2(code" + ", r))
}


case class Modifier(val value: Symbol) {

  def ::(modifier: Modifier): Modifier = {
    this
  }

  override def toString: String = value.name
}

object Public extends Modifier('public)
object Protected extends Modifier('protected)
object Default extends Modifier(Symbol(""))
object Private extends Modifier('private)
object Static extends Modifier('static)
object Final extends Modifier('final)
object Strictfp extends Modifier('strictfp)
object Override extends Modifier(Symbol("@Override"))
