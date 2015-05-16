/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
 * 
 */
package com.adal.codegen

import scala.collection.mutable.Set
import com.adal.codegen.Code._
import scala.collection.mutable.WrappedArray


object Type {
  def apply(params: Seq[Parameter]) =
    if (params.isEmpty)
      Seq()
    else
      params.map(_.typeOf)

  def apply(pkg: Option[Symbol], name: Symbol) = new Type(pkg, name)

  def apply(pkg: Symbol, name: Symbol) = new Type(pkg, name)
  
  def apply(name: Symbol) = new Type(name)
}

class Type(val pkg: Option[Symbol], val name: Symbol) {
  private val _fieldsList = collection.mutable.Set[Parameter]()
  private val _callableList = collection.mutable.Set[Callable]()

  def this(pkg: Symbol, name: Symbol) = this(if (pkg != null) Some(pkg) else None, name)
  def this(name: Symbol) = this(None, name)


  def sName = name.name

  def fieldsList = _fieldsList.toList
  def fields(name: Symbol) = _fieldsList.find(_.name == name)
  def methodsList = _callableList.toList
  def methods(name: Symbol) = _callableList.find(_.name == name)

  protected def +=(f: Parameter) = {
    _fieldsList += f

    this
  }

  protected def +=(m: Callable) = {
    _callableList += m

    this
  }

  def apply(name: Symbol, args: Value*) =
    if (args.isEmpty && fields(name).isDefined) {
      VEval(fields(name).get.typeOf, code"${name.name}")
    } else if (_callableList.find(_.name == name).isDefined) {
      _callableList.find(_.name == name).get(args:_*)
    } else {
      throw new IllegalArgumentException(s"Type $this doesn't contain a method $name")
    }

  def pkgName = if (!isPrimitive) pkg.get.name else ""
  def qName = if (!isPrimitive) pkgName +'.' + sName else sName
  def isPrimitive = pkg.isEmpty
  def importDecl: Option[String] = if (isPrimitive) None else Some(s"import $qName;")

  def canEqual(other: Any) = {
    other.isInstanceOf[com.adal.codegen.Type]
  }

  override def equals(other: Any) = {
    other match {
      case that: com.adal.codegen.Type => that.canEqual(Type.this) && pkg == that.pkg && name == that.name
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + pkg.hashCode) + name.hashCode
  }

  override def toString = if (isPrimitive) name.toString else qName
}

object JavaVoid extends Type('void)
object JavaBoolean extends Type('boolean)
object JavaByte extends Type('byte)
object JavaShort extends Type('short)
object JavaInt extends Type('int)
object JavaChar extends Type('char)
object JavaLong extends Type('long)
object JavaFloat extends Type('float)
object JavaDouble extends Type('double)

case class JavaLang(typ: Symbol) extends Type(Symbol("java.lang"), typ)

object JavaLangObject extends JavaLang('Object)
object JavaLangBoolean extends JavaLang('Boolean)
object JavaLangInteger extends JavaLang('Integer)
object JavaLangLong extends JavaLang('Long)
object JavaLangFloat extends JavaLang('Float) {
  this += CleanMethod('floatValue, JavaFloat)
}
object JavaLangDouble extends JavaLang('Double)
object JavaLangString extends JavaLang('String) {
  this += CleanMethod('length, JavaInt)
}


case class AndroidApp(typ: Symbol) extends Type(Symbol("android.app"), typ)
case class AndroidOs(typ: Symbol) extends Type(Symbol("android.os"), typ)
case class AndroidContent(typ: Symbol) extends Type(Symbol("android.content"), typ)
case class AndroidWidget(typ: Symbol) extends Type(Symbol("android.widget"), typ)
case class AndroidView(typ: Symbol) extends Type(Symbol("android.view"), typ)

object AndroidAppActivity extends AndroidApp('Activity)
object AndroidOsBundle extends AndroidOs('Bundle)
object AndroidContentIntent extends AndroidContent('Intent)
object AndroidWidgetListView extends AndroidWidget('ListView) {
  this += CleanMethod('setOnItemClickListener, AndroidWidgetAdapterViewOnItemClickListener, JavaVoid)
}
object AndroidWidgetTextView extends AndroidWidget('TextView)
object AndroidWidgetButton extends AndroidWidget('Button)
object AndroidWidgetAdapterView extends AndroidWidget('AdapterView)
object AndroidViewView extends AndroidView('View)

class AndroidWidgetAdapterView(typ: Symbol) extends AndroidWidget(Symbol("AdapterView."+typ.name))
object AndroidWidgetAdapterViewOnItemClickListener extends AndroidWidgetAdapterView('OnItemClickListener) {
  this += CleanMethod('onItemClick, AndroidWidgetAdapterView, AndroidViewView, JavaInt, JavaLong, JavaVoid)
}


/**
 * Any fragment of code with associated list of imports.
 * 
 * @author Alex Evseenko
 *
 */
object Code {
  val CRLF = System.getProperty("line.separator") //"\r\n" for Win

  implicit def exprParamWrapper(expr: Parameter) = new EParam(expr)

  implicit def exprBooleanWrapper(expr: Boolean) = new EBoolean(expr)

  implicit def exprIntWrapper(expr: Int) = new EInt(expr)

  implicit def exprLongWrapper(expr: Long) = new ELong(expr)

  implicit def exprFloatWrapper(expr: Float) = new EFloat(expr)

  implicit def exprStringWrapper(expr: String) = new EStr(expr)

  implicit def typeToParam(typ: Type) = new Parameter(typ)

  implicit def entryToParam(entry: (Symbol, Type)) = new Parameter(entry)

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

  implicit def value_to_lcode(v: Value) = () => v.code ++ code";"

  implicit def entry_to_lcode(entry: (Symbol, Value)) = entry._1 -> {() => entry._2.code ++ code";"}

  implicit def byname_to_noarg[a](a: => a) = () => a

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
  override def holder = ""
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
  private val _imports = Set[Import]()

  /**
   * Holds the code.
   */
  protected def holder: String

  def unary_~ = holder

  def importsList = _imports.toList

  /**
   * Returns list of imports without duplications.
   */
  def imports = importsList.foldLeft("")((a, i) => a+i+Code.CRLF)

  /**
   * Adds the passed import to this code,
   * the same as importsList += i but in form of builder operation.
   */
  def <~(i: Import) = {
    _imports += i
    this
  }

  def <~(xs: List[Import]) = {
    _imports ++= xs
    this
  }

  /**
   * Returns a new code fragment combines both.
   */
  def ++(c: Code) =
    new Code {
      this <~ c.importsList ::: Code.this.importsList

      override def holder = Code.this.holder + c.holder
    }

  override def toString = holder
}


/**
 * Consolidate several fragments of code into one piece of code (i.e. methods body,
 * class body, etc.).
 * 
 * Allows dynamic creation of classes/methods depending on external conditions.
 * String interpolation of code fragments is postponed till calling holder.
 */
trait SectionedCode extends Code {
  protected val _sections = scala.collection.mutable.Map[Symbol, List[LCode]]()
  this += 'Code

  protected override def holder = code

  def sections = _sections.toList

  private def toCode(llc: List[LCode]) =
    llc.foldLeft(code"")((c, lc) => c ++ lc())

  def code =
    _sections.values.foldLeft("")((a, llc) => a + ~toCode(llc))

  def apply(secName: Symbol): Option[Code] =
    if (_sections contains secName)
      Some(toCode(_sections(secName)))
    else
      None

  def +=(secName: Symbol) = {
    if (this(secName).isEmpty) {
      _sections += secName -> List()
    }

    this
  }

  def +=(entry: (Symbol, LCode)) = {
    if (this(entry._1).isEmpty) {
      _sections += entry._1 -> List(entry._2)
    } else {
      _sections(entry._1) = _sections(entry._1) :+ entry._2
    }

    this
  }

  def +=(lc: LCode): SectionedCode =
    this += 'Code -> lc

}

trait Value {
  def typeOf: Type
  def code: Code

  def unary_~ = ~code

  def apply(name: Symbol, args: Value*): Value =
    if (args.isEmpty && typeOf.fields(name).isDefined) {
      VEval(typeOf.fields(name).get.typeOf, code ++ code".${typeOf.fields(name).get.sName}")
    } else {
      val method = typeOf.methods(name)
      if (method.isDefined) {
        VEval(method.get.typeOf, code ++ code"." ++ method.get(args:_*).code)
      } else {
        typeOf(name, args:_*)
      }
    }

  override def toString = typeOf +" {"+ ~code +"}"
}

/**
 * Formal parameter
 */
object Parameter {
  def apply(types: Seq[Type]): Seq[Parameter] =
    if (types.isEmpty)
      Seq()
    else
      types.map(p => Parameter(p))

  def apply(name: Symbol, typeOf: Type) = new Parameter(name, typeOf)
  def apply(entry: (Symbol, Type)) = new Parameter(entry)
  def apply(typ: Type) = new Parameter(typ)
}

class Parameter(var name: Symbol, val typeOf: Type) {
  def this(entry: (Symbol, Type)) = this(entry._1, entry._2)
  def this(typ: Type) = this(null, typ)

  def sName = if (name != null) name.name else ""

  override def toString = (name -> typeOf).toString
}


/**
 * Callable returns a new value of type which the last arg is.
 */
trait Callable extends SectionedCode {
  val name: Symbol

  def sName = name.name

  /**
   * Formal parameters lile P1, P2...Pn-1, R,
   * where R -- return type of the callable.
   */
  val params: Seq[Parameter]

  def formals =
    if (params == null || params.isEmpty)
      Seq()
    else
      params.take(params.size - 1)

  def typeOf = params.last.typeOf

  protected def checkUnitArg =
    !formals.exists(_.typeOf == JavaVoid)

  protected def checkParams =
    if (params == null || params.isEmpty)
      throw new IllegalStateException("At least one (return) param must be defined.")

  protected def checkArgsSize(actuals: Seq[Value]) =
    actuals.size == formals.size

  protected def checkArgs(actuals: Seq[Value]): Option[Value] = {
    for (i <- 0 to actuals.size - 1) {
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
    if (!checkArgsSize(args))
      throw new IllegalArgumentException(name+": wrong amount of arguments "+(1 + args.size))

    val p = checkArgs(args)

    if (p.isDefined)
      throw new IllegalArgumentException(name+": actual args list don't match formal params list "+p.get)

    VEval(typeOf, code"$sName(${args.map(~_.code).mkString(", ")})")
  }

  def signature = s"${typeOf.sName} $sName(${formals.map(_.typeOf.sName).mkString(", ")})"

  override def equals(that: Any): Boolean =
    that.isInstanceOf[Callable] &&
    signature == (that.asInstanceOf[Callable]).signature

  override def hashCode = signature.hashCode()

  override def toString = signature.toString
}

case class VEval(typ: Type, c: Code) extends Value {
  override def typeOf = typ
  override def code = c
}

abstract class ValueExpr[T](val expr: T) extends Value {
  override def code = code"${expr.toString}"

  override def equals(that: Any): Boolean =
    that.isInstanceOf[ValueExpr[T]] &&
    expr == (that.asInstanceOf[ValueExpr[T]]).expr

  override def hashCode: Int = expr.hashCode()
}

case class EParam(override val expr: Parameter) extends ValueExpr(expr) {
  override def typeOf = expr.typeOf

  override def code = code"${expr.sName}"
}

case class EBoolean(override val expr: Boolean) extends ValueExpr(expr) {
  override def typeOf = JavaBoolean
}

case class EInt(override val expr: Int) extends ValueExpr(expr) {
  override def typeOf = JavaInt
}

case class ELong(override val expr: Long) extends ValueExpr(expr) {
  override def typeOf = JavaLong
}

case class EFloat(override val expr: Float) extends ValueExpr(expr) {
  override def typeOf = JavaFloat
}

case class EStr(override val expr: String) extends ValueExpr(expr) {
  override def code = code""""${expr.toString}""""
  override def typeOf = JavaLangString
}
