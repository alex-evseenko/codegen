/**
 * 14-JAN-2015
 * 
 * Riga, Latvia.
 * 
 */
package com.adal.codegen.test

import Util.avoidIdentation
import org.specs2.SpecificationWithJUnit
import com.adal.codegen._
import com.adal.codegen.Code._
import com.adal.codegen.types.java._
import com.adal.codegen.types.android._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import scala.xml.{Null, PrefixedAttribute}

/**
 * @author Alex Evseenko
 *
 */
@RunWith(classOf[JUnitRunner])
class CodegenBasicsSpec extends SpecificationWithJUnit { def is = s2"""
Implicit conversions should be imported:
  import com.adal.codegen.Code._ ${
    ~new CleanMethod('doRefresh, JavaInt, JavaLangString, JavaVoid)(-1, "string") === "doRefresh(-1, \"string\")"
  }

Code operations:
  imports list                ${
    (new Code { this <~ Import(AndroidAppActivity); override def holder = "" }).imports(0).toString ===
    "import android.app.Activity;"
  }
  get target code by unary ~  ${~code"code${1+2+3+5}" === "code11"}
  override holder to postpone string interpolation ${
    var otherCalc = -1
    val code = new Code { override def holder = s"code$otherCalc" }
    otherCalc = 77

    ~code() === "code77"
  }
  or use lambda () => Code ${
    var otherCalc = -1
    val code = $"code$otherCalc"
    otherCalc = 77

    ~code() === "code77"
  }
  from other hand an expression calculated immediately ${
    var otherCalc = -1
    val code = code"code$otherCalc"
    otherCalc = 77

    ~code === "code-1"
  }
  new composed code           ${~(code"code${1*1}" ++ code"code${1*2}") === "code1code2"}

EmptyCode object:
  has an empty holder         ${~EmptyCode === ""}

Sectioned Code:
  has a default section       ${onCreateDialog.section('Code).isDefined}
  can locate a section        ${~(new SectionedCode { this += 'init -> $"initcode"}).section('init).get === "initcode"}
  generate content of sections ${~new SectionedCode { this += 'init -> $"init"; this += 'listener -> $"code"} === "initcode"}
  retrieves secions as a list ${new SectionedCode {}.sections(0)._1 === 'Code}
  support insertion order of sections ${
    new SectionedCode {
      this += 'inits -> $"init"; this += 'listeners -> $"listener"
    }.sections.map(section => section._1) === List('Code, 'inits, 'listeners)
  }
  support insertion order within a section ${
    val scode = new SectionedCode {
      this += 'inits -> $"init"
      this += 'listeners -> $"listener1" += 'listeners -> $"listener2" += 'listeners -> $"listener3"
    }
    scode.section('listeners).get === code"listener1listener2listener3"
  }

*Sectioned code is mutable:
  adding a new section        ${
    val code = new SectionedCode {}
    code += 'Init

    code.sections.size === 2
  }
  injecting code fragments    ${avoidIdentation(~(onCreateDialog += timePickerCase += datePickerCase)) === mergedCases}

Value must:
  have a type ${
    new Value {
      override val typeOf = JavaLangLong
      override def code = EmptyCode
    }.typeOf === JavaLangLong
  }
  return a target code ${
    ~new Value {
      override val typeOf = JavaLangLong
      override def code = code"Long.valueOf(${65635 + 1})"
    } === "Long.valueOf(65636)" 
  }
  be comparable with other Values ${
    new Value {
      override val typeOf = JavaLangString
      override def code = code""""Hola!""""
    } === new VStr("Hola!")
  }
  consequently contcatenates with other values ${
    val getCoordinate = Method('getCoordinate, JavaLangString, Coordinate)
    ~getCoordinate("Street address")('getLatitude)('floatValue) ===
    """getCoordinate("Street address").getLatitude().floatValue()"""
  }
  address its properties ${
    ~Method('getCoordinate, JavaLangString, Coordinate)("Street address")('address)('length) ===
    """getCoordinate("Street address").address.length()"""
  }
  implicitely converts values and support operations ${
    val some = true
    ~$"${!(VBool(some) &&~ (1.5f <=~ 3.14159))}" === "!(true && 1.5f <= 3.14159)"
  }

Callable operations:
  get a signature ${
    CleanMethod('myClean, JavaInt, JavaLangString, JavaVoid).signature === "void myClean(int, String)"
  }
  be comparable ${CleanMethod('getName, JavaLangString, JavaLangString) === getName}
  get a name ${
    (new Callable {
      override val name = 'getLatitude
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangFloat))
      override def holder = ""
     }).sName === "getLatitude"
  }
  get a return type ${
    (new Callable {
      override val name = 'getLatitude
      override val params = Seq(Parameter(JavaLangFloat))
      override def holder = ""
     }).typeOf === JavaLangFloat
  }
  get a possible parameters list ${
    (new Callable {
      override val name = 'getLatitude
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangFloat))
      override def holder = ""
     }).params(0).typeOf === JavaLangString
  }
  get target code by ~ ${
    val getLat = new Callable {
      override val name = 'getLatitude
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangFloat))
      override def holder = s"public Float $sName(String address) { return 0; }"
    }
    ~getLat === "public Float getLatitude(String address) { return 0; }"
  }
  apply arguments ${
    ~(new Callable {
      override val name = 'getLatitude
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangFloat))
      override def holder = ""
     })(new VStr("Duntes iela, 28-201")) === """getLatitude("Duntes iela, 28-201")"""
  }
  apply arguments transitively ${
    val address = new VStr("Duntes iela, 28-201")
    val normalizeAddr = new Callable {
      override val name = 'normalizeAddr
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangString))
      override def holder = ""
    }
    val getLatitude = new Callable {
      override val name = 'getLatitude
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangFloat))
      override def holder = ""
    }
    val getLongitude = new Callable {
      override val name = 'getLongitude
      override val params = Seq(Parameter(JavaLangString), Parameter(JavaLangFloat))
      override def holder = ""
    }
    ~(new Callable {
      override val name = 'getCoordinate
      override val params = Seq(Parameter(JavaLangFloat), Parameter(JavaLangFloat), Parameter(JavaVoid))
      override def holder = ""
     })(getLatitude(normalizeAddr(address)),
        getLongitude(normalizeAddr(address))) ===
     """getCoordinate(getLatitude(normalizeAddr("Duntes iela, 28-201")), getLongitude(normalizeAddr("Duntes iela, 28-201")))"""
  }

Methods are callable as well:
  parameterless clean method is void ${CleanMethod('noname).typeOf === JavaVoid}
  parameterless method is void ${Method('noname).typeOf === JavaVoid}
  qualifiers may attach to the method ${~(Public :: Method('doSmth)) contains "public void doSmth() {"}
  clean method has no target code ${
    try {
      ~CleanMethod('noname)
      false
    } catch {
      case ise: IllegalStateException => true
    }
  }
  apply arguments ${
    val setOnItemClickListener = CleanMethod('setOnItemClickListener, AndroidWidgetAdapterViewOnItemClickListener, JavaVoid)
    val onItemClickListener = new AnonymousClass(AndroidWidgetAdapterViewOnItemClickListener)
    avoidIdentation(~setOnItemClickListener(onItemClickListener())) ===
avoidIdentation(
"""setOnItemClickListener(
new AdapterView.OnItemClickListener() {
  public void onItemClick(AdapterView arg1, View arg2, int arg3, long arg4) {
}
}
)""")
  }
  apply no argument ${
    ~CleanMethod('getParameterless)() === "getParameterless()"
  }
  target code might be injected ${
    avoidIdentation(~(Method('noname) += code"final int = 0;")) ===
    avoidIdentation(
"""
void noname() {
  final int = 0;
}""")
  }
  or injected by apply code ${
    avoidIdentation(~Method('noname)(code"final int = 0;")) ===
    avoidIdentation(
"""void noname() {
  final int = 0;
}""")
  }
  call another method from injected code ${
    val callee = CleanMethod('callee, JavaVoid)
    val caller = Method('caller, JavaVoid)(code"${~callee()};")
avoidIdentation(~caller) ===
avoidIdentation(
"""
void caller() {
  callee();
}
""")
  }
  injecting is immediate calculation of the string interpolation ${
  val activity = Class('adal, 'MyActivity, AndroidAppActivity)
  val listView = Property('listView, AndroidWidgetListView)
  val onCreate = Method('onCreate, 'savedInstanceState -> AndroidOsBundle, JavaVoid)(
// here is activities's code has not yet contained listView -- so used lambda-code
    $"int props = ${activity.propsList.size};"
  )
    activity += listView
    activity += onCreate

    activity.holder contains("int props = 1;")
  } 
  check list of arguments     ${try {
                                  CleanMethod('myClean, JavaInt, JavaLangString, JavaVoid)(-1)
                                  false
                              } catch {
                                case iae: IllegalArgumentException => true
                              }}
  check list of parameters    ${try {
                                  CleanMethod('myClean, JavaInt, JavaLangString, JavaVoid)(-1, 0)
                                  false
                              } catch {
                                case iae: IllegalArgumentException => true
                              }}

Property is a Value:
  contains methods  ${
    ~Property('coordinate, Coordinate)('getLatitude) === "coordinate.getLatitude()"
    }
  apply its methods ${
    val onItemClickListener = new AnonymousClass(AndroidWidgetAdapterViewOnItemClickListener)
    avoidIdentation(~Property('listView, AndroidWidgetListView)('setOnItemClickListener, onItemClickListener())) ===
    avoidIdentation(
"""listView.setOnItemClickListener(
new AdapterView.OnItemClickListener() {
  public void onItemClick(AdapterView arg1, View arg2, int arg3, long arg4) {
}
}
)""")
  }
  property contains import of itself type ${
    ~Import(Property('activity, AndroidAppActivity).typeOf) === "import android.app.Activity;"
  }
  property is a const ${(Private::Const('GREETING, JavaLangString, "Hola!")).decl === """private final static String GREETING = "Hola!";"""}
  property is a const (inference) ${(Private::Const('GREETING, "Hola!")).decl === """private final static String GREETING = "Hola!";"""}
  const has compliance type ${
    try {
      Private::Const('GREETING, JavaLangString, 3.14159)
      false
    } catch {
      case e: IllegalArgumentException => true
      case _: Throwable => false
    }
  }

Type has getters return:
  full quilified name         ${AndroidAppActivity.qName === "android.app.Activity"}
  full quilified might be short ${JavaInt.qName === "int"}
  package com.adal.codegen.test ${AndroidWidgetTextView.pkg.get === Symbol("android.widget")}
  short type name             ${AndroidWidgetTextView.sName === "TextView"}
  inner short name            ${AndroidWidgetAdapterViewOnItemClickListener.sName === "AdapterView.OnItemClickListener"}
  list of callables           ${!JavaLangString.methods.isEmpty}
  properties list             ${!Coordinate.fields.isEmpty}
  a callable by its name      ${JavaLangString.methods('length).isDefined}
  a callable and apply        ${~Coordinate('getLatitude) === "getLatitude()"}
  located a property          ${Coordinate.fields('address).isDefined}
  locate a field and apply    ${~Coordinate('address)('length) === "address.length()"}
  true if the type is primitive ${JavaVoid.isPrimitive}
  import statement if non primitive ${AndroidAppActivity.importDecl.get === "import android.app.Activity;"}

Class can contain:
  constructor ${
    val dbHelper = Protected::Final::Class('MainDatabaseHelper, SQLiteOpenHelper)
    dbHelper += Public::Constructor(dbHelper, 'context -> AndroidContentContext) {
$"""super(context, "db", null, "1.0");"""
    }
    avoidIdentation(~dbHelper) ===
    avoidIdentation(
"""
import android.content.Context;
import android.database.sqlite.SQLiteOpenHelper;

protected final class MainDatabaseHelper extends SQLiteOpenHelper {

public MainDatabaseHelper(Context context) {
super(context, "db", null, "1.0");
}

}""")
  }
  default modifiers ${ avoidIdentation(~Class('DefaultModifier)) === 
 avoidIdentation(
"""
class DefaultModifier {
}
""")
  }
  chain of modifiers ${ avoidIdentation(~(Public::Final::Static::Class('Modifiers))) === 
avoidIdentation(
"""
public final static class Modifiers {

}
""")
  }

Inner class:
  has outer class ${
    val outer = Class('OuterClass)
    val inner = Protected::Final::InnerClass(outer, 'InnerClass)
    outer.nestedClasses(0) == inner &&
    (~outer).contains("protected final class InnerClass {")
  }
  postpone generation until outer class generates ${
    val outer = Class('OuterClass)
    outer += Protected::Final::InnerClass(outer, 'InnerClass)
    outer.nestedClasses.isEmpty
  }

Template generation syntax sugar supports:
  without syntax sugar {}
  parenthesis to apply a method call and to lookup object property ${
    ~JavaLangString('substring, 1)('length) === "substring(1).length()"
  }
  chain of method calls/field accessors ${
    val s = Property('s, JavaLangString, "Hola!")
    ~$"""${ s('substring, 0, s('length) -~ 1)('length) };""" === "s.substring(0, s.length() - 1).length();"
  }

  XmlCode generation functionality allows:
    to create an xml document dynamically ${
    val prefix = new PrefixedAttribute("android", "orientation", "vertical", Null)
    val layout = new XmlCode("LinearLayout", prefix)
    val name1 = "textView1"
    val text = "Total: "
    val textView1 = <TextView android:text={text} android:id={"@+id/" + name1} android:layout_width="fill_parent" android:layout_height="wrap_content" />
    val name2 = "editText1"
    val editText1 = <EditText android:id={"@+id/" + name2} android:layout_width="fill_parent" android:layout_height="wrap_content" />
    layout += 'textView1 -> $"$textView1" += 'editText1 -> $"$editText1"

    !layout ===
      <LinearLayout android:orientation="vertical"><TextView android:layout_height="wrap_content" android:layout_width="fill_parent" android:id="@+id/textView1" android:text="Total: "/><EditText android:layout_height="wrap_content" android:layout_width="fill_parent" android:id="@+id/editText1"/></LinearLayout>
  }
  to insert nested XmlCode-fragments dynamically ${
    val layout = new XmlCode("LinearLayout")
    val inner = new XmlCode("TextView")
    layout += 'textView1 -> $"${~inner}"
    !layout === <LinearLayout><TextView /></LinearLayout>
  }
  to add nested xml-element dynamically ${
    val layout = new XmlCode("LinearLayout")
    val inner = <TextView />
    layout += 'inner -> $"$inner"
    layout += 'inner -> $"<EditText />"
    !layout === <LinearLayout><TextView /><EditText/></LinearLayout>
  }
  to insert sub-element dynamically ${
    val manifest = new XmlCode("manifest")
    val activity = <activity/>
    manifest += 'activity1 -> $"$activity"
    manifest /= 'activity1 -> <intent-filter/>
    manifest /= 'activity1 -> <intent-filter/>
    !manifest === <manifest><activity><intent-filter/><intent-filter/></activity></manifest>
  }
}
""" // End of the spec


  object Coordinate extends Type('Coordinate) {
    this += 'address -> JavaLangString
    this += Method('getParent, JavaVoid)
    this += Method('getLatitude, JavaLangFloat)
    this += Method('getLongitude, JavaLangFloat)
  }

  val onCreateDialog = new SectionedCode {
    override def holder =
s"""
@Override
protected Dialog onCreateDialog(int id) {
  switch (id) {
  $code
  }
  return null;
}
"""
  }

  val timePickerCase =
code"""
case TIME_DIALOG_ID:
  return new TimePickerDialog(this, mTimeSetListener, mHour, mMinute, false);"""

  val datePickerCase =
code"""
case DATE_DIALOG_ID:
  return new DatePickerDialog(this, mDateSetListener, mYear, mMonth, mDay);"""

  val mergedCases = avoidIdentation(
"""
@Override
protected Dialog onCreateDialog(int id) {
  switch (id) {
case TIME_DIALOG_ID:
  return new TimePickerDialog(this, mTimeSetListener, mHour, mMinute, false);
case DATE_DIALOG_ID:
  return new DatePickerDialog(this, mDateSetListener, mYear, mMonth, mDay);
  }
  return null;
}
""")


  val getName = new Method('getName, JavaLangString, JavaLangString) {
    this <~ Import("android.app.Activity")

    override def holder =
s"""
public String $name(Activity activity) {
  return activity.getLocalClassName();
}
"""
    override def typeOf = JavaLangString
  }

}
