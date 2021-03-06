/**
 * 28-FEB-2014
 * 
 * Kiev, Ukraine.
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

/**
 * @author Alex Evseenko
 *
 */
@RunWith(classOf[JUnitRunner])
class CodegenSpec extends SpecificationWithJUnit { def is = s2"""
  Specification of code generation:

  class apply its default constructor ${~Class('adal, 'Test)() === "new Test()"}
  class contains methods list 	$methodsCheck

  class contains imports list 	$importsCheck
  import's type defined         ${~Import("com.foo.Bar") === "import com.foo.Bar;"}
  import's uniqueness           $importsUniqueness

  property is a class field     $propertyIsField
  property contains listeners   $propertyContainsListeners
  property calls a method       $propertyCallsMethod
"""


  val activity = Class('adal, 'MyActivity, AndroidAppActivity)

  val listView = Private::Property('listView, AndroidWidgetListView)

  val addressLbl = Property('addressLbl, AndroidWidgetTextView)

  val onItemClickListener = new AnonymousClass(AndroidWidgetAdapterViewOnItemClickListener)

  val doRefresh = Private::Method("doRefresh", 'id -> JavaLong, JavaVoid)(
code"""${addressLbl.name}.setText("Selected id: "+id);"""
//      addressLbl.methods('setText)("Selected id: " + doRefresh.params(0))
  )

  val doActivate = Private::Method('doActivate, 'id -> JavaLong, 'R -> JavaVoid)(
code"""
    final Bundle bundle = new Bundle();
    bundle.putLong("ID", id);
    final Intent intent = new Intent(this, EditItemActivity.class);
    intent.putExtras(bundle);
    startActivity(intent);
"""
  ) <~ Import(AndroidOsBundle) <~ Import(AndroidContentIntent)

  val getId = Private::Method('getId, JavaLong)(
code"""return getIntent().getLongExtra("ID", -1);"""
  )

  val onCreate = Public::Method('onCreate, 'savedInstanceState -> AndroidOsBundle, JavaVoid)(
$"""
    super.onCreate(savedInstanceState);
    setContentView(R.layout.${activity.sName});
""")

  activity += addressLbl
  activity += listView
  // add the listener implementation
  val onItemClick = onItemClickListener.methods('onItemClick)
  if (onItemClick.isDefined) {
    onItemClick.get += 'Code -> doRefresh(getId())
    onItemClick.get += 'Code -> $";"
    onItemClick.get += 'Code -> doActivate(onItemClick.get.params(3))
    onItemClick.get += 'Code -> $";"
  }
  // add imports list from anonymous class
  activity <~ onItemClickListener.imports

  activity += onCreate
  // create the method implementation 
  onCreate += 'Code ->
$"""
    // dynamic content
    ${activity.propsInit}
"""
  onCreate += 'Code -> listView('setOnItemClickListener, onItemClickListener())

  // add private methods
  activity += doActivate
  activity += getId
  activity += doRefresh
//println(activity.holder)

  //----- Methods operations -----

  def methodsCheck =
    onCreate.holder.contains(
"""public void onCreate(Bundle savedInstanceState) {
  
    super.onCreate(savedInstanceState);
    setContentView(R.layout.MyActivity);

    // dynamic content
""") &&
  onCreate.holder.contains("doRefresh(getId())")
  onCreate.holder.contains("doActivate(arg4)")

  def propertyContainsListeners =
    avoidIdentation(~onItemClickListener) ===
    avoidIdentation(
"""
new AdapterView.OnItemClickListener() {
  
public void onItemClick(AdapterView arg1, View arg2, int arg3, long arg4) {
  doRefresh(getId());doActivate(arg4);
}


}
""")

  //----- Imports operations -----

  def importsCheck =
    activity.holder.contains("import android.app.Activity;") &&
    activity.holder.contains("import android.widget.TextView;") &&
    activity.holder.contains("import android.widget.ListView;") &&
    activity.holder.contains("import android.os.Bundle;") &&
    activity.holder.contains("import android.content.Intent;") &&
    activity.holder.contains("import android.widget.AdapterView;") &&
    activity.holder.contains("import android.view.View;")


  def importsUniqueness = {
    val code = code""
    val i1 = Import("com.foo.Bar")
    val i2 = Import("com.foo.Bar")
    code <~ i1
    code <~ i2
    code <~ Import("com.foo.bar")

    i1 == i2 &&
    code.imports.size === 2 && code.imports.toString.contains("import com.foo.bar;")
  }

  //----- Properties operations -----

  def propertyIsField =
    activity.propsList.contains(listView) &&
    activity.holder.contains("private ListView listView;")

  def propertyCallsMethod =
    activity.holder.contains(listView.name + ".setOnItemClickListener(")

}
