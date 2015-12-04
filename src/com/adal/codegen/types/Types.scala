/**
 * 29-AUG-2015
 *
 * Riga, Latvia.
 *
 */
package com.adal.codegen.types

import com.adal.codegen._


/**
 * @author Alex Evseenko
 *
 */
package java {
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
  case class JavaUtil(typ: Symbol) extends Type(Symbol("java.util"), typ)

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
    this += CleanMethod('substring, JavaInt, JavaLangString)
    this += CleanMethod('substring, JavaInt, JavaInt, JavaLangString)
  }

  object JavaUtilCalendar extends JavaUtil('Calendar)
}

package object android {
  import com.adal.codegen.types.java._

  case class AndroidApp(typ: Symbol) extends Type(Symbol("android.app"), typ)
  case class AndroidOs(typ: Symbol) extends Type(Symbol("android.os"), typ)
  case class AndroidNet(typ: Symbol) extends Type(Symbol("android.net"), typ)
  case class AndroidContent(typ: Symbol) extends Type(Symbol("android.content"), typ)
  case class AndroidWidget(typ: Symbol) extends Type(Symbol("android.widget"), typ)
  case class AndroidView(typ: Symbol) extends Type(Symbol("android.view"), typ)
  case class AndroidDatabase(typ: Symbol) extends Type(Symbol("android.database"), typ)
  case class AndroidDatabaseSqlite(typ: Symbol) extends Type(Symbol("android.database.sqlite"), typ)

  object AndroidAppActivity extends AndroidApp('Activity)
  object AndroidAppDialog extends AndroidApp('Dialog)
  object AndroidAppAlertDialog extends AndroidApp('AlertDialog)
  object AndroidAppDatePickerDialog extends AndroidApp('DatePickerDialog)
  object AndroidAppTimePickerDialog extends AndroidApp('TimePickerDialog)
  object AndroidOsBundle extends AndroidOs('Bundle)
  object AndroidNetUri extends AndroidNet('Uri)
  object AndroidContentIntent extends AndroidContent('Intent)
  object AndroidContentContext extends AndroidContent('Context)
  object AndroidContentContentValues extends AndroidContent('ContentValues)
  object AndroidContentProvider extends AndroidContent('ContentProvider)
  object AndroidWidgetListView extends AndroidWidget('ListView) {
    this += CleanMethod('setOnItemClickListener, AndroidWidgetAdapterViewOnItemClickListener, JavaVoid)
  }
  object AndroidWidgetTextView extends AndroidWidget('TextView)
  object AndroidWidgetEditText extends AndroidWidget('EditText)
  object AndroidWidgetButton extends AndroidWidget('Button)
  object AndroidWidgetCheckBox extends AndroidWidget('CheckBox)
  object AndroidWidgetDatePicker extends AndroidWidget('DatePicker)
  object AndroidWidgetTimePicker extends AndroidWidget('TimePicker)
  object AndroidWidgetAdapterView extends AndroidWidget('AdapterView)
  object AndroidViewView extends AndroidView('View)

  class AndroidWidgetAdapterView(typ: Symbol) extends AndroidWidget(Symbol("AdapterView." + typ.name))
  object AndroidWidgetAdapterViewOnItemClickListener extends AndroidWidgetAdapterView('OnItemClickListener) {
    this += CleanMethod('onItemClick, AndroidWidgetAdapterView, AndroidViewView, JavaInt, JavaLong, JavaVoid)
  }

  object AndroidDatabaseCursor extends AndroidDatabase('Cursor)
  object SQLiteOpenHelper extends AndroidDatabaseSqlite('SQLiteOpenHelper)
  object SQLiteDatabase extends AndroidDatabaseSqlite('SQLiteDatabase)
}
