/**
 * 02-JAN-2015
 * 
 * Riga, Latvia
 * 
 */
package com.adal.codegen.test

/**
 * @author Alex Evseenko
 *
 */
object Util {

  def avoidIdentation(s: String) =
    s.split("\\r?\\n").map(l => l.trim).filter(l => !l.isEmpty).mkString("\r\n").trim

}