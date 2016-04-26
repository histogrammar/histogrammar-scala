package org.dianahep.histogrammar

import scala.collection.mutable
import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.Settings
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeOf

import org.dianahep.histogrammar._

package eval {
  // Based on https://eknet.org/main/dev/runtimecompilescala.html,
  // which was in turn based on Eval from Twitter-Utils (https://github.com/twitter/util).
  object Compiler {
    private val target = new VirtualDirectory("(memory)", None)
    private var classNameNumber = 0
    private val codeToClassName = mutable.Map[String, String]()
    private val classCache = mutable.Map[String, Class[_]]()

    private val global = {
      val settings = new Settings()
      settings.deprecation.value = true     // enable detailed deprecation warnings
      settings.unchecked.value = true       // enable detailed unchecked warnings
      settings.outputDirs.setSingleOutput(target)
      settings.usejavacp.value = true
      new Global(settings)
    }

    // The embedded compiler can't see my classes, so I have to compile equivalent versions of them.
    (new global.Run).compileSources(List(new BatchSourceFile("(inline)", """
trait Selection[DATUM] extends Function1[DATUM, Double]

object SelectionImplicits {
  implicit class SelectionFromBoolean[DATUM](f: DATUM => Boolean) extends Selection[DATUM] {
    def apply(x: DATUM): Double = if (f(x)) 1.0 else 0.0
  }
  implicit class SelectionFromByte[DATUM](f: DATUM => Byte) extends Selection[DATUM] {
    def apply(x: DATUM): Double = f(x).toDouble
  }
  implicit class SelectionFromShort[DATUM](f: DATUM => Short) extends Selection[DATUM] {
    def apply(x: DATUM): Double = f(x).toDouble
  }
  implicit class SelectionFromInt[DATUM](f: DATUM => Int) extends Selection[DATUM] {
    def apply(x: DATUM): Double = f(x).toDouble
  }
  implicit class SelectionFromLong[DATUM](f: DATUM => Long) extends Selection[DATUM] {
    def apply(x: DATUM): Double = f(x).toDouble
  }
  implicit class SelectionFromFloat[DATUM](f: DATUM => Float) extends Selection[DATUM] {
    def apply(x: DATUM): Double = f(x).toDouble
  }
  implicit class SelectionFromDouble[DATUM](f: DATUM => Double) extends Selection[DATUM] {
    def apply(x: DATUM): Double = f(x)
  }
}
""")))

    private val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)

    private def classNameForCode(code: String) = codeToClassName.get(code) match {
      case Some(className) =>
        className
      case None =>
        val className = s"code_$classNameNumber"
        classNameNumber += 1
        codeToClassName(code) = className
        className
    }

    def compileSelection[DATUM : TypeTag](code: String): Function1[DATUM, Double] = {
      val className = classNameForCode(code)

      val datum = typeOf[DATUM]

      val wrappedInClass = s"""
class $className extends Function1[$datum, Double] {
  import SelectionImplicits._
  val f: Selection[$datum] = {
$code
  }
  def apply(x: $datum): Double = f(x)
} """
      compileClass[Function1[DATUM, Double]](className, wrappedInClass)
    }

    def compileClass[T](className: String, wrappedInClass: String) = {
      val cls = findClass(className).getOrElse {
        val sourceFiles = List(new BatchSourceFile("(inline)", wrappedInClass))
        (new global.Run).compileSources(sourceFiles)
        findClass(className) match {
          case Some(x) => x
          case None => throw new RuntimeException(s"compilation failed for the following code:\n\n$wrappedInClass")
        }
      }
      cls.getConstructor().newInstance().asInstanceOf[T]
    }

    private def findClass(className: String): Option[Class[_]] = {
      synchronized {
        classCache.get(className).orElse {
          try {
            val cls = classLoader.loadClass(className)
            classCache(className) = cls
            Some(cls)
          } catch {
            case e: ClassNotFoundException => None
          }
        }
      }
    }
  }
}
