package deriving.scala

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait Lens[A, B] {

  def get(a: A): B

  def set(a: A, b: B): A

}

object Lens {

  def lens[A, B](field: String): Lens[A, B] = macro lensImpl[A, B]

  def lensImpl[A, B](c: Context)(field: c.Expr[String])(implicit A: c.WeakTypeTag[A], B: c.WeakTypeTag[B]): c.Tree = {
    import c.universe._
    val Lens = symbolOf[Lens[_, _]]
    val name = TermName(c.eval(field))
    q"""new $Lens[$A, $B] {
          def get(a: $A): $B = a.$name
          def set(a: $A, b: $B): $A = a.copy($name = b)
        }"""
  }

}
