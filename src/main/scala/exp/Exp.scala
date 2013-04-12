package exp

import bound._
import bound.Scope._
import scalaz._
import Scalaz._

/**
data Exp a
  = V a
  | Exp a :@ Exp a
  | Lam (Scope () Exp a)
  | Let [Scope Int Exp a] (Scope Int Exp a)
  deriving (Eq,Ord,Show,Read)

  -- | Reduce a term to weak head normal form
  whnf :: Exp a -> Exp a
  whnf e@V{}   = e
  whnf e@Lam{} = e
  whnf (f :@ a) = case whnf f of
    Lam b -> whnf (instantiate1 a b)
    f'    -> f' :@ a
  whnf (Let bs b) = whnf (inst b)
    where es = map inst bs
          inst = instantiate (es !!)
  */

//https://github.com/ekmett/bound/blob/master/examples/Simple.hs
object Exp {

  sealed trait Exp[+A]
    case class V[+A](a:A) extends Exp[A]
    case class Lam[+A](s: Scope[Unit, Exp, A]) extends Exp[A]
    case class App[+A](f: Exp[A], x: Exp[A]) extends Exp[A]
    case class Let[+A](bindings: List[Scope[Int, Exp, A]], body: Scope[Int, Exp, A]) extends Exp[A]

  implicit def expMonad: Monad[Exp] = new Monad[Exp]{
    def point[A](a: => A) = V(a)
    def bind[A,B](e: Exp[A])(f: A => Exp[B]): Exp[B] = e match {
      case V(a)          => f(a)
      case Lam(s)        => Lam(s >>>= f)
      case App(fun, arg) => App(fun >>= f, arg >>= f)
      case Let(bs, b)    => Let(bs map (_ >>>= f), b >>>= f)
    }
  }

  def instantiateR[B,F[+_],A](f: B => F[A])(s: Scope[B,F,A])(implicit M: Monad[F]): F[A] =
    instantiate(s)(f)

  def nf[A](e:Exp[A]): Exp[A] = e match {
    case V(_)       => e
    case Lam(b)     => Lam(toScope(nf(fromScope(b))))
    case App(f, a)  => whnf(f) match {
      case Lam(b)   => nf(instantiate1(a, b))
      case f1       => App(nf(f), nf(a))
    }
    case Let(bs, b) =>
      def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Exp,A] => Exp[A]
      def es: List[Exp[A]] = bs.map(inst)
      nf(inst(b))
  }

  def whnf[A](e: Exp[A]): Exp[A] = e match {
    case V(_)       => e
    case Lam(_)    => e
    case App(f, a) => whnf(f) match {
      case Lam(b)  => instantiate1(a, b)
      case _        => App(f, a)
    }
    case Let(bs, b)  =>
      def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Exp,A] => Exp[A]
      def es: List[Exp[A]] = {
        val res = bs.map(inst)
        println(res)
        res
      }
      whnf(inst(b))
  }

  //  A smart constructor for Lamb
  //  >>> lam "y" (lam "x" (V "x" :@ V "y"))
  //  Lamb (Scope (Lamb (Scope (V (B ()) :@ V (F (V (B ())))))))
  def lam[A,F[+_]](v: A, b: Exp[A])(implicit m: Monad[F], e: Equal[A]) = Lam(abstract1(v,b))

  val x: Scope[Unit, Exp, Var[Unit, Exp[Nothing]]] = Scope(App(V (B ()), V (F (V (B ())))))
  val y: Scope[Unit, Exp, Nothing] = Scope(V (B ()))
  val z = Lam(Scope(Lam(y)))
}