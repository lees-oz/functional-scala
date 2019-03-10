// Copyright(C) 2018-2019 John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._
import net.degoes.abstractions

import scala.None
import scala.concurrent.Future

object algebra {
  type ??? = Nothing

  trait Semigroup[A] {
    // append(a, append(b, c)) == append(append(a, b), c) // (associativity law) - place parentheses wherever, don't change order
    // or same: (a |+| (b |+| c)) == ((a |+| b) |+| c)
    def append(f: A, r: => A): A
  }

  //
  // EXERCISE 1
  //
  // Define a semigroup instance for `String`.
  //
  implicit val StringSemigroup: Semigroup[String] =
    new Semigroup[String] {
      def append(l: String, r: => String): String = s"$l$r"
    }

  //
  // EXERCISE 2
  //
  // Define a semigroup instance for the `NotEmpty` data type below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] =
    new Semigroup[NotEmpty[A]] {
      def append(l: NotEmpty[A], r: => NotEmpty[A]): NotEmpty[A] =
        NotEmpty(l.head, l.tail.map(append(_, r)) orElse Some(r))
    }
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 3
  //
  // Define a semigroup for `Max` that chooses the maximum of two values.
  //
  final case class Max(value: Int)
  implicit val MaxSemigroup: Semigroup[Max] =
    new Semigroup[Max] {
//      def append(l: Max, r: => Max): Max =
//        Max(if(l.value > r.value) l.value else r.value)

      def append(l: Max, r: => Max): Max =
        Max(l.value max r.value)
    }

  //
  // EXERCISE 4
  //
  // Define a `Semigroup` for `Last[A]` that always chooses the right-most value.
  //
  final case class Last[A](value: A)
  object Last {
    implicit def LastSemigroup[A]: Semigroup[Last[A]] =
      new Semigroup[Last[A]] {
        def append(l: Last[A], r: => Last[A]): Last[A] = ???
      }
  }
  final case class First[A](value: A)
  object First {
    implicit def FirstSemigroup[A]: Semigroup[First[A]] =
      new Semigroup[First[A]] {
        def append(l: First[A], r: => First[A]): First[A] = l
      }
  }

  //
  // EXERCISE 5
  //
  // Define a `Semigroup` for `Option[A]` whenever `A` forms a `Semigroup`.
  //
  implicit def OptionSemigroup[A: Semigroup]: Semigroup[Option[A]] =
    new Semigroup[Option[A]] {
      def append(l: Option[A], r: => Option[A]): Option[A] =
        (l, r) match {
          case (   None,    None) => None
          case (Some(l),    None) => Some(l)
          case (   None, Some(r)) => Some(r)
//          case (Some(l), Some(r)) => Semigroup[A].append(Some(l), Some(r))
          case (Some(l), Some(r)) => Some(l |+| r)
        }
    }

  //
  // EXERCISE 6
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def append(l: (A, B), r: => (A, B)): (A, B) = (l._1 |+| r._1, l._2 |+| r._2)
    }

  //
  // EXERCISE 7
  //
  // Define a monoid for boolean conjunction (`&&`).
  //

  trait Monoid[A] extends Semigroup[A] {
    // methods
    def zero: A

    // laws
    def leftIdentity(a: A)(implicit eq: Equal[A]): Boolean =
      append(a, zero) === a
    def rightIdentity(a: A)(implicit eq: Equal[A]): Boolean =
      append(zero, a) === a
  }
  // a |+| zero == a
  // zero |+| a == a

  final case class Conj(value: Boolean)
  object Conj {
    implicit val ConjMonoid: Monoid[Conj] =
      new Monoid[Conj] {
        def zero: Conj = Conj(true)
        def append(l: Conj, r: => Conj): Conj = Conj(l.value && r.value)
      }
  }

  //
  // EXERCISE 8
  //
  // Define a monoid for boolean disjunction (`||`).
  //
  final case class Disj(value: Boolean)
  object Disj {
    implicit val DisjMonoid: Monoid[Disj] =
      new Monoid[Disj] {
        def zero: Disj = Disj(false)
        def append(l: Disj, r: => Disj): Disj = Disj(l.value || r.value)
      }
  }

  //
  // EXERCISE 9
  //
  // Define a `Monoid` for `Try[A]` whenever `A` forms a `Semigroup`.
  //
  case object ZeroThrowable extends Throwable
  def TryMonoid[A: Semigroup]: Monoid[scala.util.Try[A]] =
    new Monoid[scala.util.Try[A]] {
      import scala.util._

      def zero: Try[A] =

      def append(l: Try[A], r: => Try[A]): Try[A] =
        ???
    }

  //
  // EXERCISE 10
  //
  // Write the `Monoid` instance for `Map` when the values form a semigroup.
  //
  def MonoidMap[K, V: Semigroup]: Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map()

      def append(l: Map[K, V], r: => Map[K, V]): Map[K, V] = {
//        val keys = l
      }
    }

  //
  // EXERCISE 11
  //
  // Design a permission system for securing some resource. Design the core
  // permissions data type, implement a monoid for the instance, and implement
  // the missing methods for the data type.
  //
  // Assumptions:
  //   1. Users have multiple accounts (`AccountID`)
  //   2. Each account gives different capabilities (`Capability`) to
  //      different resources (`ResourceID`)
  //
  type AccountID = String
  type ResourceID = String
  sealed trait Capability
  object Capability {
//    final case object No extends Capability
    final case object Read extends Capability
    final case object Write extends Capability
  }
  case class UserPermission(permissions: Map[ResourceID, Set[(AccountID, Capability)]]) {
    def allResources: Set[ResourceID] = permissions.keySet

    def capabilitiesFor(resourceID: ResourceID): Set[Capability] =
      permissions(resourceID).map(_._2)

    def audit(resourceID: ResourceID, capability: Capability): Set[AccountID] =
      permissions(resourceID).filter(x => x._2 == capability).map(_._1)
  }
  implicit val MonoidUserPermission: Monoid[UserPermission] =
    new Monoid[UserPermission] {
      def zero: UserPermission = UserPermission(Map())

      def append(l: UserPermission, r: => UserPermission): UserPermission = {

      }
    }
  val example2 = mzero[UserPermission] |+| UserPermission(???)

  //
  // EXERCISE 12
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {
//  trait Functor[F[_]] {
//    def map[A, B](fa: F[A])(f: A => B): F[B]
//  }

  /**
   * Identity Law
   *   map(fa)(identity) == fa
   *
   * Composition Law (composition if A => B and B => C is A => C)
   *   map(map(fa)(g), f) == map(fa, f.compose(g))
   */

  val Numbers  = List(12, 123, 0, 123981)
  val Expected = List( 2,   3, 1,      6)
  val g : Int    => String = (i:    Int) => i.toString
  val f : String =>    Int = (s: String) => s.length
  Numbers.map(identity)    == Numbers
  Numbers.map(f compose g) == Numbers.map(g).map(f)

  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  final case class Leaf[A](a: A) extends BTree[A]
  final case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(l, r) => Fork(map(l)(f), map(r)(f))
        }
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  // It compiles because `Nothing` type is _polykinded_ (it is `*`, `* => *`, and etc)
  implicit val NothingFunctor: Functor[Nothing] = ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  def ParserFunctor[E]: Functor[Parser[E, ?]] =
    new Functor[Parser[E, ?]] {
      def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] =
         ???
    }
  final case class Parser[+E, +A](run: String => Either[E, (String, A)])
  object Parser {
    final def fail[E](e: E): Parser[E, Nothing] =
      Parser(input => Left(e))

    final def point[A](a: => A): Parser[Nothing, A] =
      Parser(input => Right((input, a)))

    final val char: Parser[Unit, Char] =
      Parser(input =>
        if (input.length == 0) Left(())
        else Right((input.drop(1), input.charAt(0))))
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] =
    new Functor[DataType] {
      def map[A, B](fa: DataType[A])(f: A => B): DataType[B] =
        ???
    }

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] =
      new Functor[FunctorProduct[F, G, ?]] {
        def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] =
          FunctorProduct(fa.l map f, fa.r map f) // TODO - razobratsa
      }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] =
      new Functor[FunctorSum[F, G, ?]] {
        def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] =

      }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  // e.g. Future[Option[A]], Future[Either[Error, Option[A]]]
  // Future[List[Either[Error, Option[A]]]]
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorNest[F, G, ?]] =
      new Functor[FunctorNest[F, G, ?]] {
        def map[A, B](fa: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] =
          FunctorNest(fa.run.map(_.map(f))) // TODO
      }

  //
  // EXERCISE 8
  //
  // Define a natural transformation between `List` and `Option`.
  //
  trait NaturalTransformation[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }
  type ~> [F[_], G[_]] = NaturalTransformation[F, G]
  val ListToOption: List ~> Option = ???
  ListToOption(List(1, 2, 3))
  ListToOption(List("foo", "bar", "baz"))

  new NaturalTransformation[List[_], Option[_]] {
    override def apply[A](fa: List[_][A]): Option[_][A] = ???
  }

  //
  // EXERCISE 9
  //
  // Define a natural transformation between `Either[Throwable, ?]` and
  // `Future`.
  //
  val EitherToFuture: Either[Throwable, ?] ~> scala.concurrent.Future =
    new NaturalTransformation[Either[Throwable, ?], scala.concurrent.Future] {
      def apply[A](fa: Either[Throwable, A]): scala.concurrent.Future[A] =
        fa.fold(Future.failed, Future.successful)
    }

  //
  // EXERCISE 10
  //
  // Define an instance of `Zip` for `Option`.
  //
  trait Zip[F[_]] extends Functor[F] {
    def zip[A, B](l: F[A], r: F[B]): F[(A, B)]
  }
  object Zip {
    def apply[F[_]](implicit F: Zip[F]): Zip[F] = F

    implicit val ZipOption: Zip[Option] =
      new Zip[Option] {
        def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)

        def zip[A, B](l: Option[A], r: Option[B]): Option[(A, B)] = (l, r) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }
      }
  }
  implicit class ZipSyntax[F[_], A](left: F[A]) {
    def zip[B](right: F[B])(implicit F: Zip[F]): F[(A, B)] =
      F.zip(left, right)
  }

  //
  // EXERCISE 11
  //
  // Define an instance of `Zip` for `List`
  //
  val ZipList: Zip[List] =
    new Zip[List] {
      def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa map f

      def zip[A, B](l: List[A], r: List[B]): List[(A, B)] =
        for {a <- l; b <- r} yield (a, b) // cartesian cross product

    }

  //
  // EXERCISE 12
  //
  // Define an instance of `Zip` for `Parser[E, ?]`.
  //
  def ZipParser[E]: Zip[Parser[E, ?]] =
    new Zip[Parser[E, ?]] {
      def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] =
        ParserFunctor.map(fa)(f)

      // TODO razobratsa
      def zip[A, B](l: Parser[E, A], r: Parser[E, B]): Parser[E, (A, B)] =
        Parser[E, (A, B)](input =>
          l.run(input) match {
            case Left(e1) => Left(e1)
            case Right((input, a)) => r.run(input) match {
              case Left(e2) => Left(e2)
              case Right((input, b)) => Right(input, (a, b))
            }
          }
        )
    }

  //
  // EXERCISE 13
  //
  // Define an instance of `Zip` for `Future`.
  //
  val ZipFuture: Zip[scala.concurrent.Future] =
    new Zip[scala.concurrent.Future] {
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global

      def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

      def zip[A, B](l: Future[A], r: Future[B]): Future[(A, B)] =
        for {
          a <- l
          b <- r
        } yield (a, b)
    }

  //
  // EXERCISE 14
  //
  // Define `Applicative` for `Option`.
  //
//  val OptionApplicative: Applicative[Option] =
//    new Applicative[Option] {
//      def point[A](a: => A): Option[A] = Some(a)
//
//      def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
//        a <- fa
//        b <- fb
//      } yield Some((a, b))
//
//      final def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
//        zip(f, fa).map(t => t._1(t._2))
//
//      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
//        for {
//          a <- fa
//        } yield Some(f(a))
////        fa.flatMap(x => Some(f(x)))
//    }

  //
  // EXERCISE 15
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  // Bonus: Implement `ap2` in terms of `zip`.
  //

//  trait Applicative[F[_]] extends Zip[F] {
//    def point[A](a: => A): F[A] // for Option - Some(v), for Future - Future.successful... packing into type
//  }

  val example1 = (Option(3) |@| Option(5))(_ + _)
  val example2 = zip(Option(3), Option("foo")) : Option[(Int, String)]
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    ???
  def ap2[F[_]: Zip, A, B](fa: F[A], fab: F[A => B]): F[B] =
    ???

//
//  final def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
//    zip(f, fa).map(t => t._1(t._2))
//  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] = {
//    // Applicative[F].ap(l)(r.map(b => (_: A, b)))
//    (l |@| r)(_ -> _)
//  }
//
//  def ap2[F[_]: Zip, A, B](fa: F[A], fab: F[A => B]): F[B] = {
//    Zip[F].map(fa.zip(fab)) { case (a, ab) => ab(a) }
//  }

  //
  // EXERCISE 16
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E, A] =
        ???

      def zip[A, B](fa: Parser[E, A], fb: Parser[E, B]): Parser[E, (A, B)] =
        ZipParser.zip(fa, fb)

      final def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] =
        zip(f, fa).map(t => t._1(t._2))
    }

  //
  // EXERCISE 17
  //
  // Define an instance of `Monad` for `BTree`.
  //
  // Composing to different F's into a single F.

  trait Monada[F[_]] extends Applicative[F] {
    def bind[A, B](fa: F[A])(f: A => F[B])
  }

  sealed trait DSL[+A]
  case class Fail(message: String) extends DSL[Nothing]
  case class Return[A](value: A) extends DSL[A]
  case class Push[A](value: Int, next: DSL[A]) extends DSL[A]
  case class Pop[A](next: Int => DSL[A]) extends DSL[A]

  implicit val DSLMonad: Monad[DSL] = new Monad[DSL] {
    def point[A](a: => A): DSL[A] = Return(a)
    def bind[A, B](fa: DSL[A])(f: A => DSL[B]): DSL[B] = fa match {
      case Fail(message) => Fail(message)
      case Return(value) => f(value)
      case Push(value, next) => Push(value, bind(next)(f))
      case Pop(next) => Pop(i => bind(next(i))(f))
    }
  } // TODO razobratsa


//  trait Optionn[+A] { self =>
//    def flatMap[B](f: A => ...)
//  }
  //////////


  implicit val MonadBTree: Monad[BTree] =
    new Monad[BTree] {
      def point[A](a: => A): BTree[A] =
        ???

      def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
        ???
    }

  //
  // EXERCISE 18
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] =
    new Monad[Parser[E, ?]] {
      def point[A](a: => A): Parser[E, A] =
        Parser[E, A]((input: String) => Right((input, a)))

      def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] = Parser { input =>
        fa.run(input) match {
          case  Left(e) => Left(e)
          case Right((rem, a)) =>
            f(a).run(rem)
        } // TODO razobratsa

      }
    }

  //
  // EXERCISE 19
  //
  // Define an instance of `Monad` for `Identity`.
  //
  case class Identity[A](run: A)
  implicit val IdentityMonad: Monad[Identity] =
    new Monad[Identity] {
      def point[A](a: => A): Identity[A] = Identity(a)

      def bind[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa.run) // TODO razobratsa
    }

  for {
    x <- Identity(2)
    y <- Identity(x * x)
    z <- Identity(y + x * y + 100)
    z <- Identity(z + z)
  } yield z

//  equivalent to:
//  Identity(2).flatMap(x =>
//    Identity(x * x).flatMap(y =>
//      Identity(y + x * y + 100).flatMap(z =>
//        Identity(z + z).map(z => z)
//      )
//    )
//  )

  //
  // EXERCISE 20
  //
  // Use the list monad to find pairs of numbers separated by 2 integers away
  // from each other.
  //
  val integers = List(1, 9, 2, 7, 4, 8, 2, 0, 10)
  val solution =
    for {
      x <- integers
      y <- integers
      t <- if((x - y).abs == 3) List((x, y)) else Nil
    } yield t
}

object parser {
  //
  // EXERCISE 1
  //
  // Implement all missing methods for parser.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)]) { self =>
    def ~ [E1 >: E, B](that: => Parser[E1, B]): Parser[E1, (A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

    def ~> [E1 >: E, B](that: => Parser[E1, B]): Parser[E1, B] =
      (self ~ that).map(_._2)

    def <~ [E1 >: E, B](that: => Parser[E1, B]): Parser[E1, A] =
      (self ~ that).map(_._1)

    def map[B](f: A => B): Parser[E, B] =
      flatMap(f.andThen(Parser.point[B](_)))

    def flatMap[E1 >: E, B](f: A => Parser[E1, B]): Parser[E1, B] =
      ???

    def orElse[E2, B](that: => Parser[E2, B]): Parser[E2, Either[A, B]] =
      Parser { input =>
        self
          .map(Left(_))
          .run(input)
          .left
          .flatMap(_ =>
            that
              .map(Right(_))
              .run(input))
      } // TODO razobratsa

    def filter[E1 >: E](e0: E1)(f: A => Boolean): Parser[E1, A] =
      Parser(input =>
        self.run(input) match {
          case Left(e) => Left(e)
          case Right((input, a)) => if (f(a)) Right((input, a)) else Left(e0)
        })

    def | [E2, A1 >: A](that: => Parser[E2, A1]): Parser[E2, A1] =
      (self orElse (that)).map(_.merge)

    def rep: Parser[E, List[A]] =
      ((self.map(List(_)) | Parser.point[List[A]](Nil)) ~ rep).map(t => t._1 ++ t._2)

    def rep1: Parser[E, (A, List[A])] = self ~ rep

    def repsep[E1 >: E](sep: Parser[E1, Any]): Parser[E1, List[A]] =
      ((self <~ sep).rep ~ (self ?)).map {
        case (list, opt) => list ++ opt.toList
      }

    def rep1sep[E1 >: E](sep: Parser[E1, Any]): Parser[E1, (A, List[A])] =
      (self <~ (sep ?)) ~ repsep(sep)

    def ? : Parser[Nothing, Option[A]] = self.map(Some(_)) | Parser.point(None)
  }
  object Parser {
    def fail[E](e: E): Parser[E, Nothing] =
      Parser(input => Left(e))

    def point[A](a: => A): Parser[Nothing, A] =
      ???

    def maybeChar: Parser[Nothing, Option[Char]] = char ?

    def char: Parser[Unit, Char] =
      Parser(input =>
        if (input.length == 0) Left(())
        else Right((input.drop(1), input.charAt(0))))

    def digit: Parser[Unit, Int] =
      for {
        c <- char
        option = scala.util.Try(c.toString.toInt).toOption
        d <- option.fold[Parser[Unit, Int]](Parser.fail(()))(point(_))
      } yield d

    def literal(c0: Char): Parser[Unit, Char] =
      for {
        c <- char
        _ <- if (c != c0) Parser.point(c) else Parser.fail(())
      } yield c

    def whitespace: Parser[Nothing, Unit] =
      Parser(input => Right((input.dropWhile(_ == ' '), ())))
  }

  // [1,2,3]
  import Parser.{ digit, literal }
  val parser: Parser[Unit, List[Int]] =
    for {
      _       <- literal('[')
      digits  <- digit.repsep(literal(','))
      _       <- literal(']')
    } yield digits
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `List`
  //
  implicit val FoldableList: Foldable[List] = new Foldable[List] {
    def foldMap[A, B: Monoid](fa: List[A])(f: A => B): B =
      ???

    def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B =
      ???
  }

  //
  // EXERCISE 2
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val FoldableBTree: Foldable[BTree] =
    new Foldable[BTree] {
      def foldMap[A, B: Monoid](fa: BTree[A])(f: A => B): B =
        fa match {
          case Leaf(v) => f(v)
          case Fork(l, r) => Monoid[B].append(foldMap(l)(f), foldMap(r)(f))
        }

      def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B =
        fa match {
          case Leaf(v) => f(v, z)
          case Fork(l, r) => foldRight(l, foldRight(r, z)(f))(f)
        }
    } // TODO razobratsa
  val btree: BTree[String] = Fork(Leaf("foo"), Fork(Leaf("bar"), Leaf("baz")))

  //
  // EXERCISE 3
  //
  // Try to define an instance of `Foldable` for `A0 => ?`.
  //
  implicit def FunctionFoldable[A0]: Foldable[A0 => ?] = ???

  //
  // EXERCISE 4
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit lazy val TraverseBTree: Traverse[BTree] =
    new Traverse[BTree] {
      def traverseImpl[G[_]: Applicative, A, B](fa: BTree[A])(f: A => G[B]): G[BTree[B]] =
        fa match {
          case Leaf(a) => f(a).map(Leaf(_))
          case Fork(l, r) =>
            val l2 = traverseImpl(l)(f)
            val r2 = traverseImpl(r)(f)
            (l2 |@| r2)((l, r) => Fork(l, r))  // |@| is zip TODO razobratsa
        }
    }

  //
  // EXERCISE 5
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] =
    new Traverse[Parser[E, ?]] {
       def traverseImpl[G[_]: Applicative, A, B](fa: Parser[E, A])(f: A => G[B]): G[Parser[E,B]] =
         ???
    }
}

object optics {
  sealed trait Country
  object Country {
    val usa: Prism[Country, Unit] =
      Prism[Country, Unit](
        _ match { case USA => Some(()); case _ => None },
        _ => USA)

    val uk: Prism[Country, UKRegion] = ???

    val poland: Prism[Country, Unit] = ???
  }
  case object USA                  extends Country
  case class  UK(region: UKRegion) extends Country
  case object Poland               extends Country
  sealed trait UKRegion

  case class Org(name: String, address: Address, site: Site) // could have 3 lenses - one per field
  object Org {
    val site: Lens[Org, Site] =
      Lens[Org, Site](_.site, s => _.copy(site = s))
  }

  case class Address(
    number: String,
    street: String,
    postalCode: String,
    country: Country)
  object Address {
    val country: Lens[Address, Country] =
      Lens[Address, Country](_.country, c => _.copy(country = c))
  }

  case class Site(
    manager: Employee,
    address: Address,
    employees: Set[Employee])
  object Site {
    val address: Lens[Site, Address] =
      Lens[Site, Address](_.address, a => _.copy(address = a))
    val manager: Lens[Site, Employee] =
      Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }
  case class Employee(
    name: String,
    dob: java.time.Instant,
    salary: BigDecimal,
    address: Address)
  object Employee {
    val salary: Lens[Employee, BigDecimal] =
      Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???

  //
  // EXERCISE 1
  //
  // Implement the `⋅` method of `Lens` for `Lens`.
  //
  final case class Lens[S, A](
    get: S => A,
    set: A => (S => S)
  ) { self =>
    def ⋅ [B](that: Lens[A, B]): Lens[S, B] =
      Lens[S, B](
        get = (s: S) => (that.get(self.get(s)): B) // or `that.get compose self.get`
        set = (b: B) => self.set(that.set(b)(self.get(s)))(s) // TODO that hell
      )

    def ⋅ [B](that: Optional[A, B]): Optional[S, B] = ???

    def ⋅ [B](that: Prism[A, B]): Optional[S, B] = ???

    def ⋅ [B](that: Traversal[A, B]): Traversal[S, B] = ???

    final def updated(f: A => A): S => S =
      (s: S) => self.set(f(self.get(s)))(s)
  }

  //
  // EXERCISE 2
  //
  // Create a version of `org2` that uses lenses to update the salaries.
  //
  lazy val org2 =
    org.copy(site =
      org.site.copy(manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
      ))
    ) // this is pain!
  import Org.site
  import Site.manager
  import Employee.salary
  val org2_lens: Org = (site ⋅ manager ⋅ salary).updated(_ * 0.95)

  //
  // EXERCISE 3
  //
  // Implement `⋅` for `Prism` for `Prism`.
  //
  final case class Prism[S, A](
    get: S => Option[A], // Option, because Sum might not have that term
    set: A => S) { self => //
    def ⋅ [B](that: Prism[A, B]): Prism[S, B] =
      ???

    def ⋅ [B](that: Lens[A, B]): Optional[S, B] = ???

    def ⋅ [B](that: Traversal[A, B]): Traversal[S, B] = ???

    final def select(implicit ev: Unit =:= A): S =
      set(ev(()))
  }

  //
  // EXERCISE 4
  //
  // Implement `_Left` and `_Right`.
  //
  def _Left[A, B]: Prism[Either[A, B], A] =
    ???
  def _Right[A, B]: Prism[Either[A, B], B] =
    ???

  //
  // EXERCISE 5
  //
  // Implement `⋅` for `Optional` for `Optional`.
  //
  final case class Optional[S, A](
    getOrModify: S => Either[S, A],
    set: A => (S => S)) {
    def ⋅ [B](that: Optional[A, B]): Optional[S, B] = ???

    def ⋅ [B](that: Lens[A, B]): Optional[S, B] = ???

    def ⋅ [B](that: Prism[A, B]): Optional[S, B] = ???

    def ⋅ [B](that: Traversal[A, B]): Traversal[S, B] = ???

    final def get(s: S): Option[A] = getOrModify(s).right.toOption
  }

  //
  // EXERCISE 6
  //
  // Implement `⋅` for `Traversal` for `Traversal`.
  //
  trait Traversal[S, A] { self =>
    def modifyF[F[_]: Applicative](f: A => F[A])(s: S): F[S]

    def ⋅ [B](that: Traversal[A, B]): Traversal[S, B] = ???

    def ⋅ [B](that: Optional[A, B]): Traversal[S, B] = ???

    def ⋅ [B](that: Lens[A, B]): Traversal[S, B] = ???

    def ⋅ [B](that: Prism[A, B]): Traversal[S, B] = ???
  }

  //
  // EXERCISE 7
  //
  // Modify the country of `org` using a `Prism`.
  //
  (Org.site ⋅ Site.address ⋅ Address.country ⋅ Country.usa)
}
