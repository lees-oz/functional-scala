// Copyright(C) 2018-2019 John A. De Goes. All rights reserved.

package net.degoes.essentials

import java.io.File
import java.time.{Instant, ZonedDateTime}

import scala.util.Try

object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Unit`.
  //
  // Single value there
  val UnitValues: Set[Unit] = Set(())
  val myVal: Unit = ()
//  val myVal: Unit = ... whatever

  //
  // EXERCISE 2
  //
  // List all values of the type `Nothing`.
  //
  // Empty set - no values there
  val NothingValues: Set[Nothing] = Set()
//  val a: Nothing = ... nothing can be here or "( throw ex )" will be of type Nothing throwing - EXITING value computation process
//  lazy val n: Nothing = n   // self reference, stack overflow

  //
  // EXERCISE 3
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: Set[Boolean] = Set(true, false)

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: Set[Either[Unit, Boolean]] = Set(Left(()), Right(true), Right(false))

  //
  // EXERCISE 5
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: Set[(Boolean, Boolean)] = Set((true, true), (false, true), (true, false), (false, false))

  //
  // EXERCISE 6
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: Set[Either[Either[Unit, Unit], Unit]] = Set(
    Left(Left(())),
    Left(Right(())),
    Right(Left(())),
    Right(Right(()))
  )

//  ## COmposition

  // A = { a1, a2 ..., an }
  // B = { b1, b2 ..., bn }
  // A * B = { (a, b) | a is in A, and b is in B } - product (2-way product, 2 types combined)
  // A & B - terms of product
  case class PersonA(name: String, age: Int) // there're names of Components
  type PersonB = (String, Int)                // tuple is syntax sugar on case classes
  val personB: PersonB = ("John", 42)
  personB._1
  personB._2

  //
  // EXERCISE 7
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A * B`.
  //
  val AProductB: Set[(Boolean, String)] = Set(
    (true, "red"),
    (true, "green"),
    (true, "blue"),
    (false, "red"),
    (false, "green"),
    (false, "blue")
  )
  // Size of product is product of sizes.

  // Sum type
  // A = { a1, a2 ..., an }
  // B = { b1, b2 ..., bn }
  // A + B =
  //        { Left(a)  | a is in A } UNION
  //        { Right(b) | b is in B }
  // A UNION B = { x | x is in A OR is in B }
  // Size of A + B = (size of A) + (size of B)

  sealed trait Either2[A, B] // "sealed" makes trait Sum type
  case class Left2[A, B] (value: A) extends Either2[A, B]
  case class Right2[A, B](value: B) extends Either2[A, B]

//  val either : Either[String, Int]
//  either match {
//    /// exhaustive match!
//  }

  sealed trait JobTitle
  case object Manager extends JobTitle
  case object Peon extends JobTitle
  case object Programmer extends JobTitle

  case class SalesPerson(level: Int) extends JobTitle
  // this ^^^ is a 4-way sum, size is 4bln + 3

  // |A| = size of set A
  // |A * B| = |A| * |B|
  // |A + B| = |A| + |B|
  // |Unit| = 1
  // |Nothing| = 0
  // A ~ B     - A and B are isomorphic (contains the same information, doesn't matter the order)
  // A * 1 ~ A
  // 1 * A ~ A
  // A * 0 ~ 0
  // 0 * A ~ 0
  // A * B ~ B * A


  // EXERCISE 8
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A + B`.
  //
  val ASumB: Set[Either[Boolean, String]] = ???

  //
  // EXERCISE 9
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person1 = (String, Int)
  final case class Person2(name: String, age: Int)

  //
  // EXERCISE 10
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ())

  //
  // EXERCISE 11
  //
  // Prove that `A * 0` is equivalent to `0` by implementing the following two
  // functions.
  //
  def to2[A](t: (A, Nothing)): Nothing = ??? // can't do that, can't specify a value for type Nothing!
  def from2[A](n: Nothing): (A, Nothing) = ???
  // Nothing is a subtype of every other type
  def magicalConversion[A](n: Nothing): A = n // this sounds. because of no values of type Nothing can exist.
  abstract class Void {
    def absurd[A]: A
  }
  def magical[A](v: Void): A = v.absurd


  //
  // EXERCISE 12
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or the identifier of a person (a name).
  //
  type Identifier1 = (Int, String)
  sealed trait Identifier2
  case class RobotId(id: Int) extends Identifier2
  case class PersonId(name: String) extends Identifier2

  // RDBMS - good support for Product types, weak support for Sum type (nullable column only)



  //
  // EXERCISE 13
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to3[A](t: Either[A, Nothing]): A = t.fold(identity, identity)
  def from3[A](a: A): Either[A, Nothing] = Left(a)

  //
  // EXERCISE 14
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  case class CreditCard(number: String, ownerId: String)
//  type CreditCard = ???

  //
  // EXERCISE 15
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  sealed trait PaymentMethod
  case object CreditCard extends PaymentMethod
  case object iDeal extends PaymentMethod

  //
  // EXERCISE 16
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, and employment date.
  //
  case class Employee(title: String, salary: Int, name: String, emplDate: ZonedDateTime)

  //
  // EXERCISE 17
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  sealed trait ChessPiece
  case object Pawn extends ChessPiece
  case object Rook extends ChessPiece
  case object Bishop extends ChessPiece
  case object Knight extends ChessPiece

  //
  // EXERCISE 18
  //
  // Create a "smart constructor" for `Programmer` that only permits levels
  // that are non-negative.
  //
  final case class Programmer3 private (level: Int)
  object Programmer3 {
    def apply(level: Int): Option[Programmer3] =
      if(level > 0) Some(new Programmer3(level))
      else None
  }

//  println(Programmer3(-3))

  //
  // EXERCISE 19
  // 
  // Using algebraic data types and smart constructors, make it impossible to
  // construct a `BankAccount` with an illegal (undefined) state in the 
  // business domain. Note any limitations in your solution.
  //
  case class BankAccount2[A](
    ownerId: A,
    balance: BigDecimal,
    accountType: String,
    openedDate: Long)
  val account = BankAccount2(33, -23, "fsgwrggrwg", 24440) // this is wrong - can't pass whatever values

  //
  // EXERCISE 20 
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //


  // A = { a1, a2 ... an }
  //  def constructor(a: String): Email = ???
  // string set is too big... not every string is Email. (use library "refine")
  final case class Programmer (level: Int)
  val p = new Programmer(1)
//  object Programmer {
//    def apply(level: Int): Option[Programmer] = ???
////      if(level < 0) None
////      else Some(Programmer(level))
//  }

  case class BankAcc(
                          ownerId: String, // id has structure maybe
                          balance: Int, // can't be negative maybe?
                          accountType: AccountType, // must be Sum type!
                          openedDate: Instant) // can't be in future maybe?

  sealed trait AccountType
  case object Private extends AccountType
  case object Corporate extends AccountType

  case class AccountId private (digits: Long)

  case class Balance(value: BigDecimal) {
    def apply(value: BigDecimal): Option[Balance] =
      if(value >= 0) Some(Balance(value)) else None
  }

//  final case class OwnerId private

  // Polymorphic - pushed the problem up top
  case class BankAccountTyped[A](
                          ownerId: A,
                          balance: Int,
                          accountType: AccountType,
                          openedDate: Instant)

  def transfer[A](amount: BigDecimal, acc1: BankAccountTyped[A], acc2: BankAccountTyped[A]) = ???

//  case class BankAccount private(ownerId: String,
//                                 balance: BigDecimal,
//                                 accountType: String,
//                                 openedDate: Instant)

  // Someone's solution:
//  object BankAccount {
//    def apply(ownerId: String,
//              balance: BigDecimal,
//              accountType: String,
//              openedDate: Instant): Option[BankAccount] =
//      if (balance < 0) None
//      else if (openedDate.isAfter(Instance.now) None
//      else if (!List("checkings", "savings").contains(accountType) None
//      else Some(BankAccount(ownerId, balance, accountType, openedDate))

  type GameWorld = ???
}

object functions {
  type ??? = Nothing

  // Math definition of function
  // Set Domain =     { a1, a2 ... an }
  // Set Codomain =   { b1, b2, ... bn }
  // A functio  d: Domain => Codomain
  // is a mapping from one set (Domain) to the other (Codomain)
  // such that for _every_ `a` in `Domain` , `f(a)` is in `Codomain`.

  // f is a function if and only if:
  // 1. Totality. If a is in the Domain, then f(a) is in Codomain
  //
  // 2. Deterministic. If a = b, then f(a) = f(b).
  //
  // 3. Free of side effects. The only effect of applying f to a is computing f(a).

  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  def parseInt1(s: String): Int = s.toInt
  def parseInt2(s: String): Either[Throwable, Int] = Try { s.toInt }.toEither

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.update(i, f(arr(i))) // this is not total and side effect-y
//  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Option[Array[A]] = {
//    i match {
//      case i if i >= 0 && i < arr.length =>
//        Some(arr.updated(i, f(arr(i))))
//      case _ => None
//    }
//  }

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Either[Throwable, Int] = Try { a / b}.toEither

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }
  def freshId2(currentId: Int): Int = currentId + 1 // should accept last identifier

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1)
//  def afterOneHour2(now: Instant): LocalDateTime = now. // relied on now - pass now from above!

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = { // wrong proposition - what if we pass Nil?
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }
  def head2[A](as: List[A]): Either[Unit, A] = as match {
    case Nil => Left(())
    case h :: _ => Right(h)
  }

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
//  trait Account
//  trait Processor {
//    def charge(account: Account, amount: Double): Unit
//  }
//  case class Coffee() {
//    val price = 3.14
//  }
//  def buyCoffee1(processor: Processor, account: Account): Coffee = {
//    val coffee = Coffee()
//    processor.charge(account, coffee.price)
//    coffee
//  }
//  final case class Charge(account: Account, amount: Double)
//  def buyCoffee2(account: Account): (Coffee, Charge) = ???

  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }
  final case class Charge(account: Account, amount: Double)
//  def buyCoffee2(account: Account, charge: Charge => Unit): (Coffee, Charge) = {
//    val coffee = Coffee()
//    charge(coffee.price)
//    (coffee, Charge(account, coffee.price))
//  }
//  def buyCoffee3(account: Account): (Coffee, Charge) = {
//    val coffee = Coffee()
//    (coffee, Charge(account, coffee.price))
//  }


  // Scalazzi subset
  // 1. No null (always use Option instead)
  // 2. No exceptions (are fine, but considered best practice to not use exceptions. throw is fine, catch is bad)
  //                  (use Either, Try & friends)
  // 3. Only "math" functions
  // 4. No reflection (includes type casing) (avoid, if super needed - macros instead)
  // 5. No 'Any' methods (java.lang.Object)


  def buyCoffee2(account: Account): ??? = ???

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = ()

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def readLine: String = ???

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = ???

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A =
    List(
       "Welcome to the help page!",
        "To list commands, type `commands`.",
        "For help on a command, type `help <command>`",
        "To exit the help page, type `exit`."
    ).map(println).reduce(combine)

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x = 0
    var y = 0

    def goLeft(): Unit = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit = y += 1
    def goDown(): Unit = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }
  def draw2(size: Int /* ... */): ??? = ???

  // Implementation:
//  case class CanvasState(x: Int, y: Int, bitmap: List[List[Boolean]])
//  type Draw = CanvasState => CanvasState

  // OR sumtype per command type
}

object higher_order {

//  def foo(x: Int): Int = ??? // Monomorphic function
  def foo[A, B, C](x: A): A = ??? // Polymorphic function. This is Parametric polymorphism
  // takes list of types and list of values

  // from mono to poly - there's fewer ways of implementing function
  // def foo(x: Int): Int = 4 - infinite implementations
  // def foo[A](x: A): A = x - only one implementation!
  // Can i implement it with less information? if yes - make it polymorphic

  case class Age(value: Int)
  case class Person(name: String, age: Age)
  def parseCSV(list: List[List[String]]): List[Person] = ???

  // String => Age
  // ooor
  // String => Int
  // Int => Age

//  def joinAge(f: String => Int, g: Int => Age): String => Age =
//    (s: String) => g(f(s))

  // Instead:
  def compoze[A, B, C](f: A => B, g: B => C): A => C =
    (s: A) => g(f(s))


  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) =
    (a: A) => (f(a), g(a))

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) =
    (a: A, c: C) => (f(a), g(c))

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B =
    {
      case Right(c) => g(c)
      case Left(a) => f(a)
    }

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] =
    {
      case Left(a) => Left(f(a))
      case Right(c) => Right(g(c))
    }

  //
  // EXERCISE 5
  //
  // Implement the following higher-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  //
  // EXERCISE 6
  //
  // Implement the following higher-order function. After you implement
  // the function, interpret its meaning.
  // !!! this is FP cards game - use cards (funcs and values) that are available in scope.
  // !!! figure out how that works and write yourself

  //  def alt[E1, E2, A, B](l: Parser[E1, A], r: E1 => Parser[E2, B]): Parser[(E1, E2), Either[A, B]] = //???
//  {
//
//    val stringToTupleOrTuple: String => Either[(E1, E2), (String, Either[A, B])] = (input: String) => l.run(input) match {
//      case Left(e1) => r(e1).run(input) match {
//        case Left(e2) => Left((e1, e2))
//        case Right((input, b)) => Right((input, Right(b)))
//      }
//      case Right((input, a)) => Right((input, Left(a)))
//    }
//
//    Parser[(E1, E2), Either[A, B]](stringToTupleOrTuple)
//  }


  def alt[E1, E2, A, B](l: Parser[E1, A], r: E1 => Parser[E2, B]): Parser[(E1, E2), Either[A, B]] = {

    val parse: String => Either[E, (String, A)] = ???

    Parser(parse)
  }


  case class Parser[+E, +A](run: String => Either[E, (String, A)]) // Polymorphic data type
  object Parser {
    final def fail[E](e: E): Parser[E, Nothing] = // Nothing says it cant succeed
      Parser(_ => Left(e))

    final def succeed[A](a: => A): Parser[Nothing, A] = // Nothing says it cant fail
      Parser(input => Right((input, a)))

    final def char: Parser[Unit, Char] = // Unit - because there's only one way it fails - end of string
      Parser(input =>
        if (input.length == 0) Left(())
        else Right((input.drop(1), input.charAt(0))))
  }

  def toLeft[A](v: Either[A, Nothing]): A = v match {
    case Left(a) => a
    case Right(n) => n
  }
}

object poly_functions {

  // Methods vs functions
  def identity[A](a: A): A = a
  // val identity2: A => A = (a: A) => // can't be polymorphic
  // Workaround - have it in object or trait's "apply" method

  //
  // EXERCISE 1
  //
  // Create a polymorphic function of two type parameters `A` and `B` called
  // `snd` that returns the second element out of any pair of `A` and `B`.
  //
  object snd {
    def apply[A, B](t: (A, B)): B = ???
  }
  snd((1, "foo")) // "foo"
  snd((true, List(1, 2, 3))) // List(1, 2, 3)

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    def apply[A](n: Int)(a: A, f: A => A): A =
      (1 to n).foldLeft(f(a))((acc, n) => f(acc))
  }
  repeat[       Int](100)(       0, _ +   1) // 100
  repeat[    String]( 10)(      "", _ + "*") // "**********"
  repeat[Int => Int](100)(identity, _ andThen (_ + 1)) // (_ + 100)

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = ???
  val countExample1Answer = ???

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B =
    ???
  val countExample2Answer = ???

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy1`.
  //
  val TestData =
    "poweroutage;2018-09-20;level=20" :: Nil

  val ByDate: String => String =
    (data: String) => data.split(";")(1) // groups by date

  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"
  val ExpectedResults =
    Map("2018-09-20" ->
      "On date 2018-09-20, there were 1 power outages")
////  def groupBy1(
////    events: List[String],
////    by: String => String)(
////      reducer: (String, List[String]) => String):
////        Map[String, String] =
////    events.groupBy(by) map {
////      case (k, v) => (k, reducer.apply(v))
////    }
////    events.map(e => (by(e), e)).toMap
//  // use .groupBy
//
//
//  groupBy1(TestData, By)(Reducer) == Expected   // Monomorphic

  def groupBy1(
    events: List[String],
    by: String => String)(
      reducer: (String, List[String]) => String):
      Map[String, String] =
        ???
  groupBy1(TestData, ByDate)(Reducer) == ExpectedResults

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //

//  object groupBy2 {
//    def group[A, B, C](
//                        events: List[A],
//                        by: A => B)(
//                        reducer: (B, List[A]) => C): Map[B, C] =
//    events.groupBy(by) map {
//      case (k, v) => (k, reducer(v))
//    }
//
//  }
  // groupBy2(Data, By)(Reducer) == Expected
  object groupBy2 {

  }
  // groupBy2(TestData, ByDate)(Reducer) == ExpectedResults
}

object higher_kinded {

  // Type is a sety of values
  // { 1, "foo" } is type
  // * = {Int, Boolean, List[String], Unit, Nothing..... }
  //   = {x | x is type in Scala}

  val isInsideStar: Int = ???
//  val z: List  // won't compile - List is not a type. It's `type constructor`. Type constructor is a form of function.
// Given a type - it returns a type
  // List: * => * -- not a type, but a KIND !
  // List(Int) = List[Int]
  // List(String) = List[String]
  // List(A) = List[A]
  // Type of 1 is Int
  // Kind of List is * => * - it maps from Domain of types to codomain value - of List[<type>]
  // Type of "foo" is String

  // * => * = { f | f is a type constructor that takes 1 type }
  //        = { List, Set, Option, Future, Try ... }
  // [*, *] => * = { f | f is a type constructor that takes 2 types }
  //             = { Either, Function, Tuple, ... }
  // [*, *, *] => * = { Tuple3, ZIO, Product3 ... }

//  val f: Int => Int
//  def foo[A, B] = ???
//  trait Foo[A, B]
//  type Bar = Foo[List, Int] // Kind error, because default kind is *. And we have * => *

//  def foo[A[_], B] = ???
//  trait Foo[A[_], B] // this is a type constructor 1st param kind is * => *, 2nd - *
//  type Bar = Foo[List, Int]

//  val square: Int => Int = (x: Int) = x * x

  trait Foo[A[_]] // (* => *) = *  -- this is equivalent of .map higher order function, named higher order kind param
  //  (* => *) = *   = { Functor, Monad, Foo, ... }

  trait Foo2[A[_[_]]]
  // ((* => *) => *) => *




  type ?? = Nothing
  type ???[A] = Nothing
  type ????[A, B] = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter of kind `*`
  // (i.e. has kind `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[List]

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters of kind `*` (i.e.
  // has kind `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Either]

  //
  // EXERCISE 3
  //
  // Create a new type called `Answer3` that has kind `*`.
  //
  trait Answer3 /*[]*/

  //
  // EXERCISE 4
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer4 /*[]*/

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = ???
  

  //
  // EXERCISE 5
  //
  // Create a new type that has kind `(* => *) => *`.
  // TODO
  type NewType1 /* ??? */
  type Answer5 = `(* => *) => *`[?????]

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6 /*[]*/

  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  val ListCollectionLike: CollectionLike[List] = ???

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
  }
  val ListSized: Sized[List] = ???

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //

  type MapString[A] = Map[String, A]
  val MapStringSized: Sized[MapString] = ???
//  val MapStringSized: Sized[Map[String, ?]] =
//    ???

  val plus: (Int, Int) => Int = (a, b) => a + b
  List(1, 3, 4).map(plus(1, _))

//  def plus2(v: Int) => Int = (a, b) => a + b
//  val MapStringSized: Sized[MapString] =
//    ???

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] =
    ???

  //
  // EXERCISE 11
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[C, B]: ?? = ???
}

object tc_motivating {

//  def sort(i: List[Int]): List[Int] = ??? // This monomorphic function knows too much! Let's throw away it by adding type params
  // def sort[A](i: List[A]): List[A] = ???    // And now we don't know how to sort...
  // type class is a means of identifying and describing structure for range of data types
  // type classes bring bare minimum of structure
  // we can't sort List[List[A]]...
  //

  // Commutativity: a == b THEN b == a
  // Transitivity:  a == b AND b == c THEN a == c
  // Identity       a == a

  // Interfaces in java - form of polymorphism. This is subtype (ad-hoc) polymorphism.
  // Parametric polym-sm doesn't require modifying data type (they don't make you extend their interface).
  // Ad-hoc does.
  //


  // Ad-hoc way:
//  sealed trait Ordering
//  case object Greater extends Ordering
//  case object Less extends Ordering
//  case object Eq extends Ordering
//
//  trait Comparable[A] {
//    def compare(that: A): Ordering
//  }
//
//  case class Prsn(name: String) extends Comparable[Prsn] {
//    def compare(that: Prsn): Ordering = ???
//    // if blablbalala
//  }
//
//  def sort[A <: Comparable[A]](list: List[A]): List[A] = ???
//

  // TypeClass way:
  object Scope {

    sealed trait Ordering
    case object Greater extends Ordering
    case object Less extends Ordering
    case object Eq extends Ordering

    trait Ord[A] {
      def compare(self: A, that: A): Ordering
    }

    case class Prsn(name: String)

    implicit val OrdPerson = new Ord[Prsn] {
      override def compare(self: Prsn, that: Prsn): Ordering = ??? // if blabla
    }

    implicit val OrdInt: Ord[Int] = ???
//    implicit def OrdVector[A](implicit ordA: Ord[A]): Ord[Vector[A]] = ???
    implicit def OrdVector[A: Ord]: Ord[Vector[A]] = ???

//    def sort[A](list: List[A])(implicit ord: Ord[A]): List[A] = ???
    def sort[A: Ord](list: List[A]): List[A] = {
      val a1: A = ???
      val a2: A = ???

      val ordering = implicitly[Ord[A]].compare(a1, a2)

      ???
    }

    sort(List(Vector(1, 2), Vector (3, 4, 5)))

    // improvement
    object Ord {
      def apply[A: Ord]: Ord[A] = implicitly[Ord[A]]
    }

    // more improvement
    implicit class OrdSyntax[A](self: A) {
      def =?= (that: A)(implicit O: Ord[A]): Ordering = O.compare(self, that)
    }

    4 =?= 5 // works

    Prsn("asd") =?= Prsn("wef") // works
  }



  /*
  A type class is a tuple of three things:

  1. A set of types and / or type constructors.
  2. A set of operations on values of those types.
  3. A set of laws governing the operations.

  A type class instance is an instance of a type class for a given
  set of types.

  */

  /**
   * All implementations are required to satisfy the transitivityLaw.
   *
   * Transitivity Law:
   * forall a b c.
   *   lt(a, b) && lt(b, c) ==> 
   *     lt(a, c)
   */
  trait LessThan[A] {
    def lt(l: A, r: A): Boolean

    final def transitivityLaw(a: A, b: A, c: A): Boolean =
      lt(a, c) || !lt(a, b) || !lt(b, c)
  }
  implicit class LessThanSyntax[A](l: A) {
    def < (r: A)(implicit A: LessThan[A]): Boolean = A.lt(l, r)
    def >= (r: A)(implicit A: LessThan[A]): Boolean = !A.lt(l, r)
  }
  object LessThan {
    def apply[A](implicit A: LessThan[A]): LessThan[A] = A

    implicit val LessThanInt: LessThan[Int] = new LessThan[Int] {
      def lt(l: Int, r: Int): Boolean = l < r
    }
    implicit def LessThanList[A: LessThan]: LessThan[List[A]] = new LessThan[List[A]] {
      def lt(l: List[A], r: List[A]): Boolean =
        (l, r) match {
          case (Nil, Nil) => false
          case (Nil, _) => true
          case (_, Nil) => false
          case (l :: ls, r :: rs) => l < r && lt(ls, rs)
        }
    }
  }

  def sort[A: LessThan](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort(lessThan) ++ List(x) ++ sort(notLessThan)
  }

  final case class Person(name: String, age: Int)
  object Person {
    implicit val PersonLessThan: LessThan[Person] = ???
  }

  object oop {
    trait Comparable[A] {
      def lessThan(a: A): Boolean
    }
    def sortOOP[A <: Comparable[A]](l: List[A]): List[A] =
      ???
    case class Person(name: String, age: Int) extends Comparable[Person] {
      def lessThan(a: Person): Boolean = ???
    }
  }

  sort(List(1, 2, 3))
  sort(List(List(1, 2, 3), List(9, 2, 1), List(1, 2, 9)))
}

object hashmap {
  trait Eq[A] {
    def eq(l: A, r: A): Boolean

    def transitivityLaw(a: A, b: A, c: A): Boolean = 
      eq(a, c) || !eq(a, b) || !eq(b, c)

    def identityLaw(a: A): Boolean = 
      eq(a, a)

    def reflexivityLaw(a: A, b: A): Boolean = 
      eq(a, b) == eq(b, a)
  }
  object Eq {
    def apply[A](implicit A: Eq[A]): Eq[A] = A

    implicit val EqInt: Eq[Int] =
      new Eq[Int] {
        def eq(l: Int, r: Int): Boolean = l == r
      }
    implicit val EqString: Eq[String] =
      new Eq[String] {
        def eq(l: String, r: String): Boolean = l == r
      }
  }
  final case class IgnoreCase(value: String) 
  object IgnoreCase {
    implicit val EqIgnoreCase: Eq[IgnoreCase] =
      new Eq[IgnoreCase] {
        def eq(l: IgnoreCase, r: IgnoreCase): Boolean = 
          l.value.toLowerCase == r.value.toLowerCase
      }
  }

  implicit class EqSyntax[A](l: A) {
    def === (r: A)(implicit A: Eq[A]): Boolean = A.eq(l, r)
  }

  trait Hash[A] extends Eq[A] {
    def hash(a: A): Int

    final def hashConsistencyLaw(a1: A, a2: A): Boolean =
      (hash(a1) === hash(a2)) || !eq(a1, a2)
  }
  object Hash {
    def apply[A](implicit A: Hash[A]): Hash[A] = A

    implicit val HashInt: Hash[Int] =
      new Hash[Int] {
        def hash(a: Int): Int = a

        def eq(l: Int, r: Int): Boolean = l == r
      }
  }
  implicit class HashSyntax[A](val a: A) extends AnyVal {
    def hash(implicit A: Hash[A]): Int = A.hash(a)
  }

  case class Person(age: Int, name: String)
  object Person {
    implicit val HashPerson: Hash[Person] = ???
  }

  class HashPerson(val value: Person) extends AnyVal
  object HashPerson {
    implicit val HashHashPerson: Hash[HashPerson] = ???
  }

  class HashMap[K, V] {
    def size: Int = ???

    def insert(k: K, v: V)(implicit K: Hash[K]): HashMap[K, V] = ???
  }
  object HashMap {
    def empty[K, V]: HashMap[K, V] = ???
  }

  Hash[Int].hash(3)

  trait Hashable {
    def hash: Int
  }

  class HashMapOOP[K <: Hashable, V] {
    def size: Int = ???

    def insert(k: K, v: V): HashMap[K, V] = ???
  }
}

object typeclasses {
  /**
   * {{
   * Reflexivity:   a ==> equals(a, a)
   *
   * Transitivity:  equals(a, b) && equals(b, c) ==>
   *                equals(a, c)
   *
   * Symmetry:      equals(a, b) ==> equals(b, a)
   * }}
   */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }
  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = new Eq[Int] {
      def equals(l: Int, r: Int): Boolean = l == r
    }
    implicit def EqList[A: Eq]: Eq[List[A]] =
      new Eq[List[A]] {
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil) => true
            case (Nil, _) => false
            case (_, Nil) => false
            case (l :: ls, r :: rs) =>
              Eq[A].equals(l, r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def === (r: A)(implicit eq: Eq[A]): Boolean =
      eq.equals(l, r)
  }

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering
  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = new Eq[Ordering] {
      def equals(l: Ordering, r: Ordering): Boolean = l == r
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering

    final def eq(l: A, r: A): Boolean = compare(l, r) == EQUAL
    final def lt(l: A, r: A): Boolean = compare(l, r) == LT
    final def lte(l: A, r: A): Boolean = lt(l, r) || eq(l, r)
    final def gt(l: A, r: A): Boolean = compare(l, r) == GT
    final def gte(l: A, r: A): Boolean = gt(l, r) || eq(l, r)

    final def transitivityLaw1(a: A, b: A, c: A): Boolean =
      (lt(a, b) && lt(b, c) == lt(a, c)) ||
      (!lt(a, b) || !lt(b, c))

    final def transitivityLaw2(a: A, b: A, c: A): Boolean =
      (gt(a, b) && gt(b, c) == gt(a, c)) ||
      (!gt(a, b) || !gt(b, c))

    final def equalityLaw(a: A, b: A): Boolean =
      (lt(a, b) && gt(a, b) == eq(a, b)) ||
      (!lt(a, b) || !gt(a, b))
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?= (r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def < (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <= (r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def > (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >= (r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def === (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !== (r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }
  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT else if (l.age > r.age) GT
        else if (l.name < r.name) LT else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type, and which uses the `Ord` type class, including the compare syntax
  // operator `<` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
  }
  def sort2[A: Ord](l: List[A]): List[A] = l.sortWith(implicitly[Ord[A]].compare(_, _) != GT)

  //
  // EXERCISE 2
  //
  // Create a data structure and an instance of this type class for the data
  // structure.
  //
  trait PathLike[A] {
    def child(parent: A, name: String): A

    def parent(node: A): Option[A]

    def root: A
  }
  object PathLike {
    def apply[A](implicit A: PathLike[A]): PathLike[A] = A
  }
//  type MyPath =

//  case class MyPath(nodes: List[String]) // one way

  sealed trait MyPath // other way
  case object Root extends MyPath
  case class Node(parent: MyPath, name: String) extends MyPath

  object MyPath {
    implicit val MyPathPathLike: PathLike[MyPath] =
      new PathLike[MyPath] {
        def child(parent: MyPath, name: String): MyPath = Node(parent, name)
        def parent(node: MyPath): Option[MyPath] = node match {
          case `Root` => None
          case Node(parent, _) => Some(parent)
        }
        def root: MyPath = Root
      }
  }

  //
  // EXERCISE 3
  //
  // Create an instance of the `PathLike` type class for `java.io.File`.
  //
  implicit val FilePathLike: PathLike[java.io.File] = new PathLike[java.io.File] {
    override def child(parent: File, name: String): File = new File(parent, name)

    override def parent(node: File): Option[File] = Option(node.getParentFile)

    override def root: File = new File("/")
  }

  //
  // EXERCISE 4
  //
  // Create two laws for the `PathLike` type class.
  //
  trait PathLikeLaws[A] extends PathLike[A] {
    def law1: Boolean = parent(root).isEmpty

    def law2(node: A, name: String, assertEquals: (A, A) => Boolean): Boolean =
      assertEquals(parent(child(node, name)), node)
  }

  //
  // EXERCISE 5
  //
  // Create a syntax class for path-like values with a `/` method that descends
  // into the given named node.
  //
  implicit class PathLikeSyntax[A](a: A) {
    def / (name: String)(implicit A : PathLike[A]): A = A.root

    def parent(implicit A : PathLike[A]): Option[A] = A.parent(a)
  }
  def root[A: PathLike]: A = PathLike[A].root

  root[MyPath] / "foo" / "bar" / "baz" // MyPath
  (root[MyPath] / "foo").parent        // Option[MyPath]

  //
  // EXERCISE 6
  //
  // Create an instance of the `Filterable` type class for `List`.
  //
  trait Filterable[F[_]] {
    def filter[A](fa: F[A], f: A => Boolean): F[A]
  }
  object Filterable {
    def apply[F[_]](implicit F: Filterable[F]): Filterable[F] = F
  }
  implicit val FilterableList: Filterable[List] = ???

  //
  // EXERCISE 7
  //
  // Create a syntax class for `Filterable` that lets you call `.filterWith` on any
  // type for which there exists a `Filterable` instance.
  //
  implicit class FilterableSyntax[F[_], A](fa: F[A]) {
    ???
  }
  // List(1, 2, 3).filterWith(_ == 2)

  //
  // EXERCISE 8
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait Collection[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  object Collection {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }
  implicit val ListCollection: Collection[List] = ???

  val example = Collection[List].cons(1, Collection[List].empty)

  //
  // EXERCISE 9
  //
  // Create laws for the `Collection` type class.
  //
  trait CollectionLaws[F[_]] extends Collection[F] {

  }

  //
  // EXERCISE 10
  //
  // Create syntax for values of any type that has `Collection` instances.
  // Specifically, add an `uncons` method to such types.
  //
  implicit class CollectionSyntax[F[_], A](fa: F[A]) {
    ???

    def cons(a: A)(implicit F: Collection[F]): F[A] = F.cons(a, fa)
  }
  def empty[F[_]: Collection, A]: F[A] = Collection[F].empty[A]
  // List(1, 2, 3).uncons // Some((1, List(2, 3)))
}
