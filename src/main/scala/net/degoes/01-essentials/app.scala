package net.degoes.essentials

import java.time.Instant

object app extends App {

  case class Programmer3 private (level: Int)
  object Programmer3 {
    def apply(level: Int): Option[Programmer3] =
      if(level > 0) Some(Programmer3(level))
      else None
  }
  val pro = Programmer3(-3)
  println(pro)


  case class BankAccount private(ownerId: String,
                                 balance: BigDecimal,
                                 accountType: String,
                                 openedDate: Instant)

  object BankAccount {
    def apply(ownerId: String,
              balance: BigDecimal,
              accountType: String,
              openedDate: Instant): Option[BankAccount] =
      if (balance < 0) None
      else if (openedDate.isAfter(Instant.now)) None
      else if (!List("checkings", "savings").contains(accountType)) None
      else Some(BankAccount(ownerId, balance, accountType, openedDate))
  }
}
