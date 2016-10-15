package onion
import cats.data.{NonEmptyList, Xor}
import cats.free.{Free, Inject}
import cats.implicits._
import cats.{Functor, Monad, ~>}

import scala.concurrent.{ExecutionContext, Future}

object example {
  trait Console[F[_]] {
    def readLine: F[String]
    def writeLine(line: String): F[Unit]
  }

  object Console {
    def readLine[F[_]](implicit F: Console[F]): F[String] = F.readLine
    def writeLine[F[_]](line: String)(implicit F: Console[F]): F[Unit] = F.writeLine(line)
  }

  def myProgram[F[_]: Console: Monad]: F[Unit] =
    for {
      line <- Console.readLine
      x <- Console.writeLine(s"You entered $line")
    } yield x


  type Amount = BigDecimal
  type Error = String
  type Account = String
  final case class From[A](value: A) extends AnyVal
  final case class To[A](value: A) extends AnyVal

  type TransferResult = Xor[Error, (From[Amount], To[Amount])]

  trait Banking[F[_]] {
    def accounts: F[NonEmptyList[Account]]
    def balance(account: Account): F[Amount]
    def transfer(amount: Amount, from: From[Account], to: To[Account]): F[TransferResult]
    def withdraw(amount: Amount): F[Amount]
  }

  object Banking {
    def accounts[F[_]](implicit F: Banking[F]): F[NonEmptyList[Account]] = F.accounts
    def balance[F[_]](account: Account)(implicit F: Banking[F]): F[Amount] = F.balance(account)
    def transfer[F[_]](amount: Amount, from: From[Account], to: To[Account])(implicit F: Banking[F]): F[TransferResult] = F.transfer(amount, from, to)
    def withdraw[F[_]](amount: Amount)(implicit F: Banking[F]): F[Amount] = F.withdraw(amount)
  }

  sealed trait BankingF[A]
  case class Accounts[A](next: NonEmptyList[Account] => A) extends BankingF[A]
  case class Balance[A](account: Account, next: Amount => A) extends BankingF[A]
  case class Transfer[A](amount: Amount, from: From[Account], to: To[Account], next: TransferResult => A) extends BankingF[A]
  case class Withdraw[A](amount: Amount, next: Amount => A) extends BankingF[A]

  object BankingF {
    implicit def banking: Banking[BankingF] = new Banking[BankingF] {
      override def accounts: BankingF[NonEmptyList[Account]] = Accounts(identity)
      override def balance(account: Account): BankingF[Amount] = Balance(account, identity)
      override def transfer(amount: Amount, from: From[Account], to: To[Account]): BankingF[TransferResult] = Transfer(amount, from, to, identity)
      override def withdraw(amount: Amount): BankingF[Amount] = Withdraw(amount, identity)
    }
  }


  implicit def bankingFree[F[_]](implicit F: Banking[F]): Banking[Free[F, ?]] =
    new Banking[Free[F, ?]] {
      def accounts = Free.liftF(F.accounts)
      def balance(account: Account) = Free.liftF(F.balance(account))
      def transfer(amount: Amount, from: From[Account], to: To[Account]) = Free.liftF(F.transfer(amount, from, to))
      def withdraw(amount: Amount): Free[F, Amount] = Free.liftF(F.withdraw(amount))
    }


  def injectExample[F[_]](implicit inject: Inject[BankingF, F]): Free[F, Amount] =
    for {
      as <- Free.inject(Banking.accounts[BankingF])
      b  <- Free.inject(Banking.balance[BankingF](as.head))
    } yield b

  def program[F[_]: Monad : Banking]: F[Amount] =
    for {
      as <- Banking.accounts[F]
      b  <- Banking.balance[F](as.head)
    } yield b


  type Interpreter[F[_], G[_]] = F ~> Free[G, ?]

  type ~<[F[_], G[_]] = Interpreter[F, G]

  type Halt[F[_], A] = F[Unit]

  implicit def haltFunctor[F[_]] = new Functor[Halt[F, ?]] {
    override def map[A, B](fa: Halt[F, A])(f: (A) => B): Halt[F, B] = fa
  }

  sealed trait LoggingF[A]
  case class Log(string: String) extends LoggingF[Unit]

  object LoggingF {
    def log(string: String): Free[LoggingF, Unit] = Free.liftF(Log(string))
  }

  implicit class FreeHalt[F[_], A](free   : Free[Halt[F, ?], A]) {
    def unhalt: Free[F, Unit] = free.fold[Free[F, Unit]](x => Free.pure(()), Free.liftF(_))
  }

  sealed trait ProtocolF[A]

  sealed trait SocketF[A]

  sealed trait FileF[A]

  val bankingLogging : BankingF ~< Halt[LoggingF, ?] = null

  val bankingProtocol : BankingF ~< ProtocolF = null

  val protocolSocket : ProtocolF ~< SocketF = null

  val loggingFile : LoggingF ~< FileF = null

  val execFile : FileF ~> Future = null

  val execSocket : SocketF ~> Future = null

  val bankingFProgram = program[Free[BankingF, ?]]

  def execBanking(implicit executionContext: ExecutionContext) = new (BankingF ~> Future) {
    override def apply[A](fa: BankingF[A]): Future[A] =
      for  {
        _ <- bankingLogging(fa).unhalt.foldMap(loggingFile).foldMap(execFile)
        result <- bankingProtocol(fa).foldMap[Free[SocketF, ?]](protocolSocket).foldMap(execSocket)
      } yield result
  }


}
