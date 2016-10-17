package onion
import java.io.FileWriter

import cats.data.{NonEmptyList, Xor}
import cats.free.{Free, Inject}
import cats.implicits._
import cats.{Functor, Id, Monad, ~>}
import onion.example.LoggingF.Log
import onion.example.ProtocolF.JustReturn

object example extends App {
  trait Console[F[_]] {
    def readLine: F[String]
    def writeLine(line: String): F[Unit]
  }

  object Console {
    def readLine[F[_]](implicit F: Console[F]): F[String] = F.readLine
    def writeLine[F[_]](line: String)(implicit F: Console[F]): F[Unit] = F.writeLine(line)
  }

  import Console._

  def myMonadicProgram[F[_]: Console: Monad]: F[Unit] =
    for {
      line <- readLine
      x <- writeLine(s"You entered $line")
    } yield x


  type Amount = Int
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
    implicit val banking: Banking[BankingF] = new Banking[BankingF] {
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

  import Banking._

  def injectExample[F[_]](implicit inject: Inject[BankingF, F]): Free[F, Amount] =
    for {
      as <- Free.inject(accounts[BankingF])
      b  <- Free.inject(balance[BankingF](as.head))
    } yield b

  def program[F[_]: Monad](implicit F: Banking[F]): F[Amount] =
    for {
      as <- F.accounts
      b  <- F.balance(as.head)
      x <- F.transfer(123, From("Foo"), To("Bar"))
      _ <- F.withdraw(5)
    } yield b


  type Interpreter[F[_], G[_]] = F ~> Free[G, ?]
  type ~<[F[_], G[_]] = Interpreter[F, G]

  type Halt[F[_], A] = F[Unit]

  implicit def haltFunctor[F[_]]: Functor[Halt[F, ?]] =
    new Functor[Halt[F, ?]] {
      override def map[A, B](fa: Halt[F, A])(f: (A) => B): Halt[F, B] = fa
    }

  implicit class FreeHaltOps[F[_], A](free: Free[Halt[F, ?], A]) {
    def unhalt: Free[F, Unit] =
      free.fold(x => Free.pure(()), Free.liftF(_))
  }

  sealed trait LoggingF[A]
  object LoggingF {
    case class Log(string: String) extends LoggingF[Unit]
    def log(string: String): Free[LoggingF, Unit] = Free.liftF(Log(string))
  }

  sealed trait ProtocolF[A]
  object  ProtocolF {
    case class JustReturn[A](a: A) extends ProtocolF[A]
  }

  sealed trait SocketF[A]
  object SocketF {
    case class JustReturn[A](a: A) extends SocketF[A]
  }

  sealed trait FileF[A]
  case class AppendToFile(fileName: String, string: String) extends FileF[Unit]

  val bankingLogging : BankingF ~< Halt[LoggingF, ?] =
    new (BankingF ~< Halt[LoggingF, ?]) {
      override def apply[A](fa: BankingF[A]): Free[Halt[LoggingF,?], A] = {
        def log(string: String): Free[Halt[LoggingF, ?], A] =
          Free.liftF[Halt[LoggingF, ?], A](LoggingF.Log(string))
        fa match {
          case Accounts(next) => log("Fetch accounts")
          case Balance(account, next) => log(s"Fetch balance for account = $account")
          case Transfer(amount, from, to, next) => log(s"Transfer [$amount] from $from to $to")
          case Withdraw(amount, next) => log(s"Withdraw $amount")
        }
      }
  }

  val loggingFile : LoggingF ~< FileF =
    new (LoggingF ~< FileF) {
      val fileName = "app.log"
      override def apply[A](fa: LoggingF[A]): Free[FileF, A] = fa match {
        case Log(string) =>
          Free.liftF(AppendToFile(fileName, string))
      }
}

  val execFile : FileF ~> Id =
    new (FileF ~> Id) {
      override def apply[A](fa: FileF[A]): Id[A] =
        fa match {
          case AppendToFile( fileName, string) =>
            val fw = new FileWriter(fileName, true)
            try {
              fw.write("\n" ++ string)
            }
            finally fw.close()
        }
  }

  val bankingProtocol : BankingF ~< ProtocolF =
    new (BankingF ~< ProtocolF) {
      import ProtocolF._
      override def apply[A](fa: BankingF[A]) =
        fa match {
          case Accounts(next) => Free.liftF(JustReturn(next(NonEmptyList.of("Foo", "Bar"))))
          case Balance(account, next) => Free.liftF(JustReturn(next(10000)))
          case Transfer(amount, from, to, next) => Free.liftF(JustReturn(next(Xor.left("Ooops"))))
          case Withdraw(amount, next) => Free.liftF(JustReturn(next(10000-amount)))
        }
}

  val protocolSocket : ProtocolF ~< SocketF =
    new (ProtocolF ~< SocketF) {
      override def apply[A](fa: ProtocolF[A]) =
        fa match { case JustReturn( a) => Free.liftF(SocketF.JustReturn(a))}
}

  val execSocket : SocketF ~> Id =
    new (SocketF ~> Id) {
      override def apply[A](fa: SocketF[A]): Id[A] =
        fa match {
          case SocketF.JustReturn(a) =>
            println(a)
            a
        }
}

  val bankingFProgram = program[Free[BankingF, ?]]

  val execBanking = new (BankingF ~> Id) {
    override def apply[A](fa: BankingF[A]): Id[A] =
      for  {
        _ <- bankingLogging(fa).unhalt.foldMap(loggingFile).foldMap(execFile)
        result <- bankingProtocol(fa).foldMap(protocolSocket).foldMap(execSocket)
      } yield result
  }

  bankingFProgram.foldMap(execBanking)
}
