import cats.free.Free
import cats.~>

package object onion {
  type Interpreter[F[_], G[_]] = F ~> Free[G, ?]
  type ~<[F[_], G[_]] = Interpreter[F, G]
}
