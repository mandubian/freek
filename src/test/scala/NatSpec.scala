package freek

import cats.free.Free

import org.scalatest._

import cats.free.{Free, Trampoline}
import cats.data.Xor
import cats.{~>, Id}

import scala.concurrent._
import scala.concurrent.duration._

import cats.Functor
import cats.std.future._
import cats.std.option._
import cats.std.list._
import ExecutionContext.Implicits.global

import freek._

import scala.language.reflectiveCalls


sealed trait Msg
case class Req(command: String, id: Int, payload: String) extends Msg
case class Resp(id: Int, payload: String) extends Msg

sealed trait Error
case class ExchangeError(msg: String) extends Error
case class StoreError(msg: String) extends Error

sealed trait Exchange[A]

object Exchange {
  final case object Listen extends Exchange[Xor[Error, Req]]
  final case class Send(msg: Resp) extends Exchange[Xor[Error, Unit]]
}

sealed trait Log0[A]

object Log0 {
  final case class Info(msg: String) extends Log0[Unit]
  final case class Error(msg: String) extends Log0[Unit]
}

sealed trait Store[A]

object Store {
  final case class Put(id: Int, value: Int) extends Store[Xor[Error, Unit]]
  final case class Get(id: Int) extends Store[Option[Int]]
}

sealed trait Queue[A]

object Queue {

  // case object Dequeue extends Queue[Req]
  case class Dequeue[A <: Msg]() extends Queue[Xor[Error, A]]

  // case class Enqueue(req: Resp) extends Queue[Unit]
  case class Enqueue[A <: Msg](msg: A) extends Queue[Xor[Error, Unit]]
}

object Logic {

  type PRG = Store :|: Log0 :|: FXNil
  type O = Xor[Error, ?] :&: Option :&: Bulb
  val PRG = Program[PRG]

  def put(id: Int, value: String) = for {
    _ <- Store.Put(id, value.toInt).freeko[PRG, O]
  } yield (Resp(id, s"item $id stored"))

  def get(id: Int) = for {
    value <- Store.Get(id).freeko[PRG, O]
  } yield (Resp(id, s"$id current count: $value"))

  def update(id: Int, value: String) = for {
    cur <- Store.Get(id).freeko[PRG, O]
    newValue = value.toInt + cur
    _   <- Store.Put(id, newValue).freeko[PRG, O]
  } yield (Resp(id, s"$id new count: $newValue"))

  def apply(req: Req) = for {
    _ <-  Log0.Info(s"doing things with $req").freeko[PRG, O]
    _ <-  req.command match {
            case "put"    => put(req.id, req.payload)
            case "get"    => get(req.id)
            case "update" => update(req.id, req.payload)
          }
  } yield (Resp(req.id, "blabla"))
}

sealed trait Transaction[F[_], A]

object Transaction {

  case class T[F[_], A](val fa: F[A]) extends Transaction[F, A]

  implicit val copointedk = new CoPointedK[Transaction] {
    def copurek[F[_]]: Transaction[F, ?] ~> F = new (Transaction[F, ?] ~> F) {
      def apply[A](tfa: Transaction[F, A]): F[A] = tfa match {
        case T(fa) => fa
      }
    }
  }

  implicit val pointedk = new PointedK[Transaction] {
    def purek[F[_]]: F ~> Transaction[F, ?] = new (F ~> Transaction[F, ?]) {
      def apply[A](fa: F[A]): Transaction[F, A] = T(fa) 
    }
  }

  def wrapNat[F[_]](nat: F ~> Future): Transaction[F, ?] ~> Future = new (Transaction[F, ?] ~> Future) {
    def begin: Future[Unit] = Future(println("begin transaction"))
    def commit: Future[Unit] = Future(println("commit transaction"))

    def apply[A](tfa: Transaction[F, A]): Future[A] = tfa match {
      case T(fa) =>
        for {
          _ <- begin
          a <- nat(fa)
          _ <- commit
        } yield (a)
    }
  }
}


object Server {

  type PRG = Exchange :|: Log0 :|: Logic.PRG
  type O = Xor[Error, ?] :&: Option :&: Bulb
  val PRG = Program[PRG]

  val echo = for {
    req   <-  Exchange.Listen.freeko[PRG, O]
    _     <-  Log0.Info(s"Received $req").freeko[PRG, O]
    // resp  <-  Logic(req).expand[PRG]
    resp  <-  Free.pure[PRG.Cop, Resp](Resp(req.id, req.payload)).onionT[O]
    _     <-  Exchange.Send(resp).freeko[PRG, O]
  } yield (())

  val logic = for {
    req   <-  Exchange.Listen.freeko[PRG, O]
    _     <-  Log0.Info(s"Received $req").freeko[PRG, O]
    resp  <-  Logic(req).freeko[PRG, O]
    resp  <-  Free.pure[PRG.Cop, Resp](Resp(req.id, req.payload)).onionT[O]
    _     <-  Exchange.Send(resp).freeko[PRG, O]
  } yield (())

}



class NatSpec extends FlatSpec with Matchers {

  "NatSpec" should "freek it" in {
    val r = Server.logic

    val log0I = new (Log0 ~> Future) {
      import Log0._

      def apply[A](l: Log0[A]): Future[A] = l match {
        case Info(msg) => Future(println(s"info: $msg"))
        case Error(msg) => Future(println(s"error: $msg"))
      }
    }

    val exchangeI = new (Exchange ~> Future) {
      import Exchange._
      
      val reqs = collection.mutable.Stack(
        Req("put", 1, "100")
      , Req("get", 1, "")
      )

      val resps = collection.mutable.Queue[Resp]()

      def apply[A](l: Exchange[A]): Future[A] = l match {
        case Listen => Future(Xor.right(reqs.pop))
        case Send(resp) => Future { resps += resp; Xor.right(()) }
      }
    }

    val storeI = new (Store ~> Future) {
      import Store._
      
      val store = collection.mutable.Map[Int, Int]()

      def apply[A](l: Store[A]): Future[A] = l match {
        case Put(id, payload) => Future { println(s"storing $id -> $payload"); store += (id -> payload) ; Xor.right(()) }
        case Get(id) => Future { store.get(id) }
      }
    }

    val exchange2Q = new (Exchange ~> Queue) {
      import Exchange._
      import Queue._

      def apply[A](l: Exchange[A]): Queue[A] = l match {
        case Listen => Dequeue[Req]()
        case Send(resp) => Enqueue(resp)
      }
    }

    val queueI = new (Queue ~> Future) {
      import Queue._

      val reqs = collection.mutable.Stack[Msg](
        Req("put", 1, "100")
      , Req("get", 1, "")
      )

      val resps = collection.mutable.Queue[Msg]()

      def apply[A](l: Queue[A]): Future[A] = l match {
        // compiler complains
        case Dequeue() => Future(Xor.right(reqs.pop))//.asInstanceOf[A])
        case Enqueue(resp) => Future { resps += resp; Xor.right(()) }
      }
    }

    val interpreters = log0I :&: exchangeI :&: storeI

    val fut= r.value.interpret(interpreters)
    Await.result(fut, 1.minute)

    // REPLACE
    type PRG2 = Queue :|: Log0 :|: Logic.PRG
    val PRG2 = Program[PRG2]

    val interpreters2 = log0I :&: queueI :&: storeI

    val nat2 = IsoNat[Server.PRG.Cop].replace(exchange2Q)
    val fut2 = r.value.mapSuspension(nat2).interpret(interpreters2)
    Await.result(fut2, 1.minute)

    println(s"store:${storeI.store}")
    println(s"reqs:${exchangeI.reqs}")
    println(s"resps:${exchangeI.resps}")

    val nat3 = IsoNat[Server.PRG.Cop].replace(PointedK[Transaction].purek[Store])
    
    val interpreters3 = log0I :&: exchangeI :&: Transaction.wrapNat(storeI)
    val fut3 = r.value.mapSuspension(nat3).interpret(interpreters3)
    Await.result(fut3, 1.minute)

    println(s"store:${storeI.store}")
    println(s"reqs:${exchangeI.reqs}")
    println(s"resps:${exchangeI.resps}")

  }


}
