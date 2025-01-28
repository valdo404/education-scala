package io.univalence.education

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import zio.ZIO

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

def playWithFutures = {
  val value2 = Future.successful(Option(1))
  value2.map(l => l.map(_ * 2))
  val transformed2: OptionT[Future, Int] = OptionT.apply[Future, Int](value2)
  val valueTransformed2: OptionT[Future, Int] = transformed2
    .map(_ * 2)
    .flatMap(el => OptionT.fromOption(Some(2 * el)))

  val value: Future[Option[Int]] = valueTransformed2.value
}

def playWithLists = {
  val value1 = List(Option(1), Option(2))
  value1.map(l => l.map(_ * 2))

  val transformed1: OptionT[List, Int] = OptionT.apply[List, Int](value1)
  val valueTransformed1: OptionT[List, Int] = transformed1
    .map(_ * 2)
    .flatMap(el => OptionT.fromOption(Some(2 * el)))

  val value3: List[Option[Int]] = valueTransformed1.value
}

def playWithTries = {
  val value1 = Try(Option(1))
  value1.map(l => l.map(_ * 2))

  val transformed1: OptionT[Try, Int] = OptionT.apply[Try, Int](value1)
  val valueTransformed1: OptionT[Try, Int] = transformed1
    .map(_ * 2)
    .flatMap(el => OptionT.fromOption(Some(2 * el)))

  val value3: Try[Option[Int]] = valueTransformed1.value
}

def playWithEithers = {
  val value1 = Future.successful(Right(1))
  value1.map(l => l.map(_ * 2))

  val transformed1: EitherT[Future, Exception, Int] = EitherT.apply[Future, Exception, Int](value1)
  val valueTransformed1: EitherT[Future, Exception, Int] = transformed1
    .map(_ * 2)
    .flatMap(el => EitherT.fromOption(Some(2 * el), ifNone = new Exception()))

  val value3: Future[Either[Exception, Int]] = valueTransformed1.value
}

def mapAndFlatMap = {
    def div(f1: Float, f2: Float): Either[Exception, Float] = {
      Right(0)
    }

     def ln(f1: Float): Either[Exception, Float] = Right(0)

    def sqrt(f1: Float): Either[Exception, Float] = Right(0)

    val computation: Either[Exception, Float] = for {
       ratio <- div(1, 2)
       ln <- ln(ratio)
       sqrt <- sqrt(ln)
    } yield sqrt

}


object monads_revision {
  @main
  def revision(): Unit = {
    // monades de type structure

    type Monad1[A] = Option[A]
    type Monad2[A] = List[A]
    type Monad3[A, B] = Map[A, B]

    // monades de gestion d'erreur
    type Monad4[A] = Try[A]
    type Monad5[E, A] = Either[E, A]

    // monades pour l'asynchronisme

    type Monad6[A] = Future[A]
    type Monad7[A] = IO[A]
    type Monad8[R, A, E] = ZIO[R, A, E]

    // EitherT, OptionT: monad transformers

    playWithLists
    playWithFutures

  }
}
