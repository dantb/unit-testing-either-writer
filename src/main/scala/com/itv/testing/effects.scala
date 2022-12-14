package com.itv.testing

// @main
// def run: Unit =
//   val actual1   = accumulateInts(List("4", "5", "6"))
//   val expected1 = Right(List(4, 9, 15))
//   println(actual1)
//   println(expected1)
//   assert(actual1 == expected1)
//   // val actual4 = accumulateInts4(List("4", "5", "6")).run.map(_._1)
//   // println(actual4)
//   // assert(actual4 == expected1)

//   def loop(strs: List[String], total: Int, totals: List[Int]): Either[String, List[Int]] =
//     strs match
//       case head :: next =>
//         parseInt(head) match
//           case Left(e) => Left(e)
//           case Right(value) =>
//             val newTotal = total + value
//             loop(next, newTotal, newTotal :: totals)
//       case Nil => Right(totals.reverse)

//   loop(strings, 0, Nil)

def parseInt(s: String): Either[String, Int] = s.toIntOption.toRight("Failed to parse")

// no longer tail recursive, but slightly less verbose
def accumulateInts2(strings: List[String]): Either[String, List[Int]] =
  def loop(strs: List[String], total: Int, totals: List[Int]): Either[String, List[Int]] =
    strs match
      case head :: next =>
        parseInt(head).flatMap { value =>
          val newTotal = total + value
          loop(next, newTotal, newTotal :: totals)
        }
      case Nil => Right(totals.reverse)

  loop(strings, 0, Nil)

// import cats.data.Writer
// import cats.syntax.all.*
// import cats.data.WriterT
// import cats.MonadError

// // We have access to errors, logging, pure and flatMap... but it's disjointed
// def accumulateInts3(strings: List[String]): Writer[List[Int], Either[String, Unit]] =
//   def loop(strs: List[String], total: Int): Writer[List[Int], Either[String, Unit]] =
//     strs match
//       case head :: next =>
//         parseInt(head) match
//           case Left(e) => Writer.value(Left(e))
//           case Right(value) =>
//             val newTotal = total + value
//             Writer.tell[List[Int]](List(newTotal)) *> loop(next, newTotal)
//       case Nil => Writer.value(Right(()))

//   loop(strings, 0)

// type Error[A] = Either[String, A]

// def accumulateInts4[Error[_]](strings: List[String]): WriterT[Error, List[Int], Unit] =
//   strings
//     .foldLeft(WriterT.value[Error, List[Int], Int](0)) { (acc, next) =>
//       acc.flatMap { total =>
//         for
//           value   <- WriterT.liftF(parseInt(next))
//           newTotal = total + value
//           _       <- WriterT.tell(List(newTotal))
//         yield newTotal
//       }
//     }
//     .void

// type WhatIsThis = String

// // pseudo code...
// def accumulateInts4(strings: List[String]): WhatIsThis =
//   for

final case class EitherT[F[_], A, B](value: F[Either[A, B]]) 
final case class WriterT[F[_], L, V](run: F[(L, V)]) 

type Writer[A] = (List[Int], A)
type EitherWriter[A] = EitherT[Writer, Throwable, A]

type Error[A] = Either[Throwable, A]
type WriterEither[A] = WriterT[Error, List[Int], A]

