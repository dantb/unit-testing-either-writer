package com.itv.testing

import org.scalacheck.Prop.*

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.data.*
import cats.*
import cats.syntax.all.*

class ProcessArticleSpec extends ScalaCheckSuite:

  def genUUIdString: Gen[String] = Gen.uuid.map(_.toString)
  def genArticle: Gen[Article] = for
    id    <- genUUIdString
    title <- genUUIdString
    topic <- genUUIdString
  yield Article(id, title, topic)

  given Arbitrary[Article] = Arbitrary(genArticle)

  property("process article correctly when present in database") {
    forAll { (id: String, article: Article) =>
      type F[A] = Either[Throwable, A]
      given Log[F] = _ => Right(())

      val expected: ArticleEvent = ArticleEvent(article, Nil)

      given ArticleRepo[F]      = id => Right(Some(article))
      given ContentfulClient[F] = _ => Right(Nil)
      given ProduceEvent[F]     = _ => Right(())

      val actual = ProcessArticle.mk[F].process(id)

      assertEquals(actual, Right(expected))
    }
  }

  // We're not testing much really, only the expected value.
  // There are loads of ways that could be correct,
  // but the code could be wrong.
  // E.g. Contentful may not have even been called here. We don't know.
  // There are just no articles of the same topic.
  enum Event:
    case ArticleFetched(id: String)
    case ContentfulCalled(topic: String)
    case EventProduced(event: ArticleEvent)

  import scala.collection.mutable.ListBuffer

  property("mutable: process article correctly when present in database") {
    forAll { (id: String, article: Article) =>
      type F[A] = Either[Throwable, A]
      given Log[F] = _ => Right(())

      val expected: ArticleEvent = ArticleEvent(article, Nil)
      val expectedEvents: List[Event] =
        List(Event.ArticleFetched(id), Event.ContentfulCalled(article.topic), Event.EventProduced(expected))
      val actualEvents: ListBuffer[Event] = ListBuffer.empty

      given ArticleRepo[F] = id =>
        actualEvents.append(Event.ArticleFetched(id))
        Right(Some(article))

      given ContentfulClient[F] = topic =>
        actualEvents.append(Event.ContentfulCalled(topic))
        Right(Nil)

      given ProduceEvent[F] = event =>
        actualEvents.append(Event.EventProduced(event))
        Right(())

      val actual = ProcessArticle.mk[F].process(id)

      assertEquals(actual, Right(expected))
      assertEquals(actualEvents.toList, expectedEvents)
    }
  }

  // Ok now we are asserting the order of interactions with different components.
  // And also what they're called with. Sort of the point of abstraction - break stuff
  // apart into little encapsulated units.
  // But it only  works if the interactions are correct.
  property("writer over either: process article correctly when present in database") {
    forAll { (id: String, article: Article) =>
      type Error[A] = Either[Throwable, A]
      type F[A]     = WriterT[Error, List[Event], A]
      given Log[F] = _ => WriterT.value(())

      val expected: ArticleEvent = ArticleEvent(article, Nil)
      val expectedEvents: List[Event] =
        List(Event.ArticleFetched(id), Event.ContentfulCalled(article.topic), Event.EventProduced(expected))

      given ArticleRepo[F] = id => 
        WriterT.tell[Error, List[Event]](List(Event.ArticleFetched(id))).as(Some(article))
      given ContentfulClient[F] = topic => 
        WriterT.tell[Error, List[Event]](List(Event.ContentfulCalled(topic))).as(Nil)
      given ProduceEvent[F]     = event => 
        WriterT.tell[Error, List[Event]](List(Event.EventProduced(event)))

      val actual: WriterT[Error, List[Event], ArticleEvent] =
        ProcessArticle.mk[F].process(id)

      // Either[Throwable, (List[Event], ArticleEvent)]
      assertEquals(actual.run, Right((expectedEvents, expected)))
    }
  }

  // There are a couple of issues with this
  // 1. We can't test failure properly
  // 2. It's verbose
  property("writer over either: process article correctly when absent from database") {
    forAll { (id: String) =>
      type Error[A] = Either[Throwable, A]
      type F[A]     = WriterT[Error, List[Event], A]
      given Log[F] = _ => WriterT.value(())

      val expected: Throwable         = ArticleNotFound(id)
      val expectedEvents: List[Event] = List(Event.ArticleFetched(id))

      given ArticleRepo[F]      = id => WriterT.tell[Error, List[Event]](List(Event.ArticleFetched(id))).as(None)
      given ContentfulClient[F] = _ => ???
      given ProduceEvent[F]     = _ => ???

      val actual: WriterT[Error, List[Event], ArticleEvent] =
        ProcessArticle.mk[F].process(id)

      // Either[Throwable, (List[Event], ArticleEvent)]
      assertEquals(actual.run, Left(expected))
    }
  }

  // We've lost information. The error throws away the list of events.
  // This is due to the ordering of transformers. Ordering impacts compositional semantics.
  // Let's try the other way round
  property("either over writer: process article correctly when absent from database") {
    forAll { (id: String) =>
      type EventWriter[A] = Writer[List[Event], A]
      type F[A]           = EitherT[EventWriter, Throwable, A]
      given Log[F] = _ => EitherT.pure(())

      val expected: Throwable         = ArticleNotFound(id)
      val expectedEvents: List[Event] = List(Event.ArticleFetched(id))

      given ArticleRepo[F]      = id => EitherT.liftF(Writer.tell(List(Event.ArticleFetched(id)))).as(None)
      given ContentfulClient[F] = _ => ???
      given ProduceEvent[F]     = _ => ???

      val actual: EitherT[EventWriter, Throwable, ArticleEvent] =
        ProcessArticle.mk[F].process(id)

      // (List[Event], Either[Throwable, ArticleEvent])
      assertEquals(actual.value.run, (expectedEvents, Left(expected)))
    }
  }

  type EventWriter[A] = Writer[List[Event], A]
  type F[A]           = EitherT[EventWriter, Throwable, A]
  given Log[F]        = _ => EitherT.pure(())

  def logEventAndValue[F[_], L, A](l: L, a: A): EitherT[[X] =>> Writer[List[L], X], Throwable, A] =
    EitherT.liftF(Writer.tell(List(l))).as(a)

  // Still fairly verbose, let's clean it up
  property("cleaner either over writer: process article correctly when absent from database") {
    forAll { (id: String) =>
      val expected: Throwable         = ArticleNotFound(id)
      val expectedEvents: List[Event] = List(Event.ArticleFetched(id))

      given ArticleRepo[F]      = id => logEventAndValue(Event.ArticleFetched(id), None)
      given ContentfulClient[F] = _ => ???
      given ProduceEvent[F]     = _ => ???

      val actual = ProcessArticle.mk[F].process(id)

      assertEquals(actual.value.run, (expectedEvents, Left(expected)))
    }
  }
