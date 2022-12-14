package com.itv.testing

import cats.MonadThrow
import cats.syntax.all.*

trait Log[F[_]] { def info(msg: String): F[Unit] }

object Log:
  def info[F[_]](msg: String)(using log: Log[F]) = log.info(msg)

trait ArticleRepo[F[_]] { def get(id: String): F[Option[Article]] }

object ArticleRepo:
  def get[F[_]](id: String)(using ar: ArticleRepo[F]) = ar.get(id)

trait ContentfulClient[F[_]] { def articlesByTopic(topic: String): F[List[Article]] }

object ContentfulClient:
  def articlesByTopic[F[_]](msg: String)(using cc: ContentfulClient[F]) = cc.articlesByTopic(msg)

trait ProduceEvent[F[_]] { def produce(event: ArticleEvent): F[Unit] }

object ProduceEvent:
  def produce[F[_]](event: ArticleEvent)(using pe: ProduceEvent[F]) = pe.produce(event)

trait ProcessArticle[F[_]] { def process(id: String): F[ArticleEvent] }

final case class Article(id: String, title: String, topic: String)
final case class ArticleEvent(article: Article, relatedArticles: List[Article])
final case class ArticleNotFound(id: String) extends Throwable

object ProcessArticle:
  def mk[F[_]: MonadThrow: Log: ArticleRepo: ContentfulClient: ProduceEvent]: ProcessArticle[F] = id =>
    for
      maybeArticle    <- ArticleRepo.get(id)
      article         <- maybeArticle.raiseError(ArticleNotFound(id))
      relatedArticles <- ContentfulClient.articlesByTopic(article.topic)
      event            = ArticleEvent(article, relatedArticles.filterNot(_.id == article.id))
      _               <- ProduceEvent.produce(event)
    yield event

  extension [A](opt: Option[A])
    def raiseError[F[_]: MonadThrow](t: Throwable): F[A] = opt match
      case None => MonadThrow[F].raiseError(t)
      case Some(value) => value.pure[F]
  

