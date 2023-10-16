package io.rw.app.repos

import cats.data.NonEmptyList
import cats.effect.IO
import doobie.*
import doobie.Fragments.*
import doobie.implicits.*
import doobie.implicits.legacy.instant.*
import io.rw.app.data.Entities.*

trait TagRepo[F[_]] {
  def findPopularTags(): F[List[String]]
  def findTags(articleIds: List[Int]): F[List[Tag]]

  def findTags(articleId: Int): F[List[Tag]] =
    findTags(List(articleId))
}

object TagRepo {

  def impl(xa: Transactor[IO]) = new TagRepo[IO] {
    def findPopularTags(): IO[List[String]] =
      Q.selectPopularTags.to[List].transact(xa)

    def findTags(articleIds: List[Int]): IO[List[Tag]] =
      NonEmptyList
        .fromList(articleIds.distinct)
        .map(Q.selectTags(_).to[List].transact(xa))
        .getOrElse(IO.pure(List.empty))
  }

  object Q {
    val selectPopularTags =
      sql"""
         select tag
         from tags
         group by tag
         order by count(1) desc
      """.query[String]

    def selectTags(articleIds: NonEmptyList[Int]) = {
      val q =
        fr"""
        select article_id, tag
        from tags
        where """ ++ in(fr"article_id", articleIds)

      q.query[Tag]
    }
  }
}
