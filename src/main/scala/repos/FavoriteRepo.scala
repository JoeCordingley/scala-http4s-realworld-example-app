package io.rw.app.repos

import cats.data.NonEmptyList
import cats.effect.IO
import cats.Functor
import doobie.*
import doobie.Fragments.*
import doobie.implicits.*
import doobie.implicits.legacy.instant.*
import io.rw.app.data.Entities.*

trait FavoriteRepo[F[_]] {
  def findFavorites(articleIds: List[Int], userId: Int): F[List[Favorite]]

  def findFavorite(articleId: Int, userId: Int)(implicit
      Fun: Functor[F]
  ): F[Option[Favorite]] =
    Fun.map(findFavorites(List(articleId), userId))(_.headOption)

  def countFavorites(articleIds: List[Int]): F[List[(Int, Int)]]

  def countFavorite(articleId: Int)(implicit Fun: Functor[F]): F[Int] =
    Fun.map(countFavorites(List(articleId)))(_.headOption.getOrElse((0, 0))._2)

  def createFavorite(favorite: Favorite): F[Favorite]

  def deleteFavorite(articleId: Int, userId: Int): F[Unit]
}

object FavoriteRepo {

  def impl(xa: Transactor[IO]) = new FavoriteRepo[IO] {
    def findFavorites(articleIds: List[Int], userId: Int): IO[List[Favorite]] =
      NonEmptyList
        .fromList(articleIds.distinct)
        .map(Q.selectFavorites(_, userId).to[List].transact(xa))
        .getOrElse(IO.pure(List.empty))

    def countFavorites(articleIds: List[Int]): IO[List[(Int, Int)]] =
      NonEmptyList
        .fromList(articleIds.distinct)
        .map(Q.countFavorites(_).to[List].transact(xa))
        .getOrElse(IO.pure(List.empty))

    def createFavorite(favorite: Favorite): IO[Favorite] =
      Q.insertFavorite(favorite).run.map(_ => favorite).transact(xa)

    def deleteFavorite(articleId: Int, userId: Int): IO[Unit] =
      Q.deleteFavorite(articleId, userId).run.map(_ => ()).transact(xa)
  }

  object Q {
    def selectFavorites(articleIds: NonEmptyList[Int], userId: Int) = {
      val q =
        fr"""
          select article_id, user_id
          from favorites
          where """ ++ in(fr"article_id", articleIds) ++
          fr"""
                and user_id = $userId
        """
      q.query[Favorite]
    }

    def countFavorites(articleIds: NonEmptyList[Int]) = {
      val q =
        fr"""
          select article_id, count(1)
          from favorites
          where """ ++ in(fr"article_id", articleIds) ++
          fr"""
          group by article_id
        """
      q.query[(Int, Int)]
    }

    def insertFavorite(favorite: Favorite) =
      sql"""
         insert into favorites(article_id, user_id)
         values(${favorite.articleId}, ${favorite.userId})
         on conflict do nothing
      """.update

    def deleteFavorite(articleId: Int, userId: Int) =
      sql"""
         delete from favorites
         where article_id = $articleId
               and user_id = $userId
      """.update
  }
}
