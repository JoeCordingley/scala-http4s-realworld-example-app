package io.rw.app.repos

import cats.data.NonEmptyList
import cats.effect.IO
import cats.Functor
import doobie._
import doobie.Fragments._
import doobie.implicits._
import doobie.implicits.legacy.instant._
import io.rw.app.data.Entities._

trait FollowerRepo[F[_]] {
  def findFollowers(followeeIds: List[Int], followerId: Int): F[List[Follower]]

  def findFollower(followeeId: Int, followerId: Int)(implicit Fun: Functor[F]): F[Option[Follower]] =
    Fun.map(findFollowers(List(followeeId), followerId))(_.headOption)

  def createFollower(follower: Follower): F[Follower]
  def deleteFollower(followeeId: Int, followerId: Int): F[Unit]
}

object FollowerRepo {

  def impl(xa: Transactor[IO]) = new FollowerRepo[IO] {
    def findFollowers(followeeIds: List[Int], followerId: Int): IO[List[Follower]] =
      NonEmptyList.fromList(followeeIds.distinct).map(Q.selectFollowers(_, followerId).to[List].transact(xa)).getOrElse(IO.pure(List.empty))

    def createFollower(follower: Follower): IO[Follower] =
      Q.insertFollower(follower).run.map(_ => follower).transact(xa)

    def deleteFollower(followeeId: Int, followerId: Int): IO[Unit] =
      Q.deleteFollower(followeeId, followerId).run.map(_ => ()).transact(xa)
  }

  object Q {
    def selectFollowers(followeeIds: NonEmptyList[Int], followerId: Int) = {
      val q =
        fr"""
          select followee_id, follower_id
          from followers
          where """ ++ in(fr"followee_id", followeeIds) ++
        fr"""
                and follower_id = $followerId
        """
      q.query[Follower]
    }

    def insertFollower(follower: Follower) =
      sql"""
          insert into followers(followee_id, follower_id)
          values(${follower.followeeId}, ${follower.followerId})
          on conflict do nothing
      """.update

    def deleteFollower(followeeId: Int, followerId: Int) =
      sql"""
          delete from followers
          where followee_id = $followeeId
                and follower_id = $followerId
      """.update
  }
}
