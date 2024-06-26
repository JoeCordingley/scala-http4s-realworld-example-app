package io.rw.app.repos

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import doobie.implicits.legacy.instant.*
import io.rw.app.data.Entities.*

trait UserRepo[F[_]] {
  def findUserById(id: Int): F[Option[WithId[User]]]
  def findUserByEmail(email: String): F[Option[WithId[User]]]
  def findUserByUsername(username: String): F[Option[WithId[User]]]
  def createUser(user: User): F[WithId[User]]
  def updateUser(id: Int, user: UserForUpdate): F[WithId[User]]
}

object UserRepo {

  def impl(xa: Transactor[IO]) = new UserRepo[IO] {
    def findUserById(id: Int): IO[Option[WithId[User]]] =
      Q.selectUserById(id).option.transact(xa)

    def findUserByEmail(email: String): IO[Option[WithId[User]]] =
      Q.selectUserByEmail(email).option.transact(xa)

    def findUserByUsername(username: String): IO[Option[WithId[User]]] =
      Q.selectUserByUsername(username).option.transact(xa)

    def createUser(user: User): IO[WithId[User]] =
      Q.insertUser(user)
        .withUniqueGeneratedKeys[WithId[User]](
          "id",
          "email",
          "username",
          "password",
          "bio",
          "image",
          "created_at",
          "updated_at"
        )
        .transact(xa)

    def updateUser(id: Int, user: UserForUpdate): IO[WithId[User]] =
      Q.updateUser(id, user)
        .withUniqueGeneratedKeys[WithId[User]](
          "id",
          "email",
          "username",
          "password",
          "bio",
          "image",
          "created_at",
          "updated_at"
        )
        .transact(xa)
  }

  object Q {
    def selectUserById(id: Int) =
      sql"""
         select id, email, username, password, bio, image, created_at, updated_at
         from users
         where id = $id
      """.query[WithId[User]]

    def selectUserByEmail(email: String) =
      sql"""
         select id, email, username, password, bio, image, created_at, updated_at
         from users
         where email = $email
      """.query[WithId[User]]

    def selectUserByUsername(username: String) =
      sql"""
         select id, email, username, password, bio, image, created_at, updated_at
         from users
         where username = $username
      """.query[WithId[User]]

    def insertUser(user: User) =
      sql"""
         insert into users(email, username, password, bio, image, created_at, updated_at)
         values(${user.email}, ${user.username}, ${user.password}, ${user.bio}, ${user.image}, ${user.createdAt}, ${user.updatedAt})
      """.update

    def updateUser(id: Int, user: UserForUpdate) =
      sql"""
         update users u
         set email = coalesce(${user.email}, u.email),
             username = coalesce(${user.username}, u.username),
             password = coalesce(${user.password}, u.password),
             bio = coalesce(${user.bio}, u.bio),
             image = coalesce(${user.image}, u.image),
             updated_at = ${user.updatedAt}
         where id = $id
      """.update
  }
}
