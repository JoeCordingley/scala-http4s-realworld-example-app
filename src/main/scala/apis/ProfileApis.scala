package io.rw.app.apis

import cats.data.{EitherT, OptionT}
import cats.implicits.*
import cats.Monad
import io.rw.app.data.{Entities as E, *}
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import java.time.Instant
import io.rw.app.repos.*
import io.rw.app.security.{JwtToken, PasswordHasher}
import io.rw.app.data

trait ProfileApis[F[_]] {
  def get(input: GetProfileInput): F[ApiResult[Profile]]
  def follow(input: FollowUserInput): F[ApiResult[Profile]]
  def unfollow(input: UnfollowUserInput): F[ApiResult[Profile]]
}

object ProfileApis {

  def impl[F[_]: Monad](userRepo: UserRepo[F], followerRepo: FollowerRepo[F]) =
    new ProfileApis[F]() {

      def get(input: GetProfileInput): F[ApiResult[Profile]] = {
        val profile = for {
          userWithId <- OptionT(userRepo.findUserByUsername(input.username))
          following <- OptionT.liftF(
            input.authUser
              .flatTraverse(followerRepo.findFollower(userWithId.id, _))
              .map(_.nonEmpty)
          )
        } yield mkProfile(userWithId.entity, following)

        profile.value.map(
          _.toRight(ProfileNotFound)
        )
      }

      def follow(input: FollowUserInput): F[ApiResult[Profile]] = {
        val profile = for {
          userWithId <- EitherT.fromOptionF(
            userRepo.findUserByUsername(input.username),
            ProfileNotFound
          )
          _ <- EitherT.cond[F](
            input.authUser != userWithId.id,
            (),
            UserFollowingHimself(mkProfile(userWithId.entity, false))
          )
          _ <- EitherT.liftF[F, ApiError, E.Follower](
            followerRepo.createFollower(
              E.Follower(userWithId.id, input.authUser)
            )
          )
        } yield mkProfile(userWithId.entity, true)

        profile.value.map(_.recover({ case UserFollowingHimself(p) => p }))
      }

      def unfollow(
          input: UnfollowUserInput
      ): F[ApiResult[Profile]] = {
        val profile = for {
          userWithId <- EitherT.fromOptionF(
            userRepo.findUserByUsername(input.username),
            ProfileNotFound
          )
          _ <- EitherT.cond[F](
            input.authUser != userWithId.id,
            (),
            UserUnfollowingHimself(mkProfile(userWithId.entity, false))
          )
          _ <- EitherT.liftF[F, ApiError, Unit](
            followerRepo.deleteFollower(userWithId.id, input.authUser)
          )
        } yield mkProfile(userWithId.entity, false)

        profile.value.map(_.recover({ case UserUnfollowingHimself(p) => p }))
      }
    }
}
