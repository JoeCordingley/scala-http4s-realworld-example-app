package io.rw.app.apis

import cats.data.{EitherT, OptionT}
import cats.implicits.*
import cats.Monad
import io.rw.app.data.{Entities as E, *}
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.repos.*
import io.rw.app.security.{JwtToken, PasswordHasher}
import java.time.Instant

trait UserApis[F[_]] {
  def authenticate(input: AuthenticateUserInput): F[ApiResult[AuthenticateUserOutput]]
  def register(input: RegisterUserInput): F[ApiResult[RegisterUserOutput]]
  def get(input: GetUserInput): F[ApiResult[GetUserOutput]]
  def update(input: UpdateUserInput): F[ApiResult[UpdateUserOutput]]
}

object UserApis {

  def impl[F[_] : Monad](passwordHasher: PasswordHasher[F], token: JwtToken[F], userRepo: UserRepo[F]) = new UserApis[F]() {

    def authenticate(input: AuthenticateUserInput): F[ApiResult[AuthenticateUserOutput]] = {
      val userWithToken = for {
        userWithId <- OptionT(userRepo.findUserByEmail(input.email))
        _ <- OptionT(passwordHasher.validate(input.password, userWithId.entity.password).map(if (_) Some(true) else None))
        token <- OptionT.liftF(token.generate(JwtTokenPayload(userWithId.id)))
      } yield mkUser(userWithId.entity, token)

      userWithToken.value.map(_.map(AuthenticateUserOutput.apply).toRight(UserNotFoundOrPasswordNotMatched()))
    }

    def register(input: RegisterUserInput): F[ApiResult[RegisterUserOutput]] = {
      def mkUserEntity(hashedPassword: String): E.User = {
        val now = Instant.now
        E.User(input.email, input.username, hashedPassword, None, None, now, now)
      }

      val userWithToken = for {
        _ <- EitherT(emailAndUsernameNotExist(input.email, input.username))
        hashedPsw <- EitherT.liftF(passwordHasher.hash(input.password))
        userWithId <- EitherT.liftF(userRepo.createUser(mkUserEntity(hashedPsw)))
        token <- EitherT.liftF[F, ApiError, String](token.generate(JwtTokenPayload(userWithId.id)))
      } yield mkUser(userWithId.entity, token)

      userWithToken.value.map(_.map(RegisterUserOutput.apply))
    }

    def get(input: GetUserInput): F[ApiResult[GetUserOutput]] = {
      val userWithToken = for {
        userWithId <- OptionT(userRepo.findUserById(input.authUser))
        token <- OptionT.liftF(token.generate(JwtTokenPayload(userWithId.id)))
      } yield mkUser(userWithId.entity, token)

      userWithToken.value.map(_.map(GetUserOutput.apply).toRight(UserNotFound()))
    }

    def update(input: UpdateUserInput): F[ApiResult[UpdateUserOutput]] = {
      def mkUserForUpdateEntity(hashedPassword: Option[String]): E.UserForUpdate = {
        val now = Instant.now
        E.UserForUpdate(input.username, input.email, hashedPassword, input.bio, input.image, now)
      }

      val userWithToken = for {
        _ <- input.email.traverse(e => EitherT(emailNotTakenByOthers(e, input.authUser)))
        _ <- input.username.traverse(u => EitherT(usernameNotTakenByOthers(u, input.authUser)))
        hashedPsw <- EitherT.liftF(input.password.traverse(passwordHasher.hash))
        userWithId <- EitherT.liftF(userRepo.updateUser(input.authUser, mkUserForUpdateEntity(hashedPsw)))
        token <- EitherT.liftF[F, ApiError, String](token.generate(JwtTokenPayload(userWithId.id)))
      } yield mkUser(userWithId.entity, token)

      userWithToken.value.map(_.map(UpdateUserOutput.apply))
    }

    def emailAndUsernameNotExist(email: String, username: String): F[Either[ApiError, Boolean]] = {
      val notExists = for {
        emailNotExists <- EitherT(emailNotExists(email))
        usernameNotExists <- EitherT(usernameNotExists(username))
      } yield (emailNotExists && usernameNotExists)

      notExists.value
    }

    def notExists(user: Option[E.WithId[E.User]], error: ApiError): Either[ApiError, Boolean] =
      if (user.isEmpty) Right(true) else Left(error)

    def emailNotExists(email: String): F[Either[ApiError, Boolean]] =
      userRepo.findUserByEmail(email).map(notExists(_, EmailAlreadyExists()))

    def usernameNotExists(username: String): F[Either[ApiError, Boolean]] =
      userRepo.findUserByUsername(username).map(notExists(_, UsernameAlreadyExists()))

    def notTakenByOthers(user: Option[E.WithId[E.User]], authUser: AuthUser, error: ApiError): Either[ApiError, Boolean] =
      if (user.isEmpty || user.map(_.id).getOrElse(authUser) == authUser) Right(true) else Left(error)

    def emailNotTakenByOthers(email: String, authUser: AuthUser): F[Either[ApiError, Boolean]] =
      userRepo.findUserByEmail(email).map(notTakenByOthers(_, authUser, EmailAlreadyExists()))

    def usernameNotTakenByOthers(username: String, authUser: AuthUser): F[Either[ApiError, Boolean]] =
      userRepo.findUserByUsername(username).map(notTakenByOthers(_, authUser, UsernameAlreadyExists()))
  }
}
