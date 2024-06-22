package io.rw.app.apis

import cats.data.{EitherT, OptionT}
import cats.implicits.*
import cats.Monad
import io.rw.app.data.{Entities as E, *}
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.data.UserApiInput
import io.rw.app.repos.*
import io.rw.app.security.{JwtToken, PasswordHasher}
import java.time.Instant

//TODO ApiResult to F
type UserApis[F[_]] = UserApiInput => EitherT[F, ApiError, User]

object UserApis {

  def impl[F[_]: Monad](
      passwordHasher: PasswordHasher[F],
      token: JwtToken[F],
      userRepo: UserRepo[F]
  ) = {

    def authenticate(
        email: String,
        password: String
    ): EitherT[F, ApiError, User] = {
      val userWithToken = for {
        userWithId <- OptionT(userRepo.findUserByEmail(email))
        _ <- OptionT(
          passwordHasher
            .validate(password, userWithId.entity.password)
            .map(if (_) Some(true) else None)
        )
        token <- OptionT.liftF(token.generate(JwtTokenPayload(userWithId.id)))
      } yield mkUser(userWithId.entity, token)

      userWithToken.toRight(
        UserNotFoundOrPasswordNotMatched
      )
    }

    def register(
        username: String,
        email: String,
        password: String
    ): EitherT[F, ApiError, User] = {
      def mkUserEntity(hashedPassword: String): E.User = {
        val now = Instant.now
        E.User(
          email,
          username,
          hashedPassword,
          None,
          None,
          now,
          now
        )
      }

      val userWithToken = for {
        _ <- emailAndUsernameNotExist(email, username)
        hashedPsw <- EitherT.liftF(passwordHasher.hash(password))
        userWithId <- EitherT.liftF(
          userRepo.createUser(mkUserEntity(hashedPsw))
        )
        token <- EitherT.liftF[F, ApiError, String](
          token.generate(JwtTokenPayload(userWithId.id))
        )
      } yield mkUser(userWithId.entity, token)

      userWithToken
    }

    def get(authUser: AuthUser): EitherT[F, ApiError, User] = {
      val userWithToken = for {
        userWithId <- OptionT(userRepo.findUserById(authUser))
        token <- OptionT.liftF(token.generate(JwtTokenPayload(userWithId.id)))
      } yield mkUser(userWithId.entity, token)

      userWithToken.toRight(UserNotFound)
    }

    def update(
        authUser: AuthUser,
        username: Option[String],
        email: Option[String],
        password: Option[String],
        bio: Option[String],
        image: Option[String]
    ): EitherT[F, ApiError, User] = {
      def mkUserForUpdateEntity(
          hashedPassword: Option[String]
      ): E.UserForUpdate = {
        val now = Instant.now
        E.UserForUpdate(
          username,
          email,
          hashedPassword,
          bio,
          image,
          now
        )
      }

      val userWithToken = for {
        _ <- email.traverse(e => EitherT(emailNotTakenByOthers(e, authUser)))
        _ <- username.traverse(u =>
          EitherT(usernameNotTakenByOthers(u, authUser))
        )
        hashedPsw <- EitherT.liftF(password.traverse(passwordHasher.hash))
        userWithId <- EitherT.liftF(
          userRepo.updateUser(authUser, mkUserForUpdateEntity(hashedPsw))
        )
        token <- EitherT.liftF[F, ApiError, String](
          token.generate(JwtTokenPayload(userWithId.id))
        )
      } yield mkUser(userWithId.entity, token)

      userWithToken
    }

    def emailAndUsernameNotExist(
        email: String,
        username: String
    ): EitherT[F, ApiError, Boolean] = for {
      emailNotExists <- EitherT(emailNotExists(email))
      usernameNotExists <- EitherT(usernameNotExists(username))
    } yield (emailNotExists && usernameNotExists)

    def notExists(
        user: Option[E.WithId[E.User]],
        error: ApiError
    ): Either[ApiError, Boolean] =
      if (user.isEmpty) Right(true) else Left(error)

    def emailNotExists(email: String): F[Either[ApiError, Boolean]] =
      userRepo.findUserByEmail(email).map(notExists(_, EmailAlreadyExists))

    def usernameNotExists(username: String): F[Either[ApiError, Boolean]] =
      userRepo
        .findUserByUsername(username)
        .map(notExists(_, UsernameAlreadyExists))

    def notTakenByOthers(
        user: Option[E.WithId[E.User]],
        authUser: AuthUser,
        error: ApiError
    ): Either[ApiError, Boolean] =
      if (user.isEmpty || user.map(_.id).getOrElse(authUser) == authUser)
        Right(true)
      else Left(error)

    def emailNotTakenByOthers(
        email: String,
        authUser: AuthUser
    ): F[Either[ApiError, Boolean]] =
      userRepo
        .findUserByEmail(email)
        .map(notTakenByOthers(_, authUser, EmailAlreadyExists))

    def usernameNotTakenByOthers(
        username: String,
        authUser: AuthUser
    ): F[Either[ApiError, Boolean]] =
      userRepo
        .findUserByUsername(username)
        .map(notTakenByOthers(_, authUser, UsernameAlreadyExists))
    val f: UserApis[F] = {
      case UserApiInput.AuthenticateUserInput(
            email,
            password
          ) =>
        authenticate(email, password)
      case UserApiInput.RegisterUserInput(
            username,
            email,
            password
          ) =>
        register(
          username,
          email,
          password
        )
      case UserApiInput.GetUserInput(authUser) => get(authUser)
      case UserApiInput.UpdateUserInput(
            authUser,
            username,
            email,
            password,
            bio,
            image
          ) =>
        update(authUser, username, email, password, bio, image)
    }
    f
  }
}
