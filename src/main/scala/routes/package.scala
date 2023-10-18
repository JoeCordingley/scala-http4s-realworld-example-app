package io.rw.app

import cats.Monad
import cats.data.*
import cats.effect.Sync
import cats.implicits.*
import io.circe.Encoder
import io.circe.generic.auto.*
import io.rw.app.data.*
import io.rw.app.data.ApiErrors.*
import io.rw.app.security.JwtToken
import io.rw.app.valiation.*
import io.rw.app.valiation.InvalidFields.*
import io.rw.app.utils.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization

package object routes {

  type AppRoutes[F[_]] = AuthedRoutes[Option[AuthUser], F]

  def authUser[F[_]: Monad](
      token: JwtToken[F]
  ): Kleisli[[A] =>> OptionT[F, A], Request[F], Option[AuthUser]] =
    Kleisli { rq =>
      OptionT.liftF {
        for {
          header <- Monad[F].pure(rq.headers.get[Authorization])
          jwt <- Monad[F].pure(
            header.flatMap(h => extractTokenValue(h.credentials.renderString))
          )
          payload <- jwt.flatTraverse(token.validate(_))
        } yield payload.map(_.authUser)
      }
    }

  def withAuthUser[F[_]: Sync](authUser: Option[AuthUser])(
      fn: AuthUser => F[Response[F]]
  ): F[Response[F]] =
    authUser.fold(Sync[F].pure(Response[F](Status.Unauthorized)))(fn)

  def withValidation[F[_]: Sync, A](
      validated: ValidationResult[A]
  )(fn: A => F[Response[F]])(implicit dsl: Http4sDsl[F]): F[Response[F]] = {
    import dsl.*

    validated.toEither.fold(
      errors => UnprocessableEntity(validationErrorsToResponse(errors)),
      fn
    )
  }

  val defaultNotFoundResponse = NotFoundResponse(404, "Not Found")

  def toResponse[F[_]: Sync, R](
      res: ApiResult[R]
  )(implicit dsl: Http4sDsl[F], encoder: Encoder[R]): F[Response[F]] = {
    import dsl.*

    res match {
      case Right(r)                 => Ok(r)
      case Left(_: UserNotFound)    => NotFound(defaultNotFoundResponse)
      case Left(_: ProfileNotFound) => NotFound(defaultNotFoundResponse)
      case Left(_: ArticleNotFound) => NotFound(defaultNotFoundResponse)
      case Left(_: CommentNotFound) => NotFound(defaultNotFoundResponse)
      case Left(_: EmailAlreadyExists) =>
        UnprocessableEntity(
          validationErrorsToResponse(
            NonEmptyChain.one(InvalidEmail(List("has already been taken")))
          )
        )
      case Left(_: UsernameAlreadyExists) =>
        UnprocessableEntity(
          validationErrorsToResponse(
            NonEmptyChain.one(InvalidUsername(List("has already been taken")))
          )
        )
      case Left(_: UserNotFoundOrPasswordNotMatched) =>
        UnprocessableEntity(
          validationErrorsToResponse(
            NonEmptyChain.one(InvalidEmailOrPassword(List("is invalid")))
          )
        )
      case Left(e) => InternalServerError()
    }
  }

  def validationErrorsToResponse(
      nec: NonEmptyChain[InvalidField]
  ): ValidationErrorResponse =
    ValidationErrorResponse(nec.toList.map(e => e.field -> e.errors).toMap)
}
