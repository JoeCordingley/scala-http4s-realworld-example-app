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
import io.rw.app.validation.*
import io.rw.app.validation.InvalidFields.*
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
        rq.headers
          .get[Authorization]
          .flatMap(h => extractTokenValue(h.credentials.renderString))
          .flatTraverse(token.validate(_))
          .map(_.map(_.authUser))
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
      case Right(r) => Ok(r)
      case Left(error) =>
        error match {
          case UserNotFound    => NotFound(defaultNotFoundResponse)
          case ProfileNotFound => NotFound(defaultNotFoundResponse)
          case ArticleNotFound => NotFound(defaultNotFoundResponse)
          case CommentNotFound => NotFound(defaultNotFoundResponse)
          case EmailAlreadyExists =>
            UnprocessableEntity(
              ErrorsListJson.fromErrors(NonEmptyList.one("email taken"))
            )
          case InvalidJson(messages) =>
            UnprocessableEntity(
              ErrorsListJson.fromErrors(messages)
            )
          case UsernameAlreadyExists =>
            UnprocessableEntity(
              ErrorsListJson.fromErrors(NonEmptyList.one("username taken"))
            )
          case UserNotFoundOrPasswordNotMatched =>
            UnprocessableEntity(
              ErrorsListJson.fromErrors(
                NonEmptyList.one("email or password is invalid")
              )
            )
          case e => InternalServerError()
        }
    }
  }

  def validationErrorsToResponse(
      nec: NonEmptyChain[InvalidField]
  ): ValidationErrorResponse =
    ValidationErrorResponse(nec.toList.map(e => e.field -> e.errors).toMap)
}
