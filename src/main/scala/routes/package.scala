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
//  object AppRoutes:
//    def fromValidatedRoutes[F[_]: Monad](
//        validated: ValidatedRoutes[F]
//    ): AppRoutes[F] = {
//      val dsl = new Http4sDsl[F] {}
//      import dsl.*
//      Kleisli(validated).flatMapF {
//        case Right(response) => response.pure
//        case Left(errors) =>
//          OptionT.liftF(
//            UnprocessableEntity(ErrorsListJson.fromErrors(errors))
//          )
//      }
//    }
//
//  type ValidatedRoutes[F[_]] =
//    AuthedRequest[F, Option[AuthUser]] => OptionT[F, Either[Errors, Response[
//      F
//    ]]]
//  type Errors = NonEmptyList[String]

//  case class JsonEncoded[A](value: A)
//  object JsonEncoded:
//  given [F[_], A: Decoder]: EntityDecoder[F, JsonEncoded[A]] = circeEntityDecoder[Json].flatMapR(json => Decoder[A].decodeAccumulating(json.hcursor).toEitherT)
//    case class JsonFailure(cause: Option[Throwable] = None, failures: NonEmptyList[String]) extends DecodeFailure{
//      override def message: String = "invalid json format"
//      override def toHttpResponse[F[_]](httpVersion: HttpVersion): Response[F] = ???
//    }
//    object JsonFailure:
//      def fromValidated: Validated

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
      case Left(InvalidJson(message)) =>
        UnprocessableEntity(
          ErrorsListJson.fromErrors(NonEmptyList.one(message))
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
