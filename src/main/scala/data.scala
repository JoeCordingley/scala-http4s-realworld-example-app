package io.rw.app

import java.time.Instant
import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import json.{JsonObject, Nullable, oNValue, JsonNull, JsonArray, Email}
import cats.data.NonEmptyList
import io.rw.app.validation.*
import cats.syntax.all.*
import json.{SchemaType, SchemaOf, JsonSchema}

object data {

  case class GetArticlesOutput(articles: List[Article], articlesCount: Int)
  case class GetCommentsOutput(comments: List[Comment])

  type ApiResult[R] = Either[ApiError, R]
  type AuthUser = Int

  object JsonCodec {
    type WrappedUser[T] = JsonObject.Solo[("user", T)]
    object WrappedUser:
      def apply[T]: T => WrappedUser[T] = t => JsonObject.Solo(("user", t))

    type AuthenticateUser =
      JsonObject[(("email", Email), ("password", Password))]

    case class Password(value: String)
    object Password:
      given Decoder[Password] = Decoder[String].emap(fromString)
      given SchemaOf[Password] with
        def apply: JsonSchema = JsonSchema(
          `type` = Some(Left(SchemaType.String)),
          minLength = Some(8),
          maxLength = Some(100)
        )
      def fromString: String => Either[String, Password] =
        validPassword(_)
          .map(Password(_))
          .toEither
          .leftMap(_ => "password is invalid")

    case class Username(value: String)
    object Username:
      given Decoder[Username] = Decoder[String].emap(fromString)
      given SchemaOf[Username] with
        def apply: JsonSchema = JsonSchema(
          `type` = Some(Left(SchemaType.String)),
          minLength = Some(1),
          maxLength = Some(25)
        )
      def fromString: String => Either[String, Username] =
        validUsername(_)
          .map(Username(_))
          .toEither
          .leftMap(_ => "invalid username")

    type RegisterUser = JsonObject[
      (("username", Username), ("email", Email), ("password", Password))
    ]
    type UpdateUser = JsonObject[
      (
          Option[("username", Nullable[Username])],
          Option[("email", Nullable[Email])],
          Option[("password", Nullable[Password])],
          Option[("bio", Nullable[String])],
          Option[("image", Nullable[String])],
      )
    ]
    type UpdateArticle = JsonObject[
      (
          Option[("title", Nullable[String])],
          Option[("description", Nullable[String])],
          Option[("body", Nullable[String])]
      )
    ]
    type WrappedArticle[T] = JsonObject.Solo[("article", T)]
    object WrappedArticle:
      def apply[T]: T => WrappedArticle[T] = t =>
        JsonObject.Solo(("article", t))

    type Article = JsonObject[
      (
          ("slug", String),
          ("title", String),
          ("description", String),
          ("body", String),
          ("tagList", JsonArray[String]),
          ("createdAt", Instant),
          ("updatedAt", Instant),
          ("favorited", Boolean),
          ("favoritesCount", Int),
          ("author", Profile),
      )
    ]

    object Article:
      def fromData: data.Article => Article = {
        case data.Article(
              slug,
              title,
              description,
              body,
              tagList,
              createdAt,
              updatedAt,
              favorited,
              favoritesCount,
              author
            ) =>
          JsonObject(
            ("slug", slug),
            ("title", title),
            ("description", description),
            ("body", body),
            ("tagList", JsonArray(tagList)),
            ("createdAt", createdAt),
            ("updatedAt", updatedAt),
            ("favorited", favorited),
            ("favoritesCount", favoritesCount),
            ("author", Profile.fromData(author))
          )
      }
    type Profile = JsonObject[
      (
          ("username", String),
          ("bio", Nullable[String]),
          ("image", Nullable[String]),
          ("following", Boolean),
      )
    ]

    object Profile:
      def fromData: data.Profile => Profile = {
        case data.Profile(username, maybeBio, maybeImage, following) =>
          JsonObject(
            ("username", username),
            ("bio", Nullable.fromOption(maybeBio)),
            ("image", Nullable.fromOption(maybeImage)),
            ("following", following)
          )
      }

    type User = JsonObject[
      (
          ("email", String),
          ("token", String),
          ("username", String),
          ("bio", Nullable[String]),
          ("image", Nullable[String]),
      )
    ]
    object User:
      def fromData: data.User => User = {
        case data.User(email, token, username, bio, image) =>
          JsonObject(
            (
              ("email", email),
              ("token", token),
              ("username", username),
              ("bio", Nullable.fromOption(bio)),
              ("image", Nullable.fromOption(image))
            )
          )
      }
    type GetArticlesOutput =
      JsonObject[(("articles", JsonArray[Article]), ("articlesCount", Int))]
    object GetArticlesOutput:
      def fromData: data.GetArticlesOutput => GetArticlesOutput = {
        case data.GetArticlesOutput(articles, articlesCount) =>
          JsonObject(
            ("articles", JsonArray(articles.map(Article.fromData))),
            ("articlesCount", articlesCount)
          )
      }
    type CreateArticle = JsonObject[
      (
          ("title", String),
          ("description", String),
          ("body", String),
          Option[("tagList", Nullable[JsonArray[String]])],
      )
    ]
    type WrappedComment[T] = JsonObject.Solo[("comment", T)]
    object WrappedComment:
      def apply[T]: T => WrappedComment[T] = t =>
        JsonObject.Solo(("comment", t))
    type Body = JsonObject.Solo[("body", String)]
    type Comment = JsonObject[
      (
          ("id", Int),
          ("createdAt", Instant),
          ("updatedAt", Instant),
          ("body", String),
          ("author", Profile)
      )
    ]
    object Comment:
      def fromData: data.Comment => Comment = {
        case data.Comment(id, createdAt, updatedAt, body, author) =>
          JsonObject(
            ("id", id),
            ("createdAt", createdAt),
            ("updatedAt", updatedAt),
            ("body", body),
            ("author", Profile.fromData(author))
          )
      }
    type Comments = JsonObject.Solo[("comments", JsonArray[Comment])]
    object Comments:
      def fromData: data.GetCommentsOutput => Comments = {
        case data.GetCommentsOutput(comments) =>
          JsonObject.Solo(
            ("comments", JsonArray(comments.map(Comment.fromData)))
          )
      }
    type WrappedProfile = JsonObject.Solo[("profile", Profile)]
    object WrappedProfile:
      def fromData: data.Profile => WrappedProfile = p =>
        JsonObject.Solo(("profile", Profile.fromData(p)))

    type Tags = JsonObject.Solo[("tags", JsonArray[String])]
    object Tags:
      def fromData: List[String] => Tags = tags =>
        JsonObject.Solo(("tags", JsonArray(tags)))

  }

  object RequestBodies {

    case class RegisterUserBody(
        username: String,
        email: String,
        password: String
    )
    object RegisterUserBody {}

    case class UpdateUserBody(
        username: Option[String],
        email: Option[String],
        password: Option[String],
        bio: Option[String],
        image: Option[String]
    )

    object UpdateUserBody:
      def fromCodec: JsonCodec.UpdateUser => UpdateUserBody = {
        case JsonObject(
              (maybeUsername, maybeEmail, maybePassword, maybeBio, maybeImage)
            ) =>
          UpdateUserBody(
            username = oNValue["username"](maybeUsername).map(_.value),
            email = oNValue["email"](maybeEmail).map(_.value),
            password = oNValue["password"](maybePassword).map(_.value),
            bio = oNValue["bio"](maybeBio),
            image = oNValue["image"](maybeImage)
          )
      }
    case class WrappedArticleBody[T](article: T)
    case class CreateArticleBody(
        title: String,
        description: String,
        body: String,
        tagList: Option[List[String]]
    )
    object CreateArticleBody:
      def fromCodec: JsonCodec.CreateArticle => CreateArticleBody = {
        case JsonObject(
              ("title", title),
              ("description", description),
              ("body", body),
              maybeTagList
            ) =>
          CreateArticleBody(
            title,
            description,
            body,
            oNValue["tagList"](maybeTagList).map(_.elements)
          )
      }
    case class UpdateArticleBody(
        title: Option[String],
        description: Option[String],
        body: Option[String]
    )
    object UpdateArticleBody:
      def fromCodec: JsonCodec.UpdateArticle => UpdateArticleBody = {
        case JsonObject(maybeTitle, maybeDescription, maybeBody) =>
          UpdateArticleBody(
            title = oNValue["title"](maybeTitle),
            description = oNValue["description"](maybeDescription),
            body = oNValue["body"](maybeBody)
          )
      }
    case class WrappedCommentBody[T](comment: T)
    case class AddCommentBody(body: String)
  }

  enum UserApiInput:
    case AuthenticateUserInput(email: String, password: String)
    case RegisterUserInput(
        username: String,
        email: String,
        password: String
    )
    case GetUserInput(authUser: AuthUser)
    case UpdateUserInput(
        authUser: AuthUser,
        username: Option[String] = None,
        email: Option[String] = None,
        password: Option[String] = None,
        bio: Option[String] = None,
        image: Option[String] = None
    )

  sealed trait ApiInput
  object ApiInputs {
    object AuthenticateUserInput {
      def fromCodec: JsonCodec.AuthenticateUser => UserApiInput = {
        case JsonObject(
              (
                ("email", Email(email)),
                ("password", JsonCodec.Password(password))
              )
            ) =>
          UserApiInput.AuthenticateUserInput(email, password)
      }
    }
    object RegisterUserInput:
      def fromCodec: JsonCodec.RegisterUser => UserApiInput = {
        case JsonObject(
              (
                ("username", JsonCodec.Username(username)),
                ("email", Email(email)),
                ("password", JsonCodec.Password(password))
              )
            ) =>
          UserApiInput.RegisterUserInput(username, email, password)
      }
    case class GetProfileInput(authUser: Option[AuthUser], username: String)
        extends ApiInput
    case class FollowUserInput(authUser: AuthUser, username: String)
        extends ApiInput
    case class UnfollowUserInput(authUser: AuthUser, username: String)
        extends ApiInput
    case class GetAllArticlesInput(
        authUser: Option[AuthUser],
        filter: ArticleFilter,
        pagination: Pagination
    ) extends ApiInput
    case class GetArticlesFeedInput(authUser: AuthUser, pagination: Pagination)
        extends ApiInput
    case class GetArticleInput(authUser: Option[AuthUser], slug: String)
        extends ApiInput
    case class CreateArticleInput(
        authUser: AuthUser,
        title: String,
        description: String,
        body: String,
        tagList: List[String]
    ) extends ApiInput
    case class UpdateArticleInput(
        authUser: AuthUser,
        slug: String,
        title: Option[String],
        description: Option[String],
        body: Option[String]
    ) extends ApiInput
    case class DeleteArticleInput(authUser: AuthUser, slug: String)
        extends ApiInput
    case class FavoriteArticleInput(authUser: AuthUser, slug: String)
        extends ApiInput
    case class UnfavoriteArticleInput(authUser: AuthUser, slug: String)
        extends ApiInput
    case class AddCommentInput(authUser: AuthUser, slug: String, body: String)
        extends ApiInput
    case class GetCommentsInput(authUser: Option[AuthUser], slug: String)
        extends ApiInput
    case class DeleteCommentInput(
        authUser: AuthUser,
        slug: String,
        commentId: Int
    ) extends ApiInput
    case class GetTagsInput() extends ApiInput
  }

  object ApiOutputs {
    // TODO move to test
    case class AuthenticateUserOutput(user: User)
    case class RegisterUserOutput(user: User)
    case class GetUserOutput(user: User)
    case class UpdateUserOutput(user: User)
    case class GetProfileOutput(profile: Profile)
    case class FollowUserOutput(profile: Profile)
    case class UnfollowUserOutput(profile: Profile)
    case class GetArticlesFeedOutput(
        articles: List[Article],
        articlesCount: Int
    )
    case class GetAllArticlesOutput(
        articles: List[Article],
        articlesCount: Int
    )
    case class GetArticleOutput(article: Article)
    case class CreateArticleOutput(article: Article)
    case class UpdateArticleOutput(article: Article)
    case class DeleteArticleOutput()
    case class FavoriteArticleOutput(article: Article)
    case class UnfavoriteArticleOutput(article: Article)
    case class AddCommentOutput(comment: Comment)
    // TODO return {} instead of null
    case class GetTagsOutput(tags: List[String])
  }

  sealed trait ApiError
  object ApiErrors {
    case class InvalidJson(messages: NonEmptyList[String]) extends ApiError
    case object UserNotFound extends ApiError
    case class UserFollowingHimself(profile: Profile) extends ApiError
    case class UserUnfollowingHimself(profile: Profile) extends ApiError
    case object UserNotFoundOrPasswordNotMatched extends ApiError
    case object EmailAlreadyExists extends ApiError
    case object UsernameAlreadyExists extends ApiError
    case object ProfileNotFound extends ApiError
    case object ArticleNotFound extends ApiError
    case object CommentNotFound extends ApiError
  }

  object Entities {
    case class WithId[T](id: Int, entity: T)
    case class User(
        email: String,
        username: String,
        password: String,
        bio: Option[String],
        image: Option[String],
        createdAt: Instant,
        updatedAt: Instant
    )
    case class UserForUpdate(
        username: Option[String],
        email: Option[String],
        password: Option[String],
        bio: Option[String],
        image: Option[String],
        updatedAt: Instant
    )
    case class Follower(followeeId: Int, followerId: Int)
    case class Article(
        slug: String,
        title: String,
        description: String,
        body: String,
        authorId: Int,
        createdAt: Instant,
        updatedAt: Instant
    )
    case class ArticleForUpdate(
        slug: Option[String],
        title: Option[String],
        description: Option[String],
        body: Option[String],
        updatedAt: Instant
    )
    case class Tag(articleId: Int, tag: String)
    case class Favorite(articleId: Int, userId: Int)
    case class Comment(
        body: String,
        articleId: Int,
        authorId: Int,
        createdAt: Instant,
        updatedAt: Instant
    )
  }

  case class AppConfig(
      apiHost: String,
      apiPort: Int,
      idHasherSalt: String,
      jwtTokenKey: String,
      jwtTokenExpiration: Int,
      dbUser: String,
      dbPassword: String,
      dbUrl: String
  ) derives ConfigReader

  case class JwtTokenPayload(authUser: AuthUser)
  case class ArticleFilter(
      tag: Option[String],
      author: Option[String],
      favorited: Option[String]
  )
  case class Pagination(limit: Int, offset: Int)
  case class User(
      email: String,
      token: String,
      username: String,
      bio: Option[String] = None,
      image: Option[String] = None
  )
  case class Profile(
      username: String,
      bio: Option[String],
      image: Option[String],
      following: Boolean
  )
  case class Article(
      slug: String,
      title: String,
      description: String,
      body: String,
      tagList: List[String],
      createdAt: Instant,
      updatedAt: Instant,
      favorited: Boolean,
      favoritesCount: Int,
      author: Profile
  )
  case class Comment(
      id: Int,
      createdAt: Instant,
      updatedAt: Instant,
      body: String,
      author: Profile
  )

  type ValidationErrors = Map[String, List[String]]
  case class ValidationErrorResponse(errors: ValidationErrors)
  case class NotFoundResponse(status: Int, error: String)

  type ErrorsListJson =
    JsonObject.Solo[("errors", JsonObject.Solo[("body", JsonArray[String])])]
  object ErrorsListJson:
    def fromErrors(errors: NonEmptyList[String]): ErrorsListJson =
      JsonObject.Solo(
        "errors" -> JsonObject.Solo("body" -> JsonArray(errors.toList))
      )
}
