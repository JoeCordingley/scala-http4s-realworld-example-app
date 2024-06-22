package io.rw.app

import cats.data.*
import cats.data.Validated.*
import cats.implicits.*
import io.rw.app.data.RequestBodies.*

object validation {

  sealed trait InvalidField {
    def errors: List[String]
    def field: String
  }

  object InvalidFields {
    case class InvalidEmail(
        override val errors: List[String],
        override val field: String = "email"
    ) extends InvalidField
    case class InvalidPassword(
        override val errors: List[String],
        override val field: String = "password"
    ) extends InvalidField
    case class InvalidUsername(
        override val errors: List[String],
        override val field: String = "username"
    ) extends InvalidField
    case class InvalidTitle(
        override val errors: List[String],
        override val field: String = "title"
    ) extends InvalidField
    case class InvalidDescription(
        override val errors: List[String],
        override val field: String = "description"
    ) extends InvalidField
    case class InvalidBody(
        override val errors: List[String],
        override val field: String = "body"
    ) extends InvalidField

    case class InvalidEmailOrPassword(
        override val errors: List[String],
        override val field: String = "email or password"
    ) extends InvalidField
  }

  type ValidationResult[A] = ValidatedNec[InvalidField, A]

  def validRegisterUserBody(
      body: RegisterUserBody
  ): ValidationResult[RegisterUserBody] =
    (
      validUsername(body.username),
      validEmail(body.email),
      validPassword(body.password)
    ).mapN(RegisterUserBody.apply)

  def validUpdateUserBody(
      body: UpdateUserBody
  ): ValidationResult[UpdateUserBody] =
    (
      body.username.traverse(validUsername),
      body.email.traverse(validEmail),
      body.password.traverse(validPassword),
      body.bio.validNec,
      body.image.validNec
    ).mapN(UpdateUserBody.apply)

  def validCreateArticleBody(
      body: CreateArticleBody
  ): ValidationResult[CreateArticleBody] =
    (
      validTitle(body.title),
      validDescription(body.description),
      validBody(body.body),
      body.tagList.traverse(validTags)
    ).mapN(CreateArticleBody.apply)

  def validUpdateArticleBody(
      body: UpdateArticleBody
  ): ValidationResult[UpdateArticleBody] =
    (
      body.title.traverse(validTitle),
      body.description.traverse(validDescription),
      body.body.traverse(validBody)
    ).mapN(UpdateArticleBody.apply)

  def validAddCommentBody(
      body: AddCommentBody
  ): ValidationResult[AddCommentBody] =
    validBody(body.body).map(AddCommentBody.apply)

  import validators.*
  import InvalidFields.*
  def validEmail(email: String): ValidationResult[String] = {
    val trimmedEmail = email.trim
    (
      notBlank(trimmedEmail),
      max(trimmedEmail, 350),
      looksLikeEmail(trimmedEmail)
    ).mapN({ case t => t._1 }).leftMap(toInvalidField(_, InvalidEmail.apply(_)))
  }

  def validPassword(password: String): ValidationResult[String] =
    (notBlank(password), min(password, 8), max(password, 100))
      .mapN({ case t => t._1 })
      .leftMap(toInvalidField(_, InvalidPassword.apply(_)))

  def validUsername(username: String): ValidationResult[String] = {
    val trimmedUsername = username.trim
    (
      notBlank(trimmedUsername),
      min(trimmedUsername, 1),
      max(trimmedUsername, 25)
    ).mapN({ case t => t._1 })
      .leftMap(toInvalidField(_, InvalidUsername.apply(_)))
  }

  def validTitle(title: String): ValidationResult[String] = {
    val trimmedTitle = title.trim
    notBlank(trimmedTitle).leftMap(toInvalidField(_, InvalidTitle.apply(_)))
  }

  def validDescription(description: String): ValidationResult[String] = {
    val trimmedDescription = description.trim
    notBlank(trimmedDescription).leftMap(
      toInvalidField(_, InvalidDescription.apply(_))
    )
  }

  def validBody(body: String): ValidationResult[String] = {
    val trimmedBody = body.trim
    notBlank(trimmedBody).leftMap(toInvalidField(_, InvalidBody.apply(_)))
  }

  def validTags(tags: List[String]): ValidationResult[List[String]] =
    tags.map(_.trim).filter(_.nonEmpty).distinct.validNec

  def toInvalidField[F <: InvalidField](
      nec: NonEmptyChain[String],
      mkInvalidField: List[String] => F
  ): NonEmptyChain[F] =
    NonEmptyChain.one(mkInvalidField(nec.toList))

  object validators {
    type ValidatorResult[A] = ValidatedNec[String, A]
    def notBlank(s: String): ValidatorResult[String] =
      if (s.nonEmpty) s.validNec else "can't be blank".invalidNec

    def min(s: String, minSize: Int): ValidatorResult[String] =
      if (s.size >= minSize) s.validNec
      else s"is too short (minimum is $minSize character)".invalidNec

    def max(s: String, maxSize: Int): ValidatorResult[String] =
      if (s.size <= maxSize) s.validNec
      else s"is too long (maximum is $maxSize character)".invalidNec

    val emailPattern = ".+@.+\\..+".r
    def looksLikeEmail(s: String): ValidatorResult[String] =
      if (emailPattern.matches(s)) s.validNec else "is invalid".invalidNec
  }
}
