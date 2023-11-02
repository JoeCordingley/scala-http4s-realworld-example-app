package io.rw.app.apis

import cats.data.OptionT
import cats.implicits.*
import cats.Monad
import io.rw.app.data.{Entities as E, *}
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.repos.*

trait TagApis[F[_]] {
  def get(input: GetTagsInput): F[ApiResult[List[String]]]
}

object TagApis {

  def impl[F[_]: Monad](tagRepo: TagRepo[F]) = new TagApis[F] {
    def get(input: GetTagsInput): F[ApiResult[List[String]]] =
      tagRepo.findPopularTags().map(tags => Right(tags))
  }
}
