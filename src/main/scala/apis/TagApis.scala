package io.rw.app.apis

import cats.data.OptionT
import cats.implicits._
import cats.Monad
import io.rw.app.data.{Entities => E, _}
import io.rw.app.data.ApiErrors._
import io.rw.app.data.ApiInputs._
import io.rw.app.data.ApiOutputs._
import io.rw.app.repos._

trait TagApis[F[_]] {
  def get(input: GetTagsInput): F[ApiResult[GetTagsOutput]]
}

object TagApis {

  def impl[F[_] : Monad](tagRepo: TagRepo[F]) = new TagApis[F] {
    def get(input: GetTagsInput): F[ApiResult[GetTagsOutput]] =
      tagRepo.findPopularTags().map(tags => Right(GetTagsOutput(tags)))
  }
}
