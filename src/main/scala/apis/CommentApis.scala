package io.rw.app.apis

import cats.data.OptionT
import cats.implicits.*
import cats.Monad
import io.rw.app.data.{Entities as E, *}
import io.rw.app.data.ApiErrors.*
import io.rw.app.data.ApiInputs.*
import io.rw.app.data.ApiOutputs.*
import io.rw.app.repos.*
import io.rw.app.utils.*
import java.time.Instant

trait CommentApis[F[_]] {
  def add(input: AddCommentInput): F[ApiResult[AddCommentOutput]]
  def get(input: GetCommentsInput): F[ApiResult[GetCommentsOutput]]
  def delete(input: DeleteCommentInput): F[ApiResult[DeleteCommentOutput]]
}

object CommentApis {

  def impl[F[_] : Monad](commentRepo: CommentRepo[F], articleRepo: ArticleRepo[F], followerRepo: FollowerRepo[F]) = new CommentApis[F] {

    def add(input: AddCommentInput): F[ApiResult[AddCommentOutput]] = {
      def mkCommentEntity(articleId: Int): E.Comment = {
        val now = Instant.now
        E.Comment(input.body, articleId, input.authUser, now, now)
      }

      val comment = for {
        (articleWithId, articleAuthor) <- OptionT(articleRepo.findArticleBySlug(input.slug))
        (commentWithId, commentAuthor) <- OptionT.liftF(commentRepo.createComment(mkCommentEntity(articleWithId.id)))
        following <- OptionT.liftF(followerRepo.findFollower(articleAuthor.id, input.authUser).map(_.nonEmpty))
      } yield mkComment(commentWithId, commentAuthor.entity, following)

      comment.value.map(_.map(AddCommentOutput.apply)).map(_.toRight(ArticleNotFound()))
    }

    def get(input: GetCommentsInput): F[ApiResult[GetCommentsOutput]] = {
      val comments = for {
        (articleWithId, _) <- OptionT(articleRepo.findArticleBySlug(input.slug))
        commentsWithAuthors <- OptionT.liftF(commentRepo.findCommentsByArticleId(articleWithId.id))
        authorsIds = commentsWithAuthors.map(_._2.id)
        followers <- OptionT.liftF(input.authUser.traverse(followerRepo.findFollowers(authorsIds, _)).map(_.getOrElse(List.empty)))
      } yield mkComments(commentsWithAuthors, followers)

      comments.value.map(_.map(GetCommentsOutput.apply)).map(_.toRight(ArticleNotFound()))
    }

    def delete(input: DeleteCommentInput): F[ApiResult[DeleteCommentOutput]] = {
      val deleted = for {
        (articleWithId, _) <- OptionT(articleRepo.findArticleBySlug(input.slug))
        _ <- OptionT(commentRepo.deleteComment(input.commentId, articleWithId.id, input.authUser))
      } yield ()

      deleted.value.map(_.map(_ => DeleteCommentOutput())).map(_.toRight(CommentNotFound()))
    }
  }
}
