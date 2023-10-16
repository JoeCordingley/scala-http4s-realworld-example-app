package io.rw.app.repos

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import doobie.implicits.legacy.instant.*
import io.rw.app.data.Entities.*

trait CommentRepo[F[_]] {
  def findCommentsByArticleId(
      articleId: Int
  ): F[List[(WithId[Comment], WithId[User])]]
  def createComment(comment: Comment): F[(WithId[Comment], WithId[User])]
  def deleteComment(
      commentId: Int,
      articleId: Int,
      authorId: Int
  ): F[Option[Unit]]
}

object CommentRepo {

  def impl(xa: Transactor[IO]) = new CommentRepo[IO] {
    def findCommentsByArticleId(
        articleId: Int
    ): IO[List[(WithId[Comment], WithId[User])]] =
      Q.selectCommentsByArticleId(articleId).to[List].transact(xa)

    def createComment(comment: Comment): IO[(WithId[Comment], WithId[User])] = {
      val trx = for {
        id <- Q.insertComment(comment).withUniqueGeneratedKeys[Int]("id")
        comment <- Q.selectCommentById(id).unique
      } yield comment

      trx.transact(xa)
    }

    def deleteComment(
        commentId: Int,
        articleId: Int,
        authorId: Int
    ): IO[Option[Unit]] =
      Q.deleteComment(commentId, articleId, authorId)
        .run
        .map(affectedToOption)
        .transact(xa)

  }

  object Q {
    def selectCommentsByArticleId(articleId: Int) = {
      val q = commentJoinUser ++
        fr"""
          where c.article_id = $articleId
          order by c.created_at desc
        """
      q.query[(WithId[Comment], WithId[User])]
    }

    def selectCommentById(id: Int) = {
      val q = commentJoinUser ++
        fr"""
          where c.id = $id
        """
      q.query[(WithId[Comment], WithId[User])]
    }

    def insertComment(comment: Comment) =
      sql"""
         insert into comments(body, article_id, author_id, created_at, updated_at)
         values(${comment.body}, ${comment.articleId}, ${comment.authorId}, ${comment.createdAt}, ${comment.updatedAt})
      """.update

    def deleteComment(commentId: Int, articleId: Int, authorId: Int) =
      sql"""
         delete from comments
         where id = $commentId
               and article_id = $articleId
               and author_id = $authorId
      """.update

    val commentJoinUser =
      fr"""
        select c.id, c.body, c.article_id, c.author_id, c.created_at, c.updated_at,
               u.id, u.email, u.username, u.password, u.bio, u.image, u.created_at, u.updated_at
        from comments c
        inner join users u on c.author_id = u.id
      """
  }
}
