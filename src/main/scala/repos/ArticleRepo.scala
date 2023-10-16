package io.rw.app.repos

import cats.data.OptionT
import cats.effect.IO
import cats.implicits.*
import doobie.*
import doobie.Fragments.*
import doobie.implicits.*
import doobie.implicits.legacy.instant.*
import io.rw.app.data.Entities.*
import io.rw.app.data.{ArticleFilter, Pagination}

trait ArticleRepo[F[_]] {
  def findArticleBySlug(slug: String): F[Option[(WithId[Article], WithId[User])]]
  def findArticlesFilteredBy(filter: ArticleFilter, pagination: Pagination): F[List[(WithId[Article], WithId[User])]]
  def findArticlesByFollower(followerId: Int, pagination: Pagination): F[List[(WithId[Article], WithId[User])]]
  def countArticlesFilteredBy(filter: ArticleFilter): F[Int]
  def countArticlesForFollower(followerId: Int): F[Int]
  def createArticleWithTags(article: Article, tags: List[String]): F[((WithId[Article], WithId[User]), List[Tag])]
  def updateArticleBySlug(slug: String, authorId: Int, article: ArticleForUpdate): F[Option[(WithId[Article], WithId[User])]]
  def deleteArticleBySlug(slug: String, authorId: Int): F[Option[Unit]]
}

object ArticleRepo {

  def impl(xa: Transactor[IO]) = new ArticleRepo[IO] {
    def findArticleBySlug(slug: String): IO[Option[(WithId[Article], WithId[User])]] =
      Q.selectArticleBySlug(slug).option.transact(xa)

    def findArticlesFilteredBy(filter: ArticleFilter, pagination: Pagination): IO[List[(WithId[Article], WithId[User])]] =
      Q.selectArticlesFilteredBy(filter, pagination).to[List].transact(xa)

    def findArticlesByFollower(followerId: Int, pagination: Pagination): IO[List[(WithId[Article], WithId[User])]] =
      Q.selectArticlesByFollower(followerId, pagination).to[List].transact(xa)

    def countArticlesFilteredBy(filter: ArticleFilter): IO[Int] =
      Q.countArticlesFilteredBy(filter).unique.transact(xa)

    def countArticlesForFollower(followerId: Int): IO[Int] =
      Q.countArticlesForFollower(followerId).unique.transact(xa)

    def createArticleWithTags(article: Article, tags: List[String]): IO[((WithId[Article], WithId[User]), List[Tag])] = {
      val trx = for {
        id <- Q.insertArticle(article).withUniqueGeneratedKeys[Int]("id")
        tagsEntities = tags.distinct.map(Tag(id, _))
        _ <- Q.insertTags.updateMany(tagsEntities)
        article <- Q.selectArticleById(id).unique
      } yield (article, tagsEntities)

      trx.transact(xa)
    }

    def updateArticleBySlug(slug: String, authorId: Int, article: ArticleForUpdate): IO[Option[(WithId[Article], WithId[User])]] = {
      val trx = for {
        _ <- OptionT(Q.updateArticleBySlug(slug, authorId, article).run.map(affectedToOption))
        article <- OptionT(Q.selectArticleBySlug(article.slug.getOrElse(slug)).option)
      } yield article

      trx.value.transact(xa)
    }

    def deleteArticleBySlug(slug: String, authorId: Int): IO[Option[Unit]] =
      Q.deleteArticleBySlug(slug, authorId).run.map(affectedToOption).transact(xa)
  }

  object Q {
    def selectArticleById(id: Int) = {
      val q = articleJoinUser ++
        fr"""
          where a.id = $id
        """
      q.query[(WithId[Article], WithId[User])]
    }

    def selectArticleBySlug(slug: String) = {
      val q = articleJoinUser ++
        fr"""
          where a.slug = $slug
        """
      q.query[(WithId[Article], WithId[User])]
    }

    def selectArticlesFilteredBy(filter: ArticleFilter, pagination: Pagination) = {
      val q =
        articleJoinUser ++
        whereWithFilter(filter) ++
        recentWithPagination(pagination)

      q.query[(WithId[Article], WithId[User])]
    }

    def selectArticlesByFollower(followerId: Int, pagination: Pagination) = {
      val q = articleJoinUser ++
        articlesForFollower(followerId) ++
        recentWithPagination(pagination)

        q.query[(WithId[Article], WithId[User])]
    }

    def countArticlesFilteredBy(filter: ArticleFilter) = {
      val q =
        fr"""
          select count(1)
          from articles a
          inner join users u on a.author_id = u.id
        """ ++ whereWithFilter(filter)
      q.query[Int]
    }

    def countArticlesForFollower(followerId: Int) = {
      val q =
        fr"""
          select count(1)
          from articles a
        """ ++ articlesForFollower(followerId)
      q.query[Int]
    }

    def insertArticle(article: Article) =
      sql"""
         insert into articles(slug, title, description, body, author_id, created_at, updated_at)
         values(${article.slug}, ${article.title}, ${article.description}, ${article.body}, ${article.authorId}, ${article.createdAt}, ${article.updatedAt})
      """.update

    val insertTags = {
      val q = """
              insert into tags(article_id, tag)
              values(?, ?)
          """
      Update[Tag](q)
    }

    def updateArticleBySlug(slug: String, authorId: Int, article: ArticleForUpdate) =
      sql"""
         update articles a
         set slug = coalesce(${article.slug}, a.slug),
             title = coalesce(${article.title}, a.title),
             description = coalesce(${article.description}, a.description),
             body = coalesce(${article.body}, a.body),
             updated_at = ${article.updatedAt}
         where slug = $slug
               and a.author_id = $authorId
      """.update

    def deleteArticleBySlug(slug: String, authorId: Int) =
      sql"""
         delete from articles
         where slug = $slug
               and author_id = $authorId
      """.update

    val articleJoinUser =
      fr"""
        select a.id, a.slug, a.title, a.description, a.body, a.author_id, a.created_at, a.updated_at,
               u.id, u.email, u.username, u.password, u.bio, u.image, u.created_at, u.updated_at
        from articles a
        inner join users u on a.author_id = u.id
      """

    def whereWithFilter(filter: ArticleFilter) = {
      val tag = filter.tag.map(t => fr"a.id in (select distinct article_id from tags where tag = $t)")
      val author = filter.author.map(a => fr"u.username = $a")
      val favorited = filter.favorited.map(f => fr"a.id in (select distinct f.article_id from favorites f inner join users uu on f.user_id = uu.id where uu.username = $f)")

      whereAndOpt(tag, author, favorited)
    }

    def recentWithPagination(pagination: Pagination) =
      fr"""
        order by a.created_at desc
        limit ${pagination.limit} offset ${pagination.offset}
      """

    def articlesForFollower(followerId: Int) =
      fr"""
        where a.author_id in (select distinct followee_id from followers where follower_id = $followerId)
      """
  }
}
