package io.rw.app

import io.rw.app.data.{Entities => E, _}

package object apis {

  def mkUser(user: E.User, token: String): User =
    User(user.email, token, user.username, user.bio, user.image)

  def mkProfile(user: E.User, following: Boolean): Profile =
    Profile(user.username, user.bio, user.image, following)

  def mkArticle(article: E.Article, tags: List[E.Tag], favorited: Boolean, favoritesCount: Int, user: E.User, following: Boolean): Article =
    Article(article.slug, article.title, article.description, article.body, tags.map(mkTag), article.createdAt, article.updatedAt, favorited, favoritesCount, mkProfile(user, following))

  def mkArticles(articleWithAuthors: List[(E.WithId[E.Article], E.WithId[E.User])], followers: List[E.Follower], extras: Map[Int, (List[E.Tag], Boolean, Int)]): List[Article] = {
    val groupedFollowers = followers.groupBy(_.followeeId)

    def withExtras(article: E.WithId[E.Article], author: E.WithId[E.User]): Article = {
      val following = groupedFollowers.get(article.entity.authorId).nonEmpty
      val (tags, favorited, favoritesCount) = extras.getOrElse(article.id, (List.empty, false, 0))
      mkArticle(article.entity, tags, favorited, favoritesCount, author.entity, following)
    }

    articleWithAuthors.map({case (article, author) => withExtras(article, author)})
  }

  def mkComment(comment: E.WithId[E.Comment], author: E.User, following: Boolean): Comment =
    Comment(comment.id, comment.entity.createdAt, comment.entity.updatedAt, comment.entity.body, mkProfile(author, following))

  def mkComments(commentsWithAuthors: List[(E.WithId[E.Comment], E.WithId[E.User])], followers: List[E.Follower]): List[Comment] = {
    val groupedFollowers = followers.groupBy(_.followeeId)

    def withFollowing(comment: E.WithId[E.Comment], author: E.WithId[E.User]): Option[Comment] =
      for {
        following <- Some(groupedFollowers.get(author.id).nonEmpty)
      } yield mkComment(comment, author.entity, following)

    commentsWithAuthors.map({case (comment, author) => withFollowing(comment, author)}).collect { case Some(c) => c }
  }

  def mkTag(tag: E.Tag): String =
    tag.tag
}
