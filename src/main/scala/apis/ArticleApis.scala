package io.rw.app.apis

import cats.data.OptionT
import cats.implicits._
import cats.Monad
import io.rw.app.data.{Entities => E, _}
import io.rw.app.data.ApiErrors._
import io.rw.app.data.ApiInputs._
import io.rw.app.data.ApiOutputs._
import io.rw.app.repos._
import io.rw.app.utils._
import java.time.Instant
import io.rw.app.data

trait ArticleApis[F[_]] {
  def getAll(input: GetAllArticlesInput): F[ApiResult[GetAllArticlesOutput]]
  def getFeed(input: GetArticlesFeedInput): F[ApiResult[GetArticlesFeedOutput]]
  def get(input: GetArticleInput): F[ApiResult[GetArticleOutput]]
  def create(input: CreateArticleInput): F[ApiResult[CreateArticleOutput]]
  def update(input: UpdateArticleInput): F[ApiResult[UpdateArticleOutput]]
  def delete(input: DeleteArticleInput): F[ApiResult[DeleteArticleOutput]]
  def favorite(input: FavoriteArticleInput): F[ApiResult[FavoriteArticleOutput]]
  def unfavorite(input: UnfavoriteArticleInput): F[ApiResult[UnfavoriteArticleOutput]]
}

object ArticleApis {

  def impl[F[_] : Monad](articleRepo: ArticleRepo[F], followerRepo: FollowerRepo[F], tagRepo: TagRepo[F], favoriteRepo: FavoriteRepo[F], idHasher: IdHasher) = new ArticleApis[F] {

    def getAll(input: GetAllArticlesInput): F[ApiResult[GetAllArticlesOutput]] = {
      val articles = for {
        articlesWithAuthor <- articleRepo.findArticlesFilteredBy(input.filter, input.pagination)
        articleCounts <- articleRepo.countArticlesFilteredBy(input.filter)
        authorsIds = articlesWithAuthor.map(_._2.id)
        articlesIds = articlesWithAuthor.map(_._1.id)
        followers <- input.authUser.traverse(followerRepo.findFollowers(authorsIds, _)).map(_.getOrElse(List.empty))
        extras <- articlesExtras(input.authUser, articlesIds)
      } yield (mkArticles(articlesWithAuthor, followers, extras), articleCounts)

      articles.map(p => Right(GetAllArticlesOutput(p._1, p._2)))
    }

    def getFeed(input: GetArticlesFeedInput): F[ApiResult[GetArticlesFeedOutput]] = {
      val articles = for {
        articlesWithAuthor <- articleRepo.findArticlesByFollower(input.authUser, input.pagination)
        articleCounts <- articleRepo.countArticlesForFollower(input.authUser)
        authorsIds = articlesWithAuthor.map(_._2.id)
        articlesIds = articlesWithAuthor.map(_._1.id)
        followers <- Monad[F].pure(authorsIds.map(E.Follower(_, input.authUser)))
        extras <- articlesExtras(Some(input.authUser), articlesIds)
      } yield (mkArticles(articlesWithAuthor, followers, extras), articleCounts)

      articles.map(p => Right(GetArticlesFeedOutput(p._1, p._2)))
    }

    def get(input: GetArticleInput): F[ApiResult[GetArticleOutput]] = {
      val article = for {
        (articleWithId, author) <- OptionT(articleRepo.findArticleBySlug(input.slug))
        following <- OptionT.liftF(input.authUser.flatTraverse(followerRepo.findFollower(author.id, _)).map(_.nonEmpty))
        (tags, favorited, favoritesCount) <- OptionT.liftF(articleExtra(input.authUser, articleWithId.id))
      } yield mkArticle(articleWithId.entity, tags, favorited, favoritesCount, author.entity, following)

      article.value.map(_.map(GetArticleOutput).toRight(ArticleNotFound()))
    }

    def create(input: CreateArticleInput): F[ApiResult[CreateArticleOutput]] = {
      def mkArticleEntity(authorId: Int): E.Article = {
        val now = Instant.now
        E.Article(mkSlug(input.title, authorId, now), input.title, input.description, input.body, authorId, now, now)
      }

      val article = for {
        ((articleWithId, author), tags) <- articleRepo.createArticleWithTags(mkArticleEntity(input.authUser), input.tagList)
      } yield mkArticle(articleWithId.entity, tags, false, 0, author.entity, false)

      article.map(a => Right(CreateArticleOutput(a)))
    }

    def update(input: UpdateArticleInput): F[ApiResult[UpdateArticleOutput]] = {
      def mkArticleForUpdateEntity(authorId: Int): E.ArticleForUpdate = {
        val now = Instant.now
        E.ArticleForUpdate(input.title.map(mkSlug(_, authorId, now)), input.title, input.description, input.body, now)
      }

      val article = for {
        (articleWithId, author) <- OptionT(articleRepo.updateArticleBySlug(input.slug, input.authUser, mkArticleForUpdateEntity(input.authUser)))
        (tags, favorited, favoritesCount) <- OptionT.liftF(articleExtra(Some(input.authUser), articleWithId.id))
      } yield mkArticle(articleWithId.entity, tags, favorited, favoritesCount, author.entity, false)

      article.value.map(_.map(UpdateArticleOutput).toRight(ArticleNotFound()))
    }

    def delete(input: DeleteArticleInput): F[ApiResult[DeleteArticleOutput]] =
      articleRepo.deleteArticleBySlug(input.slug, input.authUser).map(_.map(_ => DeleteArticleOutput()).toRight(ArticleNotFound()))

    def favorite(input: FavoriteArticleInput): F[ApiResult[FavoriteArticleOutput]] = {
      val article = for {
        (articleWithId, author) <- OptionT(articleRepo.findArticleBySlug(input.slug))
        _ <- OptionT.liftF(favoriteRepo.createFavorite(E.Favorite(articleWithId.id, input.authUser)))
        following <- OptionT.liftF(followerRepo.findFollower(author.id, input.authUser).map(_.nonEmpty))
        // TODO article just favorited no need to select again
        (tags, favorited, favoritesCount) <- OptionT.liftF(articleExtra(Some(input.authUser), articleWithId.id))
      } yield mkArticle(articleWithId.entity, tags, favorited, favoritesCount, author.entity, following)

      article.value.map(_.map(FavoriteArticleOutput).toRight(ArticleNotFound()))
    }

    def unfavorite(input: UnfavoriteArticleInput): F[ApiResult[UnfavoriteArticleOutput]] = {
      val article = for {
        (articleWithId, author) <- OptionT(articleRepo.findArticleBySlug(input.slug))
        _ <- OptionT.liftF(favoriteRepo.deleteFavorite(articleWithId.id, input.authUser))
        following <- OptionT.liftF(followerRepo.findFollower(author.id, input.authUser).map(_.nonEmpty))
        // TODO article just unfavorited no need to select again
        (tags, favorited, favoritesCount) <- OptionT.liftF(articleExtra(Some(input.authUser), articleWithId.id))
      } yield mkArticle(articleWithId.entity, tags, favorited, favoritesCount, author.entity, following)

      article.value.map(_.map(UnfavoriteArticleOutput).toRight(ArticleNotFound()))
    }

    def articleExtra(authUser: Option[AuthUser], articleId: Int): F[(List[E.Tag], Boolean, Int)] =
      for {
        tags <- tagRepo.findTags(articleId)
        favorited <- authUser.flatTraverse(favoriteRepo.findFavorite(articleId, _)).map(_.nonEmpty)
        favoritesCount <- favoriteRepo.countFavorite(articleId)
      } yield (tags, favorited, favoritesCount)

    def articlesExtras(authUser: Option[AuthUser], articleIds: List[Int]): F[Map[Int, (List[E.Tag], Boolean, Int)]] = {
      def withExtra(id: Int, groupedTags: Map[Int, List[E.Tag]], groupedFavorites: Map[Int, List[E.Favorite]], groupedFavoriteCounts: Map[Int, Int]): (Int, (List[E.Tag], Boolean, Int)) = {
        val tags = groupedTags.getOrElse(id, List.empty)
        val favorite = groupedFavorites.get(id).nonEmpty
        val favoriteCounts = groupedFavoriteCounts.getOrElse(id, 0)
        id -> (tags, favorite, favoriteCounts)
      }

      val groupedExtras = for {
        tags <- tagRepo.findTags(articleIds)
        favorites <- authUser.traverse(favoriteRepo.findFavorites(articleIds, _)).map(_.getOrElse(List.empty))
        favoriteCounts <- favoriteRepo.countFavorites(articleIds)
      } yield (tags.groupBy(_.articleId), favorites.groupBy(_.articleId), favoriteCounts.toMap)

      groupedExtras.map { case (groupedTags,  groupedFavorites, groupedFavoriteCounts) =>
        articleIds.map(withExtra(_, groupedTags, groupedFavorites, groupedFavoriteCounts)).toMap
      }
    }

    // same author cannot create article with the same name at exact the same time, so no collisions should be here
    def mkSlug(s: String, authorId: Int, time: Instant): String =
      slugify(s) + "-" + idHasher.hash(authorId + time.getNano)
  }
}
