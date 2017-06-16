#' Content Recommender
#'
#' Recommend based on TF-IDF & cosine similarity
#' @param tfidf sparse Matrix of TF-IDF
#' @param n_threads number of threads to use
#'
#' @return ContentRecommender
content_recommender <- function(tfidf,
                                n_threads = max(1,parallel::detectCores() - 1)){

  new("ContentRecommender",
      n_threads = as.integer(n_threads),
      tfidf = tfidf)
}

cosine_similarity <- function(x, y = x){
  x%*%t(y)/(sqrt(rowSums(x^2) %*% t(rowSums(y^2))))
}

#' @describeIn pkgrecommendr-methods recommend_ratings method for ContentRecommender
#' @export
setMethod(recommend_ratings, c(recommender = "ContentRecommender", test = "Matrix"),
          function(recommender, test){
            agg <- test %*% recommender@tfidf
            as.matrix(t(cosine_similarity(recommender@tfidf, agg)))
          })
