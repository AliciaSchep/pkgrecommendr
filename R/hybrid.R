#Need to add checks for compatibility

#' Hybrid Recommender
#'
#' @param train
#' @param n_threads
#'
#' @return
#' @export
hybrid_recommender <- function(..., weights = NULL,
                               n_threads = max(1,parallel::detectCores() - 1)){

  recommenders <- list(...)
  if (is.null(weights)) weights <- rep(1, length(recommenders))
  stopifnot(length(weights) == length(recommenders))

  new("HybridRecommender",
      n_threads = as.integer(n_threads),
      recommenders = recommenders,
      weights = weights)
}

#' @describeIn pkgrecommendr-methods recommend_ratings method for HybridRecommender
#' @export
setMethod(recommend_ratings, c(recommender = "HybridRecommender", test = "Matrix"),
          function(recommender, test){
            ratings <- purrr::map(recommonders, recommend_ratings, test)
            weighted <- purrr:map2(weights, ratings, `*`)
            purrr:reduce(weighted,`+`) / sum(weights)
          })
