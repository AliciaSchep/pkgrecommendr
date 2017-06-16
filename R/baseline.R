#' Popular Recommender
#'
#' @param train
#' @param n_threads
#'
#' @return
#' @export
popular_recommender <- function(train,
                                n_threads = max(1,parallel::detectCores() - 1)){

  popularity <- colSums(train)

  new("PopularRecommender",
      n_threads = as.integer(n_threads),
      popularity = popularity)
}

#' @describeIn pkgrecommendr-methods recommend_ratings method for PopularRecommender
#' @export
setMethod(recommend_ratings, c(recommender = "PopularRecommender", test = "Matrix"),
          function(recommender, test){
            matrix(recommender@popularity / max(recommender@popularity),
                   nrow = nrow(test),
                   ncol = ncol(test),
                   byrow = TRUE)
          })


#' Random Recommender
#'
#' @param train
#' @param n_threads
#'
#' @return
#' @export
random_recommender <- function(train, seed = round(runif(1)*1000),
                                n_threads = max(1,parallel::detectCores() - 1)){

  popularity <- colSums(train)

  new("RandomRecommender",
      n_threads = as.integer(n_threads),
      seed = seed)
}


#' @describeIn pkgrecommendr-methods recommend_ratings method for RandomRecommender
#' @export
setMethod(recommend_ratings, c(recommender = "RandomRecommender", test = "Matrix"),
          function(recommender, test){
            set.seed(recommender@seed)
            matrix(rnorm(dim(test)[1]*dim(test)[2]), nrow = nrow(test),
                   ncol = ncol(test), byrow = TRUE)
          })

