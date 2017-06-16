## UBCF Recommender

get_jaccard <- function(x, y = x) {
  u <- tcrossprod(x,y)
  t1 <- rowSums(x)
  t2 <- rowSums(y)
  i <- outer(t1, t2, FUN = "+")
  return(t(as.matrix(u/(i - u))))
}


#' User Based Collaborative Filtering Recommender
#'
#' @param train
#' @param nn
#' @param n_threads
#'
#' @return
#' @export
ubcf_recommender <- function(train, nn = 100L,
                             n_threads = max(1,parallel::detectCores() - 1)){
 new("UBCFRecommender",
     n_threads = as.integer(n_threads),
     nn = as.integer(nn),
     train = train)
}

#' @describeIn pkgrecommendr-methods recommend_ratings method for UBCF
#' @export
setMethod(recommend_ratings, c(recommender = "UBCFRecommender", test = "Matrix"),
          function(recommender, test){
            train <- recommender@train
            nn <- recommender@nn
            sim <- get_jaccard(train, test)
            neighbors <- get_nn(sim, nn, recommender@n_threads)

            tmp <- Matrix::sparseMatrix(i = as.integer(neighbors),
                                        j = rep(seq_len(nrow(test)), times = nn),
                                        x = as.numeric(attr(neighbors,"score")),
                                        dims = c(nrow(train), nrow(test)))

            as.matrix(t(t(train) %*% tmp)) / rowSums(attr(neighbors,"score"))
          })


