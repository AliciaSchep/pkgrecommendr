## Train-Test Split


## UBCF Recommender

# get_jaccard <- function(x, y = x) {
#   u <- tcrossprod(x,y)
#   t1 <- rowSums(x)
#   t2 <- rowSums(y)
#   i <- outer(t1, t2, FUN = "+")
#   return(t(as.matrix(u/(i - u))))
# }
#
#
#
# ubcf_recommender <- function(train, nn = 100,
#                              n_threads = max(1,parallel::detectCores() - 1)){
#
#   predict <- function(test){
#     sim <- get_jaccard(train, test)
#     neighbors <- get_nn(sim, nn, n_threads)
#
#     tmp <- Matrix::sparseMatrix(i = as.integer(neighbors),
#                         j = rep(seq_len(nrow(test)), times = nn),
#                         x = as.numeric(attr(neighbors,"score")),
#                         dims = c(nrow(train), nrow(test)))
#     as.matrix(t(t(train) %*% tmp)) / rowSums(attr(neighbors,"score"))
#   }
#
#   recommend <- function(test, n = 10){
#     ratings <- predict(test)
#     reco:::top_k_indices_byrow(ratings, test, 10, n_threads)
#   }
#
#   tp <- function(test, holdout, n = 10){
#     recs <- recommend(test, n)
#     sum(purrr::map_dbl(seq_len(nrow(recs)),
#                        function(x) sum(holdout[x,recs[x,]])/sum(holdout[x,]))) / nrow(recs)
#   }
#
#   out <- list(predict = predict, recommend = recommend, tp = tp)
# }

# popular_recommender <- function(train,
#                                 n_threads = max(1,parallel::detectCores() - 1)){
#
#   popularity <- colSums(train)
#   poprank <- sort
#
#   predict <- function(test){
#     matrix(popularity / max(popularity), nrow = nrow(test),
#            ncol = ncol(test), byrow = TRUE)
#   }
#
#   recommend <- function(test, n = 10){
#     ratings <- predict(test)
#     reco:::top_k_indices_byrow(ratings, test, 10, n_threads)
#   }
#
#   tp <- function(test, holdout, n = 10){
#     recs <- recommend(test, n)
#     purrr::map_dbl(seq_len(nrow(recs)), function(x) sum(holdout[x,recs[x,]])/sum(holdout[x,]))
#   }
#
#   out <- list(predict = predict, recommend = recommend, tp = tp)
# }
#
# random_recommender <- function(train, seed = round(runif(1)*1000),
#                               n_threads = max(1,parallel::detectCores() - 1)){
#
#
#   predict <- function(test){
#     set.seed(seed)
#     matrix(rnorm(dim(test)[1]*dim(test)[2]), nrow = nrow(test),
#            ncol = ncol(test), byrow = TRUE)
#   }
#
#   recommend <- function(test, n = 10){
#     ratings <- predict(test)
#     reco:::top_k_indices_byrow(ratings, test, 10, n_threads)
#   }
#
#   tp <- function(test, holdout, n = 10){
#     recs <- recommend(test, n)
#     sum(purrr::map_dbl(seq_len(nrow(recs)),
#                        function(x) sum(holdout[x,recs[x,]])/sum(holdout[x,]))) / nrow(recs)
#   }
#
#   out <- list(predict = predict, recommend = recommend, tp = tp)
# }


## TF-IDF Content
# tfidf_recommender <- function(pkg_idf){
#
#
# }
#
#
#
## ALS Recommender
# als_recommender <- function(train, rank = 100, lambda = 0.1, B = 0.5, K1 = 1.25, n_iter = 6,
#                             n_threads = min(1,parallel::detectCores() - 1)){
#
#   model <- ALS$new(rank = rank, lambda = lambda, n_threads = n_threads)
#   model$fit(bm25_weight(train, B = B, K1 = K1), n_iter = n_iter)
#
#   predict <- function(test){
#     model$transform(bm25_weight(test, lens = colSums(train), B = B, K1 = K1)) %*%
#       model$components
#   }
#
#   recommend <- function(test, n = 10){
#     model$predict(bm25_weight(test, lens = colSums(train), B = B, K1 = K1), n)
#   }
#
#   tp <- function(test, holdout, n = 10){
#     recs <- recommend(test, n)
#     sum(purrr::map_dbl(seq_len(nrow(recs)),
#                        function(x) sum(holdout[x,recs[x,]])/sum(holdout[x,]))) / nrow(recs)
#   }
#
#   out <- list(predict = predict, recommend = recommend, tp = tp)
#
# }


## Hybrid Recommender

