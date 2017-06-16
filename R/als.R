bm25_weight <- function(X, lens = colSums(X), K1 = 1, B = 0.8){
  idf <- log(ncol(X) / (1 + rowSums(X)))
  ave_len <- mean(lens)
  len_norm <- (1 - B) + B * lens / ave_len
  xtrip <- summary(X)
  xtrip$x <- xtrip$x * (K1 + 1) / (K1 * len_norm[xtrip$j] + xtrip$x) * idf[xtrip$i]
  out <- sparseMatrix(i = xtrip$i, j = xtrip$j, x = xtrip$x, dims = dim(X))
}

#' Alternating least squares implicit recommendation
#'
#' @param train
#' @param rank
#' @param lambda
#' @param B
#' @param K1
#' @param n_iter
#' @param n_threads
#'
#' @return
#' @export
#'
#' @examples
als_recommender <- function(train, rank = 100, lambda = 0.1, B = 0.5, K1 = 1.25, n_iter = 6,
                            n_threads = min(1,parallel::detectCores() - 1L)){

  fit <- als_fit(bm25_weight(train, B = B, K1 = K1), rank = rank, lambda = lambda, n_iter = n_iter,
                 n_threads = n_threads)

  new("ALSRecommender",
      item_factors = fit$item_factors,
      user_factors = fit$user_factors,
      loss = fit$loss,
      IIt = fit$IIt,
      n_threads = as.integer(n_threads),
      B = B,
      K1 = K1,
      len = colSums(train))
}

# Adapted from reco package, by Dmitriy Selivanov
als_fit <- function(c_ui, rank, lambda, n_iter, n_threads,
                    init_stdv = 0.01, non_negative = FALSE){

  stopifnot(all(c_ui@x >= 0))

  c_iu <- t(c_ui)

  # init
  n_user <- nrow(c_ui)
  n_item <- ncol(c_ui)

  U <- matrix(rnorm(n_user * rank, 0, init_stdv), ncol = n_user, nrow = rank)
  I <- matrix(rnorm(n_item * rank, 0, init_stdv), ncol = n_item, nrow = rank)
  Lambda = diag(x = lambda, nrow = rank, ncol = rank)

  trace_values = vector("numeric", n_iter)

  trace_lst = vector("numeric", n_iter)
  # iterate
  for (i in seq_len(n_iter)) {

    IIt = tcrossprod(I) + Lambda

    stopifnot(ncol(U) == ncol(c_iu))
    # U will be modified in place
    als_implicit(c_iu, I, IIt, U, n_threads = n_threads)

    # if need non-negative matrix factorization - just set all negative values to zero
    if(non_negative) U[U < 0] = 0

    UUt = tcrossprod(U) + Lambda
    stopifnot(ncol(I) == ncol(c_ui))

    # private$I will be modified in place
    als_implicit(c_ui, U, UUt, I, n_threads = n_threads)

    # if need non-negative matrix factorization - just set all negative values to zero
    if(non_negative) I[I < 0] = 0


    # calculate loss
    loss <- als_loss(c_ui, U, I, lambda, n_threads);
    trace_lst[i] <- loss
  }

  out <- list(item_factors = I, user_factors = U, loss = trace_lst, IIt = IIt)
  out
}

als_transform <- function(x, I, IIt, n_threads){
  stopifnot(ncol(x) == ncol(I))
  # allocate result matrix - will be modified in place
  res <- matrix(0, nrow = nrow(I), ncol = nrow(x))
  als_implicit(t(x), I, IIt, res, n_threads = n_threads)
  t(res)
}


setMethod(recommend_ratings, c(recommender = "ALSRecommender", test = "Matrix"),
          function(recommender, test){
            als_transform(bm25_weight(test,
                                      lens = recommender@len,
                                      B = recommender@B,
                                      K1 = recommender@K1),
                          recommender@item_factors,
                          recommender@IIt,
                          recommender@n_threads) %*%
              recommender@item_factors
          })
