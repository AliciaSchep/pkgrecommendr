'%ni%' = Negate('%in%')

#' @rdname splits
#' @export
test_train_split <- function(x, train = 0.8, given = -1L){

  stopifnot(given != 0)

  train_ix <- sample(nrow(x), train * nrow(x))

  out <- list()
  out$train <- x[train_ix,]

  test_ix <- setdiff(seq_len(nrow(x)),train_ix)

  test <- x[test_ix,]

  blank_ix <- as.integer(apply(test, 1, function(x) sample(which(x != 0), abs(given))))
  if (given < 0){
    out$test_rest <- sparseMatrix(i = rep(seq_len(nrow(test)), each = abs(given)), j = blank_ix,
                              x = 1, dims = dim(test))
    rest_trip <- summary(out$test_rest)
    test_trip <- summary(test)
    sub_trip <- dplyr::anti_join(test_trip, rest_trip, by = c('i','j'))
    out$test_sub <- sparseMatrix(i = sub_trip$i, j = sub_trip$j,
                                  x = 1, dims = dim(test))
  } else{
    out$test_sub <- sparseMatrix(i = rep(seq_len(nrow(test)), each = abs(given)),
                                 j = blank_ix,
                                  x = 1, dims = dim(test))
    sub_trip <- summary(out$test_sub)
    test_trip <- summary(test)
    rest_trip <- dplyr::anti_join(test_trip, rest_trip, by = c('i','j'))
    out$test_rest <- sparseMatrix(i = rest_trip$i, j = rest_trip$j,
                                 x = 1, dims = dim(test))
  }

  return(out)
}


#' Split matrix
#'
#' @param x
#' @param fold
#' @param given
#'
#' @return
#' @export
#' @rdname splits
#'
#' @examples
cv_splits <- function(x, fold = 5, given = -1){
  map(seq_len(fold), function(i) test_train_split(x, train = 1/fold, given = given))
}

