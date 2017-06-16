
#' Recommender methods
#'
#' @param recommender VirtualRecommender object
#' @param test test matrix
#' @param holdout
#' @param ...
#'
#' @return
#' @export
#' @rdname pkgrecommendr-methods
setGeneric("recommend_ratings", function(recommender, test, ...) standardGeneric("recommend_ratings"))

setGeneric("recommend_items", function(recommender,test, holdout, ...) standardGeneric("recommend_items"))

setGeneric("recommend_tpr", function(recommender,test, holdout, ...) standardGeneric("recommend_tpr"))
