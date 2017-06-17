#' @describeIn pkgrecommendr-methods recommend_items
#' @export
setMethod(recommend_items, c(recommender = "VirtualRecommender", test = "Matrix"),
          function(recommender, test, n = 10){
            ratings <- recommend_ratings(recommender, test)
            top_k_indices_byrow(ratings, test, n, recommender@n_threads)
          })

#' @describeIn pkgrecommendr-methods get true postive rate for recommendations
#' @export
setMethod(recommend_tpr, c(recommender = "VirtualRecommender", test = "Matrix"),
          function(recommender, test, holdout, n = 10){
            recs <- recommend_items(recommender, test, n)
            rec_df <- inner_join(data_frame(i = rep(seq_len(nrow(test)), times = n),
                                 j = as.integer(recs),
                                 y = rep(seq_len(n), each = nrow(test))),
                                 summary(holdout), by = c("i","j"))
            rec_mat <- sparseMatrix(i = rec_df$i, j = rec_df$y, x = 1,
                                    dims = c(nrow(test),n))
            rowSums(apply(rec_mat, 1, cumsum) / matrix(rowSums(holdout),
                                                       ncol = nrow(holdout),
                                                       nrow = n,
                                                       byrow = TRUE)) / nrow(test)
          })
