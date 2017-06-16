# Recommender Split

setClass("RecommenderSplit",
         slots = c(training = "Matrix",
                   validation_known = "Matrix",
                   validation_unknown = "Matrix"))

# Recommender Classes

setClass("VirtualRecommender",
         slots = c(n_threads = "integer"))

setClass("UBCFRecommender", contains = "VirtualRecommender",
         slots = c(train = "Matrix",
                   nn = "integer"))

setClass("ALSRecommender", contains = "VirtualRecommender",
         slots = c(item_factors = "matrix",
                   user_factors = "matrix",
                   loss = "numeric",
                   IIt = "matrix",
                   B = "numeric",
                   K1 = "numeric",
                   len = "numeric"))

setClass("PopularRecommender", contains = "VirtualRecommender",
         slots = c(popularity = "numeric"))

setClass("RandomRecommender", contains = "VirtualRecommender",
         slots = c(seed = "numeric"))

setClass("ContentRecommender", contains = "VirtualRecommender",
         slots = c(tfidf = "Matrix"))

setClass("HybridRecommender", contains = "VirtualRecommender",
         slots = c(recommenders = "list",
                   weights = "numeric"))
