% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/als.R
\name{als_recommender}
\alias{als_recommender}
\title{Alternating least squares implicit recommendation}
\usage{
als_recommender(train, rank = 100, lambda = 0.1, B = 0.5, K1 = 1.25,
  n_iter = 6, n_threads = min(1, parallel::detectCores() - 1L))
}
\arguments{
\item{n_threads}{}
}
\description{
Alternating least squares implicit recommendation
}
