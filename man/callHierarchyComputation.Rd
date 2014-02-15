\name{callHierarchyComputation}
\alias{callHierarchyComputation}
\title{Calls the C++ hierarchy computation.}
\usage{
callHierarchyComputation(cohesion_matrix, list.selected.item,
  list.occurrences.variables, verbose)
}
\arguments{
  \item{cohesion_matrix}{cohesion matrix of the variables}

  \item{list.selected.item}{subset of variables to apply
  the computation to.}

  \item{list.occurrences.variables}{list of the occurrences
  of the variables.}

  \item{verbose}{gives more details}
}
\description{
Interface to call the the C++ hierarchy computation.
}
\author{
Rapha\"{e}l Couturier
\email{raphael.couturier@univ-fcomte.fr}
}

