#' @docType rchic
#' @name rchic
#' @author Raphaël Couturier
#' @title Cohesitive Hierarchical Implication Classification for R: Rchic
#' @description This package is intented to implement all the features of 
#'  Statistical Implicative Analysis (SIA). This theory aims at building association
#'  rules of the form : if an object has the property A it also tends to have
#'  the property B. The SIA builds confidence of rules by computing the number
#'  of counter examples of such an rule and then by measuring the surprise
#'  (or the probability) to have so few counter examples compared to the number
#'  of counter examples we would have with two random variables have the same
#'  sizes (than A and B). With Rchic you can build a similirity tree (using
#'  similarity analysis), an implicative graph and a hierarchical tree.
#' 
#' 
#' @details In order to have more information on SIA, interested readers may consult 
#'  the following book: Gras, R., Suzuki, E., Guillet, F., & Spagnolo, F. (Eds.). (2008). Statistical implicative analysis: Theory and applications. Springer.
NULL