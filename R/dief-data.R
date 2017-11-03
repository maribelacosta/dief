#' Example of answer traces 
#'
#' A dataset containing answer traces of executing three approaches. 
#' The variables are as follows:
#'
#' \itemize{
#'  \item test: id of the test (in this case a SPARQL query) executed. Example: 'Q9.sparql'.
#'  \item approach: name of the approach (or engine) used to execute the query.
#'  \item answer: the value i indicates that this row corresponds to the ith answer produced by approach when executing query.
#'  \item time: elapsed time (in seconds) since approach started the execution of query until the answer i is produced.
#' }
#'
#' @docType data
#' @keywords traces datasets
#' @name traces
#' @usage data(traces)
#' @format A data frame with 1543 rows and 4 variables
#' @source \href{https://doi.org/10.6084/m9.figshare.5008289}{nLDE SPARQL engine: computing diefficiency metrics based on answer traces and query processing performance benchmarking}
NULL

#' Example of benchmarking performance with other metrics 
#'
#' A dataset with the results of measuring the performance of three approaches with four metrics. 
#' The variables are as follows:
#'
#' \itemize{
#'  \item test: id of the test (in this case a SPARQL query) executed. Example: 'Q9.sparql'.
#'  \item approach: name of the approach (or engine) used to execute the query.
#'  \item tfft: time (in seconds) required by approach to produce the first tuple when executing query.
#'  \item totaltime: elapsed time (in seconds) since approach started the execution of query until the answer i is produced.
#'  \item comp: number of answers produced by approach when executing query.
#' }
#'
#' @docType data
#' @keywords metrics datasets
#' @name metrics
#' @usage data(metrics)
#' @format A data frame with 3 rows and 5 variables
#' @source \href{https://doi.org/10.6084/m9.figshare.5008289}{nLDE SPARQL engine: computing diefficiency metrics based on answer traces and query processing performance benchmarking}
NULL