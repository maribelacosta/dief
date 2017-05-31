#source('~/diefMetrics.R')

#' experiment1
#'
#' This function reproduces the results reported in Experiment 1.
#' @keywords dieft, diefficiency
#' @author Maribel Acosta
#' @export experiment1
#' @seealso experiment2, dieft
#' 
experiment1 <- function() {
  
  # Input data: Outcome of query execution.
  traces <- read.csv("data/nLDE-Benchmark1-AnswerTrace.csv")
  metrics <- read.csv("data/nLDE-Benchmark1-Metrics.csv")
  
  # Compute further metrics: throughput, inverse of execution time, inverse of time for the first tuple.
  metrics$throughput <- with(metrics, metrics$comp/metrics$totaltime)
  metrics$invtfft <- with(metrics, 1/metrics$tfft)
  metrics$invtotaltime <- with(metrics, 1/metrics$totaltime)
  
  # Obtain queries.
  queries <- unique(traces$query)
  
  # Compute dieft.
  dieftDF <- data.frame(query=character(), approach=character(), dieft=double(), stringsAsFactors=TRUE)
  for (q in queries) {
    print(c("Computing dieft for all approaches for query ", q))
    dieftDF <- rbind(dieftDF, dieft(traces, q))
  }
  
  # Merge conventional metrics and dieft into a single dataframe.
  allmetrics <- merge(metrics, dieftDF)
  
  return(allmetrics)
}


#' experiment2
#'
#' This function reproduces the results reported in Experiment 2.
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @import plyr
#' @export experiment2
#' @seealso experiment1, diefk2
#'
experiment2 <- function() {
  
  # Input data: Outcome of query execution.
  traces <- read.csv("data/nLDE-Benchmark1-AnswerTrace.csv")
  
  # Obtain queries.
  queries <- unique(traces$query)
  
  # Compute diefk for different k%: 25, 50, 75, 100.
  diefkDF <- data.frame(query=character(), approach=character(), "diefk25"=double(), "diefk50"=double(), "diefk75"=double(), "diefk100"=double())
  keeps <- c("diefk25", "diefk50", "diefk75", "diefk100")
  for (q in queries) {
     
      print(c("Computing diefk for all approaches for query ", q))
      
      k25DF <- diefk2(traces, q, 0.25)
      k25DF <- plyr::rename(k25DF, c("diefk"="diefk25"))
      
      k50DF <- diefk2(traces, q, 0.50)
      k50DF <- plyr::rename(k50DF, c("diefk"="diefk50"))
      
      k75DF <- diefk2(traces, q, 0.75)
      k75DF <- plyr::rename(k75DF, c("diefk"="diefk75"))
      
      k100DF <- diefk2(traces, q, 1.00)
      k100DF <- plyr::rename(k100DF, c("diefk"="diefk100"))
      
      
      x <- cbind(k25DF, k50DF, k75DF, k100DF)
      diefkDF <- rbind(diefkDF, x)
      
  }
  
  return(diefkDF)
  
}

