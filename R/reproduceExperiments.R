#source('~/diefMetrics.R')

#' experiment1
#'
#' This function reproduces the results reported in Experiment 1.
#' @param traces_file CSV file with the result of the traces. The structure of this file is as follows: "query,approach,tuple,time".
#' @param metrics_file CSV file with the result of the other metrics. The structure of this file is as follows: "query,approach,tfft,totaltime,comp".
#' @keywords dieft, diefficiency
#' @author Maribel Acosta
#' @export experiment1
#' @seealso experiment2, dieft
#' 
experiment1 <- function(traces_file, metrics_file) {
  
  # Input data: Outcome of query execution.
  traces <- read.csv(traces_file)
  metrics <- read.csv(metrics_file) 
  
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

#' plotExperiment1
#'
#' This function plots the results reported in Experiment 1.
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @import fmsb
#' @import ggplot2
#' @export plotExperiment1
#' @seealso experiment1, diefk2
#'
plotExperiment1 <- function(allmetrics) {
  
  # Obtain queries.
  queries <- unique(allmetrics$query)
  
  # Plot metrics using spider plot. 
  keeps <- c("invtfft", "invtotaltime", "comp", "throughput", "dieft")
  for (q in queries) {
    
    data <- subset(allmetrics, query==q) 
    data <- data[keeps]
    
    maxs <- data.frame(invtfft=max(data$invtfft), invtotaltime=max(data$invtotaltime), comp=max(data$comp), throughput=max(data$throughput), dieft=max(data$dieft))
    mins <- data.frame(invtfft=0, invtotaltime=0, comp=0, throughput=0, dieft=0)
    
    data <- rbind(maxs, mins ,data)
    
    colors_border=c("#2274A5","#F75C03","#4CB944")
    colors_in=alpha(colors_border, 0.15)
    
    radarchart( data, 
                pcol=colors_border , pfcol=colors_in, plwd=4 , plty=1,
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=1.0,
                vlcex=1.5,
                title=q,
                vlabels=c("(TFFF)^-1", "(ET)^-1", "Comp", "T", "dief@t"))
    
    legend(x=0.7, y=1, legend = c("NA", "Ran", "Sel"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1.7, pt.cex=3)  
  }
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
experiment2 <- function(tracespath) {
  
  # Input data: Outcome of query execution.
  traces <- read.csv(tracespath)
  
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

