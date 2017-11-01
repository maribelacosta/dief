#' experiment1
#'
#' This function reproduces the results reported in "Experiment 1" in Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>.
#' @param traces_file CSV file with the result of the traces. The structure of this file is as follows: "test,approach,tuple,time".
#' @param metrics_file CSV file with the result of the other metrics. The structure of this file is as follows: "test,approach,tfft,totaltime,comp".
#' @keywords dieft, diefficiency
#' @author Maribel Acosta
#' @import utils
#' @export experiment1
#' @seealso experiment2, dieft
#' 
experiment1 <- function(traces_file, metrics_file) {
  
  # Input data: Outcome of test execution.
  traces <- read.csv(traces_file)
  metrics <- read.csv(metrics_file) 
  
  # Compute further metrics: throughput, inverse of execution time, inverse of time for the first tuple.
  metrics$throughput <- with(metrics, metrics$comp/metrics$totaltime)
  metrics$invtfft <- with(metrics, 1/metrics$tfft)
  metrics$invtotaltime <- with(metrics, 1/metrics$totaltime)
  
  # Obtain queries.
  queries <- unique(traces$test)
  
  # Compute dieft.
  dieftDF <- data.frame(test=character(), approach=character(), dieft=double(), stringsAsFactors=TRUE)
  for (q in queries) {
    print(c("Computing dieft for all approaches for test ", q))
    dieftDF <- rbind(dieftDF, dieft(traces, q))
  }
  
  # Merge conventional metrics and dieft into a single dataframe.
  allmetrics <- merge(metrics, dieftDF)
  
  
  return(allmetrics)
}

#' plotExperiment1Test
#'
#' This function plots the results reported for a single given test in "Experiment 1" in in Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>..
#' @keywords dieft, diefficiency
#' @author Maribel Acosta
#' @param  allmetrics dataframe with the results of all the metrics in Experiment 1. 
#' @param  q the id of the selected test to plot. 
#' @import graphics
#' @export plotExperiment1Test
#' @seealso experiment1, plotExperiment1
#'
plotExperiment1Test <- function(allmetrics, q) {
  
  # Plot metrics using spider plot. 
  test <- NULL
  keeps <- c("invtfft", "invtotaltime", "comp", "throughput", "dieft") 
  
  data <- subset(allmetrics, test==q) 
  data <- data[keeps]
  
  maxs <- data.frame(invtfft=max(data$invtfft), invtotaltime=max(data$invtotaltime), comp=max(data$comp), throughput=max(data$throughput), dieft=max(data$dieft))
  mins <- data.frame(invtfft=0, invtotaltime=0, comp=0, throughput=0, dieft=0)
  
  data <- rbind(maxs, mins ,data)
  
  colors_border=c("#ECC30B","#D56062","#84BCDA")
  colors_in=alpha(colors_border, 0.15)
  
  radarchart( data, 
              pcol=colors_border , pfcol=colors_in, plwd=4 , plty=1,
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=1.0,
              vlcex=1.5,
              title=q,
              vlabels=c("(TFFF)^-1", "(ET)^-1", "Comp", "T", "dief@t"))
  
  legend(x=0.7, y=1.3, legend = c("NA", "Ran", "Sel"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1.3, pt.cex=2.5)  
  
}

#' plotExperiment1
#'
#' This function plots the results reported in Experiment 1 in Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>.
#' @param  allmetrics dataframe with the result of all the metrics in Experiment 1. 
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @import fmsb
#' @import ggplot2
#' @export plotExperiment1
#' @seealso experiment1, diefk2
#'
plotExperiment1 <- function(allmetrics) {
  
  # Obtain queries.
  queries <- unique(allmetrics$test)
  
  # Plot the metrics for each test in Experiment 1.  
  for (q in queries) {
    plotExperiment1Test(allmetrics, q)  
  }
}  

#' experiment2
#'
#' This function reproduces the results reported in Experiment 2 in Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>.
#' @param traces_file CSV file with the result of the traces. The structure of this file is as follows: "test,approach,tuple,time".
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @import plyr
#' @import utils
#' @export experiment2
#' @seealso experiment1, diefk2
#'
experiment2 <- function(traces_file) {
  
  # Input data: Outcome of test execution.
  traces <- read.csv(traces_file)
  
  # Obtain queries.
  queries <- unique(traces$test)
  
  # Compute diefk for different k%: 25, 50, 75, 100.
  diefkDF <- data.frame(test=character(), approach=character(), "diefk25"=double(), "diefk50"=double(), "diefk75"=double(), "diefk100"=double())
  keeps <- c("diefk25", "diefk50", "diefk75", "diefk100")
  for (q in queries) {
     
      print(c("Computing diefk for all approaches for test ", q))
      
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

#' plotExperiment2Test
#'
#' This function plots the results reported for a single given test in "Experiment 2" in Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>..
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @param  diefkDF dataframe resulting from Experiment 2.
#' @param  q the id of the selected test to plot. 
#' @import fmsb
#' @import ggplot2
#' @import graphics
#' @export plotExperiment2Test
#' @seealso experiment2, plotExperiment2
#'
plotExperiment2Test <- function(diefkDF, q) {
  
  # Plot metrics using spider plot.
  test <- NULL
  keeps <- c("diefk25", "diefk50", "diefk75", "diefk100")
  
  x <- subset(diefkDF, test==q)
  x <- x[keeps]
  
  maxs <- data.frame(diefk25=max(x$diefk25), diefk50=max(x$diefk50), diefk75=max(x$diefk75), diefk100=max(x$diefk100))
  mins <- data.frame(diefk25=0, diefk50=0,  diefk75=0, diefk100=0)
  
  data <- rbind(mins, maxs, x)
  
  colors_border=c("#ECC30B","#D56062","#84BCDA")
  colors_in=alpha(colors_border, 0.15)
  radarchart( data, 
              pcol=colors_border , pfcol=colors_in, plwd=4 , plty=1,
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=1.0,
              vlcex=1.5,
              title=q,
              vlabels=c("k=25%", "k=50%", "k=75%", "k=100%"))
  legend(x=0.7, y=1.3, legend = c("NA", "Ran", "Sel"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1.3, pt.cex=2.5)

}

#' plotExperiment2
#'
#' This function plots the results reported in Experiment 2 in Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>.
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @param  diefkDF dataframe with the results of Experiment 2. 
#' @export plotExperiment2
#' @seealso experiment2, diefk2
#'
plotExperiment2 <- function(diefkDF) {
  
  # Obtain queries.
  queries <- unique(diefkDF$test)
  
  # Plot the metrics for each test in Experiment 12.  
  for (q in queries) {
    plotExperiment2Test(diefkDF, q)  
  }
}  

