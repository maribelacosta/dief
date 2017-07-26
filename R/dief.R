#' dieft
#'
#' This function computes the dief@t metric.
#' @param inputtrace Dataframe with the answer trace. Attributes of the dataframe: query, approach, tuple, time.
#' @param inputquery String that specifies the specific query to analyze from the answer trace.
#' @param t to compute dieft. By default, the function computes the minimum of the execution time among the approaches in the answer trace.
#' @keywords dieft, diefficiency
#' @author Maribel Acosta
#' @import flux
#' @export dieft
#' @seealso diefk, diefk2, plotAnswerTrace
dieft <- function(inputtrace, inputquery, t=-1) {
  
  # Initialize output structure.
  df <- data.frame(query=character(), approach=character(), dieft=double(), stringsAsFactors=TRUE)
  
  # Obtain query and approaches to compare.
  results <- subset(inputtrace, query==inputquery)
  approaches <- unique(results$approach)
  
  # Obtain t per approach. 
  if (t==-1) {
    n <- c()
    for (a in approaches) {
      x <- subset(results, approach==a)
      n <- c(n, x[nrow(x),]$time)
    }
    t<- min(n)
  }
  
  # Compute dieft per approach.
  for (a in approaches) {
    subtrace <- subset(results, approach==a & time<=t)  
    dieft <- 0
    if (nrow(subtrace) > 1) {
      dieft <- flux::auc(subtrace$time, subtrace$tuple)
    }
    df <- rbind(df, data.frame("query"=inputquery, "approach"=a, "dieft"=dieft))
  }
  
  return(df)
  
}

#' diefk
#'
#' This function computes the dief@k metric at a given k (number of answers).
#' @param inputtrace Dataframe with the answer trace. Attributes of the dataframe: query, approach, tuple, time.
#' @param inputquery String that specifies the specific query to analyze from the answer trace.
#' @param k to compute diefk. By default, the function computes the minimum of the total number of answers produced by the approaches.
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @import flux
#' @export diefk
#' @seealso dieft, diefk2, plotAnswerTrace
diefk <- function(inputtrace, inputquery, k=-1) {
  
  # Initialize output structure.
  df <- data.frame(query=character(), approach=character(), diefk=double(), stringsAsFactors=TRUE)
  
  # Obtain query and approaches to compare.
  results <- subset(inputtrace, query==inputquery)
  approaches <- unique(results$approach)
  
  # Obtain k per approach. 
  if (k==-1) {
    n <- c()
    for (a in approaches) {
      x <- subset(results, approach==a)
      n <- c(n, nrow(x))
    }
    k<- min(n)
  }
  
  # Compute diefk per approach.
  for (a in approaches) {
    subtrace <- subset(results, approach==a & tuple<=k)  
    diefk <- 0
    if (nrow(subtrace) > 1) {
      diefk <- auc(subtrace$time, subtrace$tuple)
    }
    df <- rbind(df, data.frame("query"=inputquery, "approach"=a, "diefk"=diefk))
  }
  
  return(df)
  
}

#' diefk2
#'
#' This function computes the dief@k metric at a given kp (percentage of answers).
#' @param inputtrace Dataframe with the answer trace. Attributes of the dataframe: query, approach, tuple, time.
#' @param inputquery String that specifies the specific query to analyze from the answer trace.
#' @param kp to compute diefk. By default and when kp=1.0, this function behaves the same as diefk. It computes the kp portion of of minimum of of number of answers  produced by the approaches.
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @export diefk2
#' @seealso dieft, diefk, plotAnswerTrace
#' 
diefk2 <- function(inputtrace, inputquery, kp=-1) {
  
  # Initialize output structure.
  df <- data.frame(query=character(), approach=character(), diefk=double(), stringsAsFactors=TRUE)
  
  # Obtain query and approaches to compare.
  results <- subset(inputtrace, query==inputquery)
  approaches <- unique(results$approach)
  
  # Obtain k per approach. 
  n <- c()
  for (a in approaches) {
    x <- subset(results, approach==a)
    n <- c(n, nrow(x))
  }
  k <- min(n) 
  if (kp>-1) {
    k <- k*kp
  }
  
  # Compute diefk.
  df <- diefk(inputtrace, inputquery, k)
  
  return(df)
  
}

#' plotAnswerTrace
#'
#' This function plots the answer trace of a given query.
#' @param inputtrace Dataframe with the answer trace. Attributes of the dataframe: query, approach, tuple, time.
#' @param inputquery String that specifies the specific query to analyze from the answer trace.
#' @keywords diefk, diefficiency
#' @author Maribel Acosta
#' @import ggplot2
#' @export plotAnswerTrace
#' @seealso diefk, dieft
#' 
plotAnswerTrace <- function(inputtrace, inputquery) {
  
  # Obtain query and approaches to compare.
  results <- subset(inputtrace, query==inputquery)
  
  # Generate Plot
  resplot <- ggplot(data=results,aes(x=time, y=tuple))
  resplot <- resplot + geom_point(aes(colour=approach), size=3)
  resplot <- resplot + ggtitle(inputquery)
  resplot <- resplot + xlab('Time') +  ylab('# Answers Produced')
  resplot <- resplot + theme(
    legend.justification=c(0,1), legend.position=c(0,1),
    legend.text = element_text(size = 16),
    legend.title=element_blank(),
    legend.background = element_rect(fill = 'transparent',  colour = "transparent"),
    axis.text = element_text(colour = "black", size=14),
    axis.title = element_text(colour = "black", size=12),
    panel.background = element_rect(fill = 'white', colour = 'gray'),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank())
  
  print(resplot)
  
}

