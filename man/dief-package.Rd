\name{dief-package}
\alias{dief-package}
\alias{dief}
\docType{package}
\title{
Library for computing the diefficiency of incremental approaches 
}
\description{
Library to compute the metrics dief@t and dief@k from answer traces of incremental approaches.   
}

\details{
\tabular{ll}{
Package: \tab dief\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-05-18\cr
License: \tab CC BY 4.0\cr
}

}
\author{
Maribel Acosta

Maintainer: Maribel Acosta <maribel.acosta@kit.edu>

}
\references{
Maribel Acosta, Maria-Esther Vidal, York Sure-Vetter. Diefficiency Metrics: Measuring the Continuous Efficiency of Query Processing Approaches. 
}

\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
  # Example to compute dief@t (with t=1.05) for "Q2.sparql" from the trace provided in "data/".
  
  traces <- read.csv("data/nLDE-Benchmark1-AnswerTrace.csv")
  dieft(traces, "Q2.sparql", 1.05)
  
}
