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

\examples{
	# Answer traces: three approaches "Selective", "Not Adaptive", "Random" are compared when executing the test "Q9.sparql"
	traces <- read.csv("data/example.csv")
	
	# Plot answer traces for test "Q9.sparql"
	plotAnswerTrace(traces, "Q9.sparql")
	
	# Compute dief@t when t is the time where the fastest approach produced the last answer.
	dieft(traces, "Q9.sparql")
	
	# Compute dief@t after 7.5 unit times (seconds) of execution. 
	dieft(traces, "Q9.sparql", 7.5)
}

\author{
Maribel Acosta

Maintainer: Maribel Acosta <maribel.acosta@kit.edu>

}
\references{
Maribel Acosta, Maria-Esther Vidal, York Sure-Vetter. Diefficiency Metrics: Measuring the Continuous Efficiency of Query Processing Approaches. 
}

\keyword{ package }

