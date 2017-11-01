\name{dief}
\alias{dief}
\docType{package}
\title{
Tools for Computing Diefficiency Metrics
}
\description{
An implementation of the metrics dief@t and dief@k to measure the diefficiency (or continuous efficiency) of incremental approaches, see Acosta, M., Vidal, M. E., & Sure-Vetter, Y. (2017). Diefficiency metrics: Measuring the continuous efficiency of query processing approaches. In International Semantic Web Conference (pp. 3-19). Springer, Cham. <doi:10.1007/978-3-319-68204-4_1>. The metrics dief@t and dief@k allow for measuring the diefficiency during an elapsed time period t or while k answers are produced, respectively. dief@t and dief@k rely on the computation of the area under the curve of answer traces, and thus capturing the answer rate concentration over a time interval.  
}

\details{
\tabular{ll}{
Package: \tab dief\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2017-10-30\cr
License: \tab MIT\cr
}
}

\examples{
# This example uses the answer traces provided in the package.  
# These traces record the answers produced by three approaches "Selective", 
# "Not Adaptive", "Random"  when executing the test "Q9.sparql"
data(traces)
	
# Plot answer traces for test "Q9.sparql"
plotAnswerTrace(traces, "Q9.sparql")
	
# Compute dief@t with t the time where the fastest approach produced the last answer.
dieft(traces, "Q9.sparql")
	
# Compute dief@t after 7.5 time units (seconds) of execution. 
dieft(traces, "Q9.sparql", 7.5)
}

\author{
Maribel Acosta

Maintainer: Maribel Acosta <maribel.acosta@kit.edu>

}
\references{
Acosta, Maribel, Maria-Esther Vidal, and York Sure-Vetter. "Diefficiency metrics: Measuring the continuous efficiency of query processing approaches." In International Semantic Web Conference, pp. 3-19. Springer, Cham, 2017.
}

\keyword{ package }

