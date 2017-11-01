# dief
[![Build Status](https://travis-ci.org/maribelacosta/dief.svg?branch=master)](https://travis-ci.org/maribelacosta/dief)
[![DOI](https://zenodo.org/badge/91789211.svg)](https://zenodo.org/badge/latestdoi/91789211)


R package for computing diefficiency metrics dief@t and dief@k.

The metrics dief@t and dief@k allow for measuring the diefficiency during an elapsed time period t or while k answers are produced, respectively. dief@t and dief@k rely on the computation of the area under the curve of answer traces, and thus capturing the answer rate concentration over a time interval.

## Download and Install
To download the development version of the `dief` package directly from GitHub, type the following at the R command line:
```r
# If you have not installed the "devtools" package.
install.packages("devtools")
# Install the dief package.
devtools::install_github("maribelacosta/dief")
```
## Examples 
```r
library("dief")

# Use answer traces provided in the package: Compare three approaches "Selective", "Not Adaptive", "Random" when executing the test "Q9.sparql".
traces
	
# Plot answer traces for test "Q9.sparql".
plotAnswerTrace(traces, "Q9.sparql")
	
# Compute dief@t when t is the time where the fastest approach produced the last answer.
dieft(traces, "Q9.sparql")
	
# Compute dief@t after 7.5 unit times (seconds) of execution. 
dieft(traces, "Q9.sparql", 7.5)
```

## Other Resources
Learn step by step to use the `dief` R package with Jupyter Notebooks.
- Introduction to the `dief` package and reproducibility of the experimental results reported at [1]: https://github.com/maribelacosta/dief-notebooks/blob/master/Dief-Intro.ipynb

Check the `dief-app` Shiny app.  
 - Visualize the `dief-app` at: http://km.aifb.kit.edu/services/dief-app/


## License 
This package is licensed under the MIT License.

## How to Cite
If you are using the `dief` package to compute dief@t or dief@k, please cite the `dief` package using the citation generated with the R built-in command `citation("dief")` as follows:

```r
library("dief")
citation("dief")
```

In addition, if you are reporting dief@t or dief@k, please cite our main publication [1]. 

## Publications
[1] Maribel Acosta, Maria-Esther Vidal, York Sure-Vetter. Diefficiency Metrics: Measuring the Continuous Efficiency of Query Processing Approaches. In Proceedings of the International Semantic Web Conference, 2017. Nominated to Best Paper Award at the Resource Track. 

[2] Maribel Acosta, Maria-Esther Vidal. Measuring the Performance of Continuous Query Processing Approaches with dief@t and dief@k. In  the International Semantic Web Conference, Posters and Demos, 2017.
