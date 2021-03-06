% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment.R
\name{init.tutorial}
\alias{init.tutorial}
\title{Global setting for tutorial sessions}
\usage{
init.tutorial()
}
\description{
Increases verbosity and sets the inference strategy to empirical Bayes.
}
\examples{
\dontrun{
# Note: Only run this if you want to change the inlabru options for this session

# Determine current bru default:
bo <- bru.options()

# By default, INLA's integration strategy is set to the INLA default 'auto':
bo$inla.options$control.inla

# Now, let's run init.tutorial() to make empirical Bayes the default
# integration method when \\code{bru} calls \\code{inla}

init.tutorial()

# Check if it worked:
bru.options()$inla.options$control.inla
}

}
\author{
Fabian E. Bachl <\email{bachlfab@gmail.com}>
}
