#' Fitting to 2-dimensional point data
#'=========================================================

#' Set things up
#+results="hide",warning=FALSE,message=FALSE
library(inlabru)
library(INLA)
init.tutorial()

#' Make a shortcut to a nicer colour scale:
#+results="hide",warning=FALSE,message=FALSE

colsc <- function(...) {
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
                       limits = range(..., na.rm=TRUE))
}

#' Get the data
#'-----------------------------------
#' We are going to be working with a dataset obtained from 
#' the `R` package `spatstat`, which contains the locations of 647 gorilla nests. We load the 
#' dataset like this:

data(gorillas, package = "inlabru")

#' This dataset is a list containing a number of `R` objects, including the locations of the 
#' nests, the boundary of the survey area and an `INLA` mesh - see `help(gorillas)` for 
#' details. Extract the the objects we need from the list, into other objects, so that we 
#' don't have to keep typing '`gorillas$`':

nests <- gorillas$nests
mesh <- gorillas$mesh
boundary <- gorillas$boundary

#' Plot the points (the nests(. (The `ggplot2` function `coord_fixed()` sets the aspect ratio, 
#' which defaults to 1.)
#' 
#+results="hide",warning=FALSE,message=FALSE

ggplot() + gg(mesh) + 
  gg(nests) + 
  gg(boundary) + 
  coord_fixed() + 
  ggtitle("Points")

#' Fit an LGCP model to the locations of the gorilla nests, predict on the survey region,
#' and produce a plot of the estimated density - which should look like the 
#' plot shown below.
#' 
#' Recall that the steps to specifying, fitting and predicting are:
#' 
#' 1. Specify a model, comprising (for 2D models) `coordinates` on the left of `~` and 
#' an SPDE `+ Intercept` on the right. Please use the SPDE prior specification stated below.
#' 
#' 2. Call `lgcp( )`, passing it (with 2D models) the model components, the `SpatialPointsDataFrame` 
#' containing the observed points and the `SpatialPolygonsDataFrame` defining the survey boundary
#' using the `samplers` argument.
#' 
#' 3. Call `predict( )`, passing it the fitted model from 2., locations at which to predict and
#' an appropriate predictor spcification. The locations at which to predict should be a 
#' `SpatialPixelsDataFrame` covering the mesh obtained by calling  `pixels(mesh)`.

mesh2 <- inla.mesh.2d(loc = mesh$loc,max.edge=1,cutoff = 1)

ggplot() + gg(mesh2) + 
  gg(nests) + 
  gg(boundary) + 
  coord_fixed() + 
#  gg(aa)
  ggtitle("Points")


matern <- inla.spde2.pcmatern(mesh2, 
                              prior.sigma = c(0.1, 0.01), 
                              prior.range = c(5, 0.01))


#+results="hide",warning=FALSE,message=FALSE,echo=TRUE

cmp <- coordinates ~ spat(map = coordinates,
                              model = matern) +  Intercept

fit <- lgcp(components = cmp, data=nests, samplers = boundary)

ggplot() + gg(mesh2) + 
  #gg(nests) + 
  gg(boundary) + 
  coord_fixed() + 
    gg(aa)
  ggtitle("Points")


