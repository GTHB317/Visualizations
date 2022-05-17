#load packages
library(ggplot2)
library(packcircles)
library(ggiraph)
library(ggraph)
library(reshape2)
library(tidyverse)
library(readr)
library(rgdal)
library(dplyr)
library(tmap)
library(tmaptools)
library(R6)
library(pryr)

#defining a new R6 class (the closest thing to Julia's "mutable struct" in R) for circle data
Circle <- R6Class("Circle",
                  private = list(
                    
                    #dummy imputs:
                    ..label = "name of clone",
                    ..x = 1,
                    ..y = 1,
                    ..areas = c(0.1, 0.1, 0.12, 0.15, 0.2, 0.1, 0.1, 0.12, 0.15, 0.2, 0.05, 
                                0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                                0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03,
                                0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03),
                    ..color = '#0000FF'
                    
                  ),
                  active = list(
                    super_ = function() super,
                    label = function(){
                      private$..label
                    },
                    x = function(){
                      private$..x
                    },
                    y = function(){
                      private$..y
                    },
                    areas = function(){
                      private$..areas
                    },
                    color = function(){
                      private$..color
                    }
                  ),
                  public = list(
                    #some maths
                    centre_dist = function(){
                      sqrt(((self$x)^2)+((self$y)^2))
                    },
                    
                    radii = function(){
                      rfa <- function(a){
                        sqrt((a/pi))
                      }
                      sapply(self$areas,rfa)
                    },
                    
                    initialize = function(label, x, y, areas, color){
                      if(!missing(label)) {
                        private$..label <- label
                      }
                      if(!missing(x)){
                        private$..x <- x
                      }
                      if(!missing(y)){
                        private$..y <- y
                      }
                      if(!missing(areas)){
                        private$..areas <- areas
                      }
                      if(!missing(color)){
                        private$..color <- color
                      }
                      },
                        
                    finalize = function() {
                      message("placeholder")
                    })
)

#3 dummy clusters I will work with:
ClusterA <- Circle$new(label = "bluey")

ClusterB <- Circle$new(areas = c(0.48, 0.85, 0.16, 0.15, 0.39, 0.28, 0.47, 0.4, 0.55, 0.32),
                       x = -1, y = -1,
                       color = '#FF0000',
                       label = "reddy")

ClusterC <- Circle$new(areas = c(0.57, 0.15, 0.85, 0.26, 0.43),
                       x = -1, y = 1,
                       color = '#00FF00',
                       label = "greeny")

#functions

#radius from area  
rfa<-function(a){
  sqrt(a/pi)
}

Rfa<-function(a){
  sapply(a,rfa)
}

#the linked lsit structure doesn't really exist in R, so for now I'm skipping the functions related to that.

#fit tangent circle function - issue: I think its not supposed to work for arrays and only individual circles.

fit_tang_circle <- function(C1,C2,C3){
  x1 = C1$x
  x2 = C2$x
  y1 = C1$y
  y2 = C2$y
  r1 = Rfa(C1$areas)
  r2 = Rfa(C2$areas)
  r = Rfa(C3$areas)
  
  dist = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  
  if (dist > r1 + r2 + 2*r) {
    message("Gap too large.")
    break
  } else {
    dist
  }
  
  #some trigonometry
  cos_sig = (x2 - x1)/dist
  sin_sig = (y2 - y1)/dist
  cos_gam = (dist^2 + (r + r1)^2 - (r + r2)^2)/(2*dist*(r + r1))
  sin_gam = sqrt(1 - (cos_gam)^2)
  
  #centre coordinates of the tangent circle
  C3.x = x1 + (r + r1)*(cos_sig*cos_gam - sin_sig*sin_gam)
  C3.y = y1 + (r + r1)*(cos_sig*sin_gam + sin_sig*cos_gam)
  C3
}
