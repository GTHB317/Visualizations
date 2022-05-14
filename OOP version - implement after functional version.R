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

#For list with name "V" comprised of lists, in this case just 2. Each list will have:
# label (name of clone)
# circleAreas vector 
# centroid location vector
# hexadecimal color 
# label 

#defining a new R6 class (the closest thing to Julia's "mutable struct" in R)

Circle <- R6Class("Circle",
  private = list(
    
    #dummy imputs
    ..label = "name of clone",
    ..x = 1,
    ..y = 1,
    ..areas = c(0.1, 0.1, 0.12, 0.15, 0.2, 0.1, 0.1, 0.12, 0.15, 0.2, 0.05, 
              0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
              0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03,
              0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03),
    ..color = '#FF0000'
    
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

    placeholder = "placeholder",
    initialize = function(){
      message("work in progress")
    },
    finalize = function() {
      message("placeholder")
    })
)

