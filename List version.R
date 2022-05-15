#This version takes in lists that must be ordered as such:
#Area, x cord of centroid location, y cord of centroid location, hexadecimal color, label, and radius
#Am not too sure yet if a list of lists in R is equivalent to a doubly linked list.

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

#example cluster 1
C1 <- list(AreaA, xA, yA, ColorA, LabelA, RadiusA)
AreaA <- pi*25
xA <- 5
yA <- 7
ColorA <- '#FF0000'
LabelA <- "reddy"
RadiusA <- 5

#example cluster 2
C2 <- list(AreaB, xB, yB, ColorB, LabelB, RadiusB)
AreaB <- pi*36
xB <- 4
yB <- 5
ColorB <- '#00FF00'
LabelB <- "greeny"
RadiusB<- 6

#example cluster 3
C3 <- list(AreaC, xB, yB, ColorC, LabelC, RadiusC)
AreaC <- pi*9
xC <- 100.0
yC <- 400.0
ColorC <- '#0000FF'
LabelC <- "Qile's cluster"
RadiusC <- 3

#example list that will be used for this script. 
V <- list(ClusterA, ClusterB, ClusterC)

#radius from area  
Rfa<-function(a){
  rfa<-function(a){
    sqrt(a/pi)
  }
  unlist(lapply(a,rfa))
}

#the doubly linked list structure doesn't really exist in R, but  I wonder whether or not a list of ordered lists works as well? Also, is it a circular linked list or a linear one?

#original julia code:
#function init_boundary(a::Array{Circle,1})
#for i in 1:length(a)-1
#a[i].s = a[i+1]
#a[i+1].p = a[i]
#end
#a[length(a)].s = a[1]
#a[1].p = a[length(a)]
#end

#function fwd_dist(c::Circle, d::Circle) #number of "s"'s required to move fwd from c to d
#count = 0
#circ = c
#while circ != d
#count += 1
#circ = circ.s
#end
#return count
#end

#function insert_circle!(c::Circle,d::Circle,e::Circle)
#if (c.s != d)||(d.p != c)
#error("Two circles not adjacent.")
#else
  #c.s = e
#e.p = c
#d.p = e
#e.s = d
#end
#end

#forward remove: removes the segment between c,d, exclusive,  as one moves fwd
fwd_remove <- function(c,d){
  if (identical(c,d)){
    stop("Circles are the same.")
  }else if(TRUE==TRUE){ #placeholder
    stop("Circlese are consecutive.")
  }else{
    message("unfinished")
  }
}

#original julia code for fwd remove:
#function fwd_remove!(c::Circle,d::Circle) #removes the segment between c,d, exclusive,  as one moves fwd
#if c == d
#error("Circles are the same.")
#elseif c.s == d
#error("Circles are consecutive.")
#else
  #circ = c.s
#removed = []
#while circ != d
#circ.p.s = circ.s
#circ.s.p = circ.p
#circ = circ.s
#______________________________________________

#fit tangent circle function
fit_tang_circle <- function(C1,C2,C3){
  x1 = C1[[2]]
  x2 = C2[[2]]
  y1 = C1[[3]]
  y2 = C2[[3]]
  r1 = C1[[6]]
  r2 = C2[[6]]
  r = C3[[6]]
  
  dist = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  
  if (dist > (r1 + r2 + 2*r)){
    stop("Gap too large.")}
  else{dist}

  cos_sig = (x2 - x1)/dist
  sin_sig = (y2 - y1)/dist
  cos_gam = (dist^2 + (r + r1)^2 - (r + r2)^2)/(2*dist*(r + r1))
  sin_gam = sqrt(1 - (cos_gam)^2)

  C3[[2]] = x1 + (r + r1)*(cos_sig*cos_gam - sin_sig*sin_gam)
  C3[[3]] = y1 + (r + r1)*(cos_sig*sin_gam + sin_sig*cos_gam)
  C3
}

#euclidian distance to origin
centre_dist = function(c){
  sqrt(((c[[2]])^2)+((c[[3]])^2))
}

#placement of the first three circles
place_starting_three <- function(C1,C2,C3){
  C1[[2]] = -1*(C1[[6]])
  C2[[2]] = C2[[6]]
  
  fit_tang_circle(C1,C2,C3) #Dr. Murrell's original note: it seems like it might be necessary to initialise with opposite orientation
  
  centroid_x <- (C1[[2]]+C2[[2]]+C3[[2]])/3
  centroid_y <- (C1[[3]]+C2[[3]]+C3[[3]])/3
  
  C1[[2]] = C1[[2]]-centroid_x
  C2[[2]] = C2[[2]]-centroid_x
  C3[[2]] = C3[[2]]-centroid_x
  
  C1[[3]] = C1[[3]] - centroid_y
  C2[[3]] = C2[[3]] - centroid_y
  C3[[3]] = C3[[3]] - centroid_y
}

do_intersect <- function(c,d){ #returns TRUE or FALSE
  sqrt((c[[2]]-d[[2]])^2 + (c[[3]]+d[[3]])^2) < c[[6]] + d[[6]] #could reformulate as discrepancy > threshold
}

geod_dist <- function(Cm,Cn,C){ #fwd_dist doesnt exist yet
  min(fwd_dist(Cn,C), fwd_dist(C,Cm))
}

#not finished
