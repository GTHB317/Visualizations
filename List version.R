#This version takes in lists that must be ordered as such:
#Area, x cord of centroid location, y cord of centroid location, hexadecimal color, label, and radius
#Am not too sure yet if a list of lists in R is equivalent to a doubly linked list. I am awaiting some help about how to handle this in R, hopefully.

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

#example list that will be used later for this script. 
V <- list(ClusterA, ClusterB, ClusterC)

#radius from area  
Rfa<-function(a){
  rfa<-function(a){
    sqrt(a/pi)
  }
  unlist(lapply(a,rfa))
}

#the doubly linked list structure doesn't really exist in R, but  I wonder whether or not a list of ordered lists works as well? Also, is it a circular linked list or a linear one?

init_boundary <- function(a){
  for (i in 1:length(a)-1){
    message("Work in progress")
  }
}

#original julia code:
#function init_boundary(a::Array{Circle,1})
#for i in 1:length(a)-1
#a[i].s = a[i+1]
#a[i+1].p = a[i]
#end
#a[length(a)].s = a[1]
#a[1].p = a[length(a)]
#end

fwd_dist <- function(c,d){
  count <- 0
  circ <- c
  while(circ!=d){
    count = count + 1
    message("work in progress") #I'm not sure how to implement the circ = circ.s | In R there is no doubly linked list.
    }
  count
  }
  

#function fwd_dist(c::Circle, d::Circle) #number of "s"'s required to move fwd from c to d
#count = 0
#circ = c
#while circ != d
#count += 1
#circ = circ.s
#end
#return count
#end

insert_circle <- function(c,d,e){
  message("Work in progress")
}

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
    stop("Circles are consecutive.")
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

closest <- function(c){
  closest <- c
  message("Work in progress")
}

closest_place <- function(c,d){
  closest <- c
  message("Work in progress")
  closest
}

do_intersect <- function(c,d){ #returns TRUE or FALSE
  sqrt((c[[2]]-d[[2]])^2 + (c[[3]]+d[[3]])^2) < c[[6]] + d[[6]] #could reformulate as discrepancy > threshold
}

geod_dist <- function(Cm,Cn,C){ #fwd_dist doesnt exist yet
  min(fwd_dist(Cn,C), fwd_dist(C,Cm))
}

overlap_check <- function(Cm,Cn,C){
  C_em <- Cm
  C_en <- Cn
  obstruct <- list()
  message("Work in progress")
}

circle_layout <- function(input_rad_vec, order = TRUE, try_place = TRUE){
  if (order){
    input_rad_vec = reverse(sort(input_rad_vec))
  }
  
  circles <- "placeholder"
  
  if(length(circles) == 1){
    circles[[2]]
    circles[[3]]
    circles[[6]]
  }else if(length(circles)==2){
    placeholder = "placeholder"
  }

  message("work in progress")
}

#Original Julia code below
#function circle_layout(input_rad_vec::Array{Float64,1}; order = true, try_place = true)
    #if order
        #input_rad_vec = reverse(sort(input_rad_vec))
    #end

    # Initialise the circles with radii (not areas) as specified in input_rad_vec, and no boundary relations.
    #circles = [Circle("Circle_$(i)", 0.0, 0.0, input_rad_vec[i],nothing,nothing) for i in 1:length(input_rad_vec)]

    #Taking care of "degenerate" cases when there are one or two circles

    #if length(circles) == 1
        #return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles]]
    #elseif length(circles) == 2
        #circles[1].x = - circles[1].rad
        #circles[2].x = circles[2].rad
        #return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles]]
    #end

    # Place the first three circles to be mutually tangent, with centroid the origin.
    #place_starting_three(circles[1], circles[2], circles[3])

    # Initialise the boundary
    #init_boundary(circles[1:3])

    #= for i in 1:length(circles)
        #println(circles[i].name,":",circles[i].rad)
    #end =#
    
    #Loop through the remaining circles, fitting them
    #j = 4
    #while j <= length(circles)
        # Initial attempt to fit Circle_j
        #if try_place
            #cl = closest_place(circles[j-1], circles[j])
        #else
            #cl = closest(circles[j-1])
        #end
        #fit_tang_circle!(cl, cl.s, circles[j])

        # Check for overlaps and update, refit and recheck until "clear"
        #check = overlap_check(cl, cl.s, circles[j])
        #if check == "clear"
            #insert_circle!(cl, cl.s, circles[j])
            #j += 1
        #else
            #while check != "clear"
                #Cm = check[1]
                #Cn = check[2]
                #fwd_remove!(Cm,Cn)
                #fit_tang_circle!(Cm,Cn,circles[j])
                #check = overlap_check(Cm,Cn,circles[j])
                #if check == "clear"
                    #insert_circle!(Cm,Cn, circles[j])
                    #j += 1
                #end
            #end
        #end
    #end
    #return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles] ]
#end
