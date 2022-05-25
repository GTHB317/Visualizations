#This version takes in lists that must be ordered as such:
#Area, x cord of centroid location, y cord of centroid location, hexadecimal color, label, and radius
#For quite a few of these functions, there is an "subscript out of bounds" error. 
#I'm pretty sure its because of the lack of predessesor and sucessors.
#The next thing I'll do is find a way to fix this. Perhaps have the init_boundary apply itself somehow several times?

#load packages
library(ggplot2)
library(packcircles)
library(ggiraph)
library(ggraph)
library(reshape2)
library(tidyverse)
library(readr)
library(dplyr)
library(tmap)
library(tmaptools)
library(pryr)

#example cluster 1
AreaA <- pi*25
xA <- 5
yA <- 7
ColorA <- '#FF0000'
LabelA <- "reddy"
RadiusA <- 5
p1 <- list()
s1 <- list()
C1 <- list(AreaA, xA, yA, ColorA, LabelA, RadiusA,p1,s1)

#example cluster 2
AreaB <- pi*36
xB <- 4
yB <- 5
ColorB <- '#00FF00'
LabelB <- "greeny"
RadiusB<- 6
p2 <- list()
s2 <- list()
C2 <- list(AreaB, xB, yB, ColorB, LabelB, RadiusB,p2,s2)

#example cluster 3
AreaC <- pi*9
xC <- 100.0
yC <- 400.0
ColorC <- '#0000FF'
LabelC <- "Qile's cluster"
RadiusC <- 3
p3 <- list()
s3 <- list()
C3 <- list(AreaC, xB, yB, ColorC, LabelC, RadiusC,p3,s3)

#example list of clusters used here
a <- list(C1,C2,C3)

#simple radius from area function  
Rfa<-function(a){
  rfa<-function(a){
    sqrt(a/pi)
  }
  unlist(lapply(a,rfa))
}

init_boundary <- function(a){ #where a is a list of circle lists.
  a[[length(a)]][[8]] <- a[[1]]
  a[[1]][[7]] <- a[[length(a)]]
  for (i in 1:(length(a)-1))
    {a[[i]][[8]] <- a[[i+1]]
    a[[i+1]][[7]] <- a[[i]]}
  } #It works but for the predessecors and sucessors made, they dont have predessesors and sucessors.

# BELOW FUNCTION DOESNT WORK PROPERLY______________________________
fwd_dist <- function(c,d){
  count <- 0
  circ <- c
  while(identical(circ,d)==FALSE){
    count = count + 1
    circ = circ[[8]]
  }
  count
} #error: subscript out of bounds

#original julia code
#function fwd_dist(c::Circle, d::Circle) #number of "s"'s required to move fwd from c to d
#count = 0
#circ = c
#while circ != d
#count += 1
#circ = circ.s
#end
#return count
#end_______________________________________________________________

insert_circle <- function(c,d,e){
  if((identical(c[[8]],d) == FALSE)||(identical(d[[7]],c)==FALSE)){
    stop("Two circles not adjacent")
  }else{
    c[[8]] <- e
    e[[7]] <- c
    d[[7]] <- e
    e[[8]] <- d
  }
} #I AM NOT SURE IF THIS WORKS BECAUSE I HAVENT TESTED WITH MORE CIRCLES, BUT AT LEAST THE LOGICAL STATEMENT WORKS.

#forward remove: removes the segment between c,d, exclusive,  as one moves fwd. ALSO DOESNT WORK_____________
fwd_remove <- function(c,d){
  if (identical(c,d)){
    stop("Circles are the same.")
  }else if(identical(c[[7]],d)){ 
    stop("Circles are consecutive.")
  }else{
    circ <- c[[8]]
    #removed <- c()
    while(identical(circ,d)==FALSE){
      circ[[7]][[8]] <- circ[[8]]
      circ[[8]][[7]] <- circ[[7]]
      circ <- circ[[8]] #Error in circ[[8]] : subscript out of bounds
    }
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
#____________________________________________________________________________________________________________

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

#functin doesnt work:Error in c[[2]] : subscript out of bounds__________________
closest <- function(c){
  closest <- c
  circ <- c[[8]]
  while(identical(circ,c)==FALSE){
    if(centre_dist(closest) > centre_dist(circ)){
      closest <- circ
    }
    circ <- circ[[8]]
  }
  closest
}
#_______________________________________________________________________________


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
    
    #circles[1].x = - circles[1].rad
    #circles[2].x = circles[2].rad
    #return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles]]
    #end
    
    # Place the first three circles to be mutually tangent, with centroid the origin.
    #place_starting_three(circles[1], circles[2], circles[3])
    message("placeholder")
  }
  
  place_starting_three()
  
  # Place the first three circles to be mutually tangent, with centroid the origin.
  #place_starting_three(circles[1], circles[2], circles[3])
  
  init_boundary()
  
  # Initialise the boundary
  #init_boundary(circles[1:3])
  
  for(i in 1:length(circles)){
    print("placeholder")
  }
  
  #= for i in 1:length(circles)
  #println(circles[i].name,":",circles[i].rad)
  #end =#
  
  j <- 4
  while(j <= length(circles)){
    message("work in progress")
    #if try_place
    #cl = closest_place(circles[j-1], circles[j])
    #else
    #cl = closest(circles[j-1])
    #end
  }
  
  fit_tang_circle(cl, cl.s, circles[j])
  
  check <- overlap_check(cl, cl.s, circles[j])
  if (check == "clear"){
    insert_circle(cl, cl.s, circles[j])
    j = j+1
  }else{
    while(check != "clear"){
      Cm <- check[1]
      Cn <- check[2]
      fwd_remove(Cm,Cn)
      fit_tang_circle(Cm,Cn,circles[j])
      check <- overlap_check(Cm,Cn,circles[j])
      if(check == "clear"){
        insert_circle(Cm,Cn, circles[j])
        j <- j+1
      }
    }
  }
  
  message("[[c[[2]] for c in circles],[c[[3]] for c in circles],[c[c[[6]] for c in circles] ]")
}

#for after all previous functions are done, then I will move onto the next step which is visualizing the results from the circle_layout function.
#it will definetely be done with ggplot and also the packcircles package, with a script similar to the "simple script" in the same Repo.

