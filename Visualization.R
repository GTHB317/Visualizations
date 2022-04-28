#load packages
library(ggplot2)
library(packcircles)
library(reshape2)

#For data frame with name "V" has column names:
# label (name of clone)
# circleArea, 
# x (coordinate of the centroid location), 
# y (coordinate of the centroid location),
# color 
# and im not sure if a clonotype identiy column is relevant to the plot? (I actually am not too sure what this term even means. Categorical phenotypic identities? I assume I must be wrong as that would be the same as the label?

#made up dataset with the same labels as the one in the original paper. but everything else I randomly made)
x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(2,4,6,8,10,12,14,16,18,20)
label<- c("1: TCM 1", "2: CD4+ CTL 1","3: TCM 2","4: TEM 1","5: Treg",
          "6: TH2","7: TFH","8: Undef","9: CD4+ CTL 2","10: CD4+ CTL 3 ")
circleArea <- c((pi/2), pi/2, pi/2, pi/2, pi/2, pi/2, pi/2, pi/2, pi/3, pi) 
color <- c("#FF0000", "#FF4500", "#32CD32", "#008000", "#006400", 
           "#C2E105", "#FFFF00", "#66CDAA", "#8A91D0", "#BDB76B")
V <- data.frame(label,circleArea,x,y,color)
V

#a second dataframe with just x,y, and radius because the package requires it to create the coordinates.
RadiusFromArea <- function(d){
  sqrt((d/pi))
}

circleRadius <- sapply(circleArea, RadiusFromArea)
pck <- data.frame(x,y,circleRadius)
plotcord <- circleLayoutVertices(pck)

#simple ggplot of the data.
p1 <- ggplot()
p1 <- p1 + geom_polygon(data = plotcord,
                        aes(x, y, group = id,
                            fill = as.factor(id))
                        )
p1 <- p1 + geom_text(data = V, aes(x,y, label = paste0(label))) #size of text also could be adjusted if needed
p1 <- p1 + labs(title = "Sizes of clones within each cluster")
p1 <- p1 + coord_equal()
p1 <- p1 + theme_void()
p1 <- p1 + theme(legend.position="none")
p1 <- p1 + scale_fill_manual(values = color)

p1

