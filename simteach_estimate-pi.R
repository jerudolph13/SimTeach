
#######################################################################################################
#
# Purpose: To demonstrate how the Monte Carlo method can be used to estimate pi
#
# Author: Jacqueline E. Rudolph
#
# Required packages: ggplot2, ggpol, tidyverse
#
# Last Updated: 05 feb 2020
#
#######################################################################################################


setwd("your_working_directory")

packages <- c("ggplot2", "ggpol", "tidyverse")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
  library(package, character.only=T)
}


##Set up simulation

set.seed(123)
n <- c(1000, 10000, 100000)
pi <- rep(NA, 3)


##Estimate pi and visualize results when n=1000

for (i in 1:length(n)){
  
  x <- runif(n[i], 0, 2)
  y <- runif(n[i], 0, 2)
  in_circle <- ifelse((x-1)^2 + (y-1)^2 <= 1, 1, 0)
  pi[i] <- 4*(sum(in_circle)/n[i])
  
  if (i==1){
    thm <- theme_classic()
    pdf(file="pi.pdf", height=4, width=4)
    plot <- ggplot() + thm +
      geom_point(aes(x=x, y=y), color=ifelse(in_circle==1,"blue","red"), size=0.5) +
      geom_circle(aes(x=1, y=1, r=1)) +
      geom_rect(aes(xmin=0, xmax=2, ymin=0, ymax=2), color="black", alpha=0) +
    print(plot)
    dev.off()
  }
}

