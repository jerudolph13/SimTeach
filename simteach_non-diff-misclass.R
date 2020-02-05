
#######################################################################################################
#
# Purpose: To demonstrate one way to simulate non-differential misclassification (NDM) and dependent
#           NDM, with the goals of giving an example where NDM leads to bias away from the null and 
#           concretizing the difference between bias and error
#
# Author: Jacqueline E. Rudolph
#
# Required packages: ggplot2
#
# Last Updated: 05 feb 2020
#
#######################################################################################################


setwd("your_working_directory")

packages <- c("tidyverse" ,"ggplot2", "mvtnorm")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
  library(package, character.only=T)
}


##Set up data set to store results

results <- data.frame(
  rep=NA,
  delta1=NA,
  delta2=NA,
  delta3=NA, 
  stringsAsFactors = FALSE
)


##Run simulation  

reps <- 10000
n <- 1000

simloop <- function(r, n) {
  
  set.seed(r)
  results$rep <- r
  
  #Confounders (2 continuous)
  m <- rmvnorm(n, mean=rep(0, 2))
  
  #Exposure affected by confounders
  a <- rbinom(n, 1, 1/(1 + exp(-(-log(1/0.5 - 1) + log(2)*m[ , 1] + log(2)*m[ , 2]))))
  
  #Outcome affected by exposure and confounders
  y <- rnorm(n, 10 + 2*a + 2*m[ , 1] + 2*m[ , 2], 6)
  
  #Exposure with non-differential misclassification
  error_a <- rnorm(n)
  a2 <- ifelse(error_a > qnorm(0.95), 1 - a, a)
  
  #Confounder 1 with non-differential misclassification
  m2 <- m
  m2[ , 1]  <- m[ , 1] - 0.9*error_a
  
  #Scenario 1: No misclassification
  results$delta1 <- summary(glm(y ~ a + m[ , 1] + m[ , 2], family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 2: Misclassification of binary exposure
  results$delta2 <- summary(glm(y ~ a2 + m[ , 1] + m[ , 2], family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 3: Dependent misclassification of binary exposure and continuous confounder
  results$delta3 <- summary(glm(y ~ a2 + m2[ , 1] + m2[ , 2], family=gaussian(link="identity")))$coefficients[2]
  
  return(results)
}

all.res <- lapply(1:reps, function(x) {simloop(x, n)})
all.res <- do.call(rbind, all.res)


##Summarize results

pos.error2 <- sum((all.res$delta2 - all.res$delta1) > 0)/reps
pos.error3 <- sum((all.res$delta3 - all.res$delta1) > 0)/reps

summ.res <- all.res %>% 
  summarise(avg.delta1=mean(delta1), 
            avg.delta2=mean(delta2), 
            avg.delta3=mean(delta3)) 

summ.res$bias2 <- summ.res$avg.delta2 - 2
summ.res$bias3 <- summ.res$avg.delta3 - 2


##Visualize results

thm <- theme_classic() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )

    #Results for scenario 2 compared to scenario 1
    all.res$idx <- as.numeric(all.res$delta2 > all.res$delta1)
    cols<-c("No Misclassification"="black", "Exposure Misclassification"="red")
    pdf("misclass1.pdf",height=5,width=5)
    ggplot(all.res) +
      geom_density(aes(delta1, colour="No Misclassification")) +
      geom_density(aes(delta2, colour="Exposure Misclassification")) +
      scale_colour_manual(name="Scenario:", values=cols) + 
      xlab("Mean Difference") + scale_x_continuous(expand=c(0, 0), limits=c(0, 4)) + 
      ylab("Density") + scale_y_continuous(expand=c(0,0), limits=c(0, 1)) +
      geom_label(aes(x=0.25, y=0.9, label="A"), size=8, label.size=0.5) + thm
    dev.off()
    
    #Results for scenario 3 compared to scenario 1
    all.res$idx <- as.numeric(all.res$delta3 > all.res$delta1)
    cols<-c("No Misclassification"="black", "Exposure & Confounder \nMisclassification"="red")
    pdf("misclass2.pdf",height=5,width=5)
    ggplot(all.res) +
      geom_density(aes(delta3, colour="Exposure & Confounder \nMisclassification")) +
      geom_density(aes(delta1, colour="No Misclassification")) +
      scale_colour_manual(name="Scenario:", values=cols) + 
      xlab("Mean Difference") + scale_x_continuous(expand=c(0, 0), limits=c(0, 4)) + 
      ylab("Density") + scale_y_continuous(expand=c(0, 0), limits=c(0, 1)) +
      geom_label(aes(x=0.25, y=0.9, label="B"), size=8, label.size=0.5) + thm
    dev.off()
    
    
    
    
