
#######################################################################################################
#
# Purpose: To demonstrate one way to simulate non-differential misclassification (NDM) and dependent
#           NDM, with the goals of giving an example where NDM leads to bias away from the null and 
#           concretizing the difference between bias and error
#
# Author: Jacqueline E. Rudolph
#
# Required packages: ggplot2, tidyverse, mvtnorm
#
# Last Updated: 11 mar 2020
#
#######################################################################################################

##Set your working directory and load in necessary packages

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

reps <- 10000 #The number of simulations
n <- 1000     #Sample size within each simulation

#We carry out the simulations using the following function
simloop <- function(r, n) {
  
  set.seed(r)
  results$rep <- r
  
  #Create 2 continuous confounders (M1 and M2)
  #Both are normally distributed with mean 0 and s.d. 1
  m <- rmvnorm(n, mean=rep(0, 2)) 
  
  #Create binary exposure, P(A=1)=0.5
  #A affected by M1 and M2: both with log(OR) of log(2)
  a <- rbinom(n, 1, 1/(1 + exp(-(-log(1/0.5 - 1) + log(2)*m[ , 1] + log(2)*m[ , 2]))))
  
  #Continuous outcome affected by exposure and confounders
  #Normally distributed with mean determined by formula below and s.d. 6
  #Intercept of 10; mean difference of 2 for A, M1, and M2
  y <- rnorm(n, mean=(10 + 2*a + 2*m[ , 1] + 2*m[ , 2]), sd=6)
  
  #Exposure with NDM
  #We select 15% of the sample to have misclassified A
  error_a <- rnorm(n) #We use error_a to determine misclassification in anticipation of dependency below
  a2 <- ifelse(error_a > qnorm(0.85), 1 - a, a) #Misclassified exposure
  
  #Induce NDM of M1 that is correlated with error in exposure
  #because misclassified variables both affected by error_a
  m2 <- m
  m2[ , 1]  <- m[ , 1] - 0.9*error_a #Mismeasured M1, with extent of error dependent on error_a
  
  #Scenario 1: Linear model under no misclassification
  results$delta1 <- summary(glm(y ~ a + m[ , 1] + m[ , 2], family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 2: Linear model under misclassification of binary exposure
  results$delta2 <- summary(glm(y ~ a2 + m[ , 1] + m[ , 2], family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 3: Linear model under dependent misclassification of binary exposure and continuous confounder
  results$delta3 <- summary(glm(y ~ a2 + m2[ , 1] + m2[ , 2], family=gaussian(link="identity")))$coefficients[2]
  
  return(results)
}

##Run the above function "reps" times, where reps is the number of simulations

all.res <- lapply(1:reps, function(x) {simloop(x, n)})
all.res <- do.call(rbind, all.res)


##Summarize results

  #Proportion of simulations that had an error in the mean difference greater than the null
  pos.error2 <- sum((all.res$delta2 - all.res$delta1) > 0)/reps #Comparing scenario 2 to scenario 1
  pos.error3 <- sum((all.res$delta3 - all.res$delta1) > 0)/reps #Comparing scenario 3 to scenario 1
  
  #Get the average mean difference across simulations for each scenario
  summ.res <- all.res %>% 
    summarise(avg.delta1=mean(delta1), 
              avg.delta2=mean(delta2), 
              avg.delta3=mean(delta3)) 
  
  #Compare the average mean difference to the true mean difference of 2
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
    
    
    
    
