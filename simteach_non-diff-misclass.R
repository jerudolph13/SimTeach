
#######################################################################################################
#
# Purpose: To demonstrate one way to simulate non-differential misclassification (NDM) scenarios and
#           to estimate bias under those scenarios 
#
# Author: Jacqueline E. Rudolph
#
# Required packages: ggplot2, tidyverse
#
# Last Updated: 17 Jun 2020
#
#######################################################################################################

##Load in necessary packages

packages <- c("tidyverse" ,"ggplot2")
for (package in packages) {
  library(package, character.only=T)
}


##Set up data set to store results

results <- data.frame(
  rep=NA,
  delta1=NA,
  delta2=NA,
  delta3=NA, 
  delta4=NA,
  delta5=NA,
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
  #Both are log normally distributed: the corresponding normal distributions have mean 0 and s.d. 1
  m1 <- rlnorm(n, meanlog=0, sdlog=1)
  m2 <- rlnorm(n, meanlog=0, sdlog=1)
  
  #Create binary exposure, P(A=1)=0.5
  #A affected by M1 and M2: both with log(OR) of log(2)
  a <- rbinom(n, 1, 1/(1 + exp(-(-log(1/0.5 - 1) + log(2)*m1 + log(2)*m2))))
  
  #Continuous outcome affected by exposure and confounders
  #Normally distributed with mean determined by formula below and s.d. 6
  #Intercept of 10; mean difference of 2 for A, M1, and M2
  y <- rnorm(n, mean=(10 + 2*a + 2*m1 + 2*m2), sd=6)
  
  #Exposure with NDM
  #We select 15% of the sample to have misclassified A
  error <- rnorm(n) #We use "error" to determine misclassification in anticipation of dependency below
  a.err <- a
    a.err <- ifelse(a==1 & error > qnorm(0.85), 0, a.err) #Set sensitivity to 0.85
    a.err <- ifelse(a==0 & error > qnorm(0.95), 1, a.err) #Set specificity to 0.95
    
  #Induce NDM of M1 that is (1) independent of and (2) dependent with the error in exposure
  m1.indep <- m1 + 0.9*runif(n, min=-1, max=1) #Mismeasured M1, independent of error in A
  m1.dep  <- m1 + 0.9*error #Mismeasured M1, dependent with error in A (because both depend on "error")
  
  #Scenario 1: Linear model under no misclassification
  results$delta1 <- summary(glm(y ~ a + m1 + m2, family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 2: Linear model under NDM of binary exposure only
  results$delta2 <- summary(glm(y ~ a.err + m1 + m2, family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 3: Linear model under NDM of continuous confounder only
  results$delta3 <- summary(glm(y ~ a + m1.dep + m2, family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 4: Linear model under NDM of exposue and confounder, with independent errors
  results$delta4 <- summary(glm(y ~ a.err + m1.indep + m2, family=gaussian(link="identity")))$coefficients[2]
  
  #Scenario 5: Linear model under NDM of exposure and confounder, with dependent errors
  results$delta5 <- summary(glm(y ~ a.err + m1.dep + m2, family=gaussian(link="identity")))$coefficients[2]
  
  return(results)
}

##Run the above function "reps" times, where reps is the number of simulations

all.res <- lapply(1:reps, function(x) {simloop(x, n)})
all.res <- do.call(rbind, all.res)


##Summarize results

  #Proportion of simulations that had an error in the mean difference greater than the null
  pos.error2 <- sum((all.res$delta2 - 2) > 0)/reps #Comparing scenario 2 to scenario 1
  pos.error3 <- sum((all.res$delta3 - 2) > 0)/reps #Comparing scenario 3 to scenario 1
  pos.error4 <- sum((all.res$delta4 - 2) > 0)/reps #Comparing scenario 4 to scenario 1
  pos.error5 <- sum((all.res$delta5 - 2) > 0)/reps #Comparing scenario 5 to scenario 1
  
  #Get the average mean difference across simulations for each scenario
  summ.res <- all.res %>% 
    summarise(avg.delta1=mean(delta1), 
              avg.delta2=mean(delta2), 
              avg.delta3=mean(delta3),
              avg.delta4=mean(delta4),
              avg.delta5=mean(delta5)) %>% 
    #Compute bias by comparing the average mean differences to the true mean difference of 2
    mutate(bias2=avg.delta2 - 2,
           bias3=avg.delta3 - 2,
           bias4=avg.delta4 - 2,
           bias5=avg.delta5 - 2)
  

##Visualize results

thm <- theme_classic() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )

    #Results for scenario 2 compared to scenario 1
    cols<-c("No Misclassification"="solid", "Exposure Misclassification"="dashed")
    ggplot(all.res) +
      geom_density(aes(delta1, linetype="No Misclassification")) +
      geom_density(aes(delta2, linetype="Exposure Misclassification")) +
      scale_linetype_manual(name="Scenario:", values=cols) +
      xlab("Mean Difference") + scale_x_continuous(expand=c(0, 0), limits=c(0, 4)) +
      ylab("Density") + scale_y_continuous(expand=c(0,0), limits=c(0, 1)) +
      geom_label(aes(x=0.25, y=0.9, label="A"), size=8, label.size=0.5) + thm

    #Results for scenario 5 compared to scenario 1
    cols<-c("No Misclassification"="solid", "Exposure & Confounder \nMisclassification"="dashed")
    ggplot(all.res) +
      geom_density(aes(delta5, linetype="Exposure & Confounder \nMisclassification")) +
      geom_density(aes(delta1, linetype="No Misclassification")) +
      scale_linetype_manual(name="Scenario:", values=cols) +
      xlab("Mean Difference") + scale_x_continuous(expand=c(0, 0), limits=c(0, 4)) +
      ylab("Density") + scale_y_continuous(expand=c(0, 0), limits=c(0, 1)) +
      geom_label(aes(x=0.25, y=0.9, label="B"), size=8, label.size=0.5) + thm



