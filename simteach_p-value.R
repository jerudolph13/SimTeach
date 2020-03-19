
#######################################################################################################
#
# Purpose: To show how simulation/permutation can be used to demonstrate the definition of a p-value
#
# Author: Jacqueline E. Rudolph
#
# Required packages: ggplot2
#
# Last Updated: 11 mar 2020
#
#######################################################################################################

##Set your working directory and load in necessary packages

setwd("your_working_directory")

packages <- c("ggplot2")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
  library(package, character.only=T)
}


##Set up the simulation

reps <- 10000       #Number of permutations
n <- 1000           #Number of individuals in each sample

set.seed(123)
rd <- rep(NA,reps)  #Vector to hold risk difference results

  #Simulate randomized treatment, with P(R=1)=0.5
  r <- rbinom(n, 1, 0.5)

  #Simulate binary outcome affected by treatment
  #P(Y=1)=0.1 and log OR for R is 0.75
  y <- rbinom(n, 1, 1/(1 + exp(-(-log(1/0.1 - 1) - 0.75*(0.5) + 0.75*r))))

  #Estimate the RD for R and its p-value
  res <- glm(y ~ r, family=gaussian(link="identity"))
  rd_est <- summary(res)$coefficients[2]
  rd_pvalue <- summary(res)$coefficients[8]

  #Permute exposure "reps" number of times
  for (i in 1:reps){
    set.seed(123 + i)
    u <- runif(n)         #For each individual take a draw from a uniform distribution
    r_new <- r[order(u)]  #Re-order the individuals on the basis of that draw
    
    #Using permuted exposure and original outcome, estimate the RD and save its value in the rd vector
    rd[i] <- summary(glm(y ~ r_new, family=gaussian(link="identity")))$coefficients[2]
  }
  
  #Calculate the one-sided permutation p-value, by taking the proportion
  #of permutations that had a RD greater than the RD from the original sample
  prop <- sum(abs(rd) > rd_est)/reps

  
##Visualize the results

thm <- theme_classic() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )

    #One-sided p-value
    pdf("pval1.pdf",height=5,width=5)
    ggplot() +
      geom_histogram(aes(rd, stat(density)), bins=36, color="blue", fill="lightblue") +
      geom_vline(xintercept=rd_est, color="black", size=1) +
      geom_label(aes(x=-0.06, y=21.5, label="A"), size=8, label.size=0.5) +
      xlab("Risk Difference") + scale_x_continuous(expand=c(0, 0), limits=c(-0.07, 0.07)) +  
      ylab("Density") + scale_y_continuous(expand=c(0, 0), limits=c(0, 23)) + thm
    dev.off()

    #Two-sided p-value
    pdf("pval2.pdf",height=5,width=5)
    ggplot() +
      geom_histogram(aes(abs(rd), stat(density)), bins=36, color="blue", fill="lightblue") +
      geom_vline(xintercept=rd_est, color="black", size=1) +
      geom_label(aes(x=-0.06, y=46.5, label="B"), size=8, label.size=0.5) +
      xlab("Absolute Value of the Risk Difference") + scale_x_continuous(expand=c(0, 0), limits=c(-0.07, 0.07)) + 
      ylab("Density") + scale_y_continuous(expand=c(0, 0), limits=c(0, 50)) + thm
    dev.off()

