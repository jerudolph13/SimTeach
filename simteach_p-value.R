
#######################################################################################################
#
# Purpose: To show how simulation can be used to demonstrate the definition of a p-value
#
# Author: Jacqueline E. Rudolph
#
# Required packages: ggplot2, ggpubr
#
# Last Updated: 15 Oct 2020
#
#######################################################################################################

##Load in necessary packages

packages <- c("ggplot2", "ggpubr")
for (package in packages) {
  library(package, character.only=T)
}


# Run simulation ----------------------------------------------------------

reps <- 10000       #Number of permutations
n <- 1000           #Number of individuals in each sample

set.seed(123)
rd <- rep(NA,reps)  #Vector to hold risk difference results

  #Simulate randomized treatment, with P(X=1)=0.5
  x <- rbinom(n, 1, 0.5)

  #Simulate binary outcome affected by treatment
  #P(Y=1)=0.5 and the RD for X is 0.05
  p.y <- 0.5 + 0.05*x - 0.05*0.5
  y <- rbinom(n, 1, p.y)

  #Estimate the RD for X and its p-value
  res <- glm(y ~ x, family=gaussian(link="identity"))
  rd_est <- summary(res)$coefficients[2]
  rd_pvalue <- summary(res)$coefficients[8]

  #Estimate the distribution of RDs under the null hypothesis that RD=0 
    #Randomly assign a new exposure to each individual, still with P(X'=1)=0.5
    #For this new exposure, estimate the RD on the original outcome
  for (i in 1:reps){
    #Draw new randomized exposure
    x_new <- rbinom(n, 1, 0.5)

    #Using new exposure and original outcome, estimate the RD and save its value in the rd vector
    rd[i] <- summary(glm(y ~ x_new, family=gaussian(link="identity")))$coefficients[2]
  }
  

# Summarize ---------------------------------------------------------------
  
  #Calculate the one-sided  p-value, by taking the proportion of reps
  #that had a RD greater than the RD from the original sample
  prop1 <- sum(rd > rd_est)/reps
  
  #Calculate the two-sided p-value, by taking the proportion of reps
  #that had a RD more extreme than the RD from the original sample
    #In other words, the absolute value of the RD was greater than the original RD
  prop2 <- sum(abs(rd) > rd_est)/reps
  

# Visualize ---------------------------------------------------------------

#Set formatting options
  
thm <- theme_classic() +
  theme(
    #Format text
    axis.title = element_text(family="Helvetica", size=16, color="black"),
    axis.text = element_text(family="Helvetica", size=16, color="black"),
    
    #Use tag to label paneled plots
    plot.tag.position="topleft",
    plot.tag=element_text(family="Helvetica", size=16, color="black", margin=margin(r=10, b=20)),
    
    #Format axis
    axis.line = element_line(size=0.75),
    axis.ticks = element_line(size=0.75),
    
    #Add space around plot
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
  )

#Plot one-sided p-value
  
    p1 <- ggplot() + thm +
      #Plot labels
      labs(tag="A)", x="\nRisk Difference", y="Density\n") +
      
      #Generate plots
      geom_histogram(aes(rd, stat(density)), bins=36, color="gray", fill="lightgray") +
      geom_vline(xintercept=rd_est, color="black", size=0.75, linetype="dashed") + 
      
      #Define axis options
      scale_x_continuous(expand=c(0, 0), limits=c(-0.12, 0.12)) +  
      scale_y_continuous(expand=c(0, 0), limits=c(0, 15))

#Plot two-sided p-value
    
    p2 <- ggplot() + thm +
      labs(tag="B)", x="\nAbsolute Value of Risk Difference", y="Density\n") +
      geom_histogram(aes(abs(rd), stat(density)), bins=36, color="gray", fill="lightgray") +
      geom_vline(xintercept=rd_est, color="black", size=0.75, linetype="dashed") +
      scale_x_continuous(expand=c(0, 0), limits=c(-0.12, 0.12)) + 
      scale_y_continuous(expand=c(0, 0), limits=c(0, 30))

    pdf("./pvalue_fig.pdf", height=5, width=10)
    ggarrange(p1, p2, ncol=2)
    dev.off()

