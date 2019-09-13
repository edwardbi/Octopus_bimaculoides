# Reproducibility Data Challenge
# Octopus Team
#Ahmed's modification!

rm(list=ls())

library(tidyverse)
library(RMKdiscrete)
library(ggplot2)

# Set work dir to your dir
# setwd("C:/Users/nbaughan/Documents/MBL2019/BSD-QBio5/tutorials/reproducibility")

# Load data
# Input the data
bug_data <- read_csv("./data/cole_arthropod_data_1946.csv")
egg_data <- read_csv("./data/mitchell_weevil_egg_data_1975.csv")

# Split the data in different tibbles, change colunm names and set lambda2 value
spider_data <- bug_data %>% select(k_number_of_arthropods, C_count_of_boards_with_k_spiders) %>% 
  rename(numbers = k_number_of_arthropods, times = C_count_of_boards_with_k_spiders)
lambda2.spider <- 0

sowbug_data <- bug_data %>% select(k_number_of_arthropods, C_count_of_boards_with_k_sowbugs) %>% 
  rename(numbers = k_number_of_arthropods, times = C_count_of_boards_with_k_sowbugs)
lambda2.sowbug <- 0.53214

egg_data <- egg_data %>% rename(numbers = k_number_of_eggs, times = C_count_of_beans_with_k_eggs)
lambda2.egg <- 0 # Because the data looks like those of spider,  we chose lambda2 = 0 to test

# Write a function to plot the data and poisson distribution in the same mean
overlay_poisson<- function(count.data, sample.name){
  
  # Get the mean of the observations  
  mean.number <- mean(count.data$numbers * count.data$times)
  
  # Get the poisson distribution in the same mean  
  number.max <- length(count.data$numbers) - 1
  count.data$poisson.distri <- dpois(0:number.max,lambda = 1/mean.number)
  
  # Get the propotion of "times" 
  sum.times <- sum(count.data$times)
  count.data$time.proportion <- count.data$times / sum.times
  
  # Plot the observations and Poisson distribution
  p.observation.poisson <- ggplot(count.data, aes(x = numbers)) +
    geom_point(aes(y = time.proportion), color = "darkblue") +
    geom_line(aes(y = poisson.distri), color = "darkred", linetype='dashed', alpha = 0.5) +
    scale_y_continuous(sec.axis = sec_axis(~.*sum.times, name = "Times to Have a Certain Count")) + 
    ggtitle(paste("Fit The Count of ", sample.name, " to Poisson Distribution")) +
    xlab("Count") + 
    ylab("Probability") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Return the plot
  return(p.observation.poisson)
}

# Write a function to add the LGP curve to the poisson_data plot
overlay_LGP<- function(count.data, p.observation.poisson, lambda2, sample.name){
  
  # Get the lambda1 and lambda2  
  mean.number <- mean(count.data$numbers * count.data$times)
  lambda1 <- mean.number * (1 - lambda2)
  
  # Get the LGP based on the indicated lambda  
  number.max <- length(count.data$numbers) - 1
  LGP.distri <- dLGP(x=0:number.max,theta=1/lambda1,lambda=lambda2)
  
  # Plot the LGP on the previous plot
  p.observation.poisson.LGP <- p.observation.poisson +
    geom_line(aes(y = LGP.distri), color = "darkgreen", linetype=3, alpha = 0.7) +
    ggtitle(paste("Fit The Count of ", sample.name, " to Poisson Distribution and LGP")) 

  # Return the plot
  return(p.observation.poisson.LGP)
}

# Plot data and Poisson distribution for the three datasets
p.spider.poisson <- overlay_poisson(spider_data, "Spider")
p.spider.poisson

p.sowbug.poisson <- overlay_poisson(sowbug_data, "Sowbug")
p.sowbug.poisson

p.egg.poisson <- overlay_poisson(egg_data, "Eggs")
p.egg.poisson

# Add LGP curve to the plot

p.spider.poisson.LGP <- overlay_LGP(spider_data, p.spider.poisson, lambda2.spider, "Spider")
p.spider.poisson.LGP

p.sowbug.poisson.LGP <- overlay_LGP(sowbug_data, p.sowbug.poisson, lambda2.sowbug, "Sowbug")
p.sowbug.poisson.LGP

p.egg.poisson.LGP <- overlay_LGP(egg_data, p.egg.poisson, lambda2.egg, "Eggs")
p.egg.poisson.LGP

# Seems like the lambda2 = 0 doesn't fit the data. Try lambda2 = 0.5
lambda2.egg.2 <- 0.5
p.egg.poisson.LGP.2 <- overlay_LGP(egg_data, p.egg.poisson, lambda2.egg.2, "Eggs")
p.egg.poisson.LGP.2

# This seems to be better, so they lay eggs in a group.


