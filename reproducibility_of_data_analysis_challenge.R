# Reproducibility Data Challenge
# Octopus Team


rm(list=ls())

library(tidyverse)
library(RMKdiscrete)
library(ggplot2)

# set work dir to your dir
#setwd("C:/Users/nbaughan/Documents/MBL2019/BSD-QBio5/tutorials/reproducibility")

# Load data
# input the data
bug_data <- read_csv("./data/cole_arthropod_data_1946.csv")

# Plot the Poisson distribution with the same mean as spider counts
# 1) calculate mean of spider count
mean_spider <- mean(bug_data$C_count_of_boards_with_k_spiders)
# 2) plot the poisson
spider_poisson <- dpois(0:17,lambda = 1/mean_spider)
all_spider = sum(bug_data$C_count_of_boards_with_k_spiders)
p_spider <- ggplot(bug_data,aes(x=k_number_of_arthropods))
p_spider <- p_spider + geom_line(aes(y=spider_poisson),color="darkred") + 
  geom_point(aes(y=C_count_of_boards_with_k_spiders/all_spider),color="darkblue") +
  scale_y_continuous(sec.axis = sec_axis(~.*all_spider, name = "spider_data"))
p_spider 


# Plot the Poisson with same mean as Sowbug counts
# Calculate mean of Sowbug count
mean_snowbug <- mean(bug_data$C_count_of_boards_with_k_sowbugs*bug_data$k_number_of_arthropods)
# Plot the poisson
snowbug_poisson <- dpois(0:17,lambda = 1/mean_snowbug)

lambda_1 <- (mean_snowbug)*(1-0.53214)
snowbug_poisson_LGP <- dLGP(x=0:17,theta=1/lambda_1,lambda=0.53214)
all_snowbugs = sum(bug_data$C_count_of_boards_with_k_sowbugs)
p_snowbug <- ggplot(bug_data,aes(x=k_number_of_arthropods))


