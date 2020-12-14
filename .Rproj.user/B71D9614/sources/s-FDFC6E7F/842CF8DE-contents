rm(list=ls())
library(tidyverse)
library(moments)
#install.packages("moments")

data_storage <- '/Users/Terez/OneDrive - Central European University/Data_Analysis_01/DA_team_project/data'
data_Pizza <- read_csv(paste0(data_storage,"/raw/data_Pizza.csv"))

## Data cleaning

data_Pizza = data_Pizza[-1,]
row.names(data_Pizza) <- df$id
data_Pizza$id <- NULL

str(data_Pizza)
data_Pizza$price_marg <- as.numeric(data_Pizza$price_marg)
data_Pizza$id <- as.numeric(data_Pizza$id)
data_Pizza$long <- as.numeric(data_Pizza$long)
data_Pizza$lat <- as.numeric(data_Pizza$lat)
data_Pizza$rating_google <- as.numeric(data_Pizza$rating_google)
data_Pizza$no_ratings <- as.numeric(data_Pizza$no_ratings)
data_Pizza$price_bev <- as.numeric(data_Pizza$price_bev)
data_Pizza$price_del <- as.numeric(data_Pizza$price_del)

## data Analysis
summarise(data_Pizza,
          n= n(),
          mean = mean(x = price_marg),
          median = median(x = price_marg),
          min= min(price_marg),
          max = max(price_marg),
          sd = sd(price_marg),
          skew = skewness(price_marg))

summarise(data_Pizza,
          n= n(),
          mean = mean(x = price_bev),
          median = median(x = price_bev),
          min= min(price_bev),
          max = max(price_bev),
          sd = sd(price_bev),
          skew = skewness(price_bev))


## Data Visulization Part 1
data_Pizza %>% 
ggplot(aes(x=price_marg))+
    geom_histogram(binwidth = 200, fill= "coral2", col= "black")+
    theme_bw()+
    labs(x = 'Price of Margarita Pizza', y= 'Number of Pizzas', title = "Distribution of Price for Pizza")

data_Pizza %>% 
  ggplot(aes(x=price_bev))+
  geom_histogram(binwidth = 50, fill= "light blue", col= "black")+
  theme_bw()+
  labs(x = 'Price of 0.5L beverage', y= 'Number of Pizzas', title = "Distribution of Price for Beverage")
 

## Data Visualization Part 2 - to be worked on
data_Pizza %>%
filter(category == "On-site") %>% 
  ggplot(aes(x=price_marg))+
  geom_histogram(binwidth = 200, fill= "peachpuff2", col= "black")+
  theme_bw()+
  labs(x = 'Price of Margarita Pizza', y= 'Number of Pizzas', title = "Distribution of Price for On-site")

data_Pizza %>%
filter(category == "Delivery") %>% 
  ggplot(aes(x=price_marg))+
  geom_histogram(binwidth = 200, fill= "plum2", col= "black")+
  theme_bw()+
  labs(x = 'Price of Margarita Pizza', y= 'Number of Pizzas', title = "Distribution of Price for Delivery")

#T -test

# Hypothesis
  # Null mean(on-site)=mean(delivery)
  # Alternative mean(on-site) not = mean(delivery)

t.test(price_marg,
       alternative = "two.sided", mu = 0, var.equal = TRUE, 
       )

