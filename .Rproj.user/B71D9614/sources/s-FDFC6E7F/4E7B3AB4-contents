###########################
##       Assignement     ##
##Data analysis & coding ##
## Margarita & beverage  ##
###########################




## Start script
# Remove variables from the memory
rm(list=ls())

## Install and import packages
#install.packages("geosphere")
#install.packages("pander")
library(tidyverse)
library(geosphere)
library(moments)
library(dplyr)

## Import data
my_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA_team_project/master/data/raw/dp.csv"
# Reading with csv 
dp <- read_csv(my_url)
# Check the variables
glimpse(dp)

## Create new variable : Calculate distance from CEU campus
ceu = c(47.501348, 19.049375)
dp["dist_ceu"] = distm(data.matrix(dp[6:7]), ceu, fun = distHaversine)
distm(ceu, c(47.500386, 19.049434), fun=distHaversine)

## Create new variable : Calculate usual number of hours open
get_interval <- function(open,close){
  intv = close-open
  if (intv < 0) {
    intv = as.difftime("24:00:00")-open+close
  }
  intv=as.numeric(as.difftime(intv,units = "hours"))/60
  return(intv)
}

dp$open_mins=mapply(get_interval, dp$open, dp$close)


## ## Check the summary stat: 
# 1) Margarita price
dp_summary_stats_pizza <- summarise(dp,
          n= n(),
          mean = mean(x = price_marg),
          median = median(x = price_marg),
          min= min(price_marg),
          max = max(price_marg),
          sd = sd(price_marg),
          skew = skewness(price_marg))
# 2) Beverage price
dp_summary_stats_bev <- summarise(dp,
          n= n(),
          mean = mean(x = price_bev),
          median = median(x = price_bev),
          min= min(price_bev),
          max = max(price_bev),
          sd = sd(price_bev),
          skew = skewness(price_bev))

# Join them to table
table_summary <- add_row(dp_summary_stats_bev,dp_summary_stats_bev)


## Data Visualization: part 1
# Histogram : descriptive graph of price distribution of the Margarita
dp %>% 
  ggplot(aes(x=price_marg))+
  geom_histogram(binwidth = 200, fill= "coral2", col= "black")+
  theme_bw()+
  labs(x = 'Price of Margarita Pizza (HUF)', y= 'Number of Pizzas', 
       title = "Distribution of Price for Pizza")

# Histogram : descriptive graph of price distribution of the beverage by catgegories
dp %>% 
  ggplot(aes(x=price_bev, fill=category_bev))+
  geom_histogram(binwidth = 50, col= "black")+
  theme_bw()+
  scale_fill_brewer()+
  labs(x = 'Price of 0.5L beverage (HUF)', y= 'Number of Pizzas',
       title = "Distribution of Price for Beverage by category")


## Creating a factor variable
dp$category_f <- factor(dp$category)

## Data Visulization: part 2
#  Density :  Comparaison of price distribution of the Margarita On-site and Delivery
ggplot(data = dp, aes(x = price_marg, fill = category_f, alpha = 0.2))+
  geom_density(col = "black")+
  labs(x='Price of Margarita Pizza (HUF)', y='Number of Pizzas', 
       title = "Distribution of Price for On-site and Delivery", fill= 'category')

# Scatter plot : relationship between delivery price and distance from CEU Campus
dp_del <- dp[dp$price_del!='N/A',] %>% 
  transform(price_del = as.numeric(price_del))
corr=round(cor(dp_del$dist_ceu, dp_del$price_del, method = c("pearson")),4)
ggplot(dp_del, aes(x=dist_ceu, y=price_del)) +
  geom_point(size=3, color='orange') +
  labs(x='Distance from CEU', y='Delivery price', title = expr(paste
    ("Relationship between delivery price and distance from CEU Campus, ", rho, "=",!!corr)))

## HYPOTHESIS TESTING

# Hypothesis
# Null mean(on-site)=mean(delivery)
# Alternative mean(on-site) not = mean(delivery)

price_offline <- dp$price_marg[dp$category=="On-site"]
price_online <- dp$price_marg[dp$category=="Delivery"]

t.test(price_offline,price_online, alternative = "two.sided")