# Code for: R for Marketing Research and Analytics, 2nd ed: Chapter 4

# Libraries ----
library(package = tidyverse)
library(package = skimr)

# Chapter 7

# optional instead of creating data:
# amusement_park <- read.csv("http://r-marketing.r-forge.r-project.org/data/rintro-chapter7.csv")
# Or
# amusement_park <- read.csv("http://goo.gl/HKnl74")


#### quick alternative to load the data
#### ... but better is to create the data as below
amusement_park <- read_csv("http://goo.gl/HKnl74")
skim(amusement_park)

# set random seed to make the random sequences replicable
set.seed(08226)

# Create the data ----

# Our hypothetical survey includes four questions about a 
# customer’s satisfaction with different dimensions of a visit 
# to the amusement park: satisfaction with rides (rides), 
# games (games), waiting times (wait), and cleanliness (clean), 
# along with a rating of overall satisfaction (overall). In such 
# surveys, respondents often answer similarly on all satisfaction 
# questions; this is known as the halo effect.

# number of survey respondents
nresp <- 500 

# simulate a satisfaction halo with a random variable for each 
# customer
halo <- floor(rnorm(n=nresp, mean=0, sd=5))

# Satisfaction with rides, games, waiting times and cleanliness
rides <- floor(halo + rnorm(n=nresp, mean=80, sd=3)+7)
games <- floor(halo + rnorm(n=nresp, mean=70, sd=7)+10)
wait <- floor(halo + rnorm(n=nresp, mean=65, sd=10)+6)
clean <- floor(halo + rnorm(n=nresp, mean=85, sd=2)+4)

# By adding halo to the response for each question, we create 
# positive correlation between the responses. The constants 
# +1, +5, and +9 are arbitrary to adjust the ranges just for 
# appearance.
cor(rides, games, use = 'everything', method = 'pearson')

amusement_park_score <- tibble(rides = rides,
       games = games,
       wait = wait,
       clean = clean)

amusement_park_score |> 
  summarize(across(.cols = rides:clean, 
                   .fns = list(min = min, max = max))) |> 
  pivot_longer(cols = everything(),
               names_to = 'variable_statistic',
               values_to = 'values') |> 
  separate_wider_delim(cols = variable_statistic, 
                       delim = '_', 
                       names = c('variable', 'statistic')) |> 
  pivot_wider(id_cols = variable, 
              names_from = statistic, 
              values_from = values)

# Satisfaction surveys often include other questions related to 
# the customer experience.

# For the amusement park data, we include whether the visit was 
# on a weekend, how far the customer traveled to the park in miles, 
# and the number of children in the party.
distance <- rlnorm(n=nresp, meanlog=3, sdlog=1)
num.child <- sample(x=0:5, size=nresp, replace=TRUE, 
                    prob=c(0.3, 0.15, 0.25, 0.15, 0.1, 0.05))
weekend <- as.factor(sample(x=c("yes", "no"), size=nresp, replace=TRUE, 
                            prob=c(0.5,0.5)))

# Overall satisfaction rating as a function of ratings for the 
# various aspects of the visit (satisfaction with rides, cleanliness, 
# and so forth), distance traveled, and the number of children
                 # capture the latent satisfaction
overall <- floor(halo + 
                 # satisfaction variables with weights 
                 0.5*rides + 0.1*games + 0.3*wait + 0.2*clean +
                 # customer experience with weights
                 0.03*distance + 5*(num.child==0) + 0.3*wait*(num.child>0) + 
                 rnorm(n=nresp, mean=0, sd=7) - 54)

amusement_park <- tibble(weekend, num.child, distance, 
                         rides, games, wait, clean, 
                         overall)

# Clean the environment ----
rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, 
   overall)

# Check the data created
amusement_park |> glimpse()
amusement_park |> skim()
