# Code for: R for Marketing Research and Analytics, 2nd ed: Chapter 3
# set random number generator to use pre R 3.6 method, to match book
# this affects data simulation and Bayesian methods, which use randomization
# ==> Run this line:
# if (getRversion() >= "3.6.0") suppressWarnings(RNGversion("3.5.0"))
# you could always change back to current with: 
#   RNGversion(getRversion())

# Libraries ----
library(package = tidyverse)

# alternative command to download the data. 
# ==> But don't do this! See the book and simulate the data step by step.
# Long URL:
# weekly_store <- read_csv(paste("http://r-marketing.r-forge.r-project.org/",
#                           "data/rintro-chapter3.csv", sep=""))
# Short URL:

# weekly_store <- read_csv("http://goo.gl/QPDdMl")

# Create the data ----

k_stores <- 20    # 20 stores
k_weeks <- 104    # 2 years of data each

# create a data frame of initially missing values to hold the data
weekly_store_names <- c("store_num", "year", "week", "p1_sales", "p2_sales", 
                     "p1_price", "p2_price", "p1_prom", "p2_prom", "country")
weekly_store <- as_tibble(x = matrix(NA, ncol=10, nrow=k_stores*k_weeks), 
                          .name_repair = ~ weekly_store_names)

# clean up
rm(weekly_store_names)

dim(x = weekly_store)

store_num <- 101:(100 + k_stores)
store_country <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
                rep("JP", 4), rep("AU", 1), rep("CN", 2))
length(x = store_country)    # make sure the stor number is the right length
length(x = store_country)    # make sure the country is the right length


weekly_store <- weekly_store |>
  mutate(store_num = rep({{store_num}}, each=k_weeks), # tidy evaluation: {{store_num}}
         country = rep({{store_country}}, each=k_weeks)) # tidy evaluation: {{store_country}}

weekly_store |> glimpse()

# clean up
rm(store_num, store_country)

weekly_store <- weekly_store |>
  mutate(week = rep(x = 1:52, times = k_stores*2),
         # try the inner parts of the next line to figure out how we use rep()
         year = rep(x = rep(x = 1:2, each = k_weeks/2), times = k_stores))

weekly_store |> glimpse()

weekly_store <- weekly_store |>
  mutate(store_num = factor(x = store_num, ordered = FALSE),
         country = factor(x = country, ordered = FALSE))

weekly_store |> glimpse()

head(weekly_store, n = 5)
tail(weekly_store, n = 5)

# set random seed to make the random sequences replicable
set.seed(seed = 98250, 
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion", 
         ## "Rounding" was the default in versions prior to 
         ## 3.6.0: it made sample noticeably non-uniform on
         ## large populations, and should only be used for 
         ## reproduction of old results
         sample.kind = "Rounding")

# promotion status, using binomial distribution, rbinom()
weekly_store <- weekly_store |>
  mutate(p1_prom = rbinom(n = nrow(weekly_store), size = 1, p = 0.1), # 10% promoted
         p2_prom = rbinom(n = nrow(weekly_store), size=1, p = 0.15)) # 15% promoted

weekly_store |> glimpse()

# prices
weekly_store <- weekly_store |>
  mutate(p1_price = sample(x = c(2.19, 2.29, 2.49, 2.79, 2.99), 
                           size = nrow(weekly_store), replace = TRUE),
         p2_price = sample(x= c(2.29, 2.49, 2.59, 2.99, 3.19), 
                           size = nrow(weekly_store), replace = TRUE))

weekly_store |> glimpse()

# sales data, using poisson (counts) distribution, rpois()
# first, the default sales in the absence of promotion
tmp_sales1 <- rpois(n = nrow(weekly_store), lambda = 120)  # lambda = mean sales per week
tmp_sales2 <- rpois(n = nrow(weekly_store), lambda = 100)  # lambda = mean sales per week

# scale sales according to the ratio of log(price)
tmp_sales1 <- tmp_sales1 * log(weekly_store$p2_price) / log(weekly_store$p1_price)
tmp_sales2 <- tmp_sales2 * log(weekly_store$p1_price) / log(weekly_store$p2_price)

# final sales get a 30% or 40% lift when promoted
weekly_store <- weekly_store |>
  mutate(p1_sales = floor({{tmp_sales1}} * (1 + p1_prom * 0.3)), # tidy evaluation: {{tmp_sales1}}
         p2_sales = floor({{tmp_sales2}} * (1 + p2_prom * 0.4))) # tidy evaluation: {{tmp_sales2}}

# clean up
rm(tmp_sales1, tmp_sales2,
   k_stores, k_weeks)

weekly_store |> head(n=5)
weekly_store |> tail(n=5)