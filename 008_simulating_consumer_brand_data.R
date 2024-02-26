# Code for: R for Marketing Research and Analytics, 2nd ed: Chapter 7

# Libraries ----
library(package = tidyverse)

# Chapter 8

# optional instead of creating data:
# consumer_brand <- read.csv("http://r-marketing.r-forge.r-project.org/data/rintro-chapter8.csv")
# Or
# consumer_brand <- read.csv("http://goo.gl/IQl8nc")

## There process used to simulate the data
## is not show in the chapter
consumer_brand <- read_csv("http://goo.gl/IQl8nc")
consumer_brand |> glimpse()