# Libraries ----
library(tidyverse)

# 2.11.2 Exercise ----

## 1 ----
### Check help(month.name)
Months <- month.name

## 2 ----
### c("June", "July", "August", "September")
Summer <- 6:9

## 3 ----
Months[Summer]

## 4 ----
Summer * 3
### The positions used are out the bounds so you get
### NA's
Months[Summer * 3]

## 5 ----
mean(Summer)
### It uses a ceiling to index the position
Months[mean(Summer)]
Months[7.9999]

## 6 ----
### Use help(floor)
### Use help(ceiling)
floor(mean(Summer))
ceiling(mean(Summer))

## 7 ----
store.num <- factor(c(3 ,14 ,21 ,32 ,54)) # store id
store.rev <- c(543 , 654, 345, 678, 234) # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34) # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
store.df <- tibble(store.num, store.rev, 
                   store.visits, store.manager)

### The base R way
store.df$store.visits[store.df$store.manager == 'Bert']
### The tidyverse way
store.df |>
  filter(store.manager == 'Bert') |>
  select(store.visits, store.manager)

## 8 ----
### Another option
store.df[2, 3]
store.df[2, ]

## 9 ----
PieArea <- function(r) {
  return(pi*r^2)
}

## 10 ----
PieArea(r = 4)
PieArea(r = 4.5)
PieArea(r = 5)
PieArea(r = 6)
### A more efficient way
#### This is called vectorization
PieArea(r = c(4, 4.5, 5, 6))

## 11 ----
r <- c(4, 4.5, 5, 6)
pi * r^2