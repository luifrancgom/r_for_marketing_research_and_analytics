# Libraries ----
library(tidyverse)

# Data sets
ecomm.df <- read.csv("https://goo.gl/hzRyFd")
summary(ecomm.df)

# 3.8.2 Exercises ----

## 1 ----
dim(ecomm.df)
### Another option
ecomm.df <- read_csv("https://goo.gl/hzRyFd")
ecomm.df

## 2 ----
str(as.data.frame(ecomm.df))
glimpse(ecomm.df)
### The R base way
table(ecomm.df$country)
### The tidyverse way
ecomm.df |>
  count(country, sort = TRUE)

## 3 ----
### The R base way
table(ecomm.df$profile,
      ecomm.df$intentWasPlanningToBuy)
### The tidyverse way
ecomm.df |>
  count(profile, intentWasPlanningToBuy)

## 4 ----
### The base R way
intent.tab <- table(ecomm.df$profile,
                    ecomm.df$intentWasPlanningToBuy)

### The R base way
(intent.tab[5, 3] / sum(intent.tab[5, ])) * 100
(intent.tab[8, 3] / sum(intent.tab[8, ])) * 100

### The tidyverse way
ecomm.df |> 
  count(profile, intentWasPlanningToBuy) |>
  filter(profile %in% c('Parent', 'Teacher')) |>
  filter(!is.na(intentWasPlanningToBuy)) |>
  group_by(profile) |>
  mutate(n_pct = (n / sum(n)) * 100) |>
  ungroup() |>
  filter(intentWasPlanningToBuy == 'Yes')

## 5 ----
### The R base way
table(ecomm.df$region)

### The tidyverse way
ecomm.df |>
  count(region, sort = TRUE)

## 6 ----
help("which.max")
table(ecomm.df$region) |>
  ## This pipe is more general: |> vs %>% 
  which.max() %>% 
  table(ecomm.df$region)[.]

## 7 ----
### The base R way
hist(x = ecomm.df$behavNumVisits,
     breaks = 100,
     freq = FALSE,
     col = 'blue', 
     xlab = "Number of visits to the site")
lines(x = density(x = ecomm.df$behavNumVisits, 
                  bw = 0.5, 
                  kernel = 'gaussian'),
      col='red', lw=1, type='l')

### The tidyverse way
ecomm.df |>
  ggplot() +
  geom_histogram(aes(x=behavNumVisits, y = after_stat(density)),
                 bins=50, color='black', fill='blue',
                 boundary=0) +
  labs(x="Number of visits to the site",
       y='Density')

## 8 ----
### R base way
boxplot(x = ecomm.df$behavNumVisits, horizontal = TRUE,
        xlab = "Number of visits to the site")
### Tidyverse way
ecomm.df |>
  ggplot() +
  geom_boxplot(aes(x = behavNumVisits)) +
  labs(x="Number of visits to the site")

## 9 ----
### In my case I prefer the case of an histogram

## 10 ----
### The base R way
par(mar=c(3, 12, 2, 2))
box_plot <- boxplot(data = ecomm.df,
                    behavNumVisits ~ profile,
                    horizontal = TRUE,
                    xlab = NULL,
                    ylab = NULL,
                    las=1)
dev.off()

### The tidyverse way
#### This is more easy
ecomm.df |>
  ggplot() + 
  geom_boxplot(aes(x=behavNumVisits, y=profile)) +
  labs(x='Number of visits to the site',
       y='User profile')

## 11 ----
MeanMedDiff <- function(vector) {
  return(abs(mean(vector) - median(vector)))
}

## 12 ----
MeanMedDiff(vector = ecomm.df$behavNumVisits)

ecomm.df$behavNumVisits |>
  sort(decreasing = TRUE) |>
  _[-1] |>
  MeanMedDiff(vector = _)

## 12 ----
ecomm.df[, 38:45] |>
  apply(MARGIN = 2, FUN = MeanMedDiff)

## 13 ----
ecomm.df[, 38:45] |>
  apply(MARGIN = 2, FUN = function(vector) {
                            (mean(vector) - median(vector) |>
                            abs())}
       )

## 14 ----
### I prefer the named function because I can use it multiple
### times
