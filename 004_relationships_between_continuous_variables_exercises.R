# Libraries ----
library(tidyverse)
library(psych)
library(car)
library(GGally)
library(corrr)
library(corrplot)

# Data sets
ecomm.df <- read.csv("https://goo.gl/hzRyFd")

# 4.10 Exercise ----
## 1 ----
ecomm.df |>
  ggplot() +
  geom_histogram(aes(x = behavNumVisits),
                 boundary = 0,
                 color = 'black',
                 fill = 'red',
                 bins = 100)

### I think this plot is better taking into account 
### that the number of visits is a discrete variable
geom_col_behavNumVisits <- ecomm.df |> 
  count(behavNumVisits, sort = TRUE)

geom_col_behavNumVisits |>
  ggplot() + 
  geom_col(aes(x = behavNumVisits, y = n),
           color = 'black', fill = 'red') +
  scale_x_continuous(breaks = geom_col_behavNumVisits$behavNumVisit) + 
  theme(axis.text.x = element_text(size = 8))

## 2 ----
geom_col_behavNumVisits |>
  ggplot() +
  geom_col(aes(x = behavNumVisits, y = n),
           color = 'black', fill = 'red') +
  scale_x_log10(breaks = geom_col_behavNumVisits$behavNumVisit) +
  scale_y_log10() +
  labs(x = 'Number of site visits',
       y = 'Frequency (Log)',
       title = "Frequency of site visits") +
  theme(axis.text.x = element_text(size = 8))

## 3 ----
### Using ggplot2 the y-axis labels are not misleading
#### Hurrah!!! for the tidyverse

## 4 ----
count(ecomm.df, behavPageviews)
ecomm.df <- ecomm.df |>
  as_tibble() |>
  mutate(pageViewCeil = case_when(
    behavPageviews == '0' ~ 0,
    behavPageviews == '1' ~ 1,
    behavPageviews == '2 to 3' ~ 2,
    behavPageviews == '4 to 6' ~ 4,
    behavPageviews == '7 to 9' ~ 7,
    behavPageviews == '10+' ~ 10,
    .default = NA))
count(ecomm.df, pageViewCeil)

## 5 ----
ecomm.df |> 
  ggplot() +
  geom_bar(aes(x = pageViewCeil),
           color = 'black') + 
  scale_x_continuous(breaks = 0:7)

## 6 ----
ecomm.df |>
  ggplot(aes(x=pageViewCeil, y=behavNumVisits)) + 
  geom_point(aes(fill=as.factor(pageViewCeil)),
             position = position_jitter(width = 0.1),
             shape = 21,
             show.legend = FALSE) +
  scale_x_continuous(breaks = 0:max(ecomm.df$pageViewCeil)) +
  scale_y_log10()

## 7 ----
### Hurrah!!! I already did that with the tidyverse
### using position_jitter()

## 8 ----
cor(ecomm.df$pageViewCeil, ecomm.df$behavNumVisits)
### It gets better
cor(ecomm.df$pageViewCeil, log(ecomm.df$behavNumVisits))

## 9 ----
### According to this test correlation from the previous exercise 
### is not statistically significant
cor.test(ecomm.df$pageViewCeil, log(ecomm.df$behavNumVisits))

## 10 ---
### Apparently not. According to the book we need 
### to use psych::polychoric

## 11 ----
### I really don't understand this input 
#### I need to study more: https://www.personality-project.org/r/book/
polychoric(ecomm.df |> 
             select(pageViewCeil, behavNumVisits) |>
             mutate(behavNumVisits = log(behavNumVisits)))

## 12 ----
### That the relation between pageViewCeil and behavNumVisits
### appears to be negative but not statistically significant,
### so it has a high probability to be zero
help("Salaries")
Salaries <- as_tibble(x = Salaries)
Salaries

## 13 ----
help(pairs)

## 14 ----
### Different options
names(Salaries)
pairs(formula = ~ ., data = Salaries)
scatterplotMatrix(formula = ~ rank + discipline + yrs.since.phd + sex + salary, 
                  data = Salaries)
ggpairs(data = Salaries)

## 15 ----
### Using corrr
help("correlate")

Salaries_numeric <- Salaries |>
  select(where(is.numeric))

Salaries_corrr <- Salaries_numeric |>
  # Detect NA values
  ## Let them propagate all over
  correlate(use = 'everything', 
          method = 'pearson',
          diagonal = 1)

Salaries_corrr |>
  autoplot(triangular = 'upper')

### Using corrplot
cor(Salaries_numeric)
corrplot.mixed(corr = cor(Salaries_numeric),
               tl.pos='lt')