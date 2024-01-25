# qq-plots from scratch ----

## https://www.r-bloggers.com/2020/08/q-q-plots-and-worm-plots-from-scratch/


# Libraries ----
library(tidyverse)

# Using the tidyverse ----
g <- tibble(y = 1:10) |>
  ggplot(aes(sample = y)) + 
  geom_qq(distribution = stats::qnorm) +
  geom_qq_line(distribution = stats::qnorm)

ggplot_build(g)$data[[1]]

g

# qq-plot data from scratch ----
data <- tibble(y = 1:10)

data |>
  mutate(sample = ppoints(length(y)),
         theorical = qnorm(sample)) |>
  _$theorical

# qq-plot plot from scratch
data |>
  mutate(sample = ppoints(length(y)),
         theorical = qnorm(sample)) |>
  ggplot() + 
  geom_point(aes(x = theorical, y = sample))