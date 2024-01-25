# Libraries ----
library(tidyverse)
library(lattice)
library(tidyquant)

# Data sets
ecomm.df <- read.csv("https://goo.gl/hzRyFd")

# 5.6 Exercise ----
## 1 ----
ecomm.df |> glimpse()

ecomm.df |> 
  count(behavPageviews)

ecomm.df <- ecomm.df |>
  mutate(pageViewFloor = case_when(
    behavPageviews == '0' ~ 0,
    behavPageviews == '1' ~ 1,
    behavPageviews == '2 to 3' ~ 2,
    behavPageviews == '4 to 6' ~ 4,
    behavPageviews == '7 to 9' ~ 7,
    behavPageviews == '10+' ~ 10,
    .default = NA))

ecomm.df |> 
  count(pageViewFloor)

ecomm.df |> 
  count(profile)

aggregate(pageViewFloor ~ profile, 
          data=ecomm.df, FUN=summary)

### Another alternative using the tidyverse
quantile_df <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1)) {
  tibble(
    value = quantile(x, probs = probs, 
                     na.rm = TRUE, type=7),
    quantile = probs
  )
}

quantile_df(x = ecomm.df$pageViewFloor)

ecomm.df |> 
  group_by(profile) |> 
  reframe(quantile_df(pageViewFloor))

## 2 ----
for (profile in unique(ecomm.df$profile)) {
  print(profile)
  print(summary(ecomm.df$pageViewFloor[ecomm.df$profile == profile]))
}

## 3 ----
### I prefer the tidyverse approach where you
### have the information in a tibble

## 4 ----
ecomm.df |> 
  count(gender)

ecomm.df |> 
  filter(gender %in% c('Female', 'Male')) |> 
  count(gender, profile) |> 
  group_by(profile) |> 
  mutate(pct_n = n / sum(n)) |> 
  ungroup() |> 
  arrange(profile, gender)

## 5 ----
factor_levels <- ecomm.df |> 
  count(profile) |> 
  ungroup() |> 
  arrange(n)

profile_purchasedWhen <- ecomm.df |> 
  group_by(profile, purchasedWhen) |> 
  count() |> 
  ungroup() |> 
  arrange(profile, purchasedWhen) |> 
  mutate(purchasedWhen = na_if(purchasedWhen, '')) |>
  mutate(profile=factor(x=profile, levels=factor_levels$profile,
                        ordered=TRUE))

palette_colors <- palette_light() |> unname()

profile_purchasedWhen |> 
  ggplot(aes(x=n, y=profile,
             fill=purchasedWhen)) + 
  geom_col(color='black') +
  scale_fill_manual(values = palette_colors[1:length(unique(profile_purchasedWhen$profile))]) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x=NULL,
       y=NULL,
       fill=NULL) +
  theme(legend.position = 'bottom')

## 6 ----
profile_purchasedWhen_pct <- ecomm.df |> 
  group_by(profile, purchasedWhen) |> 
  count() |> 
  ungroup(purchasedWhen) |> 
  mutate(purchasedWhen = na_if(purchasedWhen, '')) |>
  mutate(n_pct = n / sum(n)) |> 
  ungroup() |>
  mutate(profile=factor(x=profile, levels=factor_levels$profile,
                        ordered=TRUE))

profile_purchasedWhen_pct |> 
  ggplot(aes(x=n_pct, y=profile,
             fill=purchasedWhen)) + 
  geom_col(color='black') +
  scale_fill_manual(values = palette_colors[1:length(unique(profile_purchasedWhen$profile))]) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x=NULL,
       y=NULL,
       fill=NULL) +
  theme(legend.position = 'bottom')

## 7 ----
ecomm.df |> glimpse()
ecomm.df |> count(behavAnySale)

completed_sales_gender <- ecomm.df |> 
  group_by(gender, profile) |> 
  summarise(behavAnySale = mean(behavAnySale)) |> 
  ungroup() |> 
  mutate(gender = na_if(gender, '')) |> 
  filter(behavAnySale != 0)

completed_sales_gender |>
  ggplot(aes(x=behavAnySale, y=gender,
             fill=profile)) +
                      # works without a grouping variable 
                      # in a layer 
  geom_col(position = position_dodge2(preserve = 'single'),
           color='black') +
  scale_fill_manual(values = palette_colors[1:length(unique(completed_sales_gender$profile))]) +
  guides(nrow=2) +
  labs(x='Percentage of complete sales',
       fill=NULL) +
  theme(legend.position = 'bottom')
