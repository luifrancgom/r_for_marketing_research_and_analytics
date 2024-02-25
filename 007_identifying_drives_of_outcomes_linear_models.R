# Libraries ----
library(tidyverse)
library(GGally)
library(naniar)
library(correlationfunnel)
library(ggrepel)
library(tidymodels)
library(dotwhisker)

# Data sets ----
hotel_df <- read_csv(file = "https://goo.gl/oaWKgt")
## Outcome variable: satOverall
hotel_df |> glimpse()

### Satisfaction variables
#### tidyselect::starts_with()
#### See also tidyselect::matches
##### 18 variables
###### It uses a 7 point rating scale
hotel_df |> 
  select(matches(match = "^sat")) |> 
  glimpse()

### Other variables
hotel_df |> 
  select(!matches(match = "^sat")) |> 
  glimpse()

# Each observation refers to a client
# that had taken a survey or some data from
# the client is collected
hotel_df |> 
  select(matches(match = "^sat")) |> 
  ggpairs()

hotel_df |> 
  select(matches(match = "^sat")) |>
  summarise(across(everything(), 
                   list(min = min, max = max))) |> 
  pivot_longer(cols = everything()) |> 
  separate_wider_delim(cols = name, delim = "_",
                      names = c("variable", "summary")) |> 
  pivot_wider(id_cols = variable, 
              names_from = summary, 
              values_from = value)

# Check for na values
hotel_df |> 
  n_miss()

# Visualization ----

## Data prepare
cor_tbl <- hotel_df |> 
  select(where(is.numeric)) |>
  summarise(across(.cols = everything(), 
                   .fns = ~ cor(satOverall, .x))) |> 
  pivot_longer(cols = everything()) |> 
  mutate(name = fct_reorder(.f = name, .x = value))

## Plot
cor_tbl |> 
  ggplot(aes(x = value, y = name)) +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  geom_point(shape = 21, color = "black", fill = "#E31A1C") +
  scale_x_continuous(breaks = seq.int(from = -1, to = 1, by = 0.25),
                     limits = c(-1, 1))

### Apply the same process with one-hot-encoding
hotel_one_hot_tbl <- hotel_df |>
  recipe(formula = ~ .) |> 
  step_dummy(all_nominal(), one_hot = TRUE) |> 
  prep() |> 
  bake(new_data = NULL)

cor_one_hot_tbl <- hotel_one_hot_tbl |> 
  select(where(is.numeric)) |>
  summarise(across(.cols = everything(), 
                   ## Check if this is recommended
                   ## Taking into account that you are
                   ## changing a categorical variable
                   ## to numeric to find correlations
                   .fns = ~ cor(satOverall, .x))) |> 
  pivot_longer(cols = everything()) |> 
  mutate(name = fct_reorder(.f = name, .x = value))

cor_one_hot_tbl |> 
  ggplot(aes(x = value, y = name)) +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  geom_point(shape = 21, color = "black", fill = "#E31A1C") +
  scale_x_continuous(breaks = seq.int(from = -1, to = 1, by = 0.25),
                     limits = c(-1, 1))
    

# Examine distributions
hotel_df |> 
  select(!matches(match = "^sat") & where(is.numeric)) |> 
  pivot_longer(cols = everything()) |> 
  ggplot(aes(x = value)) + 
  geom_density() + 
  facet_wrap(facets = vars(name), scales = "free")

# Possible transformations in the case of
## avgFoodSpendPerNight
## distanceTraveled
## nightsStayed
(hotel_df$avgFoodSpendPerNight < 0.0009) |> sum() 
(hotel_df$distanceTraveled < 0) |> sum() 
(hotel_df$nightsStayed < 0) |> sum()

hotel_df |> 
  filter(avgFoodSpendPerNight < 0.01) |> 
  select(avgFoodSpendPerNight)

hotel_one_hot_tbl_log <- hotel_one_hot_tbl |> 
  mutate(across(.cols = c(avgFoodSpendPerNight,
                          distanceTraveled,
                          nightsStayed),
                .fns = log,
                .names = "log_{.col}")) |> 
  glimpse()

cor_one_hot_tbl_log <- hotel_one_hot_tbl_log |>
  select(where(is.numeric)) |>
  summarise(across(.cols = everything(), 
                   ## Check if this is recommended
                   ## Taking into account that you are
                   ## changing a categorical variable
                   ## to numeric to find correlations
                   .fns = ~ cor(satOverall, .x))) |> 
  pivot_longer(cols = everything()) |>
  filter(!is.na(value)) |> 
  mutate(name = fct_reorder(.f = name, .x = value))

cor_one_hot_tbl_log |> 
  ggplot(aes(x = value, y = name)) +
  geom_vline(xintercept = 0, linetype = "dotted") + 
  geom_point(shape = 21, color = "black", fill = "#E31A1C") +
  scale_x_continuous(breaks = seq.int(from = -1, to = 1, by = 0.25),
                     limits = c(-1, 1))

## Question 4 ----
hotel_df |> 
ggplot(aes(x = satOverall)) +
  geom_bar() + 
  scale_x_continuous(breaks = 1:7)

hotel_df |> 
  ggplot(aes(x = satOverall, y = satPerks)) +
  geom_point(position = position_jitter(width = NULL,
                                        height = 0.1,
                                        seed = 1234))
lm(data = hotel_df,
   formula = satOverall ~ satPerks) |> 
  tidy()

## Question 5 ----
lm(data = hotel_df,
   formula = satOverall ~ satPerks + satFrontStaff + satCity) |> 
  tidy()

## Question 6 ----
hotel_df |> 
  group_by(eliteStatus) |> 
  summarise(median = median(satRecognition))

### We are going to focus our strategy in
### the Gold and Platinum elite members
#### satRecognition: How satisfy you are in relation
#### to your status
hotel_df |> 
  filter(eliteStatus %in% c("Gold", "Platinum")) |> 
  lm(formula = satRecognition ~satFrontStaff + satCleanRoom + satPoints + satPerks, 
     data = _) |> 
  tidy()

## Question 7 ----
hotel_df |> 
  filter(eliteStatus %in% c("Gold", "Platinum")) |> 
  lm(formula = satRecognition ~satFrontStaff + satCleanRoom + satPoints + satPerks, 
     data = _) |> 
  tidy() |> 
  filter(p.value >= 0.05)
### Here we have a problem taking into 
### acouunt that the parameters is not 
### significant
hotel_df |> 
  filter(eliteStatus %in% c("Gold", "Platinum")) |> 
  lm(formula = satRecognition ~satFrontStaff + satCleanRoom + satPoints + satPerks, 
     data = _) |> 
  tidy() |> 
  dwplot(ci = 0.95, 
         vline = geom_vline(xintercept = 0,
                            linetype = "dotted"))
## Question 8 ----
### As you see in the case of question 7 
### the problem is that the parameter associated
### with this variable is not significant
hotel_df |>
  lm(formula = avgFoodSpendPerNight ~ eliteStatus + 
                                      # Satisfaction with 
                                      # food price
                                      satDiningPrice,
     data = _) |> 
  tidy()

## Question 9 ----
hotel_df |>
  lm(formula = avgFoodSpendPerNight ~ eliteStatus + 
       # Satisfaction with 
       # food price
       satDiningPrice,
     data = _) |> 
  tidy() |> 
  # The parameters related to eliteStatus
  # are not significant
  filter(p.value >= 0.05)

# Question 10 ----
## We find the second case where satisfied 
## diners spend more

# Question 11 ----
model_nightsStayed <- hotel_df |> 
  lm(formula = avgFoodSpendPerNight ~ nightsStayed,
     data = _)

model_nightsStayed |> tidy()
## Not a good model
### We need to find transformations
### There are many extreme values and 
### avgFoodSpendPerNight contains values near
### 0 so the log transformation is not possible
model_nightsStayed |> glance()

model_nightsStayed |> 
  augment() |> 
  select(avgFoodSpendPerNight, .fitted, nightsStayed) |> 
  pivot_longer(cols = c(avgFoodSpendPerNight, .fitted), 
               names_to = "variables", 
               values_to = "value") |>
  ggplot(aes(x=nightsStayed, y = value)) + 
  geom_smooth(data = model_nightsStayed,
              aes(x = nightsStayed, y = avgFoodSpendPerNight),
              method = "lm", se = FALSE,
              color = "black", linetype = "dotted") +
  geom_point(aes(fill = variables),
             shape = 21, color = "black", size = 2,
             position = position_jitter(width = 0.01)) + 
  scale_fill_manual(values = c("#2C3E50", "#E31A1C"),
                    labels = c("Fitted values", "Actual values")) + 
  labs(x = "Night Stayed",
       y = "Average Spend per Night",
       fill = NULL) + 
  theme(legend.position = "bottom")

# Question 12 ----
## The group that is different is 
## the Silver status
### However the model is not really good
#### The problem is with extreme values
#### and the values near zero in relation
#### to avgFoodSpendPerNight
hotel_df |>
  ggplot(aes(x = nightsStayed, y = avgFoodSpendPerNight,
             fill = eliteStatus)) + 
  geom_point(shape = 21, color = "black") +
  geom_smooth(aes(color = eliteStatus),
              method = "lm", se = FALSE) + 
  labs(color = NULL,
       fill = NULL) +
  theme(legend.position = "bottom")
