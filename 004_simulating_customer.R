# Code for: R for Marketing Research and Analytics, 2nd ed: Chapter 4
# set random number generator to use pre R 3.6 method, to match book
# this affects data simulation and Bayesian methods, which use randomization
# ==> Run this line:
# if (getRversion() >= "3.6.0") suppressWarnings(RNGversion("3.5.0"))
# you could always change back to current with: 
#   RNGversion(getRversion())

# Libraries ----
library(package = tidyverse)
library(package = skimr)

# Chapter 4

# optional instead of creating data:
# customer <- read_csv(paste("http://r-marketing.r-forge.r-project.org/",
#                            "data/rintro-chapter4.csv", sep=""))
# OR
# customer <- read_csv("http://goo.gl/PmPkaG")
#
# and then:
# customer$customer_id <- factor(customer$customer_id)

# set random seed to make the random sequences replicable
set.seed(seed = 21821, 
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion", 
         ## "Rounding" was the default in versions prior to 
         ## 3.6.0: it made sample noticeably non-uniform on
         ## large populations, and should only be used for 
         ## reproduction of old results
         sample.kind = "Rounding")

# Create the data ----

# Number of customers
ncust <- 1000
customer <- tibble(customer_id = factor(x = 1:ncust, ordered = FALSE))

customer <- customer |>
  mutate(age = rnorm(n=ncust, mean=35, sd=5),
         credit_score = rnorm(n=ncust, mean=3*age + 620, sd=50),
         email = factor(x = sample(c("yes", "no"), size = ncust, replace = TRUE, 
                               prob = c(0.8, 0.2)), ordered = FALSE),
         distance_to_store = exp(x = rnorm(n = ncust, mean = 2, sd = 1.2)))

customer |> glimpse()
customer |> skim()

# Try it!: customer |> 
#           ggplot() + 
#           geom_histogram(aes(x = distance_to_store, y = after_stat(density)), 
#                          color = "black", boundary = 0)

customer <- customer |>
  mutate(online_visits = rnbinom(n = ncust, size = 0.3, 
                                 mu = 15 + ifelse(test = email == "yes", yes = 15, no = 0) 
                                      - 0.7 * (age - median(age))),
         online_trans = rbinom(n = ncust, size = online_visits, prob = 0.3),
         online_spend = exp(rnorm(n = ncust, mean=3, sd=0.1)) * online_trans)

customer |> glimpse()

customer <- customer |>
  mutate(store_trans = rnbinom(n = ncust, size = 5, 
                               mu = 3 / sqrt(distance_to_store)),
         store_spend = exp(rnorm(n = ncust, mean = 3.5, sd = 0.4)) * store_trans)

customer |> glimpse()
customer |> skim()

# Satisfaction Survey Responses ----
sat_overall <- rnorm(n = ncust, mean = 3.1, sd = 0.7)
sat_overall |> skim()

sat_service <- floor(sat_overall + rnorm(n = ncust, mean = 0.5, sd = 0.4))
sat_selection <- floor(sat_overall + rnorm(n = ncust, mean = -0.2, sd = 0.6))

customer <- customer |>
  mutate(sat_service = sat_service, 
         sat_selection = sat_selection)

customer |>
  select(sat_service, sat_selection) |>
  skim()

customer <- customer |>
  mutate(sat_service = case_when(
            sat_service > 5 ~ 5,
            sat_service < 1 ~ 1,
            .default = sat_service),
         sat_selection = case_when(
            sat_selection > 5 ~ 5,
            sat_selection < 1 ~ 1,
            .default = sat_selection),
         no_response = as.logical(rbinom(n = ncust, size = 1, prob = age / 100)))

customer |> glimpse()

# Simulating Non-response Data ----
customer <- customer |>
  mutate(sat_service = if_else(no_response == TRUE, 
                               true = NA, false = sat_service),
         sat_selection = if_else(no_response == TRUE, 
                                 true = NA, false = sat_selection)) |>
  select(-c(no_response))

customer |> glimpse()

# Clean up
rm(ncust, sat_overall, sat_service, sat_selection)

# Visualization ----

# Basic scatterplot

## Tidyverse way
customer |>
  ggplot() +
  geom_point(aes(x = age, y = credit_score))

customer |>
  ggplot() +
  geom_point(aes(x = age, y = credit_score),
             fill = "blue", color = "black", shape = 21) +
  geom_hline(yintercept = mean(customer$credit_score),
             color = "darkblue", linetype = "dotted") + 
  geom_vline(xintercept = mean(customer$age),
             color = "darkblue", linetype = "dotted") +
  scale_x_continuous(limits = c(15, 55)) +
  scale_y_continuous(limits = c(500, 900)) +
  labs(x = "Customer Age (years)", 
       y = "Customer Credit Score",
       title = "Active Customers as of June 2014")

## Base R way
plot(customer$age, customer$credit_score)

plot(customer$age, customer$credit_score, 
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900), 
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Customer Credit Score")
abline(h=mean(customer$credit_score), col="dark blue", lty="dotted")
abline(v=mean(customer$age), col="dark blue", lty="dotted")

# Tidyverse way
## Scatterplot with skewed variables and color-coded factors 
customer |> ggplot() +
  geom_point(aes(x = store.spend, y = online.spend, 
                 color = email, shape = email))

customer |> ggplot() +
  geom_point(aes(x = store.spend, y = online.spend, 
                 color = email, shape = email)) +
  scale_color_manual(values = c("black", "green3"), 
                     labels = str_c('email on file: ', levels(customer$email))) +
  scale_shape_manual(values = c(1, 19), 
                     labels = str_c('email on file: ', levels(customer$email)))

customer |> ggplot() +
  geom_point(aes(x = store.spend, y = online.spend, 
                 color = email, shape = email)) +
  scale_color_manual(values = c("black", "green3"), 
                     labels = str_c('email on file: ', levels(customer$email))) +
  scale_shape_manual(values = c(1, 19), 
                     labels = str_c('email on file: ', levels(customer$email))) +
  scale_x_continuous(trans = "log1p", 
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 500)) + 
  scale_y_continuous(trans = "log1p", 
                     breaks = c(1, 5, 50, 500))

customer |> ggplot() +
  geom_point(aes(x = store.spend, y = online.spend, 
                 color = email, shape = email)) +
  scale_color_manual(values = c("black", "green3"), 
                     labels = str_c('email on file: ', levels(customer$email))) +
  scale_shape_manual(values = c(1, 19), 
                     labels = str_c('email on file: ', levels(customer$email))) +
  scale_x_continuous(trans = "log1p", 
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 500)) + 
  scale_y_continuous(trans = "log1p", 
                     breaks = c(1, 5, 50, 500)) +
  labs(x = "Prior 12 months in-store sales ($)", 
       y = "Prior 12 months online sales ($)",
       title = "Customers as of June 2014")

# Base R way
## Scatterplot with skewed variables and color-coded factors
plot(customer$store_spend, customer$online_spend, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.7)

hist(customer$store_spend, 
     breaks=(0:ceiling(max(customer$store_spend)/10))*10,
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Count of customers")

my.col <- c("black", "green3")
my.pch <- c(1, 19) # R's symbols for solid and open circles (see ?points)

head(customer$email)
as.numeric(head(customer$email))
my.col[as.numeric(head(customer$email))]
my.col[head(customer$email)]

# Try it! (not shown in book)
plot(customer$store_spend, customer$online_spend,
     col=as.numeric(customer$email))

# first the basic plot
plot(customer$store_spend, customer$online_spend,
     cex=0.7,
     col=my.col[customer$email], pch=my.pch[customer$email], 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)")

# and then add a legend
legend(x="topright", legend=paste("email on file:", levels(customer$email)), 
       col=my.col, pch=my.pch)

# Try it!
# plot with logarithmic axes, shows better detail
plot(customer$store_spend + 1, customer$online_spend + 1,
     log="xy", cex=0.7,
     col=my.col[customer$email], pch=my.pch[customer$email],
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )
legend(x="topright", legend=paste("email on file:", levels(customer$email)), 
       col=my.col, pch=my.pch)

# Multi-panel plots
par(mfrow=c(2, 2))
plot(customer$distance_to_store, customer$store_spend, main="store")
plot(customer$distance_to_store, customer$online_spend, main="online")
plot(customer$distance_to_store, customer$store_spend+1, log="xy", 
     main="store, log")
plot(customer$distance_to_store, customer$online_spend+1, log="xy", 
     main="online, log")

# Scatterplot matrix
pairs(formula = ~ age + credit_score + email +
        distance_to_store + online_visits + online_trans + 
        online_spend + store_trans + store_spend,
      data=customer)

pairs(customer[ , c(2:10)])


library(car)   # install if needed: install.packages("car")
scatterplotMatrix(formula = ~ age + credit_score + email +
                    distance_to_store + online_visits + online_trans + 
                    online_spend + store_trans + store_spend, 
                  data=customer)

# Not in 2nd edition of book: gpairs package (was unstable at print time)
#
# Try it!: install "gpairs" package and then:
#    library(gpairs)
#    gpairs(cbind(as.numeric(customer$email), customer[,6:8]))
# 

# Correlations
cov(customer$age, customer$credit_score)

cor(customer$age, customer$credit_score)

cov(customer$age, customer$credit_score)/
  (sd(customer$age)*sd(customer$credit_score))

cor.test(customer$age, customer$credit_score)

# Correlation matrix
cor(customer[, c(2, 3, 5:12)])

# Try it!: 
#   cor(customer[,3:12], use="complete.obs")

library(corrplot)    # for correlation plot
library(gplots)      # for color interpolation

## NOTE: if plot is small and doesn't fill entire plot window in RStudio
##       reset the plot pane by clicking on the "broom" icon above the plot

corrplot.mixed(corr=cor(customer[ , c(2, 3, 5:12)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               upper.col = colorpanel(50, "red", "gray60", "blue4"))


# Transformations
set.seed(49931)
x <- runif(1000, min=-10, max=10)
cor(x, x^2)

# Try it!  :  plot(x,x^2)

cor(customer$distance_to_store, customer$store_spend)
cor(1/customer$distance_to_store, customer$store_spend)

cor(1/sqrt(customer$distance_to_store), customer$store_spend)

plot(customer$distance_to_store, customer$store_spend)

plot(1/sqrt(customer$distance_to_store), customer$store_spend)

library(car)
powerTransform(customer$distance_to_store)

lambda <- coef(powerTransform(1/customer$distance_to_store))
bcPower(customer$distance_to_store, lambda)


par(mfrow=c(1,2))
hist(customer$distance_to_store, 
     xlab="Distance to Nearest Store", ylab="Count of Customers", 
     main="Original Distribution")
hist(bcPower(customer$distance_to_store, lambda),
     xlab="Box-Cox Transform of Distance", ylab="Count of Customers", 
     main="Transformed Distribution")

powerTransform(customer$age)

l.dist  <- coef(powerTransform(customer$distance_to_store))
l.spend <- coef(powerTransform(customer$store_spend+1))

cor(bcPower(customer$distance_to_store, l.dist), 
    bcPower(customer$store_spend+1, l.spend))

# Polychoric correlations
plot(customer$sat_service, customer$sat_selection, 
     xlab="Customer Satisfaction with Service", 
     ylab="Customer Satisfaction with Selection", 
     main="Customers as of June 2014")

plot(jitter(customer$sat_service), jitter(customer$sat_selection), 
     xlab="Customer Satisfaction with Service", 
     ylab="Customer Satisfaction with Selection", 
     main="Customers as of June 2014")

resp <- !is.na(customer$sat_service)
cor(customer$sat_service[resp], customer$sat_selection[resp]) 

library(psych)
# note: will give a warning
polychoric(cbind(customer$sat_service[resp], 
                 customer$sat_selection[resp]))