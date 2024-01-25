# Code for: R for Marketing Research and Analytics, 2nd ed: Chapter 4

# Libraries ----
library(package = tidyverse)
library(package = skimr)

# Chapter 5

# optional instead of creating data:
# consumer_segmentation <- read_csv(paste("http://r-marketing.r-forge.r-project.org/",
#                                   "data/rintro-chapter5.csv", sep=""))
# OR
# segmentation <- read_csv("http://goo.gl/qw303p")


#### quick alternative to load the data
#### ... but better is to create the data as below
segmentation <- read_csv("http://goo.gl/qw303p")
skim(data = segmentation)

#### Create the data

# Specifies and names the variables to create
segVars <- c("age", "gender", "income", "kids", "ownHome", "subscribe")

# What kind of data will be present in each of those variables: 
# normal data (continuous), binomial (yes/no), or Poisson (counts).
segVarType <- c("norm", "binom", "norm", "pois", "binom", "binom")

# Names of the segments
segNames <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")

# Number of observations to generate in each segment
segSize <- c(100, 50, 80, 70)

# There are four segments and six demographic variables, so we 
# create a  4 × 6 matrix to hold the mean of each.
## How does this work? It specifies, for example, that the first 
## variable (which we defined above as age) will have a mean of 
## 40 for the first segment: (age, Suburb mix) will have a mean 
## of 40
segMeans <- matrix( c(
  40, .5, 55000, 2, .5, .1,
  24, .7, 21000, 1, .2, .2,
  58, .5, 64000, 0, .7, .05,
  36, .3, 52000, 2, .3, .2  ), ncol=length(segVars), byrow=TRUE)

# The standard deviations for each segment (NA = not applicable 
# for the variable). 
## In the case of binomial and Poisson variables, we only need to 
## specify the mean.
segSDs <- matrix( c(
  5, NA, 12000, NA, NA, NA,
  2, NA,  5000, NA, NA, NA,
  8, NA, 21000, NA, NA, NA,
  4, NA, 10000, NA, NA, NA  ), ncol=length(segVars), byrow=TRUE)

# Initialize the data.frame
seg.df <- NULL
set.seed(seed = 02554)

# The base R way
for (i in seq_along(segNames)) {
  # To see that the code is working and to show progress,
  # we use
  cat(i, segNames[i], '\n')
  
  # empty matrix to hold this particular segment’s data
  ## Whenever R grows an object in memory—such as adding 
  ## a row—it makes a copy of the object. This uses twice 
  ## the memory and slows things down; by preallocating,
  ## we avoid that.
  ## Also by filling temporary and placeholder objects 
  ## with missing values (NA) instead of 0 or blank values, 
  ## we add another layer of error-checking: if we describe() 
  ## the object and discover missing values where we expect 
  ## data, we know there is a code error.
  this.seg <- data.frame(matrix(NA, 
                                nrow = segSize[i], ncol = length(segVars)))
  # within a segment, iterate over the variables and draw appropriate random data
  for (j in seq_along(segVars)) { # and iterate over each variable
    if (segVarType[j] == 'norm') { # draw random normals
      this.seg[,j] <- rnorm(n = segSize[i], mean = segMeans[i,j], sd = segSDs[i,j]) 
    } else if (segVarType[j] == 'pois') { # draw counts
      this.seg[,j] <- rpois(n = segSize[i], lambda = segMeans[i,j])
    } else if (segVarType[j] == 'binom') { # draw binomials
      this.seg[,j] <- rbinom(n = segSize[i], size = 1, prob = segMeans[i,j])
    } else {
      # if a result doesn’t fit into the data frame where it should
      # fit, we will get a warning or error
      stop(call. = "Bad segment data type: ", domain = segVarType[j])
    }
  }
  seg.df <- rbind(seg.df, this.seg)
}

## We have not added the segment
seg.df |> head()

## To finish up the data set, we perform a few housekeeping 
## tasks: we name the columns, add segment membership, and 
## convert each binomial variable to a labeled factor
names(seg.df) <- segVars
seg.df |> head()

# add segment membership for each row
seg.df$Segment <- factor(x = rep.int(x = segNames, times = segSize))
summary(seg.df)

# convert the binomial variables to nicely labeled factors
seg.df$ownHome <- factor(x = seg.df$ownHome, labels = c("ownNo", "ownYes"))
seg.df$gender <- factor(x = seg.df$gender, labels=c("Female", "Male"))
seg.df$subscribe <- factor(x = seg.df$subscribe, labels=c("subNo", "subYes"))

seg.df |> head()
summary(seg.df)

# The data frame is now suitable for exploration
