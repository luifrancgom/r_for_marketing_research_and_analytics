# Inspecting source code
## Check the code of 
### ggbiplot::get_SVD
### ggbiplot::ggbiplot
#### https://github.com/friendly/ggbiplot/blob/master/R/ggbiplot.r
#### https://github.com/friendly/ggbiplot/blob/master/R/get_SVD.R
### stats:::biplot.prcomp
#### https://github.com/r-devel/r-svn > src
#### > library > stats > R > biplot.R >
#### biplot.prcomp
#### biplot.default

# Libraries
library(tidyverse)
library(ggbiplot)

# Data ----
consumer_brand <- read_csv("http://goo.gl/IQl8nc")
consumer_brand |> head(n = 5)

set.seed(seed = 1234)
consumer_brand_sample <- consumer_brand |>
  slice_sample(n = 1, by = brand) |> 
  select(brand, perform, leader)
consumer_brand_sample

consumer_brand_sample_matrix <- consumer_brand_sample |> 
  select(-brand) |>
  as.matrix()

consumer_brand_sample_matrix_center_scale <- consumer_brand_sample_matrix |>
  scale(center = TRUE, scale = TRUE)

consumer_brand_sample_matrix_pca <- consumer_brand_sample_matrix |> 
  prcomp(center = TRUE, scale. = TRUE)
consumer_brand_sample_matrix_pca

# SVD components from a PCA-like object ----
pcobj <- consumer_brand_sample_matrix_pca
n <- nrow(pcobj$x)
D <- pcobj$sdev
D_matrix <- diag(D)
# Scores
## Correction
U <- sweep(pcobj$x, 2, 1 / (D * sqrt(n)), FUN = '*')
## Without correction: ## pcobj$x
U_not_corrected <- pcobj$x 
# Loadings
V <- pcobj$rotation

# Parameters for biplot ----
nobs.factor <- ifelse (TRUE, sqrt(n-1), sqrt(n))
choices <- 1:2
scale <- 1
obs.scale <- 1 - scale
alpha <- 1
point.size <- 1.5
var.factor <- 1
var.scale <- scale
circle.prob <- 0.68
varname.color <- "black"

# U matrix ----
df.u <- as.data.frame(sweep(U[,choices], 2, 
                            D[choices]^obs.scale, 
                            FUN='*'))
names(df.u) <- c('xvar', 'yvar')
df.u$groups <- consumer_brand_sample$brand

# V matrix ----
v <- sweep(v, 2, D^var.scale, FUN='*')
df.v <- as.data.frame(v[, choices])
df.v <- var.factor * df.v
names(df.v) <- names(df.u)[choices]
r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u[, 1:2]^2))^(1/4)
v.scale <- rowSums(v^2)
df.v <- r * df.v / sqrt(max(v.scale))


# Visualization ----
g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
  coord_equal()
g <- g + geom_point(aes(color = groups), 
                    alpha = alpha, 
                    size = point.size)
arrow_style <- arrow(length = unit(1/2, 'picas'), 
                     type="closed", angle=15) 
g <- g +
  geom_segment(data = df.v,
               aes(x = 0, y = 0, xend = xvar, yend = yvar),
               arrow = arrow_style, 
               color = varname.color,
               linewidth = 1.4)

# Check results ----
## ggbiplot ----
g2 <- ggbiplot(pcobj = consumer_brand_sample_matrix_pca,
         groups = consumer_brand_sample$brand,
         scale = 1, pc.biplot = FALSE) +
  labs(color = "Brands")

ggplot_build(g2)$data[[1]]
ggplot_build(g2)$data[[2]]

## stats:::biplot.prcomp ----
biplot(x = consumer_brand_sample_matrix_pca,
       choices = 1:2, 
       scale = 1, 
       pc.biplot = FALSE)

scale <- 1
choices <- 1:2
x <- consumer_brand_sample_matrix_pca
scores <- x$x
lam <- x$sdev[choices]
n <- NROW(scores)
lam <- lam * sqrt(n)
if (scale != 0) lam <- lam^scale else lam <- 1

axis_x <- t(t(scores[, choices]) / lam)
axis_y <- t(t(x$rotation[, choices]) * lam)

# Ask about the double axis
## https://stats.stackexchange.com/questions/66926/what-are-the-four-axes-on-pca-biplot
## https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
biplot(axis_x, axis_y)
