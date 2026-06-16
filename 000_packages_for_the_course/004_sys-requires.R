# Load packages ----
library(httr2)
library(purrr)
library(tibble)

# Custom function ----
get_pkg_sysreqs <- function(
  name,
  repo = "cran",
  distribution = "redhat",
  release = "10"
) {
  # Build the request
  req <- request(base_url) |>
    req_url_path_append("repos", repo) |>
    req_url_path_append("packages", name) |>
    req_url_path_append("sysreqs") |>
    req_url_query(
      distribution = distribution,
      release = release
    )

  # Perform the request and parse
  sysreqs <- req |> req_perform() |> resp_body_json()

  # Extract commands
  package_commands <- extract_node(sysreqs)

  deps_commands <- sysreqs$dependencies |>
    map(extract_node) |>
    keep(~ !is.null(.x)) |>
    unlist() |>
    unique()

  # Return a tibble row
  tibble(
    package = name,
    sysreqs = list(c(package_commands, deps_commands) |> unique())
  )
}

# Test function ----
get_pkg_sysreqs(name = "tidymodels")

# Generalize ----
## Packages ----
packages <- c(
  ## Chapman & Feit (2019) ----
  ### Chapter 2 ----
  "lavaan",
  "semPlot",
  "corrplot",
  "multcomp",
  ### Chapter 3 ----
  "car",
  "psych",
  "beanplot",
  ### Chapter 4 ----
  "gplots",
  ### Chapter 5 ----
  "lattice",
  ### Chapter 6 ----
  "binom",
  ### Chapter 7 ----
  "coefplot",
  ### Chapter 8 ----
  "RColorBrewer",
  "cluster",
  ## Tidyverse approach ----
  ### Data science ----
  "tidyverse",
  ### Tidying ----
  "janitor",
  ### Modeling and machine learning ----
  "tidymodels",
  "corrr",
  "hardhat",
  #### For using n_clusters
  "easystats", # for checking linear model assumptions
  "NbClust",
  "mclust",
  "factoextra",
  ### Visualization ----
  "dotwhisker",
  "tidyheatmaps",
  "ggbiplot",
  "ggforce",
  "ggrepel",
  "patchwork",
  ### Summary statistics ----
  "skimr",
  ### Prepare REPRoducible EXamples ----
  "reprex",
  ### Tables ----
  #### HTML tables ----
  "DT",
  "gt",
  ### Data: bike_sales
  "sweep",
  ### Images ----
  "imager",
  ### Quarto ----
  "knitr",
  "rmarkdown",
  ### Maps ----
  "tigris",
  "sf",
  ### LaTeX ----
  "latex2exp",
  "tinytex",
  ## Python ----
  "reticulate"
)

## Tibble system dependencies ----
sysreqs_tbl <- packages |>
  map(get_pkg_sysreqs) |>
  list_rbind()

## Extract system dependencies ----
# Observation
# Check if preinstall is already installed
# For example
# subscription-manager repos --enable codeready-builder-for-rhel-10-$(arch)-rpms
# dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-10.noarch.rpm
sysreqs_vec <- sysreqs_tbl$sysreqs |>
  discard(is.null) |>
  flatten_chr() |>
  unique()
