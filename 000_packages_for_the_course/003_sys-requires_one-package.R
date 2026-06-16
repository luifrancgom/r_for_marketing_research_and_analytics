# Load packages ----
library(httr2)

# Documentation
# Posit Package Manager offers a Swagger UI
# https://packagemanager.posit.co/__api__/swagger/index.html
# GET /repos/{repo}/packages/{name}/sysreqs Fetches lists of system requirements by package.

# Base URL
base_url <- "https://packagemanager.rstudio.com/__api__"

# Path parameters
repo <- "cran"
name <- "sf"

# Query parameters
distribution <- "redhat"
release <- "10"

# Build the request step by step
req <- request(base_url) |> # base URL
  req_url_path_append("repos", repo) |> # /repos/cran
  req_url_path_append("packages", name) |> # /packages/sf
  req_url_path_append("sysreqs") |> # /sysreqs
  req_url_query(
    # ?distribution=redhat&release=10
    distribution = distribution,
    release = release
  )

# Check the final URL before performing the request
req
