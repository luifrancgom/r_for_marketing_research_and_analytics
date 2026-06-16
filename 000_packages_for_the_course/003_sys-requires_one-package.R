# Load packages ----
library(httr2)
library(purrr)
library(tibble)

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

# Perform the request
resp <- req |> req_perform()

# Parse the JSON response
sysreqs <- resp |> resp_body_json()


# Function to extract commands from any node
extract_node <- function(node) {
  c(
    if (!is.null(node$pre_install)) node$pre_install |> map_chr("command"),
    if (!is.null(node$install_scripts)) node$install_scripts |> unlist()
  )
}

# Package itself - only the relevant fields
package_commands <- extract_node(sysreqs)

# Dependencies
deps_commands <- sysreqs$dependencies |>
  map(extract_node) |>
  keep(~ !is.null(.x)) |>
  unlist() |>
  unique()

# Organize in a tibble
sysreqs_tbl <- tibble(
  package = sysreqs$name,
  sysreqs = list(
    c(package_commands, deps_commands) |> unique()
  )
)
