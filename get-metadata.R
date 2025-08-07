library(googlesheets4)
library(readr)
library(dplyr)
library(devtools)

devtools::load_all()

gs4_auth(email = "andy@openscapes.org")

tutorials_df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1ZqlYRvoZnLZIl5eOJ2gGUuu5E9K_P2c446RPHJ4B06w",
  col_names = TRUE
)

## One test

meta <- get_metadata("GPM_3IMERGDE")
meta <- get_metadata("MUR-JPL-L4-GLOB-v4.1")

write_nasa_aws_yaml(
  "MUR-JPL-L4-GLOB-v4.1",
  tutorials_df,
  "yaml"
)

## Top 50
top_dist_datasets <- read_csv("top_dist.csv") |>
  pull("Short Name")

for (shortname in top_dist_datasets) {
  write_nasa_aws_yaml(
    shortname,
    tutorials_df,
    file.path("yaml", "nasa-top-dist")
  )
}

## First batch from NASA
first_batch <- read_csv("first_batch.csv") |>
  pull("Short Name")

for (shortname in first_batch) {
  write_nasa_aws_yaml(
    shortname,
    tutorials_df,
    file.path("yaml", "nasa-first-batch")
  )
}

## Test DOIs:

for (f in list.files(
  "yaml",
  pattern = "*.yaml",
  recursive = TRUE,
  full.names = TRUE
)) {
  url <- yaml::read_yaml(f)$Documentation

  is_error <- tryCatch(
    httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform() |>
      httr2::resp_is_error(),
    error = function(e) {
      warning(paste0(url, ": ", e$message))
      TRUE
    }
  )

  if (is_error) {
    cat(paste("Broken DOI link in", f, ":", url, "\n"))
  }
}
