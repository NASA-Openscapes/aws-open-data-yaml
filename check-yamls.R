## Check long names and manually edit:
sapply(
  list.files("yaml", pattern = "*.yaml", recursive = TRUE, full.names = TRUE),
  \(x) {
    f <- yaml::read_yaml(x)
    nchar(f$Name)
  }
) |>
  Filter(\(x) x > 130, x = _)

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
