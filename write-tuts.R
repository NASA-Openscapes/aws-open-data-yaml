library(tidyverse)
library(fs)

tutorials_df <- dir_ls("yaml", glob = "*.yaml", recurse = TRUE) |>
  map(\(x) {
    yaml <- yaml::read_yaml(x)
    shortname <- gsub("nasa-", "", path_ext_remove(path_file(x)))
    tuts <- pluck(yaml, "DataAtWork", "Tutorials")
    if (is.null(tuts) || length(tuts) == 0 || is.atomic(tuts)) {
      return(NULL)
    }

    map(tuts, \(x) {
      as_tibble(x) |>
        mutate(shortname = shortname, .before = 1)
    })
  }) |>
  compact() |>
  bind_rows()
