library(httr2)
library(purrr)
library(yaml)
library(googlesheets4)
library(dplyr)
library(slugify)
library(readr)

######

get_tutorials <- function(tutorials_df, shortname) {
  tutorials <- tutorials_df |>
    filter(.data$shortname == .env$shortname) |>
    select(-shortname)

  # Convert to a list of lists
  ret <- apply(tutorials, 1, as.list)

  # Remove any NA elements
  ret <- map(ret, \(x) {
    keep(x, \(x) !is.na(x))
  }) |>
    compact()

  if (length(ret) == 0) {
    return(NULL)
  }
  ret
}

get_metadata <- function(
  shortname,
  base_url = "https://cmr.earthdata.nasa.gov/search/collections.umm_json"
) {
  resp <- request(base_url) |>
    req_url_query(
      short_name = shortname,
      "options[short_name][ignore_case]" = TRUE,
      page_size = 1,
      cloud_hosted = TRUE
    ) |>
    req_perform()

  items <- resp_body_json(resp)$items
  if (is.null(items) || length(items) == 0) {
    warning(paste("No metadata found for shortname:", shortname), call. = FALSE)
    return(NULL)
  }
  items[[1]]
}

get_update_frequency <- function(umm) {
  if (is.null(umm$TemporalExtents) || length(umm$TemporalExtents) == 0) {
    return("")
  }

  start_date <- umm$TemporalExtents[[1]]$RangeDateTimes[[1]]$BeginningDateTime
  end_date <- umm$TemporalExtents[[1]]$RangeDateTimes[[1]]$EndingDateTime

  ret <- paste0(
    "From ",
    format(as.POSIXct(start_date), "%Y-%m-%d"),
    " to ",
    if (is.null(end_date)) {
      "Ongoing"
    } else {
      format(as.POSIXct(end_date), "%Y-%m-%d")
    }
  )

  if (!is.null(umm$TemporalKeywords)) {
    temp_kw <- trimws(unlist(umm$TemporalKeywords))
    ret <- paste0(ret, " (", temp_kw, ")")
  }
  ret
}

get_tags <- function(umm) {
  tags <- unique(unname(c(
    trimws(unlist(strsplit(unlist(umm$ScienceKeywords) %||% "", ","))),
    trimws(unlist(strsplit(unlist(umm$AncillaryKeywords) %||% "", ","))),
    get_format(umm)
  )))

  aws_tags <- aws_tags()

  not_valid_tags <- setdiff(
    tolower(tags),
    tolower(aws_tags)
  )

  if (length(not_valid_tags) > 0) {
    update_tag_counts(not_valid_tags, "unlisted_tags.csv")
  }

  c("aws-pds", aws_tags[tolower(aws_tags) %in% tolower(tags)])
}

update_tag_counts <- function(tags, csv_file) {
  # Read existing CSV or create empty data frame if file doesn't exist
  if (file.exists(csv_file)) {
    tag_df <- read_csv(
      csv_file,
      col_types = cols(tag = col_character(), n = col_integer())
    )
  } else {
    tag_df <- data.frame(tag = character(0), n = integer(0))
  }

  for (tag in tags) {
    if (tag %in% tag_df$tag) {
      # Increment existing tag count
      tag_df$n[tag_df$tag == tag] <- tag_df$n[tag_df$tag == tag] + 1
    } else {
      # Add new tag with count of 1
      tag_df <- bind_rows(tag_df, data.frame(tag = tag, n = 1L))
    }
  }

  # Write updated data frame back to CSV
  write_csv(tag_df, csv_file, append = FALSE)
}

get_contact_info <- function(umm) {
  contact_groups <- compact(map(umm$DataCenters, \(x) x[["ContactGroups"]]))

  cgs <- map_chr(contact_groups, \(x) {
    group_name <- x[[1]]$GroupName
    email <- map_chr(
      keep(x[[1]]$ContactInformation$ContactMechanisms, \(y) y$Type == "Email"),
      "Value"
    )

    # url <- keep(
    #   x[[1]]$ContactInformation$RelatedUrls,
    #   \(y) y$URL[y$URLContentType == "DataContactURL"]
    # )
    if (is.null(email) || length(email) == 0 || email == "") {
      return("")
    }

    paste0(
      group_name,
      ": ",
      email
      # ". ",
      # tools::toTitleCase(tolower(url$Type)),
      # ": ",
      # url$Value
    )
  })

  if (length(cgs) == 0) {
    return(NULL)
  }
  paste0(unique(cgs), collapse = "\n")
}

get_resources <- function(umm) {
  if (
    is.null(umm$DirectDistributionInformation) ||
      length(umm$DirectDistributionInformation) == 0
  ) {
    stop("No Direct Distribution Information found in UMM.", call. = )
  }

  bucket <- umm$DirectDistributionInformation$S3BucketAndObjectPrefixNames
  bucket_prot <- grep("protected", bucket, value = TRUE)
  if (length(bucket_prot) == 0) {
    bucket <- bucket[[1]]
  } else {
    bucket <- bucket_prot
  }

  ret <- list(
    list(
      Description = umm$EntryTitle,
      ARN = paste0(
        "arn:aws:s3:::",
        gsub("s3://", "", bucket)
      ),
      Region = umm$DirectDistributionInformation$Region,
      Type = "S3 Bucket",
      RequesterPays = FALSE,
      ControlledAccess = umm$DirectDistributionInformation$S3CredentialsAPIEndpoint
    )
  )
}

get_publications <- function(umm) {
  if (
    is.null(umm$PublicationReferences) || length(umm$PublicationReferences) == 0
  ) {
    return(NULL)
  }

  map(umm$PublicationReferences, \(pub) {
    doi <- pub$DOI[[1]]
    url <- if (!is.null(doi) && !grepl("^NASA/.+", doi)) {
      if (!grepl("^https?://", doi)) {
        paste0("https://doi.org/", doi)
      } else {
        doi
      }
    } else {
      pub$URL
    }

    compact(list(
      Title = pub$Title,
      URL = url,
      AuthorName = pub$Author
    ))
  })
}

get_doi <- function(umm) {
  paste0(umm$DOI$Authority %||% "https://doi.org", "/", umm$DOI$DOI)
}

get_format <- function(umm) {
  format <- map_chr(
    umm$ArchiveAndDistributionInformation$FileDistributionInformation,
    \(x) x$Format
  )

  if (is.null(format) || length(format) == 0) {
    format <- map_chr(
      umm$ArchiveAndDistributionInformation$FileArchiveInformation,
      \(x) x$Format
    )
  }

  if (is.null(format) || length(format) == 0) {
    return(NULL)
  }

  format <- tolower(format) |>
    stringr::str_replace_all("netcdf-?4", "netcdf") |>
    stringr::str_replace_all("(hdf5?)", "\\1") |>
    stringr::str_replace_all("cloud optimized geotiff", "cog") |>
    stringr::str_replace_all("geotiff", "tiff")

  format <- stringr::str_split(
    format,
    stringr::boundary("word")
  ) |>
    unlist() |>
    trimws()

  # Match against AWS tags
  aws_tags <- aws_tags()
  unique(aws_tags[tolower(aws_tags) %in% format])
}

write_nasa_aws_yaml <- function(shortname, tutorials_df, dir) {
  metadata <- get_metadata(shortname)

  if (is.null(metadata)) {
    warning(paste("Skipping", shortname, "due to missing metadata."))
    return(NULL)
  }

  meta <- metadata$meta
  umm <- metadata$umm

  yaml_data <- list(
    Name = umm$EntryTitle,
    Description = paste0(
      trimws(gsub("(\\n){2,}", "\n\n", umm$Abstract)),
      "\nRead our doc on how to get AWS Credentials to retrieve this data: ",
      umm$DirectDistributionInformation$S3CredentialsAPIDocumentationURL
    ),
    Documentation = get_doi(umm),
    Contact = get_contact_info(umm),
    ManagedBy = "NASA",
    UpdateFrequency = get_update_frequency(umm),
    Tags = get_tags(umm),
    License = "[Creative Commons BY 4.0](https://creativecommons.org/licenses/by/4.0/)",
    Resources = get_resources(umm),
    ## Tutorials, publications
    DataAtWork = compact(list(
      Publications = get_publications(umm),
      Tutorials = get_tutorials(tutorials_df, shortname)
    ))
  )

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  yaml::write_yaml(
    compact(yaml_data),
    file = file.path(
      dir,
      paste0("nasa-", slugify(shortname), ".yaml")
    ),
    indent.mapping.sequence = TRUE,
    handlers = list(logical = verbatim_logical, character = trimws)
  )
}

aws_tags <- function() {
  read_yaml(
    "https://raw.githubusercontent.com/awslabs/open-data-registry/refs/heads/main/tags.yaml"
  )
}

### Main script to fetch metadata and write YAML file

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
