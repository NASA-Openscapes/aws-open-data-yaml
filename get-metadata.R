library(httr2)
library(purrr)
library(yaml)
library(googlesheets4)
library(dplyr)
library(slugify)

######

get_tutorials <- function(
  shortname,
  sheet = "https://docs.google.com/spreadsheets/d/1ZqlYRvoZnLZIl5eOJ2gGUuu5E9K_P2c446RPHJ4B06w",
  email = "andy@openscapes.org"
) {
  gs4_auth(email = email)

  tutorials <- read_sheet(
    sheet,
    col_names = TRUE
  ) |>
    filter(shortname == shortname) |>
    select(-shortname)

  # Convert to a list of lists
  ret <- apply(tutorials, 1, as.list)

  # Remove any NA elements
  ret <- map(ret, \(x) {
    keep(x, \(x) !is.na(x))
  })

  ret
}

get_metadata <- function(
  shortname,
  base_url = "https://cmr.earthdata.nasa.gov/search/collections.umm_json"
) {
  resp <- request(base_url) |>
    req_url_query(
      short_name = shortname,
      page_size = 1,
      cloud_hosted = TRUE
    ) |>
    req_perform()

  resp_body_json(resp)$items[[1]]
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
  as.list(unique(unname(c(
    "aws-pds",
    trimws(unlist(umm$ScienceKeywords)),
    trimws(unlist(umm$AncillaryKeywords))
  ))))
}

get_contact_info <- function(umm) {
  contact_groups <- compact(map(umm$DataCenters, \(x) x[["ContactGroups"]]))

  cgs <- map_chr(contact_groups, \(x) {
    group_name <- x[[1]]$GroupName

    email <- map_chr(
      x[[1]]$ContactInformation$ContactMechanisms,
      \(y) y$Value[y$Type == "Email"]
    )

    url_type <- map_chr(
      x[[1]]$ContactInformation$RelatedUrls,
      \(y) y$Type[y$URLContentType == "DataContactURL"]
    )

    url <- map_chr(
      x[[1]]$ContactInformation$RelatedUrls,
      \(y) y$URL[y$URLContentType == "DataContactURL"]
    )

    paste0(
      group_name,
      ": ",
      email,
      ". ",
      tools::toTitleCase(tolower(url_type)),
      ": ",
      url
    )
  })
  paste0(unique(cgs), collapse = "\n")
}

get_resources <- function(umm) {
  if (
    is.null(umm$DirectDistributionInformation) ||
      length(umm$DirectDistributionInformation) == 0
  ) {
    stop("No Direct Distribution Information found in UMM.", call. = )
  }

  list(
    list(
      Description = paste0(
        umm$EntryTitle,
        ". (Format: ",
        paste0(
          map_chr(
            umm$ArchiveAndDistributionInformation$FileDistributionInformation,
            \(x) x$Format
          ),
          collapse = ", "
        ),
        ")"
      ),
      ARN = paste0(
        "arn:aws:s3:::",
        grep(
          "protected",
          umm$DirectDistributionInformation$S3BucketAndObjectPrefixNames,
          value = TRUE
        )
      ),
      Region = umm$DirectDistributionInformation$Region,
      Type = "S3 Bucket",
      RequesterPays = FALSE,
      ControlledAccess = umm$DirectDistributionInformation$S3CredentialsAPIDocumentationURL
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
    compact(list(
      Title = pub$Title,
      URL = pub$DOI[[1]] %||% pub$URL,
      AuthorName = pub$Author
    ))
  })
}

### Main script to fetch metadata and write YAML file

shortname <- "MUR-JPL-L4-GLOB-v4.1"

metadata <- get_metadata(shortname)

meta <- metadata$meta
umm <- metadata$umm

yaml_data <- list(
  Name = umm$EntryTitle,
  Description = paste0(
    umm$Abstract,
    "\nRead our doc on how to get AWS Credentials to retrieve this data: https://data.lpdaac.earthdatacloud.nasa.gov/s3credentialsREADME"
  ),
  Documentation = paste0(
    "https://cmr.earthdata.nasa.gov/search/concepts/",
    meta[["concept-id"]],
    ".html"
  ),
  Contact = get_contact_info(umm),
  ManagedBy = "NASA",
  UpdateFrequency = get_update_frequency(umm),
  Tags = get_tags(umm),
  License = "[Creative Commons BY 4.0](https://creativecommons.org/licenses/by/4.0/)",
  Resources = get_resources(umm),
  ## Tutorials, publications
  DataAtWork = list(
    Publications = get_publications(umm),
    Tutorials = get_tutorials(shortname)
  )
)

write_yaml(
  yaml_data,
  file = file.path("yaml", paste0("nasa-", slugify(shortname), ".yaml")),
  indent.mapping.sequence = TRUE,
  handlers = list(logical = verbatim_logical, character = trimws)
)
