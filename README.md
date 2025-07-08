# aws-open-data-yaml

Creating yaml files to record NASA datasets in the AWS Open Data Registry (https://registry.opendata.aws/).

The specs for the yaml files are at https://github.com/awslabs/open-data-registry.

We are querying the [CMR collections API](https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html#umm-json) with the `umm_json` result format to get most of the metadata.

Tutorials are being recorded in a [Google Sheet](https://docs.google.com/spreadsheets/d/1ZqlYRvoZnLZIl5eOJ2gGUuu5E9K_P2c446RPHJ4B06w) and
queried using the `googlesheets4` package.
