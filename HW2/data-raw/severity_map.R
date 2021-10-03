# code to prepare `severity_map` dataset goes here

## ---- make-severity-map ----

# Save the severity mapping table.
severity_map <- dplyr::tribble(
  ~status, ~value, ~code,
  # ---- | ----- | ------
  "severe", 1, 1,
  "not severe", 2, 0,
)

# Write *.csv file in data-raw/
readr::write_csv(severity_map, "data-raw/severity_map.csv")

# Save the prepared severity mapping tibble.
usethis::use_data(severity_map, overwrite = TRUE)
