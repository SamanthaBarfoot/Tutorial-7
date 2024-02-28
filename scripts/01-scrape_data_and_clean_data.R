#### Preamble ####
# Purpose: Scrape and clean data from https://en.wikipedia.org/wiki/List_of_prime_ministers_of_Australia
# Author: Rohan Alexander modified by Samantha Barfoot
# Date: 25 February 2024
# License: MIT
# code was taken from:
#   Alexander, R. (2023, July 27). Telling Stories with Data - 7 Gather data. 
#     Telling Stories With Data. https://tellingstorieswithdata.com/07-gather.html#exercises

### Libraries ###
#install.packages('janitor')
library(tidyverse)
library(babynames)
library(rvest)
library(xml2)
library(janitor)
library(knitr)

### Scrape Data ###
raw_data <-
  read_html(
    "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_Australia"
  )
write_html(raw_data, "pms.html")

raw_data <- read_html("pms.html")

parse_data_selector_gadget <-
  raw_data |>
  html_element(".wikitable") |>
  html_table()

parsed_data <-
  parse_data_selector_gadget |> 
  clean_names() |>
  rename(raw_text = name_birth_death_constituency) |>
  select(raw_text) |>
  filter(raw_text != "Name(Birth–Death)Constituency") |>
  distinct() 

initial_clean <-
  parsed_data |>
  separate(
    raw_text, into = c("name", "not_name"), sep = "\\(", extra = "merge",
  ) |> 
  mutate(date = str_extract(not_name, "[[:digit:]]{4}–[[:digit:]]{4}"),
         born = str_extract(not_name, "b.[[:space:]][[:digit:]]{4}")
  ) |>
  select(name, date, born)

cleaned_data <-
  initial_clean |>
  separate(date, into = c("birth", "died"), 
           sep = "–") |>   # PMs who have died have their birth and death years 
  # separated by a hyphen, but we need to be careful with the hyphen as it seems 
  # to be a slightly odd type of hyphen and we need to copy/paste it.
  mutate(
    born = str_remove_all(born, "b.[[:space:]]"),
    birth = if_else(!is.na(born), born, birth)
  ) |> # Alive PMs have slightly different format
  select(-born) |>
  rename(born = birth) |> 
  mutate(across(c(born, died), as.integer)) |> 
  mutate(Age_at_Death = died - born) |> 
  distinct() # Some of the PMs had two goes at it.

cleaned_data |>
  head() |>
  kable(
    col.names = c("Prime Minister", "Birth year", "Death year", "Age at death")
  )

write_csv(
  x = cleaned_data,
  file = "outputs/data/cleaned_data.csv"
)