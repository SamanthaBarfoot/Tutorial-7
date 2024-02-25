### Libraries ###
#install.packages('janitor')
library(tidyverse)
library(babynames)
library(rvest)
library(xml2)
library(janitor)
library(knitr)

set.seed(853)

### Simulate ####
simulated_dataset <-
  tibble(
    prime_minister = babynames |>
      filter(prop > 0.01) |>
      distinct(name) |>
      unlist() |>
      sample(size = 10, replace = FALSE),
    birth_year = sample(1700:1990, size = 10, replace = TRUE),
    years_lived = sample(50:100, size = 10, replace = TRUE),
    death_year = birth_year + years_lived
  ) |>
  select(prime_minister, birth_year, death_year, years_lived) |>
  arrange(birth_year)

simulated_dataset

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
  file = "cleaned_data.csv"
)

# ### PLOT ###
# cleaned_data |>
#   mutate(
#     still_alive = if_else(is.na(died), "Yes", "No"),
#     died = if_else(is.na(died), as.integer(2024), died)
#   ) |>
#   mutate(name = as_factor(name)) |>
#   ggplot(
#     aes(x = born, xend = died, y = name, yend = name, color = still_alive)
#   ) +
#   geom_segment() +
#   labs(
#     x = "Year of birth", y = "Prime minister", color = "PM is currently alive"
#   ) +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +
#   theme(legend.position = "bottom")