#### Preamble ####
# Purpose: Cleans the raw data compiled by the original papers researchers
# Author: Rohan Alexander
# Date: 25 February 2024
# Contact: samantha.barfoot@mail.utoronto.ca
# License: MIT
# code was taken from:
#   Alexander, R. (2023, July 27). Telling Stories with Data - 7 Gather data. 
#     Telling Stories With Data. https://tellingstorieswithdata.com/07-gather.html#exercises

# Sets seed
set.seed(853)

# creates table to simulate data
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