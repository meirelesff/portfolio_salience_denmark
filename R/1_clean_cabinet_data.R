# ---
# _CLEAN CABINET DATA (WHOGOV DATASET)
# ---


# Packages and functions
library(tidyverse)
library(here)

source(here("R", "_utils.R"))


# Load main data
whogov <- read_csv(here("raw-data", "csv", "WhoGov_within_V3.1.csv"), show_col_types = FALSE) 


# Filter and select columns
cabinet <- whogov %>%
    filter(country_name == "Denmark") %>%
    filter(year %in% 2007:2016) %>%
    filter(minister == 1) %>%
    select(year, ministry = position, party = party_english)


# Recode variables
cabinet <- cabinet %>%
    mutate(ministry = link_ministries_whogov(ministry)) %>%
    # Recode parties
    mutate(party = case_when(
        party == "Conservative People's Party" ~ "Conservative Party",
        party == "Social Democrats" ~ "Social Democratic Party",
        party == "Socialist Peoples Party" ~ "Socialist People’s Party",
        party == "Danish Social-Liberal Party / Radical Liberal Party" ~ "Social Liberal Party",
        TRUE ~ party
    )) %>%
    mutate(occupied = 1)


# Save data
save(cabinet, file = here("data", "denmark_cabinet.Rda"))
rm(list = ls())
