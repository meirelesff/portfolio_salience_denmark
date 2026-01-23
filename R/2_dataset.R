# ---
# _DATASET
# ---


# Packages and functions
library(tidyverse)
library(here)


# Load cleaned data
load(here("data", "denmark_cabinet.Rda"))
load(here("data", "denmark_panel.Rda"))


# Join cabinet data to panel data
denmark_panel <- denmark_panel %>%
  left_join(cabinet, by = c("year", "ministry", "party")) %>%
  # Fill in missings in occupied variable
  mutate(occupied = replace_na(occupied, 0))


# Calculate parties' total activities in each input
parties_to_exclude <- denmark_panel %>%
    group_by(party) %>%
    summarise(across(c(scope_interpellations, n_questions, n_motions, n_bills), \(x) sum(x), .names = "total_{col}")) %>%
    mutate(across(contains("total_"), \(x) x / sum(x), .names = "pct_{col}")) %>%
    arrange(desc(pct_total_scope_interpellations)) %>%
    # Select the last 4 parties to exclude
    slice_tail(n = 4) %>%
    pull(party)


# Calculate percentages of activities per year-party
denmark_panel <- denmark_panel %>%
    filter(!party %in% parties_to_exclude) %>%
    group_by(year, party) %>%
    mutate(across(c(scope_interpellations, n_questions, n_motions, n_bills), \(x) sum(x), .names = "total_{col}")) %>%
    group_by(year, party, ministry, occupied) %>%
    mutate(
        pct_interpellations = scope_interpellations / first(total_scope_interpellations),
        pct_questions = n_questions / first(total_n_questions),
        pct_motions = n_motions / first(total_n_motions),
        pct_bills = n_bills / first(total_n_bills)
    ) %>%
    # NaN's due to division by zero become 0
    mutate(across(contains("pct_"), ~ replace_na(.x, 0))) %>%
    ungroup() %>%
    select(year, ministry, party, contains("pct_"), occupied)


# Remove a few cases of NA in ministry
denmark_panel <- denmark_panel %>%
    filter(!is.na(ministry))


# Create integer IDs and cut intervals
denmark_panel <- denmark_panel %>%
    pivot_longer(-c(year, ministry, party, occupied), names_to = "indicator") %>%
    mutate(
        year_id = as.numeric(as.factor(year)),
        ministry_id = as.numeric(as.factor(ministry)),
        party_id = as.numeric(as.factor(party)),
        indicator_id = as.numeric(as.factor(indicator))
    ) %>%
    # Applyc cuts
    group_by(indicator) %>%
    mutate(y = as.numeric(cut_interval(value, n = 5))) %>%
    ungroup() %>%
    # Relocate IDs
    select(year, year_id, ministry, ministry_id, party, party_id, indicator, indicator_id, value, y, occupied)



# Save dataset
save(denmark_panel, file = here("data", "input_dataset.Rda"))
rm(list = ls())