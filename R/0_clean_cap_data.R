# ---
# _CLEAN COMPARATIVE AGENDAS PROJECT DATA
# ---


# Packages and functions
library(tidyverse)
library(here)

source(here("R", "_utils.R"))


# Load raw cap data
cap_interpellations <- read_csv(here("raw-data", "csv", "interpellations.csv"), show_col_types = FALSE)
cap_questions <- read_csv(here("raw-data", "csv", "questions.csv"), show_col_types = FALSE)
cap_motions <- read_csv(here("raw-data", "csv", "motions.csv"), show_col_types = FALSE)
cap_bills <- read_csv(here("raw-data", "csv", "bills.csv"), show_col_types = FALSE)


# Clean interpellations
cap_interpellations <- cap_interpellations %>%
    # From 2007 onwards
    filter(year >= 2007) %>%
    # Variables
    select(year, id, var4_1:var4_21, var5_1:var5_42, scope_debate = var7) %>%
    pivot_longer(
        cols = starts_with("var4_"),
        names_to = "party",
        values_to = "sponsored"
    ) %>%
    pivot_longer(
        cols = starts_with("var5_"),
        names_to = "ministry",
        values_to = "targeted"
    ) %>%
    # Recode parties
    mutate(party = str_remove_all(party, "var4_")) %>%
    mutate(party = recode_parties(party)) %>%
    # Recode ministries
    mutate(ministry = str_remove_all(ministry, "var5_")) %>%
    mutate(ministry = recode_ministries(ministry)) %>%
    # Summarise
    filter(sponsored == 1 & targeted == 1) %>%
    group_by(year, party, ministry) %>%
    summarise(
        scope_interpellations = sum(scope_debate, na.rm = TRUE),
        n_interpellations = n(),
        .groups = "drop"
    )


# Clean questions
cap_questions <- cap_questions %>%
    # From 2007 onwards
    filter(year >= 2007) %>%
    # Variables
    select(year, id, party = var4, ministry = var5) %>%
    # Recode parties
    mutate(party = recode_parties(as.character(party))) %>%
    # Recode ministries
    mutate(ministry = recode_ministries(as.character(ministry))) %>%
    # Summarise
    group_by(year, party, ministry) %>%
    summarise(n_questions = n(), .groups = "drop")


# Clean motions
cap_motions <- cap_motions %>%
    # From 2007 onwards
    filter(year >= 2007) %>%
    # Only private motions
    filter(var4 == 1) %>%
    # Select
    select(year, id, var5_1:var5_21, major_topic = majortopic) %>%
    pivot_longer(
        cols = starts_with("var5_"),
        names_to = "party",
        values_to = "sponsored"
    ) %>%
    # Recode parties
    mutate(party = str_remove_all(party, "var5_")) %>%
    mutate(party = recode_parties(party)) %>%
    # Recode sponsored
    mutate(sponsored = case_when(
        sponsored == 1 ~ 1,
        TRUE ~ 0
    )) %>%
    # Obtain current ministries linked to major topics
    rowwise() %>%
    mutate(ministry = list( link_ministries(major_topic, unique(cap_questions$ministry[cap_questions$year == first(year)])) ) ) %>%
    ungroup() %>%
    unnest(ministry) %>%
    # Summarise
    filter(sponsored == 1) %>%
    group_by(year, party, ministry) %>%
    summarise(n_motions = n(), .groups = "drop")


# Clean bills
cap_bills <- cap_bills %>%
    # From 2007 onwards
    filter(year >= 2007) %>%
    # Only private bills
    filter(var5 == 0) %>%
    # Select
    select(year, id, var8_1:var8_21, major_topic = majortopic) %>%
    pivot_longer(
        cols = starts_with("var8_"),
        names_to = "party",
        values_to = "sponsored"
    ) %>%
    # Recode parties
    mutate(party = str_remove_all(party, "var8_")) %>%
    # Recode sponsored
    mutate(sponsored = case_when(
        sponsored == 1 ~ 1,
        TRUE ~ 0
    )) %>%
    mutate(party = recode_parties(party)) %>%
    # Obtain current ministries linked to major topics
    rowwise() %>%
    mutate(ministry = list( link_ministries(major_topic, unique(cap_questions$ministry[cap_questions$year == first(year)])) ) ) %>%
    ungroup() %>%
    unnest(ministry) %>%
    # Summarise
    filter(sponsored == 1) %>%
    group_by(year, party, ministry) %>%
    summarise(n_bills = n(), .groups = "drop")


# Create a panel of ministry-party combinations within years
denmark_panel <- 2007:2016 %>%
    map_df(function(y) {
        expand_grid(
            year = y,
            party = unique(cap_questions$party[cap_questions$year == y]),
            ministry = unique(cap_questions$ministry[cap_questions$year == y])
        )
    }) %>%
    # Join interpellations
    left_join(cap_interpellations, by = c("year", "party", "ministry")) %>%
    mutate(scope_interpellations = replace_na(scope_interpellations, 0)) %>%
    mutate(n_interpellations = replace_na(n_interpellations, 0)) %>%
    # Join questions
    left_join(cap_questions, by = c("year", "party", "ministry")) %>%
    mutate(n_questions = replace_na(n_questions, 0)) %>%
    # Join motions
    left_join(cap_motions, by = c("year", "party", "ministry")) %>%
    mutate(n_motions = replace_na(n_motions, 0)) %>%
    # Join bills
    left_join(cap_bills, by = c("year", "party", "ministry")) %>%
    mutate(n_bills = replace_na(n_bills, 0))


# Save cleaned data
if(!dir.exists(here("data"))) dir.create(here("data"))
save(denmark_panel, file = here("data", "denmark_panel.Rda"))
rm(list = ls())
