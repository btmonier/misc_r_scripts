#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   quaranteam.R
# Description:   Randomly assign lab members to discussion teams
# Author:        Brandon Monier
# Created:       2020-04-14 at 10:44:22
# Last Modified: 2020-04-15 at 17:22:18
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to randomly assign Buckler
#    lab members to "Quaranteam" daily discussion groups.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(dplyr)
library(googledrive)
library(readr)
library(magrittr)
library(tibble)
library(tidyr)


## Load data ----
team <- readr::read_csv(
    file = "data/buckler_lab_directory_20200415.csv",
    col_types = cols_only(Name = "c", Title = "c"),
    skip_empty_rows = TRUE
)



# === Process data ==================================================

misfits <- c("Ed Buckler", "Cinta Romay", "Sara Miller", "Shawna Robertson")
leads   <- c("Postdoc", "Programmer", "Bioinformatics", "Visiting Scientist")
maybes  <- c("George Day", "Mingqiu Dai", "Nick Lepak", "Thuy La")


team <- team %>%
    tidyr::drop_na() %>%
    dplyr::filter(!(Name %in% misfits)) %>%
    dplyr::mutate(
        leads = ifelse(Title %in% leads & !(Name %in% maybes), 1, 0),
        maybes = ifelse(Name %in% maybes, 1, 0)
    )



# === Make "random" groups ==========================================

## Parameters ----
group_num <- 4


## Split into approximately equal chunks ----

### Leads
lead_ls <- team %>%
    dplyr::filter(leads == 1 & maybes == 0) %>%
    dplyr::select(Name) %>%
    split(
        x = .,
        f = rep_len(seq_len(group_num), nrow(.))
    ) %>%
    .[sample(seq_len(group_num), group_num)]

### Students
non_ls <- team %>%
    dplyr::filter(leads == 0 & maybes == 0) %>%
    dplyr::select(Name) %>%
    split(
        x = .,
        f = rep_len(seq_len(group_num), nrow(.))
    ) %>%
    .[sample(seq_len(group_num), group_num)]

### Maybes
maybe_ls <- team %>%
    dplyr::filter(maybes == 1) %>%
    dplyr::select(Name) %>%
    split(
        x = .,
        f = rep_len(seq_len(group_num), nrow(.))
    ) %>%
    .[sample(seq_len(group_num), group_num)]


## Finalize ----
group_ls <- mapply(
    FUN = rbind,
    lead_ls,
    non_ls,
    maybe_ls,
    SIMPLIFY = FALSE
)

names(group_ls) <- paste0("group_", seq_len(group_num))















