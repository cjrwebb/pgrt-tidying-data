#' These are my own solutions to the data tidying challenges that are
#' provided at the end of the workbook:

library(tidyverse)
library(janitor)
library(readxl)
library(haven)
library(labelled)
library(ggeffects)

######################################################################

# Tidying Challenge: data/sotkanet-data.xls

#' This data is from the Sotkanet.fi (https://sotkanet.fi/sotkanet/en/index)
#' statistical data service, which contains a wide range of interesting
#' municipal data from Finland. The major challenge of tidying this data 
#' comes from the fact that the years are given in the columns, and the 
#' rows represent variable, year, municipality observations. We want to
#' switch this over so that each row represents an observation in a given
#' year, for a specific municipality, and each column represents a variable.
#' 
#' Difficulty: Easy
#' - This is quite a typical and useful example of official statistical 
#' releases across many countries


sotka <- read_xls("data/sotkanet-data.xls") 
sotka

# First, pivot the data longer using the year columns
sotka <- sotka %>%
  pivot_longer(
    cols = `1990`:`2023`,
    names_to = "year"
  )
sotka

# Then, rename the variables without names and remove the
# variable id name (or combine with the variable); having 
# two variable keys will cause problems with the join as
# it will treat it as part of the unique row identifier. We
# can drop population too as it's the same for all (combined
# rather than split into male/female)
sotka <- sotka %>% 
  rename(
    variable = 1,
    variable_id = 2,
    municipality = 3,
    municipality_id = 4,
    population = 5
  ) %>%
  select(-variable_id, -population)
sotka  

# Then, let's use our variable indicator to turn out
# variables into columns
sotka <- sotka %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )
sotka

# Finally, let's tidy up the names of our columns using
# some generated short names. I'll also save a copy of 
# the original names for reference.

var_desc <- names(sotka)

var_desc

labels <- c(
  "poverty_rate",      # At-risk-of-poverty-rate for children
  "guidance_exp",      # Child guidance and family counselling, operating expenditure
  "care_rate",         # Children placed in care during the year, % of total population
  "emergency_rate",    # Children placed in emergency placement, % of total population
  "care_or_emerg_rate",# Children in care or emergency placement, % of total population
  "family_help_rate",  # Families receiving home help and child care services, % of all families
  "early_ed_days",      # Full-time early childhood education and care in daycare (municipality-funded)
  "gini",         # Gini coefficient, disposable income
  "social_exp_percap", # Net operating costs of social services for children, youth, and families, euro per capita
  "non_inst_exp_total",# Net operating costs on other non-institutional services (total)
  "net_cost_percap",   # Net operating costs for children and families aged 0-17, euro per capita
  "welfare_exp_total", # Non-institutional services in child welfare, total operating expenditure
  "welfare_exp_net",   # Operating net expenditure on non-institutional services in child welfare
  "other_exp_net",     # Operating net expenditure on other non-institutional services for children
  "other_income_total",# Other non-institutional services income for children and families (total)
  "play_activity_avg", # Play activities, average number of children per day (municipality-funded)
  "thl_sotkanet"       # © THL, Statistics and Indicator Sotkanet.fi 2005-2025
)

# create a codebook
sotka_codebook <- tibble(names = c(names(sotka)[1:3], labels), var_desc)
sotka_codebook

# Assign the names to the variables
names(sotka) <- sotka_codebook$names

# Drop the footer that become a column
sotka <-sotka %>%
  select(-thl_sotkanet)

# Make year into a numeric variable
sotka <- sotka %>% mutate(year = as.numeric(year))


# Final result:
sotka

# This would likely require further processing due to the amount 
# of missing data


######################################################################

# Tidying Challenge: data/mortality-nomis.xlsx

#' Successfully read and tidy the above spreadsheet so that all of the
#' numeric variables are of the proper type, that missing values are 
#' detected as missing. Turn the data into a long format, where each
#' row represents a region for a specific year for a specific gender 
#' (all,  male, female) and each column represents the number or
#' rate of the mortality statistic.

# Difficulty: Medium (but tedious!)
# - It would be easier to try and read in each section and then join
#   them all together at the end!


# Solution ----------------------------------------------------------------

# Read in each section of the spreadsheet that refers to different 
# measures:
nomis_all <- read_xlsx("data/nomis-mortality.xlsx", range = cell_rows(9:20), na = "-") %>%
              pivot_longer(cols = `North East`:`Unknown or Abroad`, 
                           names_to = "region", 
                           values_to = "all_deaths") %>%
              rename(year = Date)

nomis_all

nomis_rate <- read_xlsx("data/nomis-mortality.xlsx", range = cell_rows(32:43), na = "-") %>%
                pivot_longer(cols = `North East`:`Unknown or Abroad`, 
                             names_to = "region", 
                             values_to = "all_mortrate") %>%
                rename(year = Date)
nomis_rate

nomis_mdeaths <- read_xlsx("data/nomis-mortality.xlsx", range = cell_rows(56:67), na = "-") %>%
                  pivot_longer(cols = `North East`:`Unknown or Abroad`, 
                               names_to = "region", 
                               values_to = "male_deaths") %>%
                  rename(year = Date)
nomis_mdeaths

nomis_mmortrate <- read_xlsx("data/nomis-mortality.xlsx", range = cell_rows(79:90), na = "-") %>%
                    pivot_longer(cols = `North East`:`Unknown or Abroad`, 
                                 names_to = "region", 
                                 values_to = "male_mortrate") %>%
                    rename(year = Date)
nomis_mmortrate

nomis_fdeaths <- read_xlsx("data/nomis-mortality.xlsx", range = cell_rows(103:114), na = "-") %>%
                  pivot_longer(cols = `North East`:`Unknown or Abroad`, 
                               names_to = "region", 
                               values_to = "female_deaths") %>%
                  rename(year = Date)
nomis_fdeaths

nomis_fmortrate <- read_xlsx("data/nomis-mortality.xlsx", range = cell_rows(126:137), na = "-") %>%
                    pivot_longer(cols = `North East`:`Unknown or Abroad`, 
                                 names_to = "region", 
                                 values_to = "female_mortrate") %>%
                    rename(year = Date)
nomis_fmortrate

# Join all of the datasets together by region and year:

nomis <- left_join(nomis_all, nomis_rate, by = c("year", "region")) %>%
          left_join(nomis_mdeaths, by = c("year", "region")) %>%
          left_join(nomis_fdeaths, by = c("year", "region")) %>%
          left_join(nomis_mmortrate, by = c("year", "region")) %>%
          left_join(nomis_fmortrate, by = c("year", "region")) 

nomis

# Pivot all death and rate columns to longer format so that
# gender can be created as a new column:

nomis <- nomis %>%
  pivot_longer(cols = all_deaths:female_mortrate,
               names_to = "variable",
               values_to = "value")
nomis

# Use separate to split the gender from the variable name:

nomis <- nomis %>%
  separate(variable, into = c("gender", "measure"), sep = "_")
nomis

# Use pivot_wider to make measures into their own columns again:

nomis <- nomis %>%
  pivot_wider(names_from = measure,
              values_from = value)

nomis


# End of solution ---------------------------------------------------------

######################################################################

# Tidying Challenge: data/healthcare-beds-data.xlsx

#' Successfully read and tidy the above spreadsheet so that all of the
#' numeric variables are of the proper type, that missing values are 
#' detected as missing, and that country names are consistent, without
#' any footnotes or extra spaces.

# Difficulty: Hard
# - Probably need to use some regex/functions not covered: (.+), str_trim()
# - Uses merged cells that throw an error with readxl


# Solution ----------------------------------------------------------------

carebeds <- read_xlsx("data/healthcare-beds-data.xlsx",
                      sheet = "Table 1", 
                      na = ":",
                      # Note: Reading from row 11 doesn't work because of
                      # the merged cells.
                      range = cell_rows(12:44),
) %>%
  rename(
    country = 1,
    n_beds = 2,
    n_curative = 3,
    n_rehab = 4,
    n_longterm = 5,
    n_other = 6,
    n_psych = 7,
    rate_beds = 8,
    rate_curative = 9,
    rate_rehab = 10,
    rate_longterm = 11,
    rate_other = 12,
    rate_psych = 13
  ) %>%
  mutate(
    country = str_trim(str_remove_all(country, "\\((.+)\\)"))
  )
carebeds


read_csv("data/ncd-bp.csv")


# End of solution ---------------------------------------------------------

######################################################################
