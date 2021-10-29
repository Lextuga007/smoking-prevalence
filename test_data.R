library(tibble)
library(tidyverse)
library(lubridate)
library(data.table)


# Create datasets -------------------------------------------------------

set.seed(42)

# These data sets were created in Excel and copied over using the package {datapasta}.

# Frequency of smoking status

smoking_frequency <- tibble::tribble(
  ~smoking_status, ~frequency,
                 "Current Smoker", "0.204841232",
                     "Ex Smoker", "0.125691033",
                  "Never Smoked", "0.237272067",
  "Non Smoker - History Unknown", "0.240448663",
  "Not Known/Pending Assessment", "0.155548579",
  "Patient Refused to Disclose", "0.004629459",
  NA_character_, "0.031568968"
)


sample_n <- 250000

# generate a sample data frame
smoking_test <- data.frame(
  smoking_status = sample(smoking_frequency$smoking_status, sample_n, replace = TRUE,
                          prob = smoking_frequency$frequency)) %>%
  mutate(patient_id = sample(1:sample_n, replace = TRUE), # Allow for multiple repetitions of id
         assessment_date_sk = sample(seq(as.Date('2007/01/01'), as.Date('2021/01/01'),
                                         by = "day"), sample_n, replace = TRUE),
         assessment_date = ymd(assessment_date_sk))

# generate a sample data frame of referrals based on ids generated in earlier dataset

services <- tibble::tribble(
  ~code, ~service, ~ type,
  "t100", "service A", "referral",
  "t200", "service B", "referral",
  "t500", "service C", "inpatient",
  "t600", "service D", "inpatient"
)

# yearly proportions

dob_proportions <- tibble::tribble(
         ~year, ~proportion,
  "1923-01-01", 0.790185774,
  "1924-01-01", 0.911296777,
  "1925-01-01", 0.978627719,
  "1926-01-01", 1.054851427,
  "1927-01-01", 1.064167658,
  "1928-01-01",  1.21957933,
  "1929-01-01", 1.224237445,
  "1930-01-01", 1.257267719,
  "1931-01-01", 1.240752582,
  "1932-01-01", 1.261925834,
  "1933-01-01", 1.203064193,
  "1934-01-01", 1.152248388,
  "1935-01-01", 1.129381275,
  "1936-01-01", 1.095927537,
  "1937-01-01", 1.015469178,
  "1938-01-01", 1.023515014,
  "1939-01-01", 0.958724862,
  "1940-01-01", 0.884195014,
  "1941-01-01", 0.776634893,
  "1942-01-01", 0.852011671,
  "1943-01-01", 0.828297628,
  "1944-01-01", 0.861327902,
  "1945-01-01", 0.686013373,
  "1946-01-01", 0.758002431,
  "1947-01-01", 0.829144558,
  "1948-01-01", 0.701258115,
  "1949-01-01", 0.678814467,
  "1950-01-01", 0.652559634,
  "1951-01-01", 0.627575197,
  "1952-01-01", 0.665687051,
  "1953-01-01", 0.689401093,
  "1954-01-01", 0.709303951,
  "1955-01-01", 0.721160972,
  "1956-01-01", 0.734288388,
  "1957-01-01", 0.794420425,
  "1958-01-01", 0.849894345,
  "1959-01-01", 0.863445227,
  "1960-01-01", 0.941362795,
  "1961-01-01", 0.998530576,
  "1962-01-01", 1.057815683,
  "1963-01-01",  1.08364705,
  "1964-01-01", 1.137003646,
  "1965-01-01", 1.162411549,
  "1966-01-01", 1.162835014,
  "1967-01-01", 1.204334588,
  "1968-01-01", 1.255997324,
  "1969-01-01", 1.235247536,
  "1970-01-01", 1.244563767,
  "1971-01-01",  1.26658395,
  "1972-01-01", 1.253456533,
  "1973-01-01", 1.210263099,
  "1974-01-01",  1.17003392,
  "1975-01-01", 1.178926685,
  "1976-01-01", 1.186125591,
  "1977-01-01", 1.138274041,
  "1978-01-01", 1.246681093,
  "1979-01-01", 1.356782004,
  "1980-01-01", 1.419031366,
  "1981-01-01", 1.373720606,
  "1982-01-01", 1.388965348,
  "1983-01-01", 1.417337506,
  "1984-01-01", 1.444439269,
  "1985-01-01", 1.476622612,
  "1986-01-01", 1.534213858,
  "1987-01-01", 1.564279877,
  "1988-01-01", 1.621447658,
  "1989-01-01", 1.632881214,
  "1990-01-01",  1.73578322,
  "1991-01-01", 1.816241578,
  "1992-01-01", 1.791680606,
  "1993-01-01",  1.83402711,
  "1994-01-01", 1.908556958,
  "1995-01-01", 1.864093128,
  "1996-01-01", 1.790833676,
  "1997-01-01", 1.823440484,
  "1998-01-01", 1.717997688,
  "1999-01-01", 1.699365226,
  "2000-01-01", 1.610861031,
  "2001-01-01", 1.488479633,
  "2002-01-01", 1.395317324,
  "2003-01-01", 1.300461153,
  "2004-01-01", 1.241599512,
  "2005-01-01", 1.103549907,
  "2006-01-01", 0.966770698,
  "2007-01-01",  0.80246626
  )

gender_proportions <- tibble::tribble(
  ~gender, ~proportion,
  "F", 0.5,
  "M", 0.5
)

referral_test_staging <- smoking_test %>%
  select(patient_id) %>%
  unique() %>%
  mutate(code = sample(services$code, nrow(.), replace = TRUE)) %>%
  left_join(services) %>% # get service names
  mutate(referral_row_id = row_number(),
         start_date = sample(seq(as.Date('2007/01/01'), as.Date('2021/01/01'),
                                 by = "day"), nrow(.), replace = TRUE),
         end_date = start_date + sample(1:4 * 365, nrow(.), replace = TRUE),
         dob = sample(dob_proportions$year, nrow(.), replace = TRUE,
                      prob = dob_proportions$proportion),
         dob = as.Date(dob) + runif(nrow(.), 0, 365),
           # Not used as too random but useful code
           # sample(seq(Sys.Date() - 99 * 365, Sys.Date() - 4 * 365, by = "day"), length(id)),
         gender = sample(c("F", "M"), nrow(.), replace = TRUE, prob = gender_proportions$proportion))

notes_info <- referral_test_staging %>%
  left_join(smoking_test) %>%
  mutate(notes_assess = case_when(assessment_date >= start_date & assessment_date <= end_date ~ "In service on start date",
                           assessment_date > end_date ~ "After service",
                           assessment_date < start_date ~ "Before service",
                           TRUE ~ NA_character_)) %>%
  # Find where smoking_status has changed as in, is different, doesn't always make a logical change
  # as this is randomly set
  left_join(
    smoking_test %>%
      select(patient_id,
             smoking_status) %>%
      filter(!is.na(smoking_status)) %>%
      group_by(patient_id) %>%
      summarise(n = n_distinct(smoking_status)) %>%
      filter(n > 1)) %>%
  mutate(notes_status = case_when(!is.na(n) ~ "Change in status",
                                  TRUE ~ NA_character_))

referral_test <- referral_test_staging %>%
  left_join(smoking_test) %>%
  left_join(notes_info)

# Build smoking status table over time ------------------------------------

smoke_fill <- function(data){

  smoking <- data %>%
    mutate(year = floor_date(assessment_date, "year"))

  smoking_dt <- data.table(smoking)

  # the desired dates by group
  indx <- smoking_dt[,.(year = seq(min(year), max(year), "year")), patient_id]

  # key the tables and join them using a rolling join
  setkey(smoking_dt, patient_id, year)
  setkey(indx, patient_id, year)

  smoking_dt_filled <- smoking_dt[indx, roll = TRUE]

  smoke <- as_tibble(smoking_dt_filled)

  return(smoke)
}

smoke_test_filled <- smoke_fill(smoking_test)


# Building a full period dataset ------------------------------------------

#To create data to be counted by each year code is used to complete the years
# between the referral/admission date to discharge. It would be possible to create
# a dataset per year more quickly but as the analysis is from 2007 this would
# require a function or loop to complete for each year until today.

# This is computationally too slow in {dplyr} because of the use of group_by()
# required so {data.table} is used to complete the years between start and end
# dates of referrals/inpatients. It still takes a while because of the number of rows.

# Previous analysis had calculated age only at discharge which is incorrect if a
# referral is over years. This analysis calculates the age at the first of the year
# which is closer in accuracy.

### Open referrals

# Open referrals do not necessarily mean the referrals are accepted or that any
# activity (contacts) have occurred within the referral. For the purpose of
# prevalence, however, where an assessment for smoking status has taken place,
# this is included.


referrals_fill <- function(data){

  df <- data %>%
    mutate(start_year = floor_date(start_date, "year"),
           end_year = case_when(is.na(end_date) ~ floor_date(Sys.Date(), "year"),
                                TRUE ~ floor_date(end_date, "year")),
           id = case_when(is.na(referral_row_id) ~ service,
                          TRUE ~ as.character(referral_row_id))) %>%
    select(patient_id,
           gender,
           dob,
           id,
           service,
           code,
           start_year,
           end_year) %>%
    unique() %>%
    pivot_longer(cols = c("start_year", "end_year"),
                 names_to = "date_type",
                 values_to = "year" ) %>%
    select(-date_type) %>%
    unique()

  #### data.table to complete dates #####

  referrals_dt <- data.table(df)

  # the desired dates by group
  indx <- referrals_dt[,.(year = seq(min(year), max(year), "year")), id]

  # key the tables and join them using a rolling join
  setkey(referrals_dt, id, year)
  setkey(indx, id, year)

  referrals_dt_filled <- referrals_dt[indx, roll = TRUE]

  # Used data.table code to recode to dates https://stackoverflow.com/questions/46078151/efficiently-convert-a-date-column-in-data-table

  df <- referrals_dt_filled
  date_cols <- c("dob")

  setDT(df)[, (date_cols) := lapply(.SD, anytime::anydate), .SDcols = date_cols]

  # format back to tibble for later formatting

  referrals_aging <- as_tibble(df) %>%
    filter(year > dob)
  # removes IAPT when dob is a data quality mistake and CAMHS where the child was born in the floor_date(year)

  return(referrals_aging)

}

referrals_test_filled <- referrals_fill(referral_test)


# Combine referrals with smoking status -----------------------------------

# Join the two tables together on full join because some assessments occur after
# referrals/inpatients but can be assumed as relevant. For example, a patient
# with an assessment for smoking status in 2009 as Current Smoker, could be
# counted as such for a referral in 2008.

# Note that due to the join some columns are NA. These are "filled" downwards using fill().

combined_dfs <- function(data1, data2){

  df <- data1 %>%
    full_join(data2, by = c("patient_id", "year")) %>%
    filter(dob < year) %>%
    group_by(patient_id) %>%
    fill(id,
         # Missing from referrals/inpatients that occur before an assessment
         assessment_date_sk,
         smoking_status,
         assessment_date,
         gender,
         .direction = "down") %>%
    fill(gender,
         dob,
         .direction = "updown") %>%
    ungroup() %>%
    mutate(age_at_year = eeptools::age_calc(as.Date(dob),
                                            as.Date(year),
                                            units = "years",
                                            precise = FALSE),
           age_bands = case_when(age_at_year < 16 ~ 'under 16',
                                 age_at_year > 15 & age_at_year <  25 ~ '1624',
                                 age_at_year > 24 & age_at_year <  35 ~ '2534',
                                 age_at_year > 34 & age_at_year <  50 ~ '3549',
                                 age_at_year > 49 & age_at_year <  60 ~ '5059',
                                 age_at_year > 59 ~ '60andover',
                                 TRUE ~ 'no age'),
           all_ages = case_when(age_at_year >= 16 ~ 'Allaged16andover',
                                TRUE ~ NA_character_))

  return(df)

}

combined_test <- combined_dfs(smoke_test_filled, referrals_test_filled)
