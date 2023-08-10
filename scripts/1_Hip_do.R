library(tidyverse)
library(metafor)
library(broom)
library(meta)

# Do - hip/calf

# Load data
hipdata <- read_csv("data/raw/Included Hip Data 20230525.csv")
hipsummary <- read_csv("data/raw/Included Hip Studies Summary 20230525.csv")

hip_all <- left_join(hipdata, 
                     hipsummary %>% select(-c(group, groups, n, primary_acl:subsequent_acl, link_study)), 
                     by = c("study", "group" = "name")) %>%
  select(study, study_id:bmi_note, everything()) # re-order dataframe

# Extract numeric data listed in 'notes' column (e.g. containing median, iqr, range etc)
hip_all <- hip_all %>%
  mutate(strings = map_if(notes, ~str_detect(., "\\d+") & !is.na(.), extract_string, .else = ~NA)) %>% # create new string variable if contains a digit using extract_string function
  unnest(strings) %>% # unnest 3 columns created (inj, non-inj and lsi)
  mutate(data_inj = map(inj_sub, ~extract_string_data(.x, name = "inj_")), # now run custom string extraction function over each
         data_noninj = map(noninj_sub, ~extract_string_data(.x, name = "noninj_")),
         data_lsi = map(lsi_sub, ~extract_string_data(.x, name = "lsi_"))) %>%
  unnest(c(data_inj, data_noninj, data_lsi)) %>% # unnest columns to get med, iqr, range
  mutate(across(c(inj_med, noninj_med, lsi_med), as.numeric)) %>% 
  select(-c(inj_sub, noninj_sub, lsi_sub)) # remove original partially extracted data columns




# Convert Median / IQR / Range to Mean/SD
# Formulae from Wan et al 2014
# Where only median provided, equate median to mean
# ignore warnings, only occurring due to full RHS evaluation
hallhoch <- hip_all %>%
  filter(study %in% c("Hall 2015", "Hoch 2018")) %>% # these two studies report ranges and medians
  rowwise() %>%
  mutate(inj_mean = case_when(
    !is.na(inj_mean) ~ inj_mean, # if mean already provided use this
    TRUE ~ inj_med # all other cases take the median
  ),
  inj_sd = case_when(
    !is.na(inj_sd) ~ inj_sd, # if sd provided use this
    !is.na(inj_range) & is.na(inj_iqr) & str_detect(inj_range, "-") ~ (as.numeric(str_split(inj_range, "-")[[1]][2]) - as.numeric(str_split(inj_range, "-")[[1]][1])) / (2 * (qnorm((n - 0.376) / (n + 0.25)))), # range provided as low/high
    !is.na(inj_range) & is.na(inj_iqr) & str_detect(inj_range, "-", negate = TRUE) ~ (as.numeric(inj_range)) / (2 * (qnorm((n - 0.376) / (n + 0.25)))), # range provided as distance
  ),
  noninj_mean = case_when(
    !is.na(noninj_mean) ~ noninj_mean,
    TRUE ~ noninj_med
  ),
  noninj_sd = case_when(
    !is.na(noninj_sd) ~ noninj_sd,
    !is.na(noninj_range) & is.na(noninj_iqr) & str_detect(noninj_range, "-") ~ (as.numeric(str_split(noninj_range, "-")[[1]][2]) - as.numeric(str_split(noninj_range, "-")[[1]][1])) / (2 * (qnorm((n - 0.376) / (n + 0.25)))),
    !is.na(noninj_range) & is.na(noninj_iqr) & str_detect(noninj_range, "-", negate = TRUE) ~ (as.numeric(noninj_range)) / (2 * (qnorm((n - 0.376) / (n + 0.25)))),
  )) %>%
  ungroup()

## Join back to main data frame
hip_all <- hip_all %>%
  filter(!study %in% c("Hall 2015", "Hoch 2018")) %>%
  bind_rows(., hallhoch)


# Combine groups where required
hip_tocombine <- hip_all %>% 
  filter(combine == 1) %>% # choose rows to be combined
  select(-c(tegner, age_other, bmi_note, timepoint_range, timepoint_iqr, timepoint_sd, notes)) %>% # remove variables not combined
  group_by(study, measure) %>%
  mutate(timepoint_mean_new = case_when( # combine timepoints
    !study %in% c("Hadi 2019", "Barnett 2020", "Mouzopoulos 2015") ~ combine_mean(n, timepoint_mean), # don't do this for Hadi where all participants are measured across 2 different timepoints
    TRUE ~ timepoint_mean)) %>%
  ungroup() %>%
  group_by(study, measure, timepoint_mean_new) %>% # group by study and outcome measure
  mutate(row = row_number(), # adds a column to mark the two different rows
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%  # create new group name by pasteing original two together 
  mutate(n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         bmi_mean_new = combine_mean(n, bmi_mean), 
         age_mean_new = combine_mean(n, age_mean),
         timepoint_mean_new = combine_mean(n, timepoint_mean), 
         ) %>%
  distinct(measure, study, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-c(group, n, female, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, bmi_mean, bmi_sd, age_mean, age_sd, graft, timepoint_mean)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new"))

# Replace and join back to original data
hip_all <- hip_all %>%
  filter(is.na(combine)) %>%
  bind_rows(., hip_tocombine) %>%
  mutate(group = case_when( 
    is.na(graft) ~ "Healthy Control", # rename control groups for consistency
    TRUE ~ group 
  )) %>%
  filter(case_when(
    group != "Healthy Control" ~ timepoint_mean > 1, # remove timepoints <3 months for ACL groups
    TRUE ~  timepoint_mean > 0)) %>%
  select(-c(combine)) # remove un-needed variables

## Missing LSI SDs for Geoghegan 2017 and Tyler 2004
# Using the mean of SDs from other studies. 
# Only a very limited number of papers provide LSI in this review. Each similar enough so taking the mean of the vi
hip_all <- hip_all %>%
  mutate(lsi_sd = case_when(
    study %in% c("Geoghegan 2007", "Tyler 2004") ~ mean(lsi_sd, na.rm = T),
    TRUE ~ lsi_sd
  )) %>%
  mutate(across(c(lsi_mean, lsi_sd), ~.x/100))

## No SD for control group from Mouzopoulos 2015
# Using SD from same position and testing protocol in Thomas 2013 control group - 0.595.


hip_all <- hip_all %>%
  mutate(inj_sd = case_when(
    study == "Mouzopoulos 2015" & group == "Healthy Control" ~ 0.595,
    TRUE ~ inj_sd
  ))


# Need to calculate effect sizes for ACL vs Control
# Currently each is on a separate row - need to combine together into 1 row

hip_vs_control <- hip_all %>%
  filter(design == "Case Control - Healthy") %>%
  filter(!(study == "Kline 2018" & units == "N")) # remove the within person comparisons from Kline 2018 (different scale to control data)

hip_vs_control <- hip_vs_control %>%
  pivot_longer(-c(study, study_id, exclude, design, country, measure, position, analysis_group, units, group),
               names_to = "names",
               values_to = "val",
               values_transform = list(val = as.character)) %>%
  mutate(population_group = ifelse(group == "Healthy Control", "con", "acl")) %>%
  pivot_wider(id_cols = c(study, study_id, exclude, design, country, measure, position, analysis_group, units),
              names_from = c(population_group, names),
              names_sep = "_",
              values_from = "val") %>%
  mutate(across(matches("mean|sd|_n$"), as.numeric)) %>%
  select(-c(contains("con_timepoint"))) %>%
  rename_with(~str_replace(., "acl_timepoint", "timepoint"), starts_with("acl_timepoint"))

hip_casecontrol_es <- hip_vs_control %>%
  mutate(acl_n = as.integer(acl_n)) %>%
  escalc(n1i = acl_n, m1i = acl_inj_mean, sd1i = acl_inj_sd, n2i = con_n, m2i = con_inj_mean, sd2i = con_inj_sd, 
         data = ., measure = "ROM") %>% 
  summary

## Within limb comparisons

hip_within_es <- hip_all %>%
  rename(acl_n = n) %>%
  mutate(acl_n = as.integer(acl_n),
         ri = 0.85) %>% # need correlation between sides for calculation of effect. Estimating based on data from our own studies %>%
  filter(!is.na(graft), # remove control groups
         !is.na(noninj_mean) | !is.na(lsi_mean)) %>% # only include those with data for both limbs
  group_by(study, measure, group) %>%
  slice(which.min(abs(timepoint_mean - 12))) %>% # takes the closest timepoint to 12 months
  ungroup() %>%
  escalc(ni = acl_n, m1i = inj_mean, sd1i = inj_sd, m2i = noninj_mean, sd2i = noninj_sd, ri = ri, 
         data = ., measure = "ROMC") # note this needs to account for correlation of effects - so using ROMC

# For studies with only LSI, calculate RoM effect sizes using LSI mean and SD
# log(LSI Mean) =~ RoM
# for variance, need to convert SD to 95%CI, then log transform and then from that calculate SE and variance
hip_within_es <- hip_within_es %>%
  mutate(yi = case_when(
          is.na(yi) & !is.na(lsi_mean) ~ log(lsi_mean),
          TRUE ~ yi),
        vi = case_when(
          is.na(vi) & !is.na(lsi_mean) ~ lsi_sd^2/(acl_n * lsi_mean^2),
          TRUE ~ vi)) %>% 
  summary 

write_csv(hip_casecontrol_es, "data/processed/Hip Case-control.csv")
write_csv(hip_within_es, "data/processed/Hip Within.csv")

