hip_pre <- hip_all %>%
  filter(timepoint_mean == 0.1,
         study != "Balki 2019") %>%
  bind_rows(., hip_all %>% filter(study == "Rhim 2020" & group == "Healthy Control")) %>%
  bind_rows(., hip_all %>% filter(study == "Fukuyama 2022"))

hip_vs_control_pre <- hip_pre %>%
  filter(design == "Case Control - Healthy") %>%
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

hip_casecontrol_es_pre <- hip_vs_control_pre %>%
  mutate(acl_n = as.integer(acl_n)) %>%
  escalc(n1i = acl_n, m1i = acl_inj_mean, sd1i = acl_inj_sd, n2i = con_n, m2i = con_inj_mean, sd2i = con_inj_sd, 
         data = ., measure = "ROM") %>% 
  summary

## Within limb comparisons

hip_within_es_pre <- hip_pre %>%
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
hip_within_es_pre <- hip_within_es_pre %>%
  mutate(yi = case_when(
    is.na(yi) & !is.na(lsi_mean) ~ log(lsi_mean),
    TRUE ~ yi),
    vi = case_when(
      is.na(vi) & !is.na(lsi_mean) ~ lsi_sd^2/(acl_n * lsi_mean^2),
      TRUE ~ vi)) %>% 
  summary 

write_csv(hip_casecontrol_es_pre, "data/processed/Hip Case-control Preop.csv")
write_csv(hip_within_es_pre, "data/processed/Hip Within Preop.csv")



# Case-Control meta-analysis
hip_casecontrol_meta_pre <- hip_casecontrol_es_pre %>%
  #filter(is.na(exclude)) %>% # remove data to exclude from MA
  mutate(aclr_n = as.character(acl_n)) %>% # character for foresst plot purposes
  group_by(analysis_group) %>% # group by each publication and subgroups of interest in this review, for later 'pre-meta-analyses'
  nest(data = -c(analysis_group)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map(data, ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, sm = "ROM")),
         output = map(rma, ~tidymeta(.x)),
         acl_totaln = map(data, ~sum(.$acl_n)),
         con_totaln = map(data, ~sum(.$con_n))) %>%
  unnest(cols = output)

hip_within_meta <- hip_within_es_pre %>%
  filter(is.na(exclude)) %>% # remove data to exclude from MA
  mutate(aclr_n = as.character(acl_n), # character for forest plot purposes
         ) %>% # 
  group_by(analysis_group) %>% # group by each publication and subgroups of interest in this review, for later 'pre-meta-analyses'
  nest(data = -c(analysis_group)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map(data, ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, sm = "ROM")),
         output = map(rma, ~tidymeta(.x)),
         acl_totaln = map(data, ~sum(.$acl_n))) %>%
  unnest(cols = output) 
