# Do Hip/Calf

hip_casecontrol_es <- read_csv("data/processed/Hip Case-control.csv")
hip_within_es <- read_csv("data/processed/Hip Within.csv")


# Case-Control meta-analysis
hip_casecontrol_meta <- hip_casecontrol_es %>%
  filter(is.na(exclude)) %>% # remove data to exclude from MA
  mutate(aclr_n = as.character(acl_n),
         timepoint_cat = factor(if_else(timepoint_mean > 12, "More than 12 mths", "12 mths or less"))) %>% # character for foresst plot purposes
  group_by(analysis_group) %>% # group by each publication and subgroups of interest in this review, for later 'pre-meta-analyses'
  nest(data = -c(analysis_group)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map_if(data, 
                      .$analysis_group %in% c("hip abd", "hip ext", "hip er"), 
                      ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, subgroup = timepoint_cat, sm = "ROM"),
                      .else = ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, sm = "ROM")),
         output = map(rma, ~tidymeta(.x)),
         acl_totaln = map(data, ~sum(.$acl_n)),
         con_totaln = map(data, ~sum(.$con_n))) %>%
  unnest(cols = output)

hip_within_meta <- hip_within_es %>%
  filter(is.na(exclude)) %>% # remove data to exclude from MA
  mutate(aclr_n = as.character(acl_n), # character for forest plot purposes
         timepoint_cat = factor(if_else(timepoint_mean > 12, "More than 12 mths", "12 mths or less"))) %>% # 
  group_by(analysis_group) %>% # group by each publication and subgroups of interest in this review, for later 'pre-meta-analyses'
  nest(data = -c(analysis_group)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map_if(data, 
                      .$analysis_group %in% c("hip abd", "hip ext"), 
                      ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, subgroup = timepoint_cat, sm = "ROM"),
                      .else = ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, sm = "ROM")),
         output = map(rma, ~tidymeta(.x)),
         acl_totaln = map(data, ~sum(.$acl_n))) %>%
  unnest(cols = output) 

# Helper function for plotting different forest-plots
forestmeta_function <- function(casecontrol = TRUE, var, xlim = NULL, colgap.studlab = "-5mm", ...){
  dataset <- if(casecontrol == TRUE) hip_casecontrol_meta else hip_within_meta # choose which dataset
  
  label <- if(casecontrol == TRUE) c("ACL group weaker", "Control group weaker") else  c("ACL side weaker", "Contra side weaker")
  
  data <- dataset %>% # pluck data from original dataframe
    filter(analysis_group == var) %>%
    select(rma) %>%
    pluck(1) %>%
    pluck(1)
  
  xlim <- if(is.null(xlim) & casecontrol == TRUE) c(0.5, 2.0) else if(is.null(xlim) & casecontrol == FALSE) c(0.8, 1.25) else xlim
  xlab <- log(xlim)
  
  # Plot
  forest.meta(data,
              sortvar = timepoint_mean,
              common = FALSE,
              prediction = TRUE,
              xlab = label,
              xlab.pos = xlab,
              xlim = xlim,
              smlab = "",
              leftcols = c("study", "aclr_n", "timepoint_mean", "position"),
              leftlabs = c("Study", "n\nACLR", "Mths post\nACLR", "Position"),
              rightcols = c("effect.ci"),
              rightlabs = c("RoM [95% CI]"),
              just.addcols = "left",
              addrows.below.overall = 1,
              digits.addcols = 1,
              print.pval.Q = FALSE,
              fontfamily = "Karla",
              ff.predict = 1,
              ref = 1,
              col.diamond = "black",
              print.subgroup.labels = TRUE, 
              subgroup.name = "Timepoint",
              test.subgroup = FALSE, 
              subgroup.hetstat = FALSE, 
              col.random = "grey",
              colgap.studlab = colgap.studlab,
              ...) 
  
}


## Table 1 Summaries
demographic <- read.csv("data/processed/Included Hip Studies Summary 20230525.csv") %>%
  filter(!study %in% c("Mouzopoulos 2015", "Hiemstra 2005")) %>% # remove excluded studies
  mutate(population_group = if_else(is.na(graft), "con", "acl"),
         f_percent = female/n) 

# Quick summaries for text
demographic %>%
  group_by(population_group) %>%
  summarise(n = sum(n), # total n
            female = sum(female, na.rm = T), # total women
            f_percent_mean = mean(f_percent, na.rm = T),
            age_min = min(age_mean, na.rm = T), # age ranges
            age_max = max(age_mean, na.rm = T)) 

unique(demographic$country) # number of countries

# Dataframe for control populations (n (Women))
demo_control <- demographic %>%
  filter(population_group == "con") %>%
  mutate(control_n = case_when(
    is.na(female) ~ paste0(n, " (NR)"),
    TRUE ~ paste0(n, " (", female, ")"))) %>% 
  select(study, control_n)

# Dataframe for main study info + ACL populations
demo_acl <- demographic %>%
  filter(population_group == "acl") %>%
  group_by(study) %>%
  mutate(group_all = paste(name, collapse = " + "),
         distinctgroup = n_distinct(graft),
         graft_all = case_when(
           n_distinct(graft) == 1 ~ graft, # combine multiple groups together
           TRUE ~ paste(graft, collapse = ", ")),
         n_all = sum(n),
         female_all = sum(female),
         n_all = case_when(
           is.na(female_all) ~ paste0(n_all, " (NR)"),
           TRUE ~ paste0(n_all, " (", female_all, ")")),
         age_mean_all = combine_mean(n, age_mean), # combine statistics as needed
         age_sd_all = combine_sd(n, age_mean, age_sd),
         bmi_mean_all = combine_mean(n, bmi_mean),
         bmi_sd_all = combine_sd(n, bmi_mean, bmi_sd),
         ) %>%
  ungroup() %>%
  select(study:country, population_group, ends_with("all")) %>%
  rename_with(~str_replace(., "_all", ""), ends_with("_all")) %>%
  distinct(study, .keep_all = TRUE)

# Get info on what test and positions etc used
hipdata_table <- read_csv("data/processed/Included Hip Data 20221014.csv") %>%
  select(study, position, analysis_group, units) %>%
  mutate(device = if_else(str_detect(position, "HHD"), paste("HHD", units, sep = ", "), paste("Isokinetic", units, sep = ", ")),
         measure = fct_recode(analysis_group, 
                              "Hip Abduction" = "hip abd",
                              "Hip Adduction" = "hip add",
                              "Hip External Rotaiton" = "hip er",
                              "Hip Internal Rotaiton" = "hip ir",
                              "Hip Flexion" = "hip flex",
                              "Hip Extension" = "hip ext",
                              "Plantarflexion (Gastroc)" = "calf",
                              "Plantarflexion (Soleus)" = "soleus",
                              "Dorsiflexion" = "dflex")) %>%
  group_by(study) %>%
  mutate(measure = paste(unique(measure), collapse = ", ")) %>%
  ungroup() %>%
  distinct(study, .keep_all = TRUE) %>%
  select(study, device, measure)

# Join everything together
demo_table <- left_join(demo_acl, demo_control, by = "study") %>%
  left_join(., hipdata_table, by = "study") %>%
  mutate(age = case_when(
          is.na(age_mean) ~ "Not Reported",
          is.na(age_sd) ~ as.character(round(age_mean,2)),
          TRUE ~ paste0(round(age_mean,2), " (", round(age_sd,2), ")")), # combine mean and sd columbs for print
         bmi = case_when(
           is.na(bmi_mean) ~ "Not Reported",
           is.na(bmi_sd) ~ as.character(round(bmi_mean,2)),
           TRUE ~ paste0(round(bmi_mean,2), " (", round(bmi_sd,2), ")")))  %>%
  select(study:design, country, group:n, control_n, age, bmi, device, measure)

write_csv(demo_table, "output/tables/demographic_table.csv")


######
######
## SMD ##


hip_within_essmd <- hip_within_es %>%
  escalc(n1i = acl_n, m1i = inj_mean, sd1i = inj_sd, n2i = acl_n, m2i = noninj_mean, sd2i = noninj_sd, 
         data = ., measure = "SMD") %>% summary

hip_casecontrol_essmd <- hip_casecontrol_es %>%
  escalc(n1i = acl_n, m1i = acl_inj_mean, sd1i = acl_inj_sd, n2i = con_n, m2i = con_inj_mean, sd2i = con_inj_sd, 
         data = ., measure = "SMD") %>%
  summary

# Case-Control meta-analysis
hip_casecontrol_metasmd <- hip_casecontrol_essmd %>%
  mutate(aclr_n = as.character(acl_n),
         timepoint_cat = factor(if_else(timepoint_mean >= 12, "More than 12 mths", "Less than 12 mths"))) %>% # character for foresst plot purposes
  group_by(analysis_group) %>% # group by each publication and subgroups of interest in this review, for later 'pre-meta-analyses'
  nest(data = -c(analysis_group)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map_if(data, 
                      .$analysis_group %in% c("hip abd", "hip ext", "hip er"), 
                      ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, subgroup = timepoint_cat),
                      .else = ~metagen(TE = yi, seTE = sei, studlab = study, data = .x)),
         output = map(rma, ~tidymeta(.x)),
         acl_totaln = map(data, ~sum(.$acl_n)),
         con_totaln = map(data, ~sum(.$con_n))) %>%
  unnest(cols = output)

hip_within_metasmd <- hip_within_essmd %>%
  mutate(aclr_n = as.character(acl_n), # character for forest plot purposes
         timepoint_cat = factor(if_else(timepoint_mean >= 12, "More than 12 mths", "Less than 12 mths"))) %>% # 
  group_by(analysis_group) %>% # group by each publication and subgroups of interest in this review, for later 'pre-meta-analyses'
  nest(data = -c(analysis_group)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map_if(data, 
                      .$analysis_group %in% c("hip abd", "hip ext"), 
                      ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, subgroup = timepoint_cat),
                      .else = ~metagen(TE = yi, seTE = sei, studlab = study, data = .x)f),
         output = map(rma, ~tidymeta(.x)),
         acl_totaln = map(data, ~sum(.$acl_n))) %>%
  unnest(cols = output) 

# Helper function for plotting different forest-plots
forestmeta_functionsmd <- function(casecontrol = TRUE, var, ...){
  dataset <- if(casecontrol == TRUE) hip_casecontrol_metasmd else hip_within_metasmd # choose which dataset
  
  label <- if(casecontrol == TRUE) c("ACL group weaker", "Control group weaker") else  c("ACL side weaker", "Contra side weaker")
  
  data <- dataset %>% # pluck data from original dataframe
    filter(analysis_group == var) %>%
    select(rma) %>%
    pluck(1) %>%
    pluck(1)
  
  # Plot
  forest.meta(data,
              sortvar = timepoint_mean,
              common = FALSE,
              prediction = TRUE,
              xlab = label,
              xlab.pos = c(-1.5, 1.5),
              xlim = c(-1.5, 1.5),
              smlab = "",
              leftcols = c("study", "aclr_n", "timepoint_mean", "position"),
              leftlabs = c("Study", "n\nACLR", "Mths post\nACLR", "Position"),
              rightcols = c("effect.ci"),
              rightlabs = c("ROM [95% CI]"),
              colgap.studlab = "-5mm",
              just.addcols = "left",
              addrows.below.overall = 1,
              digits.addcols = 1,
              print.pval.Q = FALSE,
              fontfamily = "Karla",
              ff.predict = 1,
              ref = 0,
              col.diamond = "black",
              print.subgroup.labels = TRUE, 
              subgroup.name = "Timepoint",
              test.subgroup = FALSE, 
              subgroup.hetstat = FALSE, 
              col.random = "grey",
              ...) 
  
}


# Leave one out analysis
## First run the influence analysis with metafor package - gives all indepth statistics and measures
## Join these to original data
## Meta package provides nicer influence plot, but use the data from the metafor output in the plot.

hip_within_loo <- hip_within_es %>%
  mutate(aclr_n = as.character(acl_n), # character for forest plot purposes
         timepoint_cat = factor(if_else(timepoint_mean >= 12, "More than 12 mths", "Less than 12 mths"))) %>% # 
  group_by(analysis_group) %>%
  mutate(k = length(study)) %>%
  nest(data = -c(analysis_group, k)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map(data, ~rma(yi, vi, data = .x)), # runs metafor meta-analysis
         metaforinf = map_if(rma,  # ruin a influence analysis from metafor package if k>1
                             k > 2, 
                             ~influence(.x) %>% pluck(1) %>% data.frame, # take the output
                             .else = ~data.frame(inf = NA))) %>% # for k <=1 return df with NAs
  mutate(new = map2(data, metaforinf, ~bind_cols(.x, .y))) %>% # combine influence data with original data
  select(-c(data, rma, metaforinf)) %>% # remove all nested columns now not needed
  # now run meta with metagen (inorder to get nice influence plots)
  mutate(rma = map(new, ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, sm = "ROM")),
         influence = map_if(rma, k > 2, ~metainf(.x, pooled = "random"))) # run influence analysis with meta package

hip_casecontrol_loo <- hip_casecontrol_es %>%
  mutate(aclr_n = as.character(acl_n), # character for forest plot purposes
         timepoint_cat = factor(if_else(timepoint_mean >= 12, "More than 12 mths", "Less than 12 mths"))) %>% # 
  group_by(analysis_group) %>%
  mutate(k = length(study)) %>%
  nest(data = -c(analysis_group, k)) %>%# nest data, remove data that is consistent across subgroups
  ungroup() %>%
  mutate(rma = map(data, ~rma(yi, vi, data = .x)), # runs metafor meta-analysis
         metaforinf = map_if(rma,  # ruin a influence analysis from metafor package if k>1
                             k > 2, 
                             ~influence(.x) %>% pluck(1) %>% data.frame, # take the output
                             .else = ~data.frame(inf = NA))) %>% # for k <=1 return df with NAs
  mutate(new = map2(data, metaforinf, ~bind_cols(.x, .y))) %>% # combine influence data with original data
  select(-c(data, rma, metaforinf)) %>% # remove all nested columns now not needed
  # now run meta with metagen (inorder to get nice influence plots)
  mutate(rma = map(new, ~metagen(TE = yi, seTE = sei, studlab = study, data = .x, sm = "ROM")),
         influence = map_if(rma, k > 2, ~metainf(.x, pooled = "random"))) # run influence analysis with meta package


# Helper function for plotting different forest-plots
forest_loo <- function(casecontrol = TRUE, var, xlim = NULL, ...){
  dataset <- if(casecontrol == TRUE) hip_casecontrol_loo else hip_within_loo # choose which dataset
  
  label <- if(casecontrol == TRUE) c("ACL group weaker", "Control group weaker") else  c("ACL side weaker", "Contra side weaker")
  
  data <- dataset %>% # pluck data from original dataframe
    filter(analysis_group == var) %>%
    select(influence) %>%
    pluck(1) %>% 
    pluck(1)
  
  xlim <- if(is.null(xlim) & casecontrol == TRUE) c(0.5, 2.0) else if(is.null(xlim) & casecontrol == FALSE) c(0.8, 1.25) else xlim
  xlab <- log(xlim)
  
  # Plot
  forest.meta(data,
              lower.equi = exp(last(data$lower)), 
              upper.equi = exp(last(data$upper)), 
              col.equi = "green", 
              fill.equi = "green",
              leftcols = c("study", "inf"),
              leftlabs = c("Omitted Study", "Sig Influence"),
              text.random = "Original random effects model\n(All studies included)",
              rightcols = c("effect.ci", "I2"),
              backtransf = TRUE,
              fontfamily = "Karla",
              xlab = label,
              xlab.pos = xlab,
              xlim = xlim,
              ...) 
}

## Summary plot

casecontrol_summary <- hip_casecontrol_meta %>%
  select(analysis_group, estimate, ci.lb, ci.ub, pi.ub, pi.lb, i.squared, tau.squared, nobs, acl_totaln) %>%
  mutate(i.squared = round(i.squared*100, 0),
         region = case_when(
    analysis_group %in% c("calf", "soleus", "dflex") ~ "Leg",
    TRUE ~ "Hip"),
    analysis_group = recode(analysis_group,
                                 "hip abd" = "Abduction",
                                 "hip add" = "Adduction",
                                 "hip er" = "External Rotation",
                                 "hip ir" = "Internal Rotation",
                                 "hip ext" = "Extension",
                                 "hip flex" = "Flexion",
                                 "calf" = "Plantarflexion",
                                 "dflex" = "Dorsiflexion"),
    analysis_group = factor(analysis_group, levels = c("Abduction", "Adduction", "External Rotation", "Internal Rotation",
                            "Extension", "Flexion", "Plantarflexion", "Dorsiflexion"))) %>%
  arrange(desc(region), desc(analysis_group)) %>%
  mutate(pi.lb = ifelse(pi.lb < log(0.5), log(0.5), pi.lb),
         pi.ub = ifelse(pi.ub > log(2), log(2), pi.ub),)

within_summary <- hip_within_meta %>%
  select(analysis_group, estimate, ci.lb, ci.ub, pred, pi.ub, pi.lb, i.squared, tau.squared, nobs, acl_totaln) %>%
  mutate(i.squared = round(i.squared*100, 0),
         region = case_when(
    analysis_group %in% c("calf", "soleus", "dflex") ~ "Leg",
    TRUE ~ "Hip"),
    analysis_group = recode(analysis_group,
                            "hip abd" = "Abduction",
                            "hip add" = "Adduction",
                            "hip er" = "External Rotation",
                            "hip ir" = "Internal Rotation",
                            "hip ext" = "Extension",
                            "hip flex" = "Flexion",
                            "calf" = "Plantarflexion",
                            "dflex" = "Dorsiflexion"),
    analysis_group = factor(analysis_group, levels = c("Abduction", "Adduction", "External Rotation", "Internal Rotation",
                                                       "Extension", "Flexion", "Plantarflexion", "Dorsiflexion"))) %>%
  arrange(desc(region), desc(analysis_group)) %>%
  mutate(pi.lb = ifelse(pi.lb < log(0.8), log(0.8), pi.lb), # clip pi that exceed plot limits
         pi.ub = ifelse(pi.ub > log(1.25), log(1.25), pi.ub),)

png("output/plots/within summary.png", height = 700, pointsize = 25, width = 1000)
forest(
  x = within_summary$estimate, 
  ci.lb = within_summary$ci.lb,
  ci.ub = within_summary$ci.ub,
  alim = c(log(0.8), log(1.25)),
  at = c(log(0.8), log(1), log(1.25)),
  ylim = c(0,12.5),
  xlim = c(-1.1, 0.65),
  xlab = "",
  slab = within_summary$analysis_group,
  atransf = exp,
  refline = NA,
  pch = 20,
  psize = 1,
  rows = c(0.75, 1.75, 4:9),
  ilab = cbind(within_summary$nobs,
               within_summary$acl_totaln),
  ilab.xpos = c(-0.65, -0.5),
  ilab.pos = 4,
  fonts = "Karla", 
  header = c("Outcome", "Ratio of Means [95%CI]"),
  efac = c(0,1))
rect(xleft = log(0.9),
     xright = log(1.1),
     ybottom =  -0.45,
     ytop = 10.5,
     col = "#EDEDED",
     border = NA)
segments(x0 = within_summary$pi.lb,
         x1 = within_summary$pi.ub,
         y0 = c(0.75, 1.75, 4:9),
         lwd = 10, 
         col = "#F9C79F")
text(-1.1, c(2.75, 10), pos = 4, c("Lower Leg", "Hip"), font = 2)
text(x = c(-0.70, -0.5),
     y = 11.5,
     pos = 4,
     cex = 0.9,
     labels = c("No. of\nStudies", "n\nACL"))
segments(x0 = 0, y0 = -1, y1 = 10.5)
addpoly(x = within_summary$estimate, 
        ci.lb = within_summary$ci.lb,
        ci.ub = within_summary$ci.ub,
        atransf = exp,
        cex = 0.8, 
        annotate = FALSE,
        rows = c(0.75, 1.75, 4:9))
abline(h = 10.5, col = "white", lwd = 4)
par(xpd = NA)
text(x = c(log(0.8), log(1.25)),
     y = -3,
     labels = c("ACL side weaker", "Contra side weaker"))
text(x = c(log(0.8), log(1.25)), y = 7, # arrows for pi outside of limits
     label = c("<", ">"),
     cex = 1,
     col = "#F9C79F")
dev.off()

##
##
png("output/plots/casecontrol summary.png", height = 700, pointsize = 25, width = 1000)
forest(
  x = casecontrol_summary$estimate, 
  ci.lb = casecontrol_summary$ci.lb,
  ci.ub = casecontrol_summary$ci.ub,
  alim = c(log(0.5), log(2)),
  at = c(log(0.5), log(1), log(2)),
  ylim = c(0,12.5),
  xlim = c(-2.5, 1.75),
  xlab = "",
  slab = casecontrol_summary$analysis_group,
  atransf = exp,
  refline = NA,
  pch = 20,
  psize = 1,
  rows = c(0.75, 1.75, 4:9),
  ilab = cbind(casecontrol_summary$nobs,
               casecontrol_summary$acl_totaln),
  ilab.xpos = c(-1.45, -1.15),
  ilab.pos = 4,
  fonts = "Karla", 
  header = c("Outcome", "Ratio of Means [95%CI]"),
  efac = c(0,1))
rect(xleft = log(0.9),
     xright = log(1.1),
     ybottom =  -0.45,
     ytop = 10.5,
     col = "#EDEDED",
     border = NA)
segments(x0 = casecontrol_summary$pi.lb,
         x1 = casecontrol_summary$pi.ub,
         y0 = c(0.75, 1.75, 4:9),
         lwd = 10, 
         col = "#F9C79F")
text(-2.5, c(2.75, 10), pos = 4, c("Lower Leg", "Hip"), font = 2)
text(x = c(-1.55, -1.15),
     y = 11.5,
     pos = 4,
     cex = 0.9,
     labels = c("No. of \nStudies", "n\nACL"))
segments(x0 = 0, y0 = -1, y1 = 10.5)
addpoly(x = casecontrol_summary$estimate, 
        ci.lb = casecontrol_summary$ci.lb,
        ci.ub = casecontrol_summary$ci.ub,
        atransf = exp,
        cex = 0.8, 
        annotate = FALSE,
        rows = c(0.75, 1.75, 4:9))
abline(h = 10.5 , col = "white", lwd = 4)
par(xpd = NA)
text(x = c(log(0.5), log(2)),
     y = -3,
     labels = c("ACL group weaker", "Uninjured control weaker"))
text(x = c(log(0.5), log(2)), y = 8, # arrows for pi outside of limits
     label = c("<", ">"),
     cex = 1,
     col = "#F9C79F")
dev.off()

## Risk of Bias Plot
rob <- read_csv("data/raw/hiprob_final.csv")

rob <- rob %>%
  select(study, ends_with("_c")) %>%
  rename_with(~str_replace(.x, "_c", ""), ends_with("_c")) %>%
  pivot_longer(., -study, names_to = "Domain", values_to = "Value") %>%
  mutate(val2=if_else(Value == "Low", "+", if_else(Value == "High", "-", if_else(is.na(Value), "", "?")))) %>%
  mutate(Domain=factor(Domain,levels=unique(Domain))) %>%
  mutate(Value = factor(Value, levels = c("Low", "Unclear", "High")))

#Plot with traffic light table
rob.table <- ggplot(data = rob, aes(y = study, x = Domain)) +
  geom_tile(color="black", fill="white", size = 0.7) +
  geom_point(aes(color=as.factor(val2)), size=8) +
  geom_text(aes(label = val2), size = 8) +
  scale_x_discrete(position = "top", labels = c("Sequence Generation", "Allocation", "Patient Blinding", "Therapist Blinding", "Assessor Blinding",
                                                "Outcome Measurement", "Selection", "Attrition", "Analysis")) +
  scale_y_discrete(limits=rev(levels(as.factor(rob$study)))) +
  scale_color_manual(values = c("-" = "#BF0000",
                                "+" = "#02C100",
                                "?" = "#E2DF07"), na.value = "white") +
  theme_minimal() +
  coord_equal() +
  theme_mgpub() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 13, color = "black", angle = 60, hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
  
ggsave("output/plots/rob.png", width = 6.5, height = 11)



## Funnel Plot
## Hip abduction k=12

png("output/plots/funnelplot.png", height = 800, pointsize = 25, width = 1000)
par(family = "Karla")
hip_within_meta %>%
  filter(analysis_group == "hip abd") %>%
  select(rma) %>%
  pluck(1) %>%
  pluck(1) %>%
  metafor::funnel(.,
              studlab = TRUE)
dev.off()

hip_within_es %>%
  filter(analysis_group == "hip abd") %>%
  mutate(y = yi/sei, x = 1/sei) %>%
  lm(y ~ x, data = .) %>%
  summary()

