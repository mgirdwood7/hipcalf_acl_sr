# Functions

# Function for combining groups and adding as new columns
combine_groups <- function(data, x){
  
  # creates the column calls for the formula. Use of sym + {{}} to work with dplyr later
  n1 <- sym("n_1")
  n2 <- sym("n_2")
  m1 <- sym(paste0(x, "_mean_1")) 
  m2 <- sym(paste0(x, "_mean_2"))
  sd1 <- sym(paste0(x, "_sd_1"))
  sd2 <- sym(paste0(x, "_sd_2"))

  # names of new variables (no suffix)
  mean_name = paste0(x, "_mean")
  sd_name = paste0(x, "_sd")
  n_name = "n"
  
  # new data frame
  # need to use walrus := with !!names
  data %>%
    mutate(!!n_name := {{ n1 }} + {{ n2 }},
           !!mean_name := ({{ n1 }} * {{ m1 }} + {{ n2 }} * {{ m2 }})/({{ n1 }} + {{ n2}}),
           !!sd_name := sqrt((({{ n1 }} - 1) * {{ sd1 }}^2 + ({{ n2 }} - 1) * {{ sd2 }}^2 + ((({{ n1 }} * {{ n2 }})/({{ n1 }} + {{ n2 }})) * ({{ m1 }}^2 + {{ m2 }}^2 - 2 * {{ m1 }} *
                                                                                                {{ m2 }})))/({{ n1 }} + {{ n2 }} - 1))) 
}



# Combine means - provide with a vectors of n, means to combine
combine_mean <- function(ncol, meancol){
  n <- ncol
  mean <- meancol
  
out <- sum({{n}}*{{mean}})/sum({{n}})
return(out)
}

# Combine sds - provide with a vectors of n, means, sd to combine
combine_sd <- function(ncol, meancol, sdcol){
  n <- ncol
  mean <- meancol
  sd <- sdcol
  
  mean_combined <- sum({{n}}*{{mean}})/sum({{n}})
  out <- sqrt((sum(({{n}}-1)*{{sd}}^2) + sum({{n}}*({{mean}}-mean_combined)^2))/(sum({{n}})-1))
  return(out)
}
  
  

# function for creating a tidy output for meta, like broom::tidy, as not available for class: meta objects.
tidymeta <- function(x){
  tibble(
    "estimate" = x$TE.random,
    "std.error" = x$seTE.random,
    "ci.lb" = x$lower.random,
    "ci.ub" = x$upper.random,
    "statistic" = x$statistic.random,
    "p.value" = x$pval.random,
    "pred" = x$seTE.predict,
    "pi.lb" = x$lower.predict,
    "pi.ub" = x$upper.predict,
    "i.squared" = x$I2,
    "h" = x$H,
    "tau.squared" = x$tau2,
    "tau.squared.se" = x$se.tau2,
    "cochran.qe" = x$Q,
    "p.value.cochran.qe" = x$pval.Q,
    "df.residual" = x$df.Q,
    "nobs" = x$k)
}


# Function to extract information from notes section of data entry form - (e.g. median, iqr etc)
extract_string <- function(x){
  # locate position of each element
  inj_pos <- str_locate(x, "(?<!-)Inj|(?<!-)inj|(?<!-)Injured") # need to make sure "inj" not preceded by non-inj etc
  noninj_pos <- str_locate(x, "Non-inj|non-inj|Noninj|noninj")
  lsi_pos <- str_locate(x, "LSI|lsi")
  
  # extract string based on end of "inj" string location, and start of next string location
  inj_sub <- if (is.na(lsi_pos[1]) & is.na(noninj_pos[1])) { # if only inj information, then take from end of inj location to end of string
    substring(x, inj_pos[2]+1, nchar(x)) 
  } else if(is.na(noninj_pos[1])) { # if no non-inj, then take from end of inj locaiton to start of lsi
    substring(x, inj_pos[2]+1, lsi_pos[1]-1)
  } else { # otherwise split based on positions as normal
    substring(x, inj_pos[2]+1, noninj_pos[1]-1)
  }
  
  noninj_sub <- if(is.na(lsi_pos[1])) {
    substring(x, noninj_pos[2]+1, nchar(x)) 
  } else {
    substring(x, noninj_pos[2]+1, lsi_pos[1]-1) 
  }
  
  lsi_sub <- substring(x, lsi_pos[2]+1, nchar(x)) 
  
  # remove any punctiation and trim whitespace
  inj_sub <- inj_sub %>% str_replace(",|;", "") %>% str_trim()
  noninj_sub <- noninj_sub %>% str_replace(",|;", "") %>% str_trim()
  lsi_sub <- lsi_sub %>% str_replace(",|;", "") %>% str_trim()
  
  return(data.frame(inj_sub, noninj_sub, lsi_sub))
}


# Similar function to above: extract information from notes section of data entry form - (e.g. median, iqr etc)
extract_string_data <- function(x, name){
  # locate position of each element
  med_pos <- str_locate(x, "MD|Median") # 
  iqr_pos <- str_locate(x, "IQR|iqr")
  range_pos <- str_locate(x, "Range|range")
  
  
  med <- if (is.na(iqr_pos[1]) & is.na(range_pos[1])) {
      substring(x, med_pos[2]+1, nchar(x))
    } else if (is.na(iqr_pos[1])) {
      substring(x, med_pos[2]+1, range_pos[1]-1)
    } else {
      substring(x, med_pos[2]+1, iqr_pos[1]-1)
    }
  
  iqr <- if (is.na(range_pos[1])) {
      substring(x, iqr_pos[2]+1, nchar(x))
    } else {
      substring(x, iqr_pos[2]+1, range_pos[1]-1)
    } 
  
  range <- substring(x, range_pos[2]+1, nchar(x)) 
  
  # remove any punctuation and symbols and remove all white space
  med <- med %>% str_replace(",|;|\\(|\\)", "") %>% str_trim()
  iqr <- iqr %>% str_replace(",|;|\\(|\\)", "") %>% str_replace("to", "-") %>% str_replace_all(" ", "")
  range <- range %>% str_replace(",|;|\\(|\\)", "") %>% str_replace("to", "-") %>% str_replace_all(" ", "")
  
  return(data.frame(med, iqr, range) %>% rename_with(., ~paste0(name, .x))) # paste name of data before each name
}


# Plot function

mv_plot <- function(mv, data, type){
  
  if(type == "linear") {points <- data.frame(predict(robust(mv, cluster = cohort, clubSandwich = TRUE), newmods = seq(1,200, length = 200))) %>% mutate(x = row_number())}
  
  if(type == "poly") {
    points <- data.frame(predict(robust(mv, cluster = cohort, clubSandwich = TRUE), newmods = unname(poly((seq(1,200, length = 200)), degree=2, raw=TRUE)))) %>% 
      mutate(x = row_number())}
  
  if(type == "spline"){
    knots <- attr(rcs(model.matrix(robust(mv, cluster = cohort, clubSandwich = TRUE))[,2], 3), "parms")
    points <- data.frame(predict(robust(mv, cluster = cohort, clubSandwich = TRUE), newmods = rcspline.eval((seq(1,200, length = 200)), knots, inclx = TRUE))) %>%
      mutate(x = row_number())}
  
  if(type == "log") {
    points <- data.frame(predict(robust(mv, cluster = cohort, clubSandwich = TRUE), newmods = log(seq(1,200, length = 200)))) %>% mutate(x = row_number())
  }
  
  if(type == "factor") {
    points <- data.frame(predict(robust(mv, cluster = cohort, clubSandwich = TRUE), newmods = diag(1, mv$tau2s, mv$tau2s))) %>% mutate(x = c(3,6,9,12,24,48,96))
  }

#get the ci points
poly <- points %>% 
  select(ci.ub, x) %>% 
  bind_rows(., points %>% 
              select(ci.lb, x) %>% 
              rename(ci.ub = ci.lb) %>% 
              arrange(desc(x))) # reverse them so the path isnt crossed

#get the pi points
#poly2 <- points %>% 
#  select(pi.ub, x) %>% 
#  bind_rows(., points %>% 
#             select(pi.lb, x) %>% 
#              rename(pi.ub = pi.lb) %>% 
#              arrange(desc(x))) # reverse them so the path isnt crossed

#plot

if(type == "factor") {
  data %>%
    ggplot(aes(x = as.numeric(as.character(timepoint_cut)), y = yi, group = interaction(cohort, group))) +
    geom_point(aes(size = acl_n), colour = "black", alpha = 0.3) + 
    geom_line(colour = "dark grey", alpha = 0.8) + 
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Time since surgery (Months)", y = "Percentage Deficit") +
    scale_size(range = c(0, 10)) +
    #geom_smooth(aes(x = timepoint_mean, y = yi), method = "lm", colour = "green", inherit.aes = FALSE) +
    #geom_abline(intercept = -0.2198, slope = 0.0014, colour = "red") +
    geom_line(data = points, aes(x = x, y = pred), colour = "orange", inherit.aes = FALSE) +
    geom_polygon(data = poly, aes(x = x, y = ci.ub), fill = "orange", alpha = 0.3, inherit.aes = FALSE) 
  #geom_polygon(data = poly2, aes(x = x, y = pi.ub), colour = "grey", alpha = 0.1, inherit.aes = FALSE)
} else {
data %>%
  ggplot(aes(x = timepoint_mean, y = yi, group = interaction(cohort, group))) +
  geom_point(aes(size = acl_n), colour = "black", alpha = 0.3) + 
  geom_line(colour = "dark grey", alpha = 0.8) + 
  scale_y_continuous(labels = scales::percent) +
  scale_size(range = c(0, 10)) +
  labs(x = "Time since surgery (Months)", y = "Percentage Deficit") +
  #geom_smooth(aes(x = timepoint_mean, y = yi), method = "lm", colour = "green", inherit.aes = FALSE) +
  #geom_abline(intercept = -0.2198, slope = 0.0014, colour = "red") +
  geom_line(data = points, aes(x = x, y = pred), colour = "orange", inherit.aes = FALSE) +
  geom_polygon(data = poly, aes(x = x, y = ci.ub), fill = "orange", alpha = 0.3, inherit.aes = FALSE) 
  #geom_polygon(data = poly2, aes(x = x, y = pi.ub), colour = "grey", alpha = 0.1, inherit.aes = FALSE)
}
}

