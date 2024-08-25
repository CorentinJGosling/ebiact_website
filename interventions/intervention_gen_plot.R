library(jsonlite)
library(tidyverse)
chemin = paste0("D:/drive_gmail/Recherche",
                "/Article 2 - Base de Donnees/7 - Data analysis/data/")

dat_ur_raw = readxl::read_excel(paste0(chemin, "UR_TOTAL_analysis.xlsx")) %>%
  filter(IN_meta == 1) %>%
  mutate(
    # size = log(n_studies) * 10 + 5
    size = n_studies * 1.5 + 5
  ) %>%
  group_by(intervention_general) %>%
  mutate(min_studies = min(n_studies),
         max_studies = max(n_studies))


dat_ur_raw$customLabel = paste0("In ", dat_ur_raw$age_pre, " ",
       " participants, there is <b>", 
       dat_ur_raw$effect_text, "</b> on <u>", 
       dat_ur_raw$outcome_general, 
       "</u>. Based on the analysis of the quality of the evidence produced by the ", 
       dat_ur_raw$n_studies, " clinical trials exploring this effect, we have a <b>",
       tolower(dat_ur_raw$GRADE), " confidence</b> in the estimation of this effect.")


dat_ur_raw$Outcome_group = factor(dat_ur_raw$Outcome_group, levels = c(
  "Core ASD symptoms",
  "Safety",
  "Functioning",
  "Psych. comorbidity",
  "Sleep",
  "ASD-related symptoms")
)

dat_ur_raw = dat_ur_raw %>%
  arrange(Outcome_group, outcome_short)

for (INTR in unique(dat_ur_raw$intervention_general)) {
  print(INTR)
  dat_plot = dat_sig = star = background = main = NA
  dat_plot = subset(dat_ur_raw, intervention_general == INTR)
  

  dat_grade_v_low = dat_plot %>% filter(GRADE == "Very low")
  
  background_dashed <- data.frame(
    x = dat_grade_v_low$age_vshort,
    y = dat_grade_v_low$outcome_short,
    backgroundColor = "rgba(255,255, 255, 0)",
    borderColor = dat_grade_v_low$col_grade,
    borderWidth = 2,
    pointRadius = max(dat_plot$size) + 2,
    Outcome_group = dat_grade_v_low$Outcome_group,
    customLabel = dat_grade_v_low$customLabel,
    n_studies = dat_grade_v_low$n_studies
    
  )
  
  background <- data.frame(
    x = dat_plot$age_vshort,
    y = dat_plot$outcome_short,
    backgroundColor = "rgba(255,255, 255, 0)",
    borderColor = dat_plot$col_grade,
    borderWidth = dat_plot$GRADE_rank,
    pointRadius = max(dat_plot$size) + 2,
    Outcome_group = dat_plot$Outcome_group,
    customLabel = dat_plot$customLabel,
    n_studies = dat_plot$n_studies
  )
  
  dat_sig = dat_plot %>% filter(as.numeric(p_value) < 0.05)
  
  if (nrow(dat_sig) != 0) {
    star <- data.frame(
      x = dat_sig$age_vshort,
      y = dat_sig$outcome_short,
      backgroundColor = "red",
      borderColor = "#red",
      r = dat_sig$size + 5,
      pointRadius = dat_sig$size,
      Outcome_group = dat_sig$Outcome_group
      # ,
      # customLabel = dat_sig$customLabel
    )
    
  } else {
    star <- data.frame(
      x = NA,
      y = NA,
      backgroundColor = "red",
      borderColor = "#red",
      r = NA,
      pointRadius = NA,
      Outcome_group = NA,
      customLabel = NA
    )
    
  }
  
  main <- data.frame(
    x = dat_plot$age_vshort,
    y = dat_plot$outcome_short,
    r = dat_plot$size,
    backgroundColor = dat_plot$col_sig,
    # borderColor = c("rgba(255, 99, 132, 1)", "rgba(54, 162, 235, 1)", "rgba(75, 192, 192, 1)", "rgba(255, 206, 86, 1)"),
    borderWidth = 0,
    Outcome_group = dat_plot$Outcome_group
    #,
    # customLabel = dat_plot$customLabel
  )
  
  
  
  main = main %>% arrange(Outcome_group)
  background_dashed = background_dashed %>% arrange(Outcome_group)
  background = background %>% arrange(Outcome_group)
  star = star %>% arrange(Outcome_group)

  x_labels <- c("Pre-school", 
                "School-age",
                "Adolescents",
                "Adults")
  
  df_with_gaps <- main %>%
    group_by(Outcome_group) %>%
    select(y, Outcome_group) %>%
    distinct() %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()
  
  gaps <- df_with_gaps %>%
    group_by(group_id) %>%
    filter(row_number() == n()) %>%
    mutate(X = "") %>%
    ungroup()
  
  result <- bind_rows(df_with_gaps, gaps) %>%
    arrange(group_id, desc(y == "")) %>%
    select(X, y)
  y_labels = ifelse(!is.na(result$X), "", result$y)
  
  
  y_labels = c("Overall ASD symptoms" , "Social-communication", 
               "Rest./repet. behaviors", "Sensory Profile", 
               "",
               "Acceptability", "Tolerability", "Adverse events",
               "",
               "Global cognition (IQ)", "Adaptive behaviors", 
               "Quality of life", "Language (Overall)", 
               "Language (Expressive)", 
               "Language (Receptive)", 
               "",
               "ADHD symptoms", "Anxiety", "Mood related symptoms",
               "",
               "Disruptive behaviors", "Sleep quality", "Sleep quantity")
  # y_labels = unique(main$y)
  # Combine into a list
  json_data <- list(
    background_dashed = as.data.frame(background_dashed),
    background = as.data.frame(background),
    main = as.data.frame(main),
    star = as.data.frame(star),
    xLabels = x_labels,
    yLabels = y_labels
  )
  write_json(json_data, path = 
               paste0("D:/drive_gmail/Recherche/",
                      "Article 2 - Base de Donnees/ebiact/website/js/",
                      INTR, ".json"), pretty=TRUE)
}
