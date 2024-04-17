colapsunique <- function(x) paste(unique(x), collapse = " | ")

library(readxl)
library(tableHTML)
library(tidyverse)
dat = readxl::read_excel(
  paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/Article 2 - Base de Données/ebiact/website/interventions/",
         "list_interventions.xlsx"))
dat_ur = readxl::read_excel(
  paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/Article 2 - Base de Données/7 - Data analysis/data/",
         "ur_homogeneized_scored.xlsx"))
# df_agg_meta$Age <- with(df_agg_meta,
#                         ifelse(mean_age < 6, ,
#                                ifelse(mean_age < 13, ,
#                                       ifelse(mean_age < 20, , 
#                                              ifelse(mean_age >= 20, , 
#                                                     "Unknown")))))

dat_ur$Age_cont = factor(dat_ur$Age, levels = c("< 6 yo", "6-12 yo", "13-19 yo", ">= 20 yo"))
dat_ur$Acronym = dat_ur$intervention_general
dat_agg = dat_ur %>%
  group_by(Acronym) %>%
  slice(1)
dat = dplyr::left_join(dat, dat_agg[, 
       c("Acronym", "n_cct_intervention", "n_outcome_intervention",
         "n_meta_intervention", "meta_intervention",
         "URL_meta", "outcome_intervention",
         "Age", "min_age_intervention", "max_age_intervention",
         "IQ", "min_IQ_intervention", "max_IQ_intervention",
         "parent_imp", "prof_imp", "tech_imp", 
         "sett_home", "sett_clin", "sett_class",
         "outcome_test", "outcome_report", "outcome_observation",
         "duration_month",
         "Duration")])


# SECTION - Key characteristics
dat_agg_age = dat_ur %>% group_by(Acronym) %>%
  arrange(mean_age) %>%
# %>% 
#   slice(c(which.min(mean_age), which.max(mean_age))) %>%
  summarise(n_age = length(unique(Age)),
            Age_bounds = colapsunique(Age))
dat = dplyr::left_join(dat, dat_agg_age)
for (age in c("Age", "Age_bounds")) {
  dat[[age]] <- gsub("< 6 yo", "very young children (<6 yo)", dat[[age]])
  dat[[age]] <- gsub("6-12 yo", "school-age children (6-12 yo)", dat[[age]])
  dat[[age]] <- gsub("13-19 yo", "adolescents (13-19 yo)", dat[[age]])
  dat[[age]] <- gsub(">= 20 yo", "adults (>19 yo)", dat[[age]])
}
dat$Age_bounds = gsub("\\|", "to", dat$Age_bounds)

Age_text = with(dat, paste0("The intervention is typically designed for ",
                            ifelse(n_age == 1, 
                                   paste0("<u>", Age, "</u>, "), 
                                   paste0("different age groups, <u>", Age_bounds, "</u>, but "))))

IQ_text = with(dat, paste0("with ",
                           ifelse(IQ == "Low (< 70)", 
                                  "<u>important cognitive difficulties</u>. ",
                                  "<u>no specific cognitive difficulties</u>. ")))

dat_agg_dur = dat_ur %>% group_by(Acronym) %>% 
  slice(c(which.min(duration_month), which.max(duration_month))) %>%
  summarise(n_duration = length(unique(Duration)),
            Duration_bounds = colapsunique(Duration))
dat = dplyr::left_join(dat, dat_agg_dur)

dat$Duration_bounds = gsub("\\|", "to", dat$Duration_bounds)
Duration_text = with(dat, paste0("Regarding the length, this type of intervention lasts ",
                            ifelse(n_duration == 1, 
                                   paste0("around <u>", Duration), 
                                   paste0("from <u>", Duration_bounds)),
                            "</u>. "))



exam = function(x, y, z, type = "pro") {
  if (type == "pro") {
    typeok = c("Profesionnal", "Caregiver", "Technology")
  } else if (type == "set") {
    typeok = c("Home", "Clinic", "School")
  } else if (type == "test") {
    typeok = c("Test", "Report", "Observation")
  }
  res = ifelse(x>30, typeok[1], NA)
  res = ifelse(y>30, paste(c(res, typeok[2]), collapse = " and "), res)
  res = ifelse(z>30, paste(c(res, typeok[3]), collapse = " and "), res)
  return(res)
}
dat$Implementer = dat$Setting = dat$Measure = NA
for (i in 1:nrow(dat)) {
  dat$Implementer[i] = with(dat[i, ], exam(parent_imp, prof_imp, tech_imp, type = "pro"))
  dat$Setting[i] = with(dat[i, ], exam(sett_home, sett_clin, sett_class, type = "set"))
  dat$Measure[i] = with(dat[i, ], exam(outcome_test, outcome_report, outcome_observation,  type = "test"))
}
dat$Implementer = gsub("NA and ", "", dat$Implementer)
dat$Setting = gsub("NA and ", "", dat$Setting)
dat$Measure = gsub("NA and ", "", dat$Measure)

Implent_setting_text = with(dat, paste0(
  "This type of intervention generally takes place at ", stringr::str_to_lower(Setting), 
  " and is dispensed by ",
  stringr::str_to_lower(Implementer), ". "
))

key = with(dat, paste0(Age_text, IQ_text, Implent_setting_text, Duration_text))


# SECTION - What we found in the scientific literature
combined <- Map(function(x, y) paste("<a href='", y, "' target='_blank'>", x, "</a>", sep = ""), 
                strsplit(dat$meta_intervention, " \\| "), 
                strsplit(dat$URL_meta, " \\| "))
list_meta = lapply(combined, function(x) paste0("<ul class='hrztl_list'>", paste("<li>", x, collapse="</li>"), 
                                                "</li></ul>"))
database = with(dat, paste0(
  "There is a total of ", n_meta_intervention, " meta-analyses (synthesizing",
  " the results of <u>", n_cct_intervention, " clinical trials</u>). They explored the effects",
  " of ", Interventions, " on ", n_outcome_intervention, " different oucomes (",
  outcome_intervention, ")."
))

button = paste0('<div class="btn_container"><a href="interventions.html" class="button"
                ><i class="far fa-arrow-alt-circle-left fa-2x"></i>Back to other
                interventions</a
                ></div>')
nrow = 1:nrow(dat)
dat$text = with(dat, paste0(
  paste0("<div class='desc_tab'>More information about<br>", 
         Interventions, " (", Acronym, ")</div>"),
  button,
  
  "<div class='hero'>",
    "<div class='hero_image'>",
      paste0("<img src='", dat$img, "' />"),
    "</div>",
    "<div class='hero_left'>",
      "<div class='hero_history'>",
      ifelse(is.na(dat$History), "", paste0("<div class='header_tab'>History of ", Acronym, "</div>")),
       
      ifelse(is.na(dat$History), "", paste0("<div class='history'>", 
                                            History, 
                                            "</div>")),
      "</div>", # hero history
      "<div class='hero_description'>",
      "<div class='header_tab'>Description of ", Acronym, "</div>",
      Description,
      "</div>", # hero description
    "</div>", # hero left
  "</div>", # hero 
  
  '<hr class="custom-hr">',
  "<div class='header_shalf'>What we found in the scientific literature</div>",
  
  "<div class='shalf'>",
    "<div class='shalf_carac'>",
      "<div class='header_tab'>Key characteristics of ", Acronym, "</div>",
      key,
    "</div>",
    "<div class='shalf_literature'>",
      "<div class='header_tab'>Scientific papers</div>",
      database,
    "</div>",
  "</div>",
  
  "<div class='bottom_ressources'>",
  
    paste0("<div class='header_tab'>Links to identified meta-analyses for ", dat$Acronym, "</div>"),
    "<div class='ressources'>",
    list_meta,
    "</div><br>",

    "<div class='header_tab'>Additional ressources</div>",
    "<div class='ressources'><ul>", 
    gsub("</a>", "</li></a>", gsub("<a", "<li><a", Ressources)), 
    "<ul></div>",
  "</div>"

))

dat$'More information' = paste0('<a class="learnMORE" href="../html/', 
                                dat$Acronym, '.html" target="_blank"',
                                '>Learn<br>more</a>')

# dat$'More information' = paste0('<button class="learnMORE" onclick="openModal',
#                    "(this)",
#                    '">Learn more</button>')

# dat$"N<br>(CCT)" = dat$n_cct_intervention
# dat$"N<br>(SR/MA)" = dat$n_meta_intervention
dat$"Number of clinical trials<br>[meta-analyses]" = paste0("nCCT=", dat$n_cct_intervention, " [nSR/MA=", 
                                                         dat$n_meta_intervention, "]")
dat$Outcome = dat$outcome_intervention
dat$Age = stringr::str_to_sentence(dat$Age)

dat$Age_bounds = gsub(" to ", "", dat$Age_bounds)
dat$Age_bounds = gsub("\\s*\\([^\\)]+\\)", "", dat$Age_bounds)

dat$Age_bounds = gsub("very young children", "<li>Very young children</li>", dat$Age_bounds)
dat$Age_bounds = gsub("school-age children", "<li>School-age children</li>", dat$Age_bounds)
dat$Age_bounds = gsub("adolescents", "<li>Adolescents</li>", dat$Age_bounds)
dat$Age_bounds = gsub("adults", "<li>Adults</li>", dat$Age_bounds)
dat$Age_bounds = paste0("<ul>", dat$Age_bounds, "</ul>")
dat$Age = dat$Age_bounds
for (i in 1:nrow(dat)) {
  words <- strsplit(dat$Outcome[i], "\\|")[[1]]
  words <- trimws(words)
  sorted_words <- sort(words)
  sorted_string <- paste(sorted_words, collapse = "</li><li>")
  dat$Outcome[i] <- paste0("<ul><li>", sorted_string, "</li></ul>")
}
dat$Outcome = gsub("<li></li>", "",   dat$Outcome)
dat$Interventions = paste0('<a class="learnMORE" href="../html/', 
                            dat$Acronym, '.html" target="_blank"',
                            '>', dat$Interventions, '</a>')
dat$Group = ifelse(dat$Group == "Psychosocial", 
                   paste0('<div class="classPSY">', dat$Group, '</div>'),
                   paste0('<div class="classCOMP">', dat$Group, '</div>'))

# TABLE INTERVENTIONS =============================================
html_tbl = dat[, c("Group", "Interventions", "Age", "Outcome",
                   "Number of clinical trials<br>[meta-analyses]"#,  "N<br>(CCT)", "N<br>(SR/MA)", 
                   # "More information"
                   )] %>%
            distinct() %>%
  filter(grepl("ychosocial", Group, fixed=TRUE)) %>%
  arrange(Interventions) %>%
  tableHTML(class='table-fill', escape = FALSE,
            rownames = FALSE)

writeLines(as.character(html_tbl),
           paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/Article 2 - Base de Données/ebiact/",
                  "website/interventions/interventions_list",
                  ".html"))
# INDIVIDUAL PAGES =============================================
for (fact in dat$Acronym) {
  
 page = '<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />

    <!----======== CSS ======== -->
    <link rel="stylesheet" href="../styles/interventions_pages.css" />
    <link rel="stylesheet" href="../styles/nav.css" />
    <script src="https://unpkg.com/scrollreveal"></script>
    <link
      rel="stylesheet"
      href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@48,400,0,0"
    />
    <!----===== Boxicons CSS ===== -->
    <link
      rel="stylesheet"
      href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.14.0/css/all.min.css"
    />
    <script src="https://cdnjs.cloudflare.com/ajax/libs/animejs/3.2.1/anime.min.js"></script>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script src="https://code.jquery.com/ui/1.13.1/jquery-ui.min.js"></script>
    <link
      href="https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"
      rel="stylesheet"
    />
    <script src="https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js"></script>

    <!--<title>Dashboard Sidebar Menu</title>-->
  </head>
  <body>'  
  writeLines(as.character(
    paste0(
      page,
      dat[dat$Acronym == fact, "text"],
      "</body>
        </html>"
    )),
    paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/",
             "Article 2 - Base de Données/ebiact/website/",
             "html/",
              fact,
             ".html"))
  
}
word = NULL
for (i in 1:nrow(dat)){
  w = rep(dat$Acronym[i], each=dat$n_cct_intervention[i])
  
  word = paste0(word, ", ", paste(w, collapse=", "))
}

rio::export(
  data.frame(word),
  paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/",
  "Article 2 - Base de Données/ebiact/website/",
  "js/world_cloud.txt")
)
