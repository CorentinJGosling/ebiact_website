library(officer)
library(tidyverse)
library(readxl)
library(tableHTML)
library(docxtractr)
collapsunique <- function(x) paste(unique(sort(x)), collapse = " | ")

chemin = paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche",
"/Article 2 - Base de Donnees/7 - Data analysis/data/")

modify_between_ul_p <- function(text) {
  ul_pattern <- "<ul>(.*?)</p>"
  matches <- gregexpr(ul_pattern, text, perl = TRUE)
  match_positions <- regmatches(text, matches)
  for (ul_content in match_positions[[1]]) {
    modified_text <- gsub("</p>", "</ul></li></p>", ul_content)
    text <- sub(ul_content, modified_text, text, fixed = TRUE)
  }
  return(text)
}

generate_url <- function(input) {
  url_pattern <- "= \\[([^]]+)\\]"
  
  urls <- str_extract_all(input, url_pattern)
  urls <- unlist(urls) # Flatten the list
  
  urls <- str_remove_all(urls, "= \\[")
  urls <- str_remove_all(urls, "\\]")
  
  text_segments <- str_split(input, url_pattern)
  text_segments <- unlist(text_segments) # Flatten the list
  
  text_segments <- str_trim(text_segments)
  if (length(text_segments) > length(urls)) {
    text_segments <- text_segments[1:length(urls)]
  } else if (length(text_segments) < length(urls)) {
    text_segments <- c(text_segments, rep("", length(urls) - length(text_segments)))
  }
  
  result_df <- data.frame(
    text = text_segments,
    url = urls
  )
  return(result_df)
}

filter_dfs <- function(df_list) {
  filtered_list <- lapply(df_list, function(df) {
    if (df[1, 2] != "") {
      return(df)
    } else {
      return(NULL)
    }
  })
  filtered_list <- Filter(Negate(is.null), filtered_list)
  return(filtered_list)
}
doc_path <- "C:/Users/Corentin Gosling/drive_gmail/Recherche/Article 2 - Base de Donnees/ebiact/website/interventions/list_interventions.docx"
doc <- docxtractr::read_docx(doc_path)
tables <- docx_extract_all_tbls(doc, guess_header=FALSE)

filter_tables <- filter_dfs(tables)

dat = data.frame(Interventions = rep(NA, length(filter_tables)))
dat$Ressources = dat$Description = dat$History = dat$img = dat$Acronym =
  dat$Group = NA

for (i in 1:length(filter_tables)) {
  tab = tables[[i]]
  hist = urls = urls_clickable = NA
  for (r in 5:6) {
    hist = paste0("<p>", tab[r, 2], "</p>")
    hist = gsub("§", "</p><p>", hist)
    hist = gsub(":\\+", ":<ul><li>", hist)
    hist = gsub(": \\+", ":<ul><li>", hist)
    hist = gsub("\\+", "</li><li>", hist)
    hist = modify_between_ul_p(hist)
    tab[r, 2] <- hist
  }
  
  urls = generate_url(tab[7, 2])
  
  urls_clickable = paste(
    paste0("<a href='", urls$url, "' target='_blank'>", urls$text, "</a>"),
    collapse=""
  )

  dat[i, "Acronym"] <- as.character(tab[tab[, 1] == "Acronym", 2])
  dat[i, "Interventions"] <- as.character(tab[tab[, 1] == "Name", 2])
  dat[i, "Group"] <- as.character(tab[tab[, 1] == "Group", 2])
  dat[i, "img"] <- as.character(tab[tab[, 1] == "Link image", 2])
  dat[i, "History"] <- as.character(tab[tab[, 1] == "History", 2])
  dat[i, "Description"] <- as.character(tab[tab[, 1] == "Description", 2])
  dat[i, "Ressources"] <- urls_clickable
}


dat_ur_raw = readxl::read_excel(paste0(chemin, "UR_TOTAL_analysis.xlsx"))
info_meta = readxl::read_excel(paste0(chemin, "intervention_features_level.xlsx"))

dat_ur_raw$Acronym = dat_ur_raw$intervention_general
info_meta$Acronym = info_meta$intervention_general

dat_ur = dat_ur_raw %>% group_by(Acronym) %>% slice(1)
info_meta = info_meta %>% group_by(Acronym) %>% slice(1)


dat_ur$Age_cont = factor(dat_ur$Age, levels = c("< 6 yo", "6-12 yo", "13-19 yo", ">= 20 yo"))

dat_1 = dplyr::left_join(dat, dat_ur[, 
                                    c("Acronym", "n_cct_intervention", "n_outcome_intervention",
                                      "n_meta_intervention", "meta_intervention",
                                      "URL_meta", "outcome_intervention",
                                      "Age", "IQ",
                                      "min_age_intervention", "max_age_intervention",
                                      "min_IQ_intervention", "max_IQ_intervention",
                                      "outcome_test", "outcome_report", "outcome_observation",
                                      "duration_month",
                                      "Duration")])
dat = dplyr::left_join(dat_1, info_meta)

# SECTION - Key characteristics

###################################################################################
####################               PARTICIPANTS              ######################
###################################################################################

dat_agg_age = dat_ur_raw %>% #filter(intervention_general == "SSG")
  group_by(Acronym) %>%
  arrange(mean_age) %>%
  summarise(n_age = length(unique(Age)),
            Age_bounds = collapsunique(Age))
dat = dplyr::left_join(dat, dat_agg_age)

for (age in c("Age", "Age_bounds")) {
  dat[[age]] <- gsub("< 6 yo", "very young children (<6 yo)", dat[[age]])
  dat[[age]] <- gsub("6-12 yo", "school-age children (6-12 yo)", dat[[age]])
  dat[[age]] <- gsub("13-19 yo", "adolescents (13-19 yo)", dat[[age]])
  dat[[age]] <- gsub(">= 20 yo", "adults (>=20 yo)", dat[[age]])
}
dat_ur$Age = factor(dat_ur$Age, levels = c("very young children (<6 yo)", 
                                           "school-age children (6-12 yo)", 
                                           "adolescents (13-19 yo)", 
                                           "adults (>=20 yo)"))

dat$Age_bounds = gsub("\\|", "to", dat$Age_bounds)

Age_text = with(dat, paste0("The intervention is typically designed for ",
                            ifelse(n_age == 1, 
                                   paste0("<u>", Age, "</u>"), 
                                   paste0("different age groups, <u>", Age_bounds, "</u>"))))

IQ_text = with(dat, paste0(ifelse(
  is.na(INTER_IQ), ". ",
  ifelse(INTER_IQ < 80, 
         ", with <b>important cognitive difficulties</b>. ",
         ", with <b>no specific cognitive difficulties</b>. "))))



###################################################################################
####################             DURATION AND DOSE           ######################
###################################################################################

dat_agg_dur = dat_ur_raw %>% group_by(Acronym) %>% 
  slice(c(which.min(duration_month), which.max(duration_month))) %>%
  summarise(n_duration = length(unique(Duration)),
            Duration_bounds = collapsunique(Duration))
dat = dplyr::left_join(dat, dat_agg_dur)

dat$INTER_length  = ifelse(round(dat$INTER_length) < 1,
                               ifelse(round(dat$INTER_length * 4.345) >= 1,
                                      paste0(round(dat$INTER_length * 4.345), " weeks"),
                                      paste0(round(dat$INTER_length * 4.345 * 7), " days")),
                               paste0(round(dat$INTER_length), " months"))

dat$INTER_min_length  = ifelse(round(dat$INTER_min_length) < 1,
                               ifelse(round(dat$INTER_min_length * 4.345) >= 1,
                                      paste0(round(dat$INTER_min_length * 4.345), " weeks"),
                                      paste0(round(dat$INTER_min_length * 4.345 * 7), " days")),
                               paste0(round(dat$INTER_min_length), " months"))
                               
dat$INTER_max_length  = ifelse(round(dat$INTER_max_length) < 1,
                               ifelse(round(dat$INTER_max_length * 4.345) >= 1,
                                      paste0(round(dat$INTER_max_length * 4.345), " weeks"),
                                      paste0(round(dat$INTER_max_length * 4.345 * 7), " days")),
                               paste0(round(dat$INTER_max_length), " months"))

dat$Duration_bounds = gsub("\\,", " to", dat$Duration_bounds)
Duration_text = with(dat, paste0(
  ifelse(is.na(Duration), "No specific information was available on duration. ",
         paste0("Regarding the length, we found that on average this type of intervention lasts about <b>",
                INTER_length, "</b>",
                ifelse(INTER_min_length == INTER_max_length, ". ",
                       paste0(" (from ", INTER_min_length, 
                              " to ", INTER_max_length, "). "))))))
                
                
dat$INTER_dose = ifelse(
  round(dat$INTER_dose) == 0, round(dat$INTER_dose, 2), round(dat$INTER_dose))
dat$INTER_min_dose = ifelse(
  round(dat$INTER_min_dose) == 0, round(dat$INTER_min_dose, 2), round(dat$INTER_min_dose))
dat$INTER_max_dose = ifelse(
  round(dat$INTER_max_dose) == 0, round(dat$INTER_max_dose, 2), round(dat$INTER_max_dose))

Dose_text = ifelse(is.na(dat$INTER_dose), 
       "No specific information on dosage was available. ", paste0(
         "Regarding dosage/intensity, we found that ", dat$Interventions, 
         " is provided for an average of ", dat$INTER_dose, " ", dat$INTER_dose_metric, 
         ifelse(dat$INTER_min_dose == dat$INTER_max_dose, ". ",
                paste0(" (from ", dat$INTER_min_dose, " ", dat$INTER_dose_metric,
                       " to ", dat$INTER_max_dose, " ", dat$INTER_dose_metric, "). "))))

###################################################################################
####################                IMPLEMENTER              ######################
###################################################################################
# dat1 = dat[1,]
# x = dat2$INTER_implementer_prof
# y = dat2$INTER_implementer_parent
# z = dat2$INTER_implementer_tech
# y = dat1$INTER_setting_home
# z = dat1$INTER_setting_clin
# x = dat1$INTER_setting_class
type = "set"
exam = function(x, y, z, type = "pro") {
  if (type == "pro") {
    typeok = data.frame(imp = c("Caregiver", "Professional", "technological supports"),
                        value = c(x, y, z))
  } else if (type == "set") {
    typeok = data.frame(place = c("at home", "in clinic/community", "at school"),
                        value = c(x, y, z))
  } else if (type == "test") {
    typeok = c("Test", "Report", "Observation")
  }
  
  typeok = typeok[which(typeok$value > 25), ]
  typeok = typeok[order(typeok$value, decreasing = TRUE), ]
  
  if (nrow(typeok) == 1 & type == "pro") {
    return(paste0("is typically administered by ", typeok$imp))
  } else if (type == "pro") {
    return(paste0("is mainly administered by ", typeok$imp[1],
                  ", and is supported by ", paste(typeok$imp[2:nrow(typeok)], collapse = " and ")))
  }
  
  if (nrow(typeok) == 1 & type == "set") {
    return(paste0("is delivered ", typeok$place))
  } else if (type == "set") {
    return(paste0("is primarily delivered ", typeok$place[1],
                  ", but is also provided ", paste(typeok$place[2:nrow(typeok)], collapse = " and ")))
  }
  
  
}
dat$Implementer = dat$Setting = dat$Measure = NA
for (i in 1:nrow(dat)) {
  dat$Implementer[i] = with(dat[i, ], 
       exam(x=INTER_implementer_parent, y=INTER_implementer_prof, z=INTER_implementer_tech, type = "pro"))
  dat$Setting[i] = with(dat[i, ], 
       exam(INTER_setting_home, INTER_setting_clin, INTER_setting_class, type = "set"))
}
dat$Implementer = gsub("NA and ", "", dat$Implementer)
dat$Setting = gsub("NA and ", "", dat$Setting)
# dat$Measure = gsub("NA and ", "", dat$Measure)



Implent_setting_text = with(dat, paste0(
  "This type of intervention ", stringr::str_to_lower(Setting), 
  ". It ", stringr::str_to_lower(Implementer), ". "
  ))

key = with(dat, paste0(Age_text, IQ_text, Implent_setting_text, Duration_text, Dose_text))






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
  ifelse(dat$History == "<p></p>", "", paste0("<div class='header_tab'>History of ", Acronym, "</div>")),
  
  ifelse(dat$History == "<p></p>", "", paste0("<div class='history'>", 
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

dat$"Number of clinical trials<br>[meta-analyses]" = paste0("nCCT=", dat$n_cct_intervention, " [nSR/MA=", 
                                                            dat$n_meta_intervention, "]")
dat$Outcome = dat$outcome_intervention
dat$Age = stringr::str_to_sentence(dat$Age)

dat$Age_bounds = gsub(" to ", "", dat$Age_bounds)
dat$Age_bounds = gsub("\\s*\\([^\\)]+\\)", "", dat$Age_bounds)
dat$Age_boundsSAVE = dat$Age_bounds
dat$Age_bounds = gsub("very young children", "<li>Very young children</li>", dat$Age_bounds)
dat$Age_bounds = gsub("school-age children", "<li>School-age children</li>", dat$Age_bounds)
dat$Age_bounds = gsub("adolescents", "<li>Adolescents</li>", dat$Age_bounds)
dat$Age_bounds = gsub("adults", "<li>Adults</li>", dat$Age_bounds)
dat$Age_bounds = paste0("<ul>", dat$Age_bounds, "</ul>")
dat$Age = dat$Age_bounds


dat$Age_boundsSAVE = gsub("very young children", "<li class='baby'>Very young children</li>", dat$Age_boundsSAVE)
dat$Age_boundsSAVE = gsub("school-age children", "<li class='child'>School-age children</li>", dat$Age_boundsSAVE)
dat$Age_boundsSAVE = gsub("adolescents", "<li class='adol'>Adolescents</li>", dat$Age_boundsSAVE)
dat$Age_boundsSAVE = gsub("adults", "<li class='adult'>Adults</li>", dat$Age_boundsSAVE)
dat$Age_boundsSAVE = paste0("<ul class='ul_age'>", dat$Age_boundsSAVE, "</ul>")
dat$Age = dat$Age_boundsSAVE

custom_order <- c("Overall ASD symptoms",
                  "Social-communication",
                  "Restricted/repetitive behaviors", "Sensory Profile",
                  "Acceptability", "Tolerability", "Adverse events",
                  "Language", 
                  "Global cognition (IQ)", 
                  "Specific cognition (nvIQ)",
                  "Adaptive behaviors", "Quality of life",
                  "Disruptive behaviors", "ADHD symptoms",
                  "Anxiety", "Mood related symptoms",
                  "Sleep quality", "Sleep quantity")

dat$Outcome = gsub("\\(Overall skills\\)", "", dat$Outcome)
dat$Outcome = gsub("\\(Receptive skills\\)", "", dat$Outcome)
dat$Outcome = gsub("\\(Expressive skills\\)", "", dat$Outcome)

for (i in 1:nrow(dat)) {
  words <- strsplit(dat$Outcome[i], "\\|")[[1]]
  words <- unique(trimws(words))
  ordered_vector <- factor(words, levels = custom_order)
  sorted_words <- ordered_vector[order(ordered_vector)]
  sorted_string <- paste(sorted_words, collapse = "</li><li>")
  dat$Outcome[i] <- paste0("<ul class='ul_outcome'><li>", sorted_string, "</li></ul>")
}

dat$Outcome = gsub("<li>Social\\-communication</li>", 
                   "<li class='A_CORE_SYMPT'>Social\\-communication</li>", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Overall ASD symptoms</li>", 
                   "<li class='A_CORE_SYMPT'>Overall ASD symptoms</li>", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Restricted/repetitive behaviors</li>", 
                   "<li class='A_CORE_SYMPT'>Restricted/repetitive behaviors</li>", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Sensory Profile</li>", 
                   "<li class='A_CORE_SYMPT'>Sensory Profile</li>", 
                   dat$Outcome)

dat$Outcome = gsub("<li>Disruptive behaviors</li>", 
                   "<li class='D_PROB'>Disruptive behaviors</li>", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Language", 
                   "<li class='C_LANG'>Language", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Quality of life</li>", 
                   "<li class='B_ADAPT'>Quality of life</li>", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Adaptive behaviors</li>", 
                   "<li class='B_ADAPT'>Adaptive behaviors</li>", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Global cognition (IQ)", 
                   "<li class='B_ADAPT'>Global cognition", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Specific cognition (nvIQ)", 
                   "<li class='B_ADAPT'>Specific cognition", 
                   dat$Outcome)

dat$Outcome = gsub("<li>ADHD symptoms", 
                   "<li class='E_COMORB'>ADHD symptoms", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Anxiety", 
                   "<li class='E_COMORB'>Anxiety", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Mood related symptoms", 
                   "<li class='E_COMORB'>Mood-related symptoms", 
                   dat$Outcome)

dat$Outcome = gsub("<li>Acceptability", 
                   "<li class='F_SAFETY'>Acceptability", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Tolerability", 
                   "<li class='F_SAFETY'>Tolerability", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Adverse events", 
                   "<li class='F_SAFETY'>Adverse events", 
                   dat$Outcome)


dat$Outcome = gsub("<li>Sleep quality", 
                   "<li class='G_SLEEP'>Sleep quality", 
                   dat$Outcome)
dat$Outcome = gsub("<li>Sleep quantity", 
                   "<li class='G_SLEEP'>Sleep quantity", 
                   dat$Outcome)

dat$Outcome = gsub("<li></li>", "",   dat$Outcome)


dat$Interventions = paste0('<a class="learnMORE" style="font-weight: 600" href="../html/', 
                           dat$Acronym, '.html" target="_blank"',
                           '>', dat$Interventions, '<br>[',
                           dat$Acronym, ']')
dat$Group = ifelse(dat$Group == "Psychosocial", 
                   paste0('<div class="classPSY">', dat$Group, '</div>'),
             ifelse(dat$Group ==  "Pharmacological", 
                    paste0('<div class="classPHARMA">', dat$Group, '</div>'),
             ifelse(dat$Group ==  "Complementary", 
                   paste0('<div class="classCOMP">', dat$Group, '</div>'), NA)))

# TABLE INTERVENTIONS =============================================
html_tbl = dat[, c("Group", "Interventions", "Age", "Outcome",
                   "Number of clinical trials<br>[meta-analyses]"#,  "N<br>(CCT)", "N<br>(SR/MA)", 
                   # "More information"
)] %>%
  distinct() %>%
  arrange(Interventions) %>%
  tableHTML(class='table-fill', escape = FALSE,
            rownames = FALSE)

writeLines(as.character(html_tbl),
           paste0("C:/Users/Corentin Gosling/drive_gmail/Recherche/Article 2 - Base de Donnees/ebiact/",
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
    <link rel="icon" href="../img/profile_icon_inter.svg" />
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
           "Article 2 - Base de Donnees/ebiact/website/",
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
         "Article 2 - Base de Donnees/ebiact/website/",
         "js/world_cloud.txt")
)
