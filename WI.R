# --- Amy Jiravisitcul. 25 May 2023 ----
rm(list = ls()) # clear working environment
setwd("~/Documents/DCSC/member_data/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages('segregation')
library(segregation)
getwd()

# Data downloaded from https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment
# File link https://dpi.wi.gov/sites/default/files/wise/downloads/enrollment_by_gradelevel_certified_2022-23.zip

# --- FULL SCHOOL-LEVEL ENROLLMENT FILE ------
enrl <- read.csv('raw data/WI_enrollment_by_gradelevel_certified_2022-23/enrollment_by_gradelevel_certified_2022-23.csv')
str(enrl)
names(enrl) <- tolower(names(enrl))
levels(as.factor(enrl$group))
enrl<- enrl %>% 
  mutate(district = case_when(district_name == "Milestone Democratic School, Inc." ~ "Madison Metropolitan",
                              TRUE ~ district_name), # Change to comparison district
         school = school_name,
         n = as.numeric(gsub("[^0-9.-]","",student_count)),
         group = as.factor(group_by_value), 
         grade = grade_level) %>% 
  filter(group_by == "All Students"|
           group_by == "Disability Status"|
           group_by == "Economic Status"|
           group_by == "EL Status"|
           group_by == "Race/Ethnicity")%>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  select(county,district:group,grade)
levels(enrl$group)[1] <- "drop"

# ---- FILTERED TO ONLY WISCONSIN SCHOOLS IN THE SAME COUNTY AS MILESTONE ------
dane_cty <- enrl %>% 
  filter(county == "Dane",
         group != "drop",
         group != "Unknown",
         school != "[Districtwide]") %>% select(-grade) %>% 
  group_by(county, district, school, group) %>% 
  summarize(n= sum(n),
            .groups = 'drop') %>% 
  spread(group, n) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  group_by(county, district, school) %>% 
  summarize(amind = sum(`Amer Indian`),
            asian = sum(Asian) + sum(`Pacific Isle`),
            black = sum(Black),
            hisp = sum(Hispanic),
            white = sum(White),
            mult = sum(`Two or More`),
            ecdis = sum(`Econ Disadv`),
            el = sum(EL),
            swd = sum(SwD),
            .groups = 'drop') %>% 
  mutate(total = amind + asian + black + hisp + white + mult) %>% 
  select(county, district, school, total, amind:swd)

# ---- COUNTY- AND DISTRICT- AGGREGATES BY SUBGROUP ----
dane_agg <- dane_cty %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/sum(total),
            cty_asian = sum(asian)/sum(total),
            cty_black = sum(black)/sum(total),
            cty_hisp = sum(hisp)/sum(total),
            cty_white = sum(white)/sum(total),
            cty_mult = sum(mult)/sum(total),
            cty_ecdis = sum(ecdis)/sum(total),
            cty_el = sum(el)/sum(total),
            cty_swd = sum(swd)/sum(total)) 
memb_enrl <- dane_cty %>% 
  filter(str_detect(school,"Milestone")) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         ecdis = ecdis/total,
         el = el/total,
         swd = swd/total) %>% 
  merge(dane_agg)

mad_agg <- dane_cty %>% 
  filter(district == "Madison Metropolitan") %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/sum(total),
            dist_asian = sum(asian)/sum(total),
            dist_black = sum(black)/sum(total),
            dist_hisp = sum(hisp)/sum(total),
            dist_white = sum(white)/sum(total),
            dist_mult = sum(mult)/sum(total),
            dist_ecdis = sum(ecdis)/sum(total),
            dist_el = sum(el)/sum(total),
            dist_swd = sum(swd)/sum(total))
memb_enrl <- memb_enrl %>% merge(mad_agg)

# ----- LOCAL SEGREGATION MEASURES ------
ls_mad <- enrl %>% 
  filter(group == "Amer Indian"|
           group == "Asian"|
           group == "Black"|
           group == "Hispanic"|
           group == "Pacific Isle"|
           group == "Two or More"|
           group == "White",
         district == "Madison Metropolitan",
         school != "[Districtwide]") %>% 
  mutual_local("group", "school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

ls_dane <- enrl %>% 
  filter(group == "Amer Indian"|
           group == "Asian"|
           group == "Black"|
           group == "Hispanic"|
           group == "Pacific Isle"|
           group == "Two or More"|
           group == "White",
         county == "Dane",
         school != "[Districtwide]") %>% 
  mutual_local("group", "school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)

memb_enrl <- memb_enrl %>% 
  merge(ls_mad) %>% 
  merge(ls_dane) %>% 
  select(school, total:swd, ls_dist, ls_cty, district, dist_total:dist_swd,
         county, cty_total:cty_swd)

write.csv(memb_enrl, file = file.path('output data/wi_enrl.csv'), row.names = FALSE)

# ----- ASSESSMENTS ------
# District file downloaded from https://apps2.dpi.wi.gov/reportcards/

acad <- read_excel('raw data/WI_2021-22_district_reportcard_data.xlsx',
                   sheet = "Data")
acad_doc <- read_excel('raw data/WI_2021-22_district_reportcard_data.xlsx',
                       sheet = "About the Data")

names(acad) <- tolower(names(acad))
str(acad)

acad <- acad %>% 
  mutate(district = as.factor(`district name`),
         dist_ela = `district percent proficient ela 2022`,
         dist_math = `district percent proficient mathematics 2022`) %>% 
  select(district, dist_math, dist_ela) %>% 
  filter(district == "Madison Metropolitan")

fmemb_acad <- read_excel('raw data/WI_2021-22_school_reportcard_data.xlsx',
                        sheet = "Data")
names(memb_acad) <- tolower(names(memb_acad))
str(memb_acad)
memb_acad <- memb_acad %>% 
  mutate(school = as.factor(`district name`),
         p_ela = `school percent proficient ela 2022`,
         p_math = `school percent proficient mathematics 2022`,
         district = "Madison Metropolitan") %>% 
  select(school, p_math, p_ela) %>% 
  filter(str_detect(school, "Milestone"))
memb_acad <- merge(memb_acad, acad) %>% select(district, school, p_math, dist_math, p_ela, dist_ela)
write.csv(memb_acad, file = file.path('output data/wi_acad.csv'), row.names = FALSE)
