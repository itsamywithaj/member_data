# --- Amy Jiravisitcul. 22 May 2023 ----
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

# ----- DETROIT PREP ENROLLMENT BY RACE --------
# https://www.mischooldata.org/DistrictSchoolProfiles2/StudentInformation/StudentCounts/StudentCount.aspx

# School = Detroit Prep (03032)
# Report Category = Race/Ethnicity
# All Grades, All Students
dp <- read.csv('raw data/MI_DPStudentCountSnapshot.csv', header = FALSE)
names(dp) <- tolower(dp[3,])
dp <- dp[-(1:3),] %>% 
  mutate(school = `location name`,
         race = `student group`,
         n = `student count`) %>% 
  select(school:n) %>% 
  spread(race, n) %>%
  mutate(district = "Detroit Public Schools Community District",
         county = "Wayne",
         total = as.numeric(`All Students`),
         amind = as.numeric(`American Indian or Alaska Native`),
         asian = as.numeric(Asian),
         black = as.numeric(`African American`),
         hisp = as.numeric(`Hispanic/Latino`),
         white = as.numeric(White),
         mult = as.numeric(`Two or More Races`)) %>% 
    select(school, district:mult)

# ---- DETROIT PUBLIC SCHOOLS COMMUNITY DISTRICT ----
# Data files for download at https://www.mischooldata.org/k-12-data-files/
# District = Detroit Public Schools Community (82015)
# ISD (Intermediate School District) = Wayne RESA (82)
# School = All Schools in District
# Report Category = Student Counts (no crosstab)
# Requires email submission of CSV file and file to download within 72 hours
enrl <- read.csv('raw data/MI_86336553-7b62-422c-aa99-81dc74a0fd8a.csv')
names(enrl) <- tolower(names(enrl))
str(enrl)
enrl <- enrl %>% 
  mutate(district = districtname,
         school = buildingname,
         county = countyname,
         total = total_enrollment,
         amind = american_indian_enrollment,
         asian = asian_enrollment + hawaiian_enrollment,
         black = african_american_enrollment,
         hisp = hispanic_enrollment,
         white = white_enrollment,
         mult = two_or_more_races_enrollment,
         ecdis = as.numeric(economic_disadvantaged_enrollment),
         ell = as.numeric(gsub("[^0-9.-]", "", english_language_learners_enrollment)),
         swd = as.numeric(gsub("[^0-9.-]", "", special_education_enrollment))) %>% 
  select(district:swd)

detroit <- enrl %>% 
  filter(school == "All Buildings") %>% 
  mutate(dist_total = total,
         dist_amind = amind/total,
         dist_asian = asian/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total,
         dist_ecdis = ecdis/total,
         dist_ell = ell/total,
         dist_swd = swd/total) %>% 
  select(district, dist_total:dist_swd)
names(enrl)
dist_ls <- rbind(dp, enrl[,1:10]) %>% # Add Detroit Prep back into the data
  select(-total, -county) %>% 
  filter(school != "All Buildings") %>% # Remove the full district row
  gather(race, n, amind:mult) %>%
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(school == "Detroit Prep") %>% 
  mutate(ls_dist = ls) %>% select(school, ls_dist)

dp <- dp %>% merge(dist_ls, by = "school") %>% 
  merge(detroit)

# ------ WAYNE COUNTY -------
# Data files for download at https://www.mischooldata.org/k-12-data-files/
# District = All Districts
# ISD (Intermediate School District) = Wayne RESA (82)
# School = All Schools in District
# Report Category = Student Counts (no crosstab)
# Requires email submission of CSV file and file to download within 72 hours

wayne <- read.csv('raw data/MI_Wayne4c75c1bd-453d-41da-8086-b8c1c6787fe8.csv')
names(wayne) <- tolower(names(wayne))
wayne <- wayne %>% 
  mutate(district = districtname,
         school = buildingname,
         county = countyname,
         total = total_enrollment,
         amind = american_indian_enrollment,
         asian = asian_enrollment + hawaiian_enrollment,
         black = african_american_enrollment,
         hisp = hispanic_enrollment,
         white = white_enrollment,
         mult = two_or_more_races_enrollment,
         ecdis = as.numeric(economic_disadvantaged_enrollment),
         ell = as.numeric(gsub("[^0-9.-]", "", english_language_learners_enrollment)),
         swd = as.numeric(gsub("[^0-9.-]", "", special_education_enrollment))) %>% 
  select(district:swd)

dp <- wayne %>% 
  filter(district == "All Districts") %>% 
  mutate(cty_total = total,
         cty_amind = amind/total,
         cty_asian = asian/total,
         cty_black = black/total,
         cty_hisp = hisp/total,
         cty_white = white/total,
         cty_mult = mult/total,
         cty_ecdis = ecdis/total,
         cty_ell = ell/total,
         cty_swd = swd/total) %>% 
  select(county, cty_total:cty_swd) %>% merge(dp, by= "county")

names(dp)
names(wayne)
cty_ls <- rbind(dp[,c(1,12:20)], wayne[,1:10]) %>% # Add Detroit Prep back into the data
  filter(district != "All Districts") %>% # Remove the full Wayne County aggregate row 
  mutate(school = paste(district, school, sep = "_")) %>% # make a unique column to avoid dupliccate named schools of separate districts
  select(-total, -district, -county) %>%
  gather(race, n, amind:mult) %>%
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(school == "Detroit Public Schools Community District_Detroit Prep") %>%
  mutate(school = "Detroit Prep",
         ls_cty = ls) %>% select(school, ls_cty)

dp <- dp %>% merge(cty_ls, by = "school")

# ----- SUBGROUPS FOR DETROIT PREP -----
# https://www.mischooldata.org/DistrictSchoolProfiles2/StudentInformation/StudentCounts/StudentCount.aspx
# Use drop downs rather than exporting and cleaning a file for one member
dp$ecdis = 225/376
dp$ell = 0
dp$swd = 29/376

# ----- PREP FOR FILE EXPORT -----
dp <- dp %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total) %>% 
  select(school, total:mult,ecdis:swd,ls_dist, ls_cty, district, dist_total:dist_swd,
         county, cty_total:cty_swd)

write.csv(dp,file = file.path('output data/mi_enrl.csv'), row.names = FALSE)

# ------ 3-8 ASSESSMENTS -----
acad <- read.csv('raw data/MI_assessments7f2f1c0b-29c1-497e-b3b3-f269152efce8.csv')
names(acad) <- tolower(names(acad))
str(acad)
acad <- acad %>% 
  mutate(district = as.factor(districtname),
         school = as.factor(buildingname),
         grade = as.factor(gradecontenttested),
         n_tests = as.numeric(numberassessed),
         n_prof = as.numeric(gsub("[^0-9.-]", "", totalmet))) %>% 
  filter(reportcategory == "All Students",
         subject == "ELA"|
           subject == "Mathematics",
         testtype == "M-STEP") %>%
  select(subject, district:n_prof) %>% 
  mutate_if(is.numeric, ~replace_na(.,0))
levels(acad$district)
memb_acad <- acad %>% 
  filter(school == "All Buildings" & district == "Detroit Public Schools Community District"|
           school == "Detroit Prep") %>% 
  select(-district, -grade) %>% 
  group_by(school, subject) %>% 
  summarize(n_tests = sum(n_tests),
            n_prof = sum(n_prof),
            .groups = 'drop') %>% 
  mutate(p_prof = n_prof/n_tests) %>% 
  select(-n_prof, -n_tests) %>% 
  spread(subject, p_prof)

write.csv(memb_acad, file = file.path('output data/MI_acad.csv'), row.names = FALSE)
#----- END ------