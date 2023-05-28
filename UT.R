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

# Data downloaded from https://www.schools.utah.gov/data/reports?mid=1424&tid=4
# File accessed from https://www.schools.utah.gov/file/3bfa46ef-fd48-4b24-a925-20aab2f36a6b

# --- FULL SCHOOL-LEVEL ENROLLMENT FILE ------
enrl <- read_excel('raw data/UT_2023FallEnrollmentGradeLevelDemographics.xlsx', sheet = "By School")
str(enrl)
names(enrl) <- tolower(names(enrl))
enrl <- enrl %>% 
  mutate(district = `lea name`,
         school = `school name`,
         district = case_when(district == "Beehive Science & Technology Academy" ~ "Canyons District",
                              TRUE ~ district), # rename for comparison district
         total = `total k-12`,
         amind = `american indian`,
         asian = asian + `pacific islander`,
         black = `afam/black`,
         hisp = hispanic,
         white = white,
         mult = `multiple race`,
         econdis = `economically disadvantaged`,
         el = `english learner`,
         swd = `student with a disability`) %>% 
  select(district, school, total, amind, asian, black, hisp, white, mult:swd)

agg_canyons <- enrl %>% 
  filter(district == "Canyons District")%>% 
  na.omit() %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hisp = sum(hisp)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_mult = sum(mult)/dist_total,
            dist_ecdis = sum(econdis)/dist_total,
            dist_el = sum(el)/dist_total,
            dist_swd = sum(swd)/dist_total)

memb_enrl <- enrl %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         econdis = econdis/total,
         el = el/total,
         swd = swd/total) %>% 
  filter(str_detect(school,"Beehive Science")) %>% 
  merge(agg_canyons)

locseg <- enrl %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  filter(district == "Canyons District") %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% select(school, ls_dist)

memb_enrl <- locseg %>% 
  filter(str_detect(school,"Beehive Science")) %>% 
  merge(memb_enrl)

# ----- COUNTY-LEVEL ----
cty_agg <- read_excel('raw data/UT_2023FallEnrollmentGradeLevelDemographics.xlsx', sheet = "By County")
str(cty_agg)
names(cty_agg) <- tolower(names(cty_agg))
cty_agg <- cty_agg %>% 
  mutate(total = `total k-12`,
         cty_total = total,
         amind = `american indian`,
         asian = asian + `pacific islander`,
         black = `afam/black`,
         hisp = hispanic,
         white = white,
         mult = `multiple race`,
         econdis = `economically disadvantaged`,
         el = `english learner`,
         swd = `student with a disability`,
         cty_amind = amind/total,
         cty_asian = asian/total,
         cty_black = black/total,
         cty_hisp = hisp/total,
         cty_white = white/total,
         cty_mult = mult/total,
         cty_econdis = econdis/total,
         cty_el = el/total,
         cty_swd = swd/total) %>% 
  select(county, cty_total, cty_amind:cty_swd)
cty_agg <- cty_agg %>% 
  filter(str_detect(county, "Salt Lake"))
# Salt Lake City County
# Districts based on https://en.wikipedia.org/wiki/Salt_Lake_County,_Utah#Communities

memb_enrl <- enrl %>% 
  filter(district == "Canyons District"|
           district == "Jordan District"|
           district == "Granite District"|
           district == "Salt Lake City District"|
           district == "Murray District") %>% 
  mutate(county = "Salt Lake") %>% 
  select(county, school, amind:mult) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>%  #  county-level segregation measure
  mutate(ls_cty = ls) %>% select(school, ls_cty) %>% 
  merge(memb_enrl, by= "school") %>%  # add to existing clean data
  mutate(county = "Salt Lake") %>% # add column for county to merge with aggregate figures
  merge(cty_agg, by= "county")%>%
  select(school, total:swd, ls_dist, ls_cty,district, dist_total:dist_swd,
         county, cty_total:cty_swd)

write.csv(memb_enrl, file = file.path('output data/ut_enrl.csv'), row.names = FALSE)

# ----- ASSESSMENTS --------
# Downloaded from https://www.schools.utah.gov/file/4107b79b-083b-41b9-a023-a0f5eff2127c

acad <- read_excel('raw data/UT_RISEAspireProficiencyLevels2022.xlsx',
                   sheet = "LEA Prof Levels by Subject")
names(acad) <- tolower(names(acad))
str(acad)

acad<- acad %>% 
  mutate(district = `lea name`,
         subject = `subject area`,
         test = `assessment type`,
         p_prof = as.numeric(`proficient`) + as.numeric(`highly proficient`)) %>% 
  filter(subject == "English Language Arts"|
           subject == "Mathematics") %>% 
  select(district, subject, test, p_prof)

memb_acad <- acad %>% 
  filter(str_detect(district, "Beehive")|
           district == "Canyons District") %>% 
  spread(subject, p_prof)

write.csv(memb_acad,file = file.path('output data/ut_acad.csv'), row.names = FALSE)
