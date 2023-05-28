# --- Amy Jiravisitcul. 24 May 2023 ----
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

# ----- ENROLLMENT BY RACE w/ LOCAL SEG NUMBERS --------
# Data downloaded from https://ed.sc.gov/data/other/student-counts/active-student-headcounts/
# 135-day head count 2022-2023 data
# includes Liberty STEAM, member founded in 2020
enrl <- read_excel('raw data/SC_School Headcount by Gender, Ethnicity and Pupils in Poverty.xlsx')
names(enrl) <- tolower(enrl[5,]) # rename columns
names(enrl)[5:15] <- tolower(enrl[6,5:15])
enrl <- enrl[-c(1:6,1222:1224),-c(5:7)] # remove empty rows and duplicate "missing" column and statewide total row
names(enrl)
str(enrl) 
enrl[(str_detect(enrl$school,"Liberty STEAM")),2] <- "Sumter 01"

wide_enrl <- enrl %>% 
  mutate(total = as.numeric(`total # actively enrolled students`),
         amind = as.numeric(`american indian`)/total,
         asian = (as.numeric(asian)+as.numeric(`hawaiian or other pacific islander`))/total,
         black = as.numeric(`black or african-american`)/total,
         hisp = as.numeric(`hispanic or latino`)/total,
         white = as.numeric(white)/total,
         mult = as.numeric(`two or more races`)/total,
         frpl = as.numeric(`pupils in poverty`)/total) %>% # stand-in for FRPL
  select(`school id`,district, school, total, amind,asian,black,hisp,white, mult, frpl)

memb_enrl <- wide_enrl %>% 
  filter(str_detect(school, "Liberty STEAM"))

long_enrl <- enrl %>%
  select(`school id`, district, school, `black or african-american`:white) %>% 
  gather(race, n, `black or african-american`:white) %>% 
  mutate(n = as.numeric(n)) 

locseg_ <- long_enrl %>% 
  filter(district == "Sumter 01") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Liberty STEAM")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

memb_enrl <- merge(memb_enrl,locseg_)

# ----- DISTRICT AGGREGATES RACE ------
agg <- long_enrl %>% 
  spread(race,n) %>% 
  group_by(district) %>% 
  summarize(amind = sum(`american indian`),
            asian = sum(asian) + sum(`hawaiian or other pacific islander`),
            black = sum(`black or african-american`),
            hisp = sum(`hispanic or latino`),
            white = sum(white),
            mult = sum(`two or more races`)) %>% 
  mutate(dist_total = amind + asian + black + hisp + white + mult,
         dist_amind = amind/dist_total,
         dist_asian = asian/dist_total,
         dist_black = black/dist_total,
         dist_hisp = hisp/dist_total,
         dist_white = white/dist_total,
         dist_mult = mult/dist_total) %>% 
  select(district, dist_total:dist_mult) %>% 
  filter(str_detect(district, "Sumter"))

memb_enrl <- merge(memb_enrl, agg)

agg_frpl <- enrl %>% 
  mutate(frpl = as.numeric(`pupils in poverty`)) %>% 
  select(district, frpl) %>% 
  group_by(district) %>% 
  summarize(dist_frpl = sum(frpl)) %>% 
  filter(str_detect(district, "Sumter"))

memb_enrl <- memb_enrl %>% 
  merge(agg_frpl) %>% 
  mutate(dist_frpl = dist_frpl/dist_total)

# ---- EL FROM REPORT CARD DATA FILE -------
# Data downloaded from https://screportcards.ed.sc.gov/files/2021//data-files/
ell <- read_excel('raw data/SC_ReportCardData_forResearchers2022.XLSx',
                  sheet = '5.EnglishLearners')
names(ell) <- tolower(names(ell))
str(ell)
ell <- ell %>% 
  select(districtnm,schoolid,schooltypecd,schoolnm,numelsubgroup) %>% 
  filter(str_detect(schoolnm,"Liberty Steam Charter")|
           str_detect(schoolnm,"Sumter School"))%>%
  mutate(ell = as.numeric(numelsubgroup)) %>% 
  group_by(schoolnm) %>% 
  summarize(ell = sum(ell))

ell <- ell %>% 
  mutate(district = "Sumter 01") %>% 
  spread(schoolnm, ell)
names(ell)[2:3] <- c("ell","dist_ell")

memb_enrl <- memb_enrl %>% 
  merge(ell) %>% 
  mutate(dist_ell = dist_ell/dist_total,
         ell = ell/total)

# ---- SPECIAL EDUCATION -------
# Data downloaded from https://ed.sc.gov/districts-schools/special-education-services/data-and-technology-d-t/data-collection-and-reporting/sc-data-collection-history/idea-child-count-data/

# District summary report for ages 5 to 21

subs <- read.csv('raw data/SC_Ages 5 to 21 - District Demographic Summary.csv')
swd <- subs[1:93,] # take the top rows of raw count by gender
names(swd) <- tolower(swd[8,])
names(swd)[1] <- "district"
swd <- swd[-(1:9),-c(3,5:35)]
str(swd)
swd <- swd %>% 
  mutate(dist_swd = case_when(male == "*" ~ 0,
                              TRUE ~ as.numeric(male)) +
           case_when(female == "*" ~ 0,
                     TRUE ~ as.numeric(female))) %>% 
  select(district, dist_swd) %>% 
  filter(district == "Sumter")
swd[,1] <- c("Sumter 01")

memb_enrl <- memb_enrl %>% 
  merge(swd) %>% 
  mutate(dist_swd = dist_swd/dist_total)

# school-level special ed from the report card file
sped <- read_excel('raw data/SC_ReportCardData_forResearchers2022.XLSx',
                   sheet = "2c.Participation")
names(sped) <- tolower(names(sped))
sped <- sped[,c(2:5,29)]

memb_sped <- sped %>% 
  filter(str_detect(schoolnm,"Liberty Steam Charter"))%>%
  mutate(sped = as.numeric(totaln_disabled),
         school = schoolnm) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  select(school, sped)
memb_sped[,1] <- memb_enrl$school

memb_enrl <- memb_sped %>% 
  merge(memb_enrl) %>% 
  mutate(sped = sped/total) %>% 
  select(school, total:frpl,ell,sped,ls_dist, district, dist_total:dist_ell,dist_swd)

write.csv(memb_enrl, file = file.path('output data/sc_enrl.csv'),row.names = FALSE)
# ---- ASSESSMENT? -----
acad <- read_excel('raw data/SC_ReportCardData_forResearchers2022.XLSx',
                   sheet = "2c.Participation")
acad_doc <- read_excel('raw data/SC_ReportCardData_forResearchers2022.XLSx',
                      sheet = "KEY")
levels(as.factor(acad_doc$WORKSHEET))

#Liberty STEAM Charter did not participate in assessments
