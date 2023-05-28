# --- Amy Jiravisitcul. 21 May 2023 ----
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

# ---- 
# https://webnew.ped.state.nm.us/bureaus/information-technology/stars/
# Enrollment Subgroup Percentages 2021-2022
# https://webnew.ped.state.nm.us/wp-content/uploads/2022/12/SY-22-23-40D-Enrollment-by-district-by-location-by-grade-Certified.xlsx

enrl <- read_excel('raw data/NM_SY-22-23-40D-Enrollment-Subgroup-Percentages-with-Averages-Certified.xlsx')
names(enrl) <- tolower(enrl[10,])
enrl <- enrl[-(1:10),-c(2:3,8,10,14,15,19)] # remove duplicate "na" column names
str(enrl)
names(enrl)

enrl <- enrl %>% 
  mutate(district = as.factor(`district name`),
         school = `location name`,
         total = as.numeric(`all students`),
         amind = gsub("[^0-9.-]", "", indian),
         amind = case_when(amind == "" ~ .01* total * as.numeric(gsub("[^0-9.-]", "", `pct indian`)),
                           TRUE ~ as.numeric(indian)),
         asian = gsub("[^0-9.-]","",asian),
         pac = gsub("[^0-9.-]","",pacific),
         asian = case_when(asian == "" ~ .01* total * as.numeric(gsub("[^0-9.-]","",`pct asian`)),
                           TRUE ~ as.numeric(asian)) +
           case_when(pac == "" ~ .01 * total * as.numeric(gsub("[^0-9.-]","",`pct pacific`)),
                     TRUE ~ as.numeric(pacific)),
         black = gsub("[^0-9.-]","",black),
         black = case_when(black == "" ~ .01* total * as.numeric(gsub("[^0-9.-]","",`pct black`)),
                           TRUE ~ as.numeric(black)),
         hisp = gsub("[^0-9.-]","",hispanic),
         hisp = case_when(hisp == "" ~ .01* total * as.numeric(gsub("[^0-9.-]","",`pct hispanic`)),
                          TRUE ~ as.numeric(hispanic)),
         white = gsub("[^0-9.-]","",caucasian),
         white = case_when(white == "" ~ .01 * total * as.numeric(gsub("[^0-9.-]","",`pct caucasian`)),
                           TRUE ~ as.numeric(white)),
         mult = gsub("[^0-9.-]","",`multi race`),
         mult = case_when(mult == "" ~ .01 * total * as.numeric(gsub("[^0-9.-]","",`pct multi race`)),
                          TRUE ~ as.numeric(`multi race`)),
         frl = gsub("[^0-9.-]","",frl),
         frl = case_when(frl == "" ~ .01 * total * as.numeric(gsub("[^0-9.-]","",`pct frl`)),
                           TRUE ~ as.numeric(frl)),
         ell = gsub("[^0-9.-]","",ell),
         ell = case_when(ell == "" ~ .01 * total * as.numeric(gsub("[^0-9.-]","",`pct ell`)),
                         TRUE ~ as.numeric(ell)),
         swd = gsub("[^0-9.-]","",sped),
         swd = case_when(swd == "" ~ .01 * total * as.numeric(gsub("[^0-9.-]","",`pct sped`)),
                         TRUE ~ as.numeric(sped)))%>% 
  select(district, school, total, amind, asian, black, hisp, white, mult, frl,ell,swd)

memb_enrl <- enrl %>% 
  filter(school == "ALTURA PREPARATORY SCHOOL"|
           school == "THRIVE COMMUNITY SCHOOL") %>% 
  mutate(district = case_when(str_detect(school, "ALTURA") ~ "ALBUQUERQUE",
                              str_detect(school, "THRIVE") ~ "SANTA FE"))
enrl <- enrl %>% 
  filter(school != "ALTURA PREPARATORY SCHOOL"&
           school != "THRIVE COMMUNITY SCHOOL") %>% 
  rbind(memb_enrl)

# ---- DISTRICT AGGREGATES -----
dist_agg <- enrl %>% 
  filter(district == "ALBUQUERQUE"|
           district == "SANTA FE") %>% 
  group_by(district) %>% na.omit() %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/dist_total,
            dist_asian = sum(asian)/dist_total,
            dist_black = sum(black)/dist_total,
            dist_hisp = sum(hisp)/dist_total,
            dist_white = sum(white)/dist_total,
            dist_mult = sum(mult)/dist_total,
            dist_frl = sum(frl)/dist_total,
            dist_ell = sum(ell)/dist_total,
            dist_swd = sum(swd)/dist_total) %>% 
  select(district, dist_total:dist_swd)

memb_enrl <- memb_enrl %>% 
  merge(dist_agg)

# ----- DISTRICT-LEVEL LOCAL SEGREGATION -----
ls_memb <- enrl %>% 
  select(district, school, amind:mult) %>% 
  filter(district == "ALBUQUERQUE"|
           district == "SANTA FE") %>% 
  gather(race,n,amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  filter(school == "ALTURA PREPARATORY SCHOOL"|
           school == "THRIVE COMMUNITY SCHOOL") %>% 
  select(school, ls_dist)

memb_enrl <- memb_enrl %>% 
  merge(ls_memb) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         frl = frl/total,
         ell = ell/total,
         swd = swd/total)

# ---- COUNTY-LEVEL AGGREGATE AND LOCAL SEGREGATION ----
# Defined as Bernalillo County according to https://nces.ed.gov/ccd/districtsearch/district_list.asp?Search=1&details=1&State=35&Zip=87101&Miles=10&County=bernalillo&NumOfStudentsRange=more&NumOfSchoolsRange=more&DistrictPageNum=1
bern <- enrl %>% 
  filter(district == "ALBUQUERQUE"|
           district == "ACES TECHNICAL CHARTER SCHOOL"|
           district == "MONTESSORI ELEMENTARY SCHOOL"|
           district == "SEQUOYAH"|
           district == "ALBUQUERQUE BILINGUAL ACADEMY"|
           district == "HORIZON ACADEMY WEST"|
           district == "EXPLORE ACADEMY"|
           district == "THE GREAT ACADEMY"|
           district == "21ST CENTURY PUBLIC ACADEMY"|
           district == "JUVENILE JUSTICE"|
           district == "NORTH VALLEY ACADEMIC CHARTER"|
           district == "ALBUQUERQUE INSTITUTE OF MATH & SCIENCE"|
           district == "MISSION ACHIEVEMENT AND SUCCESS"|
           district == "TIERRA ADENTRO"|
           district == "UNM MIMBRES SCHOOL"|
           district == "ABQ SIGN LANGUAGE ACADEMY"|
           district == "ALBUQUERQUE COLLEGIATE CHARTER SCHOOL"|
           district == "AMY BIEHL CHARTER HIGH SCHOOL"|
           district == "BERNALILLO"|
           district == "CESAR CHAVEZ COMMUNITY SCHOOL"|
           district == "MEDIA ARTS COLLABORATIVE CHARTER") %>% 
  mutate(county = "BERNALILLO") %>% 
  group_by(county) %>% na.omit() %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hisp)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(mult)/cty_total,
            cty_frl = sum(frl)/cty_total,
            cty_ell = sum(ell)/cty_total,
            cty_swd = sum(swd)/cty_total)

ls_bern <- enrl %>% 
  filter(district == "ALBUQUERQUE"|
           district == "ACES TECHNICAL CHARTER SCHOOL"|
           district == "MONTESSORI ELEMENTARY SCHOOL"|
           district == "SEQUOYAH"|
           district == "ALBUQUERQUE BILINGUAL ACADEMY"|
           district == "HORIZON ACADEMY WEST"|
           district == "EXPLORE ACADEMY"|
           district == "THE GREAT ACADEMY"|
           district == "21ST CENTURY PUBLIC ACADEMY"|
           district == "JUVENILE JUSTICE"|
           district == "NORTH VALLEY ACADEMIC CHARTER"|
           district == "ALBUQUERQUE INSTITUTE OF MATH & SCIENCE"|
           district == "MISSION ACHIEVEMENT AND SUCCESS"|
           district == "TIERRA ADENTRO"|
           district == "UNM MIMBRES SCHOOL"|
           district == "ABQ SIGN LANGUAGE ACADEMY"|
           district == "ALBUQUERQUE COLLEGIATE CHARTER SCHOOL"|
           district == "AMY BIEHL CHARTER HIGH SCHOOL"|
           district == "BERNALILLO"|
           district == "CESAR CHAVEZ COMMUNITY SCHOOL"|
           district == "MEDIA ARTS COLLABORATIVE CHARTER") %>% 
  mutate(county = "BERNALILLO") %>% 
  select(county, school, amind:mult) %>%  
  gather(race,n,amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  filter(school == "ALTURA PREPARATORY SCHOOL") %>% 
  select(school, ls_cty)

# Santa Fe County: https://nces.ed.gov/ccd/districtsearch/district_list.asp?Search=1&details=1&InstName=&DistrictID=&Address=&City=&State=35&Zip=&Miles=1&County=santa+fe+county&PhoneAreaCode=&Phone=&NumOfStudents=&NumOfStudentsRange=more&NumOfSchools=&NumOfSchoolsRange=more --------

cty_agg <- enrl %>% 
  filter(district == "ESTANCIA VALLEY CLASSICAL ACADEMY"|
           district == "MCCURDY CHARTER SCHOOL"|
           district == "MONTE DEL SOL CHARTER"|
           district == "NEW MEXICO CONNECTIONS ACADEMY"|
           district == "NM CORRECTIONS"|
           district == "NM SCHOOL FOR ARTS"|
           district == "NM SCHOOL FOR THE DEAF"|
           district == "POJOAQUE VALLEY PUBLIC SCHOOLS"|
           district == "SANTA FE"|
           district == "THE MASTERS PROGRAM"|
           district == "TIERRA ENCANTADA CHARTER SCHOOL"|
           district == "TURQUOISE TRAIL CHARTER SCHOOL") %>% 
  mutate(county = "SANTA FE COUNTY") %>% 
  group_by(county) %>% na.omit() %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hisp)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(mult)/cty_total,
            cty_frl = sum(frl)/cty_total,
            cty_ell = sum(ell)/cty_total,
            cty_swd = sum(swd)/cty_total) %>% 
  rbind(bern)

ls_counties <- enrl %>% 
  filter(district == "ESTANCIA VALLEY CLASSICAL ACADEMY"|
           district == "MCCURDY CHARTER SCHOOL"|
           district == "MONTE DEL SOL CHARTER"|
           district == "NEW MEXICO CONNECTIONS ACADEMY"|
           district == "NM CORRECTIONS"|
           district == "NM SCHOOL FOR ARTS"|
           district == "NM SCHOOL FOR THE DEAF"|
           district == "POJOAQUE VALLEY PUBLIC SCHOOLS"|
           district == "SANTA FE"|
           district == "THE MASTERS PROGRAM"|
           district == "TIERRA ENCANTADA CHARTER SCHOOL"|
           district == "TURQUOISE TRAIL CHARTER SCHOOL") %>% 
  mutate(county = "SANTA FE COUNTY") %>% 
  select(county, school, amind:mult) %>%  
  gather(race,n,amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  filter(school == "THRIVE COMMUNITY SCHOOL") %>% 
  select(school, ls_cty) %>% 
  rbind(ls_bern)

memb_enrl <- memb_enrl %>% 
  mutate(county = case_when(str_detect(school, "ALTURA") ~ "BERNALILLO",
                            str_detect(school, "THRIVE") ~ "SANTA FE COUNTY")) %>% 
  merge(cty_agg) %>% 
  merge(ls_counties)

memb_enrl <- memb_enrl %>% 
  select(school, total:swd,ls_dist, ls_cty, district, dist_total:dist_swd, 
         county, cty_total:cty_swd)

write.csv(memb_enrl, file = file.path('output data/nm_enrl.csv'),row.names = FALSE)

# ---- MSSA DATA ------
acad <- read_excel('raw data/NM_MSSA-ESSA-2021.xlsx')
names(acad) <- tolower(acad[2,])
names(acad)[6:9] <- c("n_ela","p_ela","n_math","p_math")
acad <- acad[-(1:2),]
str(acad)

memb_acad <- acad %>% 
  mutate(district = `state or district`,
         p_math = as.numeric(gsub("[^0-9.-]","" , p_math)),
         p_ela = as.numeric(gsub("[^0-9.-]","",p_ela))) %>% 
  filter(group == "All",
         school == "Districtwide" & 
           (district == "ALBUQUERQUE"|district == "SANTA FE")|
           str_detect(school, "ALTURA"))
# Thrive has no assessment data available
write.csv(memb_acad,file = file.path('output data/nm_acad.csv'), row.names = FALSE)
# ---- END -----