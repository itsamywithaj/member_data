# --- Amy Jiravisitcul. 20 May 2023 ----
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

# ----- ENROLLMENT BY RACE --------
# LEA comparison datasets from http://apps.schools.nc.gov/ords/f?p=145:220:12562863778588::NO::P220_SELECTLEA:320
# Table 10.1 - PUPILS IN MEMBERSHIP BY RACE and SEX (at School Level)

durh <- read.csv('raw data/NC_durham_report.csv')
char <- read.csv('raw data/NC_charlotte_report.csv')

# Member dataset from http://apps.schools.nc.gov/ords/f?p=145:73:::NO:::
# Part III, Table 37 	Charter and Regional School Membership by Race and Sex

memb_enrl <- read_excel('raw data/NC_Pupils_by_Race_and_Sex.xlsx')
names(memb_enrl) <- tolower(memb_enrl[2,])
memb_enrl <- memb_enrl[-c(1:2),]
str(memb_enrl)
memb_enrl <- memb_enrl %>%
  mutate(school = `charter school name`) %>% 
  filter(school=="Charlotte Lab School"|
           school=="Central Park School For Children") %>%
  mutate(school = `charter school name`,
         total = as.numeric(gsub(",","",total)),
         amind = as.numeric(gsub(",","",`indian male`))+as.numeric(gsub(",","",`indian female`)),
         asian = as.numeric(gsub(",","",`asian male`))+as.numeric(gsub(",","",`asian female`))+
           as.numeric(gsub(",","",`pacific island  male`))+ 
           as.numeric(gsub(",","",`pacific island  female`)),
         black = as.numeric(gsub(",","",`black male`))+as.numeric(gsub(",","",`black female`)),
         hisp = as.numeric(gsub(",","",`hispanic male`))+as.numeric(gsub(",","",`hispanic female`)),
         white = as.numeric(gsub(",","",`white male`))+as.numeric(gsub(",","",`white female`)),
         mult = as.numeric(gsub(",","",`two or more male`))+
           as.numeric(gsub(",","",`two or more female`))) %>% 
  select(school,total:mult)

memb_enrl$district = c("Durham County Schools","Charlotte-Mecklenburg County Schools")

# ----- AGGREGATE NUMBERS -----
names(char) <- tolower(names(char))
str(char)
char <- char %>% 
  mutate(school = school.name,
         district = x_____lea.name_____,
         total = as.numeric(gsub(",","",total)),
         amind = as.numeric(gsub(",","",indian.male))+as.numeric(gsub(",","",indian.female)),
         asian = as.numeric(gsub(",","",asian.male))+as.numeric(gsub(",","",asian.female))+
                    as.numeric(gsub(",","",pacific.island..male))+ 
           as.numeric(gsub(",","",pacific.island..female)),
         black = as.numeric(gsub(",","",black.male))+as.numeric(gsub(",","",black.female)),
         hisp = as.numeric(gsub(",","",hispanic.male))+as.numeric(gsub(",","",hispanic.female)),
         white = as.numeric(gsub(",","",white.male))+as.numeric(gsub(",","",white.female)),
         mult = as.numeric(gsub(",","",two.or.more.male))+as.numeric(gsub(",","",two.or.morefemale))) %>% 
  select(year,school, district,total:mult)

agg <- char %>% 
  filter(year == "Total") %>% 
  mutate(district = "Charlotte-Mecklenburg County Schools",
         dist_total = total,
         dist_amind = amind/total,
         dist_asian = asian/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total) %>% 
  select(district, dist_total:dist_mult)

names(durh) <- tolower(names(durh))
str(durh)
durh <- durh %>% 
  mutate(school = school.name,
         district = x_____lea.name_____,
         total = as.numeric(gsub(",","",total)),
         amind = as.numeric(gsub(",","",indian.male))+as.numeric(gsub(",","",indian.female)),
         asian = as.numeric(gsub(",","",asian.male))+as.numeric(gsub(",","",asian.female))+
           as.numeric(gsub(",","",pacific.island..male))+ 
           as.numeric(gsub(",","",pacific.island..female)),
         black = as.numeric(gsub(",","",black.male))+as.numeric(gsub(",","",black.female)),
         hisp = as.numeric(gsub(",","",hispanic.male))+as.numeric(gsub(",","",hispanic.female)),
         white = as.numeric(gsub(",","",white.male))+as.numeric(gsub(",","",white.female)),
         mult = as.numeric(gsub(",","",two.or.more.male))+as.numeric(gsub(",","",two.or.morefemale))) %>% 
  select(year,school, district,total:mult)

agg2 <- durh %>% 
  filter(year == "Total") %>% 
  mutate(district = "Durham County Schools",
         dist_total = total,
         dist_amind = amind/total,
         dist_asian = asian/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total) %>% 
  select(district, dist_total:dist_mult)

agg <- rbind(agg, agg2)
memb_enrl <- merge(memb_enrl,agg)

# ----- LOCAL SEGREGATION NUMBERS BY DISTRICT ------
locseg <- memb_enrl %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n,amind:mult) %>% 
  arrange(school)

char_long <- char %>% 
  filter(year != "Total") %>% 
  select(district, school, amind:mult) %>% 
  gather(race,n,amind:mult)

char_long <- rbind(char_long,locseg[7:12,])

memb_ls <- char_long %>% 
  mutual_local("race", "school", weight ="n",wide = TRUE) %>% 
  filter(school == "Charlotte Lab School") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

durh_long <- durh %>% 
  filter(year != "Total") %>% 
  select(district, school, amind:mult) %>% 
  gather(race, n, amind:mult)
durh_long <- rbind(durh_long,locseg[1:6,])
m_2 <- durh_long %>% 
  mutual_local("race", "school", weight ="n",wide = TRUE) %>% 
  filter(school == "Central Park School For Children") %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)

memb_ls <- rbind(memb_ls,m_2)
memb_enrl <- merge(memb_enrl,memb_ls)
memb_enrl<- memb_enrl %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total)

# ---- FRPL -------
# Data downloaded from https://www.dpi.nc.gov/districts-schools/district-operations/school-nutrition/sn-data-reports#cep-annual-notification-of-schools-eligibility-report
# https://www.dpi.nc.gov/documents/schoolnutrition/2021-annual-notification-schools-eligibility-report/download?attachment

frpl <- read_excel('raw data/NCDPI CEP Notification - April 2023.xlsx',
                   sheet = "LEA-wide Notification Report")
names(frpl) <- tolower(frpl[4,])
frpl <- frpl[-c(1:4),1:3]
frpl <- frpl %>% 
  filter(str_detect(`lea name`,"Charlotte-Meck")|
           str_detect(`lea name`,"Durham Public")|
           str_detect(`lea name`,"Central Park")|
           str_detect(`lea name`, "Charlotte Lab")) %>% 
  mutate(frpl = as.numeric(`identified student percentage\r\r\n(isp)`)*.01,
         school = `lea name`) %>% 
  select(school, frpl)
memb_enrl$district
frpl[c(1:3),1] <- c("Durham County Schools", # match the enrollment dataset
              "Central Park School For Children",
              "Charlotte-Mecklenburg County Schools")
memb_enrl <- merge(memb_enrl,frpl[c(2,4),]) 


d_frpl <- frpl %>% 
  filter(school == "Durham County Schools"|
           school == "Charlotte-Mecklenburg County Schools") %>% 
  mutate(district = school,
         dist_frpl = frpl) %>%
  select(district, dist_frpl)

memb_enrl <- merge(memb_enrl,d_frpl) 

# ------ SWD ------
# http://apps.schools.nc.gov/ords/f?p=145:14:::NO:::
# Table 9: Pupils in Membership Being Served by Exceptional Children Programs
# Not worth exporting to Excel for aggregate data
swd <- data.frame(district = c("Durham County Schools",
                             "Charlotte-Mecklenburg County Schools"),
                  dist_swd = c(4292,14410))
memb_enrl <- merge(memb_enrl,swd)
memb_enrl <- memb_enrl %>% 
  mutate(dist_swd = dist_swd/dist_total)

# Table 38: Charter and Regional School Pupils in Membership Being Served by Exceptional Children Programs

memb_swd <- data.frame(school = c("Central Park School For Children",
                                  "Charlotte Lab School"),
                       swd = c(99,91))
memb_enrl <- merge(memb_enrl,memb_swd)
memb_enrl <- memb_enrl %>% 
  mutate(swd = swd/total)

# ---- ELL ------
# https://www.dpi.nc.gov/districts-schools/classroom-resources/academic-standards/standard-course-study/english-language-development/english-learner-el-data
# a PDF from the state https://drive.google.com/file/d/1iKucMKlGR-GgDlH71muTHwTohmjD2Iz7/view
ell <- data.frame(district = c("Charlotte-Mecklenburg County Schools",
                               "Durham County Schools"),
                  dist_ell = c(27405,6061))
memb_enrl <- merge(memb_enrl, ell)

memb_ell <- data.frame(school = c("Central Park School For Children",
                                  "Charlotte Lab School"),
                       ell = c(25,28))
memb_enrl <- merge(memb_enrl,memb_ell)
memb_enrl <- memb_enrl %>% 
  mutate(dist_ell = dist_ell/dist_total,
         ell = ell/total)
# --- prepare for export ----
names(memb_enrl)
memb_enrl <- memb_enrl %>% 
  select(school, total:mult,frpl, ell, swd, ls_dist, district,dist_total:dist_mult, dist_frpl,
         dist_ell, dist_swd)
str(memb_enrl)
write.csv(memb_enrl, file = file.path("output data/nc_enrl.csv"), row.names = FALSE)

# ---- ASSESSMENT DATA -----
# https://www.dpi.nc.gov/2021-22-school-assessment-and-other-indicator-data
acad_doc <- read_excel('raw data/NC_2021-22 School Assessment and Other Indicator Data.xlsx')
acad <- read_excel('raw data/NC_2021-22 School Assessment and Other Indicator Data.xlsx',
                   sheet = "EOG and EOC ")
names(acad) <- tolower(acad[2,])
acad <- acad[-(1:2),]
str(acad)
levels(as.factor(acad$subject))
acad <- acad %>% 
  filter(subgroup == "All Students",
         subject == "English II"|
           subject == "Math Grade 3-8"|
           subject == "NC Math 1 (9-12)"|
           subject == "NC Math 3 (9-12)"|
           subject == "Reading Grade 3-8") %>% 
  mutate(district = as.factor(`district name`),
         school = as.factor(`school name`),
         p_prof = as.numeric(`percent\r\nlevel 3 and above\r\n(glp)`)*.01) %>% 
  select(district, school, subject, p_prof)

memb_acad <- acad %>% 
  filter(str_detect(school, "Central Park")|
           str_detect(school, "Charlotte Lab")) %>% 
  mutate(district = case_when(str_detect(school, "Central") ~ "Durham Public Schools",
                              TRUE ~ "Charlotte-Mecklenburg Schools"))

agg_acad <- acad %>% 
  filter(school == "Charlotte-Mecklenburg Schools"|
           school == "Durham Public Schools") %>% 
  mutate(dist_prof = p_prof) %>% 
  select(district, subject, dist_prof)

memb_acad <- merge(memb_acad, agg_acad, by= c("district","subject"))
write.csv(memb_acad, file = file.path('output data/nc_acad.csv'), row.names = FALSE)
