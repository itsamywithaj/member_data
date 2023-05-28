# --- Amy Jiravisitcul. 18 May 2023 ----
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

#--- ENROLLMENT BY RACE AND ETHNICITY -----
# data imported from https://www.education.pa.gov/DataAndReporting/Enrollment/Pages/PublicSchEnrReports.aspx
# PIMS from 4/25/2023. Only LEA-level available for race. Oct 1 from 2022-2023

enrl <- read_excel("raw data/PA_Enrollment Public Schools 2022-23.xlsx", sheet = "LEA and Race")
names(enrl) <- tolower(enrl[4,]) # change column names
enrl <- enrl[-c(1:4),] # remove rows with no data
enrl <- enrl %>% 
  mutate(lea = as.factor(`lea name`),
         lea_type = as.factor(`lea type`),
         race = as.factor(race),
         n = as.numeric(total)) %>% 
  select(lea, lea_type, county, race, n)
levels(enrl$race) <- c("amind", "asian", "black","hisp","mult","nhpi","white")

enrl_wide <- enrl %>% 
  spread(race, n) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(asian = asian + nhpi,
         total = amind + asian + black + hisp + mult + white,
         amind = amind/total,
         asian = asian/ total,
         black = black/total,
         hisp = hisp/total,
         white = white/ total,
         mult = mult/total) %>% 
  select(lea, lea_type, county, total, amind, asian, black, hisp, white, mult)
memb_enrl <- enrl_wide %>% 
  filter(lea == "Vida CS"|
           str_detect(lea, "Hebrew")|
           str_detect(lea,"Memphis"))

# ---- LOCAL SEGREGATION INDEX BY COUNTY ------
memb_enrl$county
locseg <- enrl %>% 
  filter(county == "Philadelphia"|
           county == "Adams") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

phil_ls <- locseg %>% 
  filter(county == "Philadelphia") %>% 
  mutual_local("race","lea", weight = "n", wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(lea, "Hebrew")|
           str_detect(lea, "Memphis")) %>% 
  mutate(ls_cty = ls) %>% 
  select(lea, ls_cty)

adams_ls <- locseg %>% 
  filter(county == "Adams") %>% 
  mutual_local("race","lea",weight = "n",wide = TRUE) %>% 
  arrange(ls) %>% 
  filter(str_detect(lea,"Vida")) %>% 
  mutate(ls_cty = ls) %>% 
  select(lea, ls_cty)

memb_enrl <- merge(memb_enrl,rbind(adams_ls,phil_ls))

# ----- AGGREGATES FOR SCHOOL DISTRICTS AND COUNTIES -----
agg <- read_excel("raw data/PA_Enrollment Public Schools 2022-23.xlsx", sheet = "LEA and Race")
names(agg) <- tolower(agg[4,]) # change column names
agg <- agg[-c(1:4),] # remove rows with no data
agg <- agg %>% 
  mutate(lea = as.factor(`lea name`),
         lea_type = as.factor(`lea type`),
         race = as.factor(race),
         n = as.numeric(total)) %>% 
  select(lea, lea_type, county, race, n)
levels(agg$race) <- c("amind", "asian", "black","hisp","mult","nhpi","white")

agg <- agg %>% 
  spread(race, n) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(asian = asian + nhpi)

agg <- agg %>% 
  group_by(county) %>% 
  summarize(cty_amind = sum(amind),
   cty_asian = sum(asian),
   cty_black = sum(black),
   cty_hisp = sum(hisp),
   cty_white = sum(white),
   cty_mult = sum(mult)) %>% 
  mutate(cty_total = cty_amind + cty_asian + cty_black + cty_hisp + cty_white +
           cty_mult,
         cty_amind = cty_amind/cty_total,
         cty_asian = cty_asian/cty_total,
         cty_black = cty_black/cty_total,
         cty_hisp = cty_hisp/cty_total,
         cty_white = cty_white/cty_total,
         cty_mult = cty_mult/cty_total)%>% 
  filter(county == "Philadelphia"|
           county == "Adams")
memb_enrl <- merge(memb_enrl,agg)

dist <- enrl %>% 
  filter(lea_type == "SD") %>% 
  spread(race,n) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  filter(lea == "Philadelphia City SD"|
           lea == "Gettysburg Area SD") %>% 
  mutate(asian = asian + nhpi,
         dist_total = asian + amind + black +hisp + white + mult,
         total = dist_total,
         dist_asian = asian/total,
         dist_amind = amind/total,
         dist_black = black/total,
         dist_hisp = hisp/total,
         dist_white = white/total,
         dist_mult = mult/total,
         dist = lea) %>% 
  select(dist, county, dist_total, dist_amind, dist_asian, 
         dist_black, dist_hisp, dist_white, dist_mult)

memb_enrl <- merge(memb_enrl,dist,by="county")

# ------- EL STUDENTS -----
# Data from https://www.education.pa.gov/DataAndReporting/EnglishLearners/Pages/default.aspx
el <- read_excel("raw data/PA_2021-2022 EL Students by LEA and School.xlsx", 
                 sheet = "LEP Students_1")
names(el) <- tolower(el[5,])
el <- el[-(1:5),]
dist_el <- el %>% 
  filter(`lea name`=="Philadelphia City SD"| # comparison districts
           `lea name` == "Gettysburg Area SD") %>% 
  mutate(dist = `lea name`,
         n_el = as.numeric(`el students`)) %>% 
  select(dist, n_el)
dist_el$total = c(2850,118401) #from agg data
dist_el <- dist_el %>% 
  mutate(d_el = n_el/total) %>% 
  select(dist, d_el)

memb_enrl <- merge(memb_enrl, dist_el)

z <- enrl %>% # add county to the EL data set
  select(lea,county) %>% unique()
el$lea = el$`lea name`
el <- merge(el, z)
cty_el <- el %>% 
  mutate(n_el = as.numeric(`el students`)) %>% 
  group_by(county) %>% 
  summarize(cty_el = sum(n_el)) %>% 
  select(county, cty_el) %>% 
  filter(county == "Adams"|
           county == "Philadelphia")

cty_el$total = c(14647,183928) # from agg data
cty_el <- cty_el %>% 
  mutate(cty_el = cty_el/total) %>% 
  select(county, cty_el)
memb_el <- el %>% 
  filter(str_detect(lea, "Hebrew")|
           str_detect(lea, "Memphis")|
           str_detect(lea, "Vida")) %>% 
  mutate(n_el = as.numeric(`el students`)) %>% 
  select(lea, n_el)

memb_enrl <- merge(memb_enrl,cty_el)
memb_enrl <- merge(memb_enrl, memb_el)
memb_enrl <- memb_enrl %>% 
  mutate(el = n_el/total) %>% 
  select(-n_el)

# ----- SPECIAL EDUCATION ENROLLMENT ----
# Available in PDFs for LEAs at https://penndata.hbg.psu.edu/Public-Reporting/Data-at-a-Glance

sped <- data.frame(lea = c("Vida CS",
                           "Memphis Street Academy CS @ JP Jones",
                           "Philadelphia Hebrew Public CS"), # match the ordering of memb_enrl
                   sped = c(0.093,0.22,0.191),
                   dist_sped = c(0.151,.176,.176),
                   cty_sped = 0.186) # PA-wide. county-level unavailable
memb_enrl <- merge(memb_enrl,sped)

# ---- FRPL ------
# Data downloaded from https://www.education.pa.gov/Teachers%20-%20Administrators/Food-Nutrition/reports/Pages/National-School-Lunch-Program-Reports.aspx with YTD Excel file

frpl <- read_excel('raw data/PA_2022-2023 Building Data Report.xlsx', sheet = "Building Data Report ")
View(frpl)
names(frpl) <- tolower(frpl[3,])
str(frpl)
frpl <- frpl[-c(1:3),] # Remove rows with no data

frpl <- frpl[,c(1,3,6,27,32)]
names(frpl) <- c("county","district","school","total","p_frpl")
frpl$p_frpl <- as.numeric(frpl$p_frpl)*.01

memb_frpl <- frpl %>% 
  filter(school == "Vida Charter School"|
           str_detect(school, "MEMPHIS STREET")|
           str_detect(school, "Philadelphia Hebrew"))
memb_frpl$district[2:3] <- "SCHOOL DISTRICT OF PHILADELPHIA"

agg_frpl <- frpl %>% 
  mutate(n_frpl = p_frpl * as.numeric(total), 
         total = as.numeric(total)) %>% 
  group_by(district) %>% 
  summarize(n_frpl = sum(n_frpl),
            total = sum(total)) %>% 
  filter(str_detect(district,"GETTYSBURG AREA")|
           district == "SCHOOL DISTRICT OF PHILADELPHIA") %>% 
  mutate(d_frpl = n_frpl/total) %>% 
  select(district, d_frpl)
c <- frpl %>% 
  mutate(n_frpl = p_frpl * as.numeric(total), 
         total = as.numeric(total)) %>% 
  group_by(county) %>% 
  summarize(n_frpl = sum(n_frpl),
            total = sum(total)) %>% 
  filter(str_detect(county,"ADAMS")|
           county == "PHILADELPHIA") %>% 
  mutate(cty_frpl = n_frpl/total) %>% 
  select(county, cty_frpl)

memb_frpl <- merge(agg_frpl,memb_frpl)
memb_frpl <- merge(memb_frpl,c)
memb_frpl <- memb_frpl %>% select(school, p_frpl, d_frpl, cty_frpl)
names(memb_enrl)[c(1,3)] <- c("school","district")
memb_frpl$school <- c("Vida CS","Memphis Street Academy CS @ JP Jones","Philadelphia Hebrew Public CS")

memb_enrl <- merge(memb_frpl,memb_enrl, by=c("school"))
# ---- PREP TO COPY/PASTE INTO MEMBER LOG -----
sort(names(memb_enrl))
memb_enrl <- memb_enrl %>% 
  select(school, total, amind, asian, black, hisp, white, mult, p_frpl, el, sped,ls_cty,
         district, dist_total, dist_amind, dist_asian, dist_black, dist_hisp, dist_white, dist_mult,
         d_frpl, d_el, dist_sped, county, cty_total, cty_amind, cty_asian, cty_black, cty_hisp,
         cty_white, cty_mult, cty_frpl, cty_el, cty_sped)
write.csv(memb_enrl, file = file.path('output data/pa_enrl.csv'),row.names = FALSE)

# ---- PSSA DATA -----
acad <- read_excel('raw data/PA_ 2022 PSSA School level data.xlsx')
names(acad) <- tolower(acad[4,])
acad <- acad[-(1:4),]
levels(as.factor(acad$subject))
acad <- acad %>% 
  mutate(school = `school name`,
         district = `district name`,
         n_tests = as.numeric(`number scored`),
         p_prof = (as.numeric(`percent advanced`) + as.numeric(`percent proficient`))*.01) %>% 
  select(school:p_prof,grade,subject,group) %>% 
  filter(grade == "School Total",
         district == "GETTYSBURG AREA SD"|
           district == "PHILADELPHIA CITY SD"|
           school == "PHILADELPHIA HEBREW PUBLIC CS"|
           school == "MEMPHIS STREET ACADEMY CS @ JP"|
           school == "VIDA CHARTER SCHOOL",
         subject == "English Language Arts"|
           subject == "Math",
         group == "All Students") %>% 
  select(school, district, subject, n_tests, p_prof)

memb_acad <- acad %>% 
  filter(school == "PHILADELPHIA HEBREW PUBLIC CS"|
           school == "MEMPHIS STREET ACADEMY CS @ JP"|
           school == "VIDA CHARTER SCHOOL") %>%
  select(-n_tests) %>% 
  spread(key = subject, value = p_prof)
names(memb_acad)[3:4] <- c("ela","math")
memb_acad$district <- c("PHILADELPHIA CITY SD","PHILADELPHIA CITY SD","GETTYSBURG AREA SD")

dist_acad <- acad %>% 
  mutate(n_prof = p_prof * n_tests) %>% 
  group_by(district,subject) %>% 
  na.omit() %>% 
  summarize(n_tests = sum(n_tests),
            n_prof = sum(n_prof),
            .groups = 'drop') %>% 
  mutate(p_prof = n_prof/n_tests) %>% 
  select(district,subject, p_prof) %>% 
  spread(key = subject, value = p_prof)
names(dist_acad)[2:3] <- c("dist_ela","dist_math")

memb_acad <- merge(memb_acad,dist_acad[c(1,3),],by = "district")
memb_acad <- memb_acad %>% select(school, math, dist_math, ela, dist_ela)
write.csv(memb_acad, file = file.path('output data/pa_acad.csv'),row.names = FALSE)
