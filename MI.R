rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)

mi_specialedtest1 <- read.csv("raw_data/MI_632b586e-1f7c-4dc5-944c-a46142f0e3ca.csv")
mi_specialedtest <- read.csv("raw_data/MI_6be40f62-6690-48d2-967c-cff2e37ce978.csv")
mi_enrl <- read.csv("raw_data/MI_c1a77121-9453-4a78-9352-80ff0486e4a8.csv")

#----- MI math and ELA ------
mi_acad_all<- read.csv("raw_data/MI_3ed373cd-1aea-4725-a151-9a096a6c2b12.csv")
# Data downloaded from https://www.mischooldata.org/DistrictSchoolProfiles2/EntitySummary/SchoolDataFile.aspx on Wayne ISD, based on mapping in https://www.michigan.gov/documents/CGI-sd_zone_6_67416_7.pdf
summary(mi_acad_all)
names(mi_acad_all) <- tolower(names(mi_acad_all))
str(mi_acad_all)
levels(mi_acad_all$entitytype)[1:5] <- c("isd","lea_dist","lea_school","psa_dist","psa_school")
# entity is confusing https://www.michigan.gov/documents/cepi/EEM_Defs_238605_7.pdf
mi_acad <- mi_acad_all %>% 
  filter(testtype == "M-STEP",
         reportcategory == "All Students",
         entitytype == "lea_school",
         districtname == "Detroit Public Schools Community District",
         subject == "ELA"|subject == "Mathematics") %>% 
  mutate(percentmet = .01 * as.numeric(gsub("[^0-9.-]","",as.character(percentmet))), #remove symbols like <,=,%
         totalmet = as.numeric(gsub("[^0-9.-]","",as.character(totalmet))), # and change percentage points to decimal form
         n_tested = round(totalmet/percentmet)) %>% 
  select(buildingname, gradecontenttested,subject,n_tested,totalmet, percentmet) # percent meeting benchmark
summary(mi_acad$percentmet)
install.packages("data.table")
library(data.table)
DT <- data.table(mi_acad)
h <- DT[, lapply(.SD, sum), by=list(buildingname, subject)] #add up student counts for all grades
mi_acad <- h %>% select(buildingname,subject,n_tested,totalmet) %>% 
  na.omit() %>% 
  mutate(percentmet = totalmet/n_tested) # recalculate percentage for each of 101 schools

mi_ela <- mi_acad %>% 
  filter(subject == "ELA") %>% 
  summarize(mean_ela = mean(percentmet),
            sd_ela = sd(percentmet))
mi_stan_ela <- mi_acad_all %>% 
  filter(subject == "ELA",
         buildingname == "Detroit Prep") %>% #specific to member
  mutate(percentmet = .01 * as.numeric(gsub("[^0-9.-]","",as.character(percentmet))), #remove symbols like <,=,%
         totalmet = as.numeric(gsub("[^0-9.-]","",as.character(totalmet))), # and change percentage points to decimal form
         n_tested = round(totalmet/percentmet)) %>% 
  select(buildingname, gradecontenttested,subject,n_tested,totalmet, percentmet) %>%  # percent meeting benchmark
  na.omit()
DT <- data.table(mi_stan_ela)
g <- DT[, lapply(.SD, sum), by=list(buildingname, subject)] #add up student counts for all grades
mi_stan_ela <- g %>% select(buildingname,subject,n_tested,totalmet) %>% 
  na.omit() %>% 
  mutate(percentmet = totalmet/n_tested, # recalculate ELA percentage for Detroit Prep
         mean_ela = mi_ela$mean_ela,
         sd_ela = mi_ela$sd_ela,
         comp_ela = (percentmet - mean_ela)/sd_ela) %>% 
  select(buildingname, subject, n_tested, percentmet, mean_ela, comp_ela)

mi_math <- mi_acad %>% 
  filter(subject == "Mathematics") %>% 
  summarize(mean_math = mean(percentmet),
            sd_math = sd(percentmet))
mi_stan_math <- mi_acad_all %>% 
  filter(subject == "Mathematics",
         buildingname == "Detroit Prep") %>% #specific to member
  mutate(percentmet = .01 * as.numeric(gsub("[^0-9.-]","",as.character(percentmet))), #remove symbols like <,=,%
         totalmet = as.numeric(gsub("[^0-9.-]","",as.character(totalmet))), # and change percentage points to decimal form
         n_tested = round(totalmet/percentmet)) %>% 
  select(buildingname, gradecontenttested,subject,n_tested,totalmet, percentmet) %>%  # percent meeting benchmark
  na.omit()
DT <- data.table(mi_stan_math)
g <- DT[, lapply(.SD, sum), by=list(buildingname, subject)] #add up student counts for all grades
mi_stan_math <- g %>% select(buildingname,subject,n_tested,totalmet) %>% 
  na.omit() %>% 
  mutate(percentmet = totalmet/n_tested, # recalculate ELA percentage for Detroit Prep
         mean_math = mi_math$mean_math,
         sd_math = mi_math$sd_math,
         comp_math = (percentmet - mean_math)/sd_math) %>% 
  select(buildingname, subject, n_tested, percentmet, mean_math, comp_math)

write.csv(mi_stan_ela, file = file.path("output_data/mi_stan_ela.csv"),row.names = FALSE)
write.csv(mi_stan_math, file = file.path("output_data/mi_stan_math.csv"), row.names = FALSE)

mi_acad2 <- mi_acad_all %>% 
  filter(testtype == "M-STEP",
         reportcategory == "All Students",
         entitytype == "psa_school", # is this group more similar to Detroit Prep??
         subject == "ELA"|subject == "Mathematics") %>% 
  mutate(percentmet = .01 * as.numeric(gsub("[^0-9.-]","",as.character(percentmet))), #remove symbols like <,=,%
         totalmet = as.numeric(gsub("[^0-9.-]","",as.character(totalmet))), # and change percentage points to decimal form
         n_tested = round(totalmet/percentmet)) %>% 
  select(buildingname, gradecontenttested,subject,n_tested,totalmet, percentmet) # percent meeting benchmark
summary(mi_acad2$percentmet)
install.packages("data.table")
library(data.table)
DT <- data.table(mi_acad2)
h <- DT[, lapply(.SD, sum), by=list(buildingname, subject)] #add up student counts for all grades
mi_acad2 <- h %>% select(buildingname,subject,n_tested,totalmet) %>% 
  na.omit() %>% 
  mutate(percentmet = totalmet/n_tested) # recalculate percentage for each of 101 schools

#----- MI student counts -------
summary(mi_enrl)
names(mi_enrl) <- tolower(names(mi_enrl))
names(mi_enrl)[c(13,16:22,37:39)] <- c("total", "count_amind", "count_asian", "count_black", "count_hispanic", "count_hawpi",
                                       "count_white", "count_multiple", "count_econ_dis", "count_spec_ed", "count_ell")
str(mi_enrl)
mi_enrl <- mi_enrl %>% 
  mutate(amind = count_amind/total,
         asian = (count_asian + count_hawpi)/total, # to match coding AAPI in other states
         black = count_black/total,
         hispanic = count_hispanic/total,
         white = count_white/total,
         multiple = count_multiple/total,
         econ_dis = count_econ_dis/total,
         spec_ed = as.numeric(gsub("[^0-9.-]","",as.character(count_spec_ed)))/total,
         ell = as.numeric(gsub("[^0-9.-]","",as.character(count_ell)))/total) %>% 
  select(districtname, buildingname,entitytype,total,
         amind, asian, black, hispanic, white, multiple, econ_dis, spec_ed, ell)
dp_enrl <- mi_enrl %>% 
  filter(buildingname == "Detroit Prep")
mi <- mi_enrl %>% 
  filter(districtname == "Detroit Public Schools Community District") %>% 
  summarize(mean_amind = mean(amind),
          sd_amind = sd(amind),
          mean_asian = mean(asian),
          sd_asian = sd(asian),
          mean_black = mean(black),
          sd_black = sd(black),
          mean_hispanic = mean(hispanic),
          sd_hispanic = sd(hispanic),
          mean_white = mean(white),
          sd_white = sd(white),
          mean_multiple = mean(multiple),
          sd_multiple = sd(multiple),
          mean_econdis = mean(econ_dis),
          sd_econdis = sd(econ_dis),
          mean_spec = mean(spec_ed),
          sd_spec = sd(spec_ed),
          mean_ell = mean(ell),
          sd_ell = sd(ell))
dp_enrl <- dp_enrl %>% 
  mutate(mean_amind = mi$mean_amind,
         comp_amind = (amind - mean_amind)/mi$sd_amind,
         mean_asian = mi$mean_asian,
         comp_asian = (asian - mean_asian)/mi$sd_asian,
         mean_black = mi$mean_black,
         comp_black = (black - mean_black)/mi$sd_black,
         mean_hispanic = mi$mean_hispanic,
         comp_hispanic = (hispanic - mean_hispanic)/mi$sd_amind,
         mean_white = mi$mean_white,
         comp_white = (white - mean_white)/mi$sd_white,
         mean_multiple = mi$mean_multiple,
         comp_multiple = (multiple - mean_multiple)/mi$sd_multiple,
         mean_econdis = mi$mean_econdis,
         comp_econdis = (econ_dis - mean_econdis)/mi$sd_econdis,
         mean_spec = mi$mean_spec,
         comp_spec = (spec_ed - mean_spec)/mi$sd_spec,
         mean_ell = mi$mean_ell,
         comp_ell = (ell - mean_ell)/mi$sd_ell) %>% 
  select(districtname, buildingname, entitytype, total,
         amind, mean_amind, comp_amind,
         asian, mean_asian, comp_asian,
         black, mean_black, comp_black,
         hispanic, mean_hispanic, comp_hispanic,
         white, mean_white, comp_white,
         multiple, mean_multiple,comp_multiple,
         econ_dis, mean_econdis, comp_econdis,
         ell, mean_ell, comp_ell,
         spec_ed, mean_spec, comp_spec)
write.csv(dp_enrl,file = file.path("output_data/mi_stan_enrl.csv"),row.names = FALSE)
