rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
getwd()

# --- loading enrollment data ----
# https://osse.dc.gov/page/2019-20-school-year-enrollment-audit-report-and-data
# https://osse.dc.gov/sites/default/files/dc/sites/osse/page_content/attachments/2019%20DC%20School%20Report%20Card%20Aggregate%20Public%20Data_.xlsx updated 12/18/2019

dc_enrl <- read_excel("raw_data/DC_2019 DC School Report Card Aggregate Public Data_.xlsx", sheet = "Enrollment")
summary(dc_enrl)
names(dc_enrl) <- c("lea_code","lea_name","school_code","school","entity","ward","subgroup","grade","perc_enrolled","count","total")
dc_enrl <- dc_enrl %>% 
  mutate(entity = as.factor(entity),
         ward = as.factor(ward),
         grade = as.factor(grade),
         subgroup = as.factor(subgroup),
         perc_enrolled = .01* as.numeric(perc_enrolled)) %>% 
  filter(subgroup == "American Indian/Alaskan Native"|
           subgroup == "Asian"|
           subgroup == "Black/African-American"|
           subgroup == "Hispanic/Latino of any race"|
           subgroup == "Native Hawaiian/Other Pacific Islander"|
           subgroup == "White"|
           subgroup == "Two or more races"|
           subgroup == "At-Risk"|
           subgroup == "English Learners"|
           subgroup == "Students with Disabilities")
levels(dc_enrl$subgroup) <- c("","amind","asian","at-risk","black","","ell","hispanic","","","hawpi","swd","multiple","white")
names(dc_enrl)
dc_enrl <- dc_enrl %>% 
  spread(key = subgroup,
         value = perc_enrolled,
         fill = 0) %>% 
  mutate(asian = asian + hawpi) %>% 
  select(lea_name, school, entity, ward, total, amind, asian, black, hispanic, white, multiple, `at-risk`, ell, swd)
dc_stan <- dc_enrl %>% 
  group_by(ward) %>% 
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
            mean_frpm = mean(`at-risk`),
            sd_frpm = sd(`at-risk`),
            mean_ell = mean(ell),
            sd_ell = sd(ell),
            mean_swd = mean(swd),
            sd_swd = sd(swd))
dc_enrl <- dc_enrl %>% 
  filter(str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           school == "Lee Montessori PCS"|
          str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE) %>% 
  arrange(lea_name, ward, school)
dc_enrl <- merge(dc_stan, dc_enrl, by="ward")
dc_enrl <- dc_enrl %>% 
  mutate(comp_amind = (amind - mean_amind)/sd_amind,
         comp_asian = (asian - mean_asian)/sd_asian,
         comp_black = (black - mean_black)/sd_black,
         comp_hispanic = (hispanic - mean_hispanic)/sd_hispanic,
         comp_white = (white - mean_white)/sd_white,
         comp_multiple = (multiple - mean_multiple)/sd_multiple,
         comp_frpm = (`at-risk` - mean_frpm)/sd_frpm,
         comp_ell = (ell - mean_ell)/sd_ell,
         comp_swd = (swd - mean_swd)/sd_swd) %>% 
  select(school, ward, total, amind, mean_amind, comp_amind,
         asian, mean_asian, comp_asian, black, mean_black, comp_black,
         hispanic, mean_hispanic, comp_hispanic, white, mean_white, comp_white,
         multiple, mean_multiple, comp_multiple, `at-risk`, mean_frpm, comp_frpm,
         ell, mean_ell, comp_ell, swd, mean_swd, comp_swd)
write.csv(dc_enrl, file = file.path("output_data/dc_stan_enrl.csv"),row.names = FALSE)

# ----- math and ela, PARCC -----
# import data from https://osse.dc.gov/sites/default/files/dc/sites/osse/page_content/attachments/Detailed%202018-19%20PARCC%20And%20MSAA%20Performance%202.19.20.Xlsx

dc_parcc <- read_excel("raw_data/DC_Detailed 2018-19 PARCC And MSAA Performance 2.19.20.Xlsx", sheet = "School Performance")
names(dc_parcc)[1:22] <- c("ward","lea_code","lea","school_code","school","test","subject","grade_test","grade_enrl","subgroup_type",
                           "subgroup","perc_me","a","b","c","d","e","f","n_tested","g","h","i")
summary(dc_parcc)
dc_parcc <- dc_parcc %>% 
  mutate(ward = as.factor(ward),
         lea = as.factor(lea),
         school = as.factor(school),
         test = as.factor(test),
         subject = as.factor(subject),
         subgroup_type = as.factor(subgroup_type),
         subgroup = as.factor(subgroup),
         perc_me = .01* as.numeric(gsub("[^0-9.-]","",as.character(perc_me))),
         n_tested = as.numeric(gsub("[^0-9.-]","",n_tested))) %>% 
  filter(subgroup == "All",
         test == "PARCC",
         grade_test == "All",
         grade_enrl == "All") %>% 
  select(lea, school, ward, test, subject, perc_me, n_tested) 

dc_ela <- dc_parcc %>% 
  filter(subject == "ELA",
         str_detect(school, "Capital City PCS")==TRUE|
            school == "District of Columbia International School"|
            str_detect(school,"E.L. Haynes PCS")==TRUE |
            str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
            school == "Inspired Teaching Demonstration PCS"|
            school == "Lee Montessori PCS"|
            str_detect(school, "Washington Latin PCS") == TRUE|
            school == "Washington Yu Ying PCS"|
            str_detect(school, "Two Rivers PCS") == TRUE)
dc_ela_stan <- dc_parcc %>% 
  filter(subject == "ELA") %>% 
  group_by(ward) %>% 
  summarize(mean_me = mean(perc_me),
            sd_me = sd(perc_me),
            n_schools = n())
dc_ela_stan <- dc_ela_stan[-9,]
dc_ela <- merge(dc_ela, dc_ela_stan, by="ward")
dc_ela<- dc_ela %>% 
  mutate(comp_ela = (perc_me - mean_me)/sd_me) %>% 
  select(ward, lea, school, subject, n_tested, perc_me, mean_me, comp_ela) %>% 
  arrange(school)

dc_math <- dc_parcc %>% 
  filter(subject == "Math",
         str_detect(school, "Capital City PCS")==TRUE|
           school == "District of Columbia International School"|
           str_detect(school,"E.L. Haynes PCS")==TRUE |
           str_detect(school,"Elsie Whitlow Stokes Community Freedom PCS")==TRUE |
           school == "Inspired Teaching Demonstration PCS"|
           school == "Lee Montessori PCS"|
           str_detect(school, "Washington Latin PCS") == TRUE|
           school == "Washington Yu Ying PCS"|
           str_detect(school, "Two Rivers PCS") == TRUE)
dc_math_stan <- dc_parcc %>% 
  filter(subject == "Math") %>% 
  group_by(ward) %>% 
  summarize(mean_me = mean(perc_me),
            sd_me = sd(perc_me),
            n_schools = n())
dc_math_stan <- dc_math_stan[-9,]
dc_math <- merge(dc_math, dc_math_stan, by="ward")
dc_math<- dc_math %>% 
  mutate(comp_math = (perc_me - mean_me)/sd_me) %>% 
  select(ward, lea, school, subject, n_tested, perc_me, mean_me, comp_math) %>% 
  arrange(school)

write.csv(dc_ela, file = file.path("output_data/dc_stan_ela.csv"),row.names = FALSE)
write.csv(dc_math, file = file.path("output_data/dc_stan_math.csv"), row.names = FALSE)
