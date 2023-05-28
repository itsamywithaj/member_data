# --- Amy Jiravisitcul. 12 May 2023 ----
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


#--- ENROLLMENT BY RACE/ETHNICITY, SES, LEP, SWD  -----
# Data downloaded from https://www.tn.gov/content/dam/tn/education/data/school-profile-2021-2022-updated-2022-12-06.xlsx
enrl <- read_excel('raw data/TN_school-profile-2021-2022-updated-2022-12-06.xlsx', sheet = "By Student Group")
names(enrl) <- tolower(names(enrl))
str(enrl)
names(enrl)[c(3,5,7,8,10,13,14,16,20:23)] <- c("district","school","black","asian","ecdis","nhpi",
                                         "hisp","lep","mult","amind","swd","white")
enrl <- enrl %>% 
  mutate(total = as.numeric(total),
         amind = case_when(amind == "*"~ 5/total, # asterisk represents fewer than 10 students
                           str_detect(amind, "Less than 5%") ~ .03), # estimated from suppression
         asian = case_when(asian == "*" ~ 5/total,
                           str_detect(asian,"Less than 5%") ~ .03,
                           TRUE ~ as.numeric(asian)) * .01,
         black = case_when(black == "*" ~ 5/total,
                           str_detect(black, "Less than 5%") ~ .03,
                           str_detect(black, "Greater than 95%") ~ .97,
                           TRUE ~ as.numeric(black) * .01),
         hisp = case_when(hisp == "*" ~ 5/total,
                          str_detect(hisp, "Less than 5%") ~ .03,
                          TRUE ~ as.numeric(hisp) * .01),
         white = case_when(white == "*" ~ 5/total,
                           str_detect(white, "Less than 5%") ~ .03,
                           str_detect(white, "Greater than 95%") ~ .97,
                           TRUE ~ as.numeric(white) * .01),
         mult = case_when(mult == "*"~ 5/total, # asterisk represents fewer than 10 students
                          str_detect(mult, "Less than 5%") ~ .03,
                          TRUE ~ as.numeric(mult) * .01), # estimated from suppression)
         ecdis = case_when(ecdis == "*" ~ 5/total,
                           str_detect(ecdis, "Less than 5%") ~ .03,
                           str_detect(ecdis, "Greater than 95%") ~ .97,
                           TRUE ~ as.numeric(ecdis) * .01),
         lep = case_when(lep == "*" ~ 5/total,
                         str_detect(lep, "Less than 5%") ~ .03,
                         str_detect(lep, "Greater than 95%") ~ .97,
                         TRUE ~ as.numeric(lep) * .01),
         swd = case_when(swd == "*" ~ 5/total,
                         str_detect(swd, "Less than 5%") ~ .03,
                         str_detect(swd, "Greater than 95%") ~ .97,
                         TRUE ~ as.numeric(swd) * .01)) %>% 
  # omitting mult as a category sums closer to projected 100%. Does TN count race as non-exclusive?
  select(district, school, total, amind, asian, black, hisp, 
         white, mult, ecdis, lep, swd)

memb_enrl <- enrl %>% 
  filter(school == "Nashville Classical"|
           str_detect(school, "Valor"))

# ------ DISTRICT AGGREGATES -------
nash <- enrl %>% 
  filter(str_detect(district, "Nashville")) %>% na.omit() %>% 
  mutate(n_amind = amind * total,
         n_asian = asian * total,
         n_black = black * total,
         n_hisp = hisp * total,
         n_white = white * total,
         n_mult = mult * total,
         n_ecdis = ecdis * total,
         n_lep = lep * total,
         n_swd = swd * total) %>% 
  group_by(district) %>% 
  summarize(d_total = sum(total),
            d_amind = sum(n_amind)/d_total,
            d_asian = sum(n_asian)/d_total,
            d_black = sum(n_black)/d_total,
            d_hisp = sum(n_hisp)/d_total,
            d_white = sum(n_white)/d_total,
            d_mult = sum(n_mult)/d_total,
            d_ecdis = sum(n_ecdis)/d_total,
            d_lep = sum(n_lep)/d_total,
            d_swd = sum(n_swd)/d_total) 

memb_enrl <- merge(memb_enrl, nash)

# ----- LOCAL SEGREGATION BY DISTRICT -----
locseg <- enrl %>% 
  mutate(n_amind = amind * total,
         n_asian = asian * total,
         n_black = black * total,
         n_hisp = hisp * total,
         n_white = white * total,
         n_mult = mult * total,
         n_ecdis = ecdis * total,
         n_lep = lep * total,
         n_swd = swd * total) %>% 
  select(-c(total:swd),-c(n_ecdis:n_swd)) %>% 
  gather(race, n, n_amind:n_mult) %>% 
  filter(str_detect(district,"Nashville")) %>% 
  mutual_local("race","school",weight="n", wide = TRUE) %>% 
  filter(school == "Nashville Classical"|
           str_detect(school, "Valor")) %>% 
  select(-p)
memb_enrl <- merge(memb_enrl,locseg)

#----- COUNTY LEVEL AGGREGATE AND LOCSEG -------
# https://www.tn.gov/education/district-resources.html
unique(enrl$district) %>% sort() %>% View()
midcumberland <- enrl %>% 
  filter(district == "Stewart County"| # All the school districts in Mid-Cumberland CORE Region
           district == "Montgomery"|
           district == "Houston County"|
           district == "Humphreys County"|
           district == "Dickson County"|
           district == "Robertson County"|
           district == "Sumner County"|
           district == "Lebanon"|
           district == "Wilson County"|
           district == "Cheatham County"|
           district == "Williamson County"|
           district == "Rutherford County"|
           district == "Franklin SSD"|
           district == "Murfreesboro"|
           district == "Metro Nashville Public Schools") %>% 
  mutate(n_amind = amind * total,
         n_asian = asian * total,
         n_black = black * total,
         n_hisp = hisp * total,
         n_white = white * total,
         n_mult = mult * total,
         n_ecdis = ecdis * total,
         n_lep = lep * total,
         n_swd = swd * total) %>% na.omit() %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(n_amind)/cty_total,
            cty_asian = sum(n_asian)/cty_total,
            cty_black = sum(n_black)/cty_total,
            cty_hisp = sum(n_hisp)/cty_total,
            cty_white = sum(n_white)/cty_total,
            cty_mult = sum(n_mult)/cty_total,
            cty_ecdis = sum(n_ecdis)/cty_total,
            cty_lep = sum(n_lep)/cty_total,
            cty_swd = sum(n_swd)/cty_total)

midcumberland$dummy = "dummy"
memb_enrl$dummy = "dummy"

memb_enrl <- merge(memb_enrl,midcumberland) %>% select(-dummy)

county_loc <- enrl %>% 
  mutate(n_amind = amind * total,
         n_asian = asian * total,
         n_black = black * total,
         n_hisp = hisp * total,
         n_white = white * total,
         n_mult = mult * total,
         n_ecdis = ecdis * total,
         n_lep = lep * total,
         n_swd = swd * total) %>% 
  select(-c(total:swd),-c(n_ecdis:n_swd)) %>% 
  gather(race, n, n_amind:n_mult) %>% 
  filter(district == "Stewart County"| # All the school districts in Mid-Cumberland CORE Region
           district == "Montgomery"|
           district == "Houston County"|
           district == "Humphreys County"|
           district == "Dickson County"|
           district == "Robertson County"|
           district == "Sumner County"|
           district == "Lebanon"|
           district == "Wilson County"|
           district == "Cheatham County"|
           district == "Williamson County"|
           district == "Rutherford County"|
           district == "Franklin SSD"|
           district == "Murfreesboro"|
           district == "Metro Nashville Public Schools") %>%  
  mutual_local("race","school",weight="n", wide = TRUE) %>% 
  filter(school == "Nashville Classical"|
           str_detect(school, "Valor")) %>% 
  mutate(ls_cty = ls) %>% 
  select(-c(p,ls))

memb_enrl <- merge(memb_enrl,county_loc)
write.csv(memb_enrl,file = file.path('output data/tn_enrl.csv'),row.names = FALSE)  

# ---- ASSESSMENT DATA ----
acad <- read_excel('raw data/TN_school_release_file_suppressed_2022.xlsx')
levels(as.factor(acad$subject))
str(acad)
acad<- acad %>% 
  mutate(system_name = as.factor(system_name),
         school_name = as.factor(school_name),
         n_met = as.numeric(n_met_expectations),
         n_exceed = as.numeric(n_exceeded_expectations),
         n_met_exceed = n_met + n_exceed,
         pct_met_exceeded = as.numeric(pct_met_exceeded)*.01)
memb_acad <- acad %>% 
  filter(school_name == "Valor Flagship Academy"|
           school_name == "Valor Voyager Academy"|
           school_name == "Nashville Classical",
         subject == "HS Math"|
           subject == "Math"|
           subject == "ELA"|
           subject == "HS English",
         student_group == "All Students") %>% 
  select(system, system_name, school, school_name, subject, grade, 
         student_group, valid_tests, n_met_exceed,pct_met_exceeded)
memb_acad[7,9] <- 274 # manually calculated from row 7. 61.7% of 444
memb_acad$subject[7:8] <- c("ELA","Math") # rename subject listed for HS to match ELA and math
memb_acad <- memb_acad %>% 
  select(school_name, subject, valid_tests, n_met_exceed) %>% 
  group_by(school_name,subject) %>% 
  summarize(n_tested = sum(valid_tests),
            n_met_exceed = sum(n_met_exceed),
            .groups = 'drop') %>% 
  mutate(p_met_exceed = n_met_exceed/n_tested)

dist_acad <- read_excel('raw data/TN_district_release_file_suppressed_2022.xlsx')
# Davidson County tracks with members' system names
str(dist_acad)
levels(as.factor(dist_acad$student_group))
dist_acad <- dist_acad %>% 
  filter(system_name == "Davidson County",
         subject == "HS Math"|
           subject == "Math"|
           subject == "ELA"|
           subject == "HS English",
         student_group == "All Students") %>% 
  mutate(n_met = as.numeric(n_met_expectations),
         n_exceed = as.numeric(n_exceeded_expectations),
         n_met_exceed = n_met + n_exceed,
         pct_met_exceeded = as.numeric(pct_met_exceeded)*.01) %>%
  select(system_name, subject, grade, student_group, valid_tests, n_met_exceed,pct_met_exceeded)
dist_acad$subject[4:5] <- c("ELA","Math") # Rename HS English and HS Math to ELA and Math
dist_acad <- dist_acad %>% 
  select(system_name, subject, valid_tests, n_met_exceed) %>% 
  group_by(system_name,subject) %>% 
  summarize(n_tested = sum(valid_tests),
            n_met_exceed = sum(n_met_exceed),
            .groups = 'drop') %>% 
  mutate(dist_met_exceed = n_met_exceed/n_tested) %>% 
  select(system_name, subject, dist_met_exceed)

memb_acad$system_name <- "Davidson County"
memb_acad <- merge(memb_acad,dist_acad)
write.csv(memb_acad, file = file.path('output data/tn_acad.csv'),row.names = FALSE)
