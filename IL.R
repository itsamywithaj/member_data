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

# ----- SCHOOL-LEVEL AND AGGREGATE ENROLLMENT ------
# Downloaded from https://www.isbe.net/Pages/Fall-Enrollment-Counts.aspx
enrl <- read_excel('raw data/IL_FY2023-Fall-Enroll-Report.xlsx',
                   sheet = "Home School")
names(enrl) <- tolower(names(enrl))
str(enrl)
enrl <- enrl %>% 
  mutate(county = as.factor(countyname),
         district = as.factor(`district name`),
         school = as.factor(`school name`),
         total = as.numeric(`prek-12`),
         amind = as.numeric(gsub("[^0-9.-]","",`native american/alaskan`)),
         asian = as.numeric(gsub("[^0-9.-]","",asian)) + as.numeric(gsub("[^0-9.-]","",`pacific islander`)),
         black = as.numeric(gsub("[^0-9.-]","",`african american`)),
         hisp = as.numeric(gsub("[^0-9.-]","",hispanic)),
         white = as.numeric(gsub("[^0-9.-]","",white)),
         mult = as.numeric(gsub("[^0-9.-]","",`two or more`)),
         frpl = as.numeric(gsub("[^0-9.-]","",`low income`)),
         el = as.numeric(gsub("[^0-9.-]","",`english learner`)),
         swd = as.numeric(gsub("[^0-9.-]","",iep))) %>% 
  select(county:county:amind, asian, black, hisp, white, mult:swd)

memb_enrl <- enrl %>% 
  filter(str_detect(school, "Great La")) %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total,
         frpl = frpl/total,
         el = el/total,
         swd = swd/total)

memb_enrl <- enrl %>% 
  filter(district == "City of Chicago SD 299") %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  group_by(district) %>% 
  summarize(dist_total = sum(total),
            dist_amind = sum(amind)/sum(total),
            dist_asian = sum(asian)/sum(total),
            dist_black = sum(black)/sum(total),
            dist_hisp = sum(hisp)/sum(total),
            dist_white = sum(white)/sum(total),
            dist_mult = sum(mult)/sum(total),
            dist_frpl = sum(frpl)/sum(total),
            dist_el = sum(el)/sum(total),
            dist_swd = sum(swd)/sum(total)) %>% 
  merge(memb_enrl)
  
memb_enrl <- enrl %>% 
  filter(county == "Cook") %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind)/sum(total),
            cty_asian = sum(asian)/sum(total),
            cty_black = sum(black)/sum(total),
            cty_hisp = sum(hisp)/sum(total),
            cty_white = sum(white)/sum(total),
            cty_mult = sum(mult)/sum(total),
            cty_frpl = sum(frpl)/sum(total),
            cty_el = sum(el)/sum(total),
            cty_swd = sum(swd)/sum(total)) %>% 
  merge(memb_enrl)

# ---- LOCAL SEGREGATION AT DISTRICT- AND COUNTY-LEVEL-----
ls_dist <- enrl %>% 
  filter(district == "City of Chicago SD 299") %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  select(-county, -district, -total, -frpl, -el, -swd) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>%
  mutate(ls_dist = ls) %>% 
  filter(str_detect(school, "Great L")) %>% 
  select(school, ls_dist)

ls_cty <- enrl %>% 
  filter(county == "Cook") %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  select(-county, -district, -total, -frpl, -el, -swd) %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race", "school", weight = "n", wide = TRUE) %>%
  mutate(ls_cty = ls) %>% 
  filter(str_detect(school, "Great L")) %>% 
  select(school, ls_cty)

memb_enrl <- merge(memb_enrl,merge(ls_dist,ls_cty)) %>% 
  select(school, total, amind:ls_cty,district:dist_swd,county:cty_swd)

write.csv(memb_enrl, file = file.path('output data/il_enrl.csv'),row.names = FALSE)

# ----- ASSESSMENT ------
# Data downloaded from https://www.isbe.net/pages/illinois-state-report-card-data.aspx
acad <- read_excel('raw data/IL_2022-Report-Card-Public-Data-Set.xlsx',
                   sheet = "ELA Math Science")
names(acad) <- tolower(names(acad))
levels(as.factor(acad$type))
str(acad)
memb_acad <- acad %>% 
  mutate(school = `school name`,
         district = district,
         p_ela = `% ela proficiency` * .01,
         p_math = `% math proficiency` * .01) %>% 
  filter(str_detect(school, "Great L")|
           district == "City of Chicago SD 299" & type == "District") %>% 
  select(school, district, p_ela, p_math)

write.csv(memb_acad, file = file.path('output data/il_acad.csv'), row.names = FALSE)
