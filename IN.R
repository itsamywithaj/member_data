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

# ----- ENROLLMENT BY RACE AND FRPM BY SCHOOL --------
# Downloaded from https://www.in.gov/doe/it/data-center-and-reports/ 
enrl <- read_excel('raw data/IN_school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-23.xlsx',
                   sheet = "2023")
names(enrl) <- tolower(names(enrl))
str(enrl)

enrl <- enrl %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(district = `corp name`,
         total = `total enrollment`,
         school = `schl name`,
         amind = `american indian`/total,
         asian = (asian + `native hawaiian or other pacific islander`)/total,
         black = black/total,
         hisp = hispanic/total,
         white = white/total,
         mult = multiracial/total,
         frpm = `free/reduced price meals`/total) %>% 
  select(district, school, total, amind, asian, black, hisp, white, mult,frpm) %>% 
  arrange(school)

enrl[(str_detect(enrl$school,"Purdue")),1] <- "Indianapolis Public Schools"
enrl[(str_detect(enrl$school,"Global")),1] <- "Indianapolis Public Schools"


memb_enrl <- enrl %>% 
  filter(str_detect(school,"Purdue")|
           str_detect(school, "Global"))

locseg <- enrl %>% 
  mutate(amind = amind * total,
         asian = asian * total,
         black = black * total,
         hisp = hisp * total,
         white = white * total,
         mult = mult * total) %>% 
  select(-total, -frpm) %>% 
  gather(key = "race", value = "n",amind:mult)

dist_locseg <- locseg %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist) %>% 
  filter(str_detect(school, "Purdue")|
           str_detect(school, "Global"))

memb_enrl <- merge(memb_enrl,dist_locseg)

cty_locseg <- locseg %>% 
  filter(district == "Beech Grove City Schools"|
           str_detect(district, "Franklin Township")|
           str_detect(district, "Indianapolis Public")|
           str_detect(district, "MSD Decatur")|
           str_detect(district, "MSD Lawrence")|
           str_detect(district, "Perry Township")|
           str_detect(district, "MSD Pike")|
           str_detect(district, "MSD Warren")|
           str_detect(district, "MSD Washington")|
           str_detect(district, "MSD Wayne")|
           district == "School Town of Speedway") %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty) %>% 
  filter(str_detect(school, "Purdue")|
           str_detect(school, "Global"))

memb_enrl <- merge(memb_enrl,cty_locseg) %>% 
  select(school:ls_dist, ls_cty,district:cty_frpm)

# AGGREGATE DEMOGRAPHICS FROM THE 'CORP' FILE --------
agg <- read_excel('raw data/IN_corporation-enrollment-ethnicity-free-reduced-price-meal-status-2006-23.xlsx',
                  sheet = "2023")
names(agg) <- tolower(names(agg))
agg <- agg %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(district = `corp name`,
         total = `total enrollment`,
         dist_amind = `american indian`/total,
         dist_asian = (asian + `native hawaiian or other pacific islander`)/total,
         dist_black = black/total,
         dist_hisp = hispanic/total,
         dist_white = white/total,
         dist_mult = multiracial/total,
         dist_frpm = `free/reduced price meals`/total,
         dist_total = total) %>% 
  select(district, dist_total, dist_amind, dist_asian, dist_black, dist_hisp, dist_white, dist_mult, dist_frpm)

memb_enrl <- agg %>% 
  filter(district == "Indianapolis Public Schools") %>% 
  merge(memb_enrl) %>% 
  select(school:ls_dist,district:dist_frpm)

# https://www.k12academics.com/national-directories/school-district/Indiana/Marion
memb_enrl$county = "Marion County"
memb_enrl <- agg %>% 
  filter(district == "Beech Grove City Schools"|
           str_detect(district, "Franklin Township")|
           str_detect(district, "Indianapolis Public")|
           str_detect(district, "MSD Decatur")|
           str_detect(district, "MSD Lawrence")|
           str_detect(district, "Perry Township")|
           str_detect(district, "MSD Pike")|
           str_detect(district, "MSD Warren")|
           str_detect(district, "MSD Washington")|
           str_detect(district, "MSD Wayne")|
           district == "School Town of Speedway") %>% 
  mutate(county = "Marion County",
         amind = dist_amind*dist_total,
         asian = dist_asian *dist_total,
         black = dist_black * dist_total,
         hisp = dist_hisp * dist_total,
         white = dist_white * dist_total,
         mult = dist_mult * dist_total,
         frpm = dist_frpm * dist_total) %>% 
  group_by(county) %>% 
  summarize(cty_total = sum(dist_total),
            cty_amind = sum(amind)/cty_total,
            cty_asian = sum(asian)/cty_total,
            cty_black = sum(black)/cty_total,
            cty_hisp = sum(hisp)/cty_total,
            cty_white = sum(white)/cty_total,
            cty_mult = sum(mult)/cty_total,
            cty_frpm = sum(frpm)/cty_total)%>% 
  merge(memb_enrl) %>% 
  select(school:dist_frpm,county:cty_frpm)
  
# SWD AND ENGLISH LEARNERS 2021-2022 DATA ONLY -------
subgroups <- read_excel('raw data/IN_school-enrollment-ell-special-education-2006-22-v2.xlsx')
names(subgroups) <- tolower(names(subgroups))
subgroups <- subgroups %>% 
  mutate(p_ell = `ell %`,
         p_swd = `special education %`,
         school = `school name`,
         district = `corp name`) %>% 
  select(school, district, p_ell, p_swd)

memb_enrl <- subgroups %>% 
  filter(str_detect(school, "Purdue")|
           str_detect(school, "Global")) %>% 
  select(-district) %>% 
  merge(memb_enrl) %>% 
  select(school, total:frpm, p_ell, p_swd,ls_dist:cty_frpm)

agg_subgroups <- read_excel('raw data/IN_corporation-enrollment-ell-special-education-2006-22-v2.xlsx')
names(agg_subgroups) <- tolower(names(agg_subgroups))
memb_enrl <- agg_subgroups %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(district = `corp name`,
         dist_ell = `ell %`,
         dist_swd = `special education %`) %>% 
  select(district, dist_ell, dist_swd) %>% 
  filter(district == "Indianapolis Public Schools") %>% 
  merge(memb_enrl) %>% 
  select(school:dist_frpm, dist_ell, dist_swd,county:cty_frpm)

memb_enrl <- agg_subgroups %>% 
  mutate_if(is.numeric, ~replace_na(.,0)) %>% 
  mutate(district = `corp name`,
         ell = `ell n`,
         swd = `special education n`,
         total = `total enrollment`) %>% 
  select(district, ell, swd, total) %>% 
  filter(district == "Beech Grove City Schools"|
           str_detect(district, "Franklin Township")|
           str_detect(district, "Indianapolis Public")|
           str_detect(district, "MSD Decatur")|
           str_detect(district, "MSD Lawrence")|
           str_detect(district, "Perry Township")|
           str_detect(district, "MSD Pike")|
           str_detect(district, "MSD Warren")|
           str_detect(district, "MSD Washington")|
           str_detect(district, "MSD Wayne")|
           district == "School Town of Speedway") %>% 
  mutate(county = "Marion County") %>% 
  group_by(county) %>% 
  summarize(cty_ell = sum(ell)/sum(total),
            cty_swd = sum(swd/sum(total)),
            .groups = 'drop') %>% 
  merge(memb_enrl) %>% 
  select(school:dist_swd,county,cty_total:cty_frpm, cty_ell, cty_swd)

write.csv(memb_enrl, file = file.path('output data/in_enrl.csv'), row.names = FALSE)

# ---- ASSESSMENT DATA ------
# disaggregated dataset available at https://www.in.gov/doe/files/ILEARN-2022-Grade3-8-Final-School-Gender-and-Ethnicity-Disaggregated.xlsx
# https://www.in.gov/doe/files/istep-2021-grade10-final-school-disaggregated.xlsx
ela <- read_excel('raw data/IN_ILEARN-2022-Grade3-8-Final-Corporation.xlsx',
                  sheet = "ELA")
names(ela) <- tolower(ela[4,])
ela <- ela[-(1:4),c(1,2,45:51)]
str(ela)
ela <- ela %>% 
  mutate(district = `corp name`,
         n_ela = as.numeric(`ela\r\ntotal\r\ntested`),
         p_ela = as.numeric(`ela\r\nproficient \r\n%`)) %>% 
  select(district:p_ela)
memb_ela <- ela %>% 
  filter(district == "Indianapolis Public Schools"|
           str_detect(district, "Global"))

math <- read_excel('raw data/IN_ILEARN-2022-Grade3-8-Final-Corporation.xlsx',
                  sheet = "Math")
names(math) <- tolower(math[4,])
math <- math[-(1:4),c(1,2,45:51)]
str(math)
math <- math %>% 
  mutate(district = `corp name`,
         n_math = as.numeric(`math\r\ntotal\r\ntested`),
         p_math = as.numeric(`math\r\nproficient \r\n%`)) %>% 
  select(district:p_math)
memb_math <- math %>% 
  filter(district == "Indianapolis Public Schools"|
           str_detect(district, "Global"))
acad <- merge(memb_math, memb_ela)
z <- acad %>% 
  mutate(dist_math = p_math,
         dist_ela = p_ela) %>% 
  select(district, dist_math, dist_ela) %>% 
  filter(district == "Indianapolis Public Schools")
memb_acad <- acad %>% 
  mutate(school = district,
         district = "Indianapolis Public Schools") %>% 
  select(school, district, p_math, p_ela) %>% 
  filter(school != "Indianapolis Public Schools") %>% 
  merge(z) %>% 
  select(school, district, p_math, dist_math, p_ela, dist_ela)

hs <- read_excel('raw data/IN_istep-2021-grade10-final-corporation.xlsx')
names(hs) <- tolower(hs[5,])
hs <- hs[-(1:5),]
hs <- hs %>% 
  mutate(district = `corp name`,
         p_ela = `ela\r\npercent pass`,
         n_ela = `ela\r\ntest n`,
         p_math = `math\r\npercent pass`,
         n_math = `math\r\ntest n`) %>% 
  select(district:n_math) 
x <- hs %>% 
  mutate(dist_math = p_math,
         dist_ela = p_ela) %>% 
  select(district, dist_math, dist_ela) %>% 
  filter(district == "Indianapolis Public Schools")

memb_hs <- hs %>% 
  filter(str_detect(district, "Purdue")) %>% 
  mutate(school = district,
         district = "Indianapolis Public Schools") %>% 
  merge(x) %>% 
  select(school, district, n_math, p_math, dist_math, n_ela, p_ela, dist_ela)

write.csv(memb_acad, file = file.path('output data/in_acad.csv'),row.names = FALSE)
write.csv(memb_hs, file = file.path('output data/in_hs.csv'), row.names = FALSE)
