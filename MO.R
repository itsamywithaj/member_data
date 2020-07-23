rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
getwd()
# data import from https://apps.dese.mo.gov/MCDS/FileDownloadWebHandler.ashx?filename=3bd91069-2465Building%20Demographic%20Data%202006-2018.xls

# ---- enrollment ----
mo_enrl <- read_excel("raw_data/MO_Building Demographic Data 2006-2018.xls")
names(mo_enrl)<- tolower(names(mo_enrl))
str(mo_enrl)

mo_enrl <- mo_enrl %>% 
  filter(year == "2019",
         district_name == "ST. LOUIS CITY"|district_name == "NORTH KANSAS CITY 74"|district_name == "KANSAS CITY 33"|
           school_name == "CITY GARDEN MONTESSORI SCHOOL"|
           str_detect(school_name,"ACADEMIE LAFAYETTE")==TRUE|
           str_detect(school_name,"CITIZENS OF THE WORLD CHARTER")==TRUE,
         enrollment_grades_k_12 > 0) %>% 
  mutate(district = district_name,
         school = school_name,
         total = enrollment_grades_k_12,
         c_amind = as.numeric(enrollment_indian),
         c_asian = as.numeric(enrollment_asian),
         c_black = as.numeric(enrollment_black),
         c_hispanic = as.numeric(enrollment_hispanic),
         c_white = as.numeric(enrollment_white),
         c_multiple = as.numeric(enrollment_multiracial),
         c_frpl = as.numeric(lunch_count_free_reduced),
         c_lep = as.numeric(enrollment_ell_lep),
         c_iep = as.numeric(iep_schoolage_childcount),
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total,
         frpl = c_frpl/total,
         lep = c_lep/total,
         iep = c_iep/total) %>% 
  select(district, school, total, amind, asian, black, hispanic, white, multiple, frpl, lep, iep,
         c_amind, c_asian, c_black, c_hispanic, c_white, c_multiple, c_frpl, c_lep, c_iep)
mo_enrl[is.na(mo_enrl)] = 0 # fewer than 5 students leads to an asterisk, rounded down to 0
mo_comp <- mo_enrl %>%
  group_by(district) %>%
  summarize(m_amind = mean(amind),
            sd_amind = sd(amind),
            m_asian = mean(asian),
            sd_asian = sd(asian),
            m_black = mean(black),
            sd_black = sd(black),
            m_hispanic = mean(hispanic),
            sd_hispanic = sd(hispanic),
            m_white = mean(white),
            sd_white =sd(white),
            m_multiple = mean(multiple),
            sd_multiple = sd(multiple),
            m_lep = mean(lep),
            sd_lep = sd(lep),
            m_iep = mean(iep),
            sd_iep = sd(iep),
            n= n())
mo_memb <- mo_enrl %>% 
  filter(school == "CITY GARDEN MONTESSORI SCHOOL"|
           str_detect(school,"ACADEMIE LAFAYETTE")==TRUE|
           str_detect(school,"CITIZENS OF THE WORLD CHARTER")==TRUE) %>% 
  arrange(school)
mo_memb$district <- c("NORTH KANSAS CITY 74","NORTH KANSAS CITY 74", "KANSAS CITY 33", # Academie Lafayette campuses
                      "NORTH KANSAS CITY 74", "ST. LOUIS CITY")
mo_memb[6,] <- NA
mo_memb[6,c(1:12)] <- list("ST. LOUIS CITY","Kairos Academies",117,.01,.03,.56,
                        .03,.21,.15,.68,.01,.15)
mo_stan_enrl <- merge(mo_memb, mo_comp, by="district")
mo_stan_enrl <- mo_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_lep = (lep - m_lep)/sd_lep,
         comp_iep = (iep - m_iep)/sd_iep) %>%
  select(school, total, amind, m_amind, comp_amind, asian, m_asian, comp_asian,
         black, m_black, comp_black, hispanic, m_hispanic, comp_hispanic,white, m_white, comp_white,
         multiple, m_multiple, comp_multiple, frpl, lep, m_lep, comp_lep, iep, m_iep, comp_iep)
write.csv(mo_stan_enrl, file = file.path("output_data/mo_stan_enrl.csv"),row.names = FALSE)

#---- FRPL ----
# data import from https://apps.dese.mo.gov/MCDS/FileDownloadWebHandler.ashx?filename=45910fce-6507Free%20and%20Reduced%20Priced%20Lunch%20Percentage%20by%20Building%202019-20.xlsx
mo_frpl <- read_excel("raw_data/MO_Free and Reduced Priced Lunch Percentage by Building 2019-20.xlsx")
names(mo_frpl)<- tolower(mo_frpl[9,])
mo_frpl <- mo_frpl[-c(1:9),]
str(mo_frpl)
mo_frpl <- mo_frpl %>% 
  mutate(district = as.factor(`district name`),
         school = as.factor(`building name`),
         c_frpl = as.numeric(`january state fte f&rl count`),
         total = as.numeric(`january membership`),
         frpl = as.numeric(`2020 f&rl percentage`)*.01) %>% 
  select(district, school, total, c_frpl, frpl) %>% 
  filter(district == "ST. LOUIS CITY"| district == "NORTH KANSAS CITY 74"|district == "KANSAS CITY 33"|
           school == "CITY GARDEN MONTESSORI SCHOOL"|
           str_detect(school,"ACADEMIE LAFAYETTE")==TRUE|
           str_detect(school,"CITIZENS OF THE WORLD CHARTER")==TRUE,
         total >0) %>% 
  arrange(school)
mo_stan_frpl <- mo_frpl %>% 
  group_by(district) %>% 
  summarize(m_frpl = mean(frpl),
            sd_frpl = sd(frpl),
            n = n())
write.csv(mo_stan_frpl, file = file.path("output_data/mo_stan_frpl.csv"),row.names = FALSE)

#---- Academics -----
# data of all grades combined by school imported from https://apps.dese.mo.gov/MCDS/FileDownloadWebHandler.ashx?filename=afbf75fa-d277School%20-%20content%20area%20all%20and%20disag.zip

mo_acad <- read_excel("raw_data/MO_School - State Assessment Results.xlsx")
names(mo_acad) <- tolower(names(mo_acad))
str(mo_acad)
mo_acad <- mo_acad %>% 
  filter(year == 2019) %>%
  mutate(category = as.factor(category),
         type = as.factor(type),
         district = as.factor(district_name),
         school = as.factor(school_name),
         subject = as.factor(content_area),
         n_tested = as.numeric(reportable),
         adv_prof = (as.numeric(proficient_pct)*.01)+(as.numeric(advanced_pct)*.01)) %>% 
  select(category, type, district, school, subject, n_tested, adv_prof)
levels(mo_acad$subject)[1:2] <- c("ela","math")
levels(mo_acad$type)
mo_acad <- mo_acad %>% 
  filter(district == "ACADEMIE LAFAYETTE"|district == "NORTH KANSAS CITY 74"| district == "KANSAS CITY 33"|
           district == "ST. LOUIS CITY"|
           district == "CITIZENS OF THE WORLD CHARTER"|
           district == "CITY GARDEN MONTESSORI",
         subject == "math"|subject == "ela",
         type =="Total") %>% 
  na.omit() %>% 
  arrange(school)
mo_comp <- mo_acad %>% 
  group_by(subject, district) %>% 
  summarize(m_advprof = mean(adv_prof),
            sd_advprof = sd(adv_prof),
            n=n())

mo_memb <- mo_acad %>% 
  filter(district == "ACADEMIE LAFAYETTE"|district == "CITIZENS OF THE WORLD CHARTER"|
           district == "CITY GARDEN MONTESSORI")
mo_memb$district <- c("NORTH KANSAS CITY 74","NORTH KANSAS CITY 74", 
                      "NORTH KANSAS CITY 74","NORTH KANSAS CITY 74",
                      "KANSAS CITY 33","KANSAS CITY 33", # Academie Lafayette Armour, Cherry, Oak Campuses
                      "NORTH KANSAS CITY 74", "NORTH KANSAS CITY 74", # CWC KC
                      "ST. LOUIS CITY","ST. LOUIS CITY") # City Garden
mo_stan_acad <- merge(mo_memb,mo_comp,by=c("subject","district"))
names(mo_stan_acad)
mo_stan_acad <- mo_stan_acad %>% 
  mutate(comp = (adv_prof - m_advprof)/sd_advprof) %>% 
  select(district, subject, school, n_tested, adv_prof,m_advprof,comp) %>% 
  arrange(subject, school)
write.csv(mo_stan_acad,file = file.path("output_data/mo_stan_acad.csv"),row.names = FALSE)

z <- mo_enrl %>% 
  filter(district == "NORTH KANSAS CITY 74")
mean(z$frpl)
sd(z$frpl)
(.64-.5)/(.1956465)
