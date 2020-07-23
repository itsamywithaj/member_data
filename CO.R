rm(list = ls())
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)

# data all from https://www.cde.state.co.us/cdereval/pupilcurrent
# ----- racial demographics -----
co_dem <- read_excel("raw_data/CO_2019-20-Membership-Race-Gender-byGradeSchool.xlsx")
str(co_dem)
names(co_dem) <- tolower(co_dem[2,])
co_dem <- co_dem[-c(1:3),]
co_dem <- co_dem %>% 
  mutate(grade = as.factor(`grade level`),
         c_amind = as.numeric(`american indian or alaskan native female`)+
           as.numeric(`american indian or alaskan native male`),
         c_asian = as.numeric(`asian female`)+as.numeric(`asian male`)+
           as.numeric(`native hawaiian or other pacific islander female`)+
           as.numeric(`native hawaiian or other pacific islander male`),
         c_black = as.numeric(`black or african american female`)+as.numeric(`black or african american male`),
         c_hispanic = as.numeric(`hispanic or latino male`)+as.numeric(`hispanic or latino female`),
         c_white = as.numeric(`white female`)+as.numeric(`white male`),
         c_multiple = as.numeric(`two or more races female`)+as.numeric(`two or more races male`),
         total = as.numeric(`pk-12 total`),
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total) %>%
  select(`org. code`,`organization name`,`school code`,`school name`,grade,
         total, amind, asian, black, hispanic, white, multiple) %>% 
  filter(grade == "ALL GRADE LEVELS",
         `organization name` == "Denver County 1")
dsst_enrl <- co_dem %>% 
  filter(str_detect(`school name`,"DSST")==TRUE|
           `school name` == "Downtown Denver Expeditionary School") %>% 
  arrange(`school code`)
co_enrl <- co_dem %>% 
  group_by(`organization name`) %>% 
  summarize(m_amind = mean(amind),
            m_asian = mean(asian),
            m_black = mean(black),
            m_hispanic = mean(hispanic),
            m_white = mean(white),
            m_multiple = mean(multiple),
            sd_amind = sd(amind),
            sd_asian = sd(asian),
            sd_black = sd(black),
            sd_hispanic = sd(hispanic),
            sd_white = sd(white),
            sd_multiple = sd(multiple))
dsst_enrl <- merge(dsst_enrl, co_enrl, by="organization name")
dsst_enrl <- dsst_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple) %>% 
  select(`school name`,total,amind,m_amind, comp_amind, asian, m_asian, comp_asian,
         black, m_black, comp_black, hispanic, m_hispanic, comp_hispanic,
         white, m_white, comp_white, multiple, m_multiple, comp_multiple)
write.csv(dsst_enrl,file = file.path("output_data/co_stan_enrl.csv"), row.names = FALSE)

#----- FRPL, EL, SPED ----
co_frpl <- read_excel("raw_data/CO_2019-20_K12_FRL_bySchool2a.xlsx")
names(co_frpl) <- tolower(co_frpl[2,])
co_frpl <- co_frpl[-c(1:2),]
str(co_frpl)
co_frpl <- co_frpl %>% 
  mutate(district = as.factor(`district name`),
         total = as.numeric(`k-12 count`),
         c_frpl = as.numeric(`free and reduced count`),
         frpl = c_frpl/total,
         school = `school name`) %>% 
  select(district, school,total, c_frpl, frpl) %>% 
  filter(district == "Denver County 1") %>% 
  na.omit()
co_frpl_ <- co_frpl %>% 
  group_by(district) %>% 
  summarize(m_frpl = mean(frpl),
            sd_frpl = sd(frpl))
memb_frpl <- co_frpl %>% 
  filter(school == "Downtown Denver Expeditionary School"|
           str_detect(school, "DSST")==TRUE) %>% 
  arrange(school)
co_stan_frpl <- merge(memb_frpl, co_frpl_,by="district")
co_stan_frpl <- co_stan_frpl %>% 
  mutate(comp_frpl = (frpl - m_frpl)/sd_frpl) %>% 
  select(district, school, frpl, m_frpl, comp_frpl)
write.csv(co_stan_frpl, file = file.path("output_data/co_stan_frpl.csv"),row.names = FALSE)

co_ipst <- read_excel("raw_data/CO_2019-20-IPST-bySchool2a.xlsx")
names(co_ipst) <- tolower(co_ipst[1,])
co_ipst <- co_ipst[-1,]
str(co_ipst)
co_ipst <- co_ipst %>% 
  mutate(district = as.factor(`district name`),
         total = as.numeric(`total pk-12 pupil membership`),
         c_el = as.numeric(`el count`),
         el = c_el/total,
         school = `school name`,
         c_sped = as.numeric(`special education count`),
         sped = c_sped/total) %>% 
  select(district, school,total, c_el, el, c_sped, sped) %>% 
  filter(district == "Denver County 1") %>% 
  mutate_if(is.numeric, replace_na, 0)
memb <- co_ipst %>% 
  filter(school == "Downtown Denver Expeditionary School"|
           str_detect(school, "DSST")==TRUE) %>% 
  arrange(school)
co_ipst <- co_ipst %>% 
  group_by(district) %>% 
  summarize(m_el = mean(el),
            sd_el = sd(el),
            m_sped = mean(sped),
            sd_sped = sd(sped))

co_stan_elsped <- merge(memb, co_ipst,by="district")
co_stan_elsped <- co_stan_elsped %>% 
  mutate(comp_el = (el - m_el)/sd_el,
         comp_sped = (sped - m_sped)/sd_sped) %>% 
  select(district, school, el, m_el, comp_el,sped, m_sped, comp_sped)
write.csv(co_stan_elsped, file = file.path("output_data/co_stan_elsped.csv"),row.names = FALSE)

#----- CMAS scores -----
# data imported from https://www.cde.state.co.us/assessment/cmas-dataandresults-2019
cmas <- read_excel("raw_data/CO_2019 CMAS ELA MATH District and School Achievement Results.xlsx")
str(cmas)
names(cmas) <- tolower(cmas[10,])
names(cmas)[29] <- "z"
cmas[,27]
cmas <- cmas[-c(1:10),-c(14,27:28)]
cmas <- cmas %>% 
  mutate(district = as.factor(`district name`),
         school = as.factor(`school name`),
         subject = as.factor(subject),
         grade = as.factor(grade),
         n_tested = as.numeric(`number of valid scores`),
         c_metexc = as.numeric(`number met or exceeded expectations`)) %>% 
  filter(level == "SCHOOL",
         grade == "All Grades",
         district == "DENVER COUNTY 1") %>% 
  select(district, school, subject, n_tested, c_metexc) %>%
  mutate(metexc = c_metexc/n_tested) %>% na.omit()
levels(cmas$subject) <- c("ela","math")
cmas_s <- cmas %>% 
  group_by(district, subject) %>% 
  summarize(m_metexc = mean(metexc),
            sd_metexc = sd(metexc))
memb_cmas <- cmas %>% 
  filter(str_detect(school, "DOWNTOWN DENVER") == TRUE|
         str_detect(school, "DSST")==TRUE)
co_stan_cmas <- merge(memb_cmas, cmas_s, by="subject")
co_stan_cmas <- co_stan_cmas %>% 
  mutate(comp_metexc = (metexc - m_metexc)/sd_metexc) %>% 
  select(school, subject, n_tested, metexc, m_metexc,comp_metexc) %>% 
  arrange(subject, school) 
write.csv(co_stan_cmas, file = file.path("output_data/co_stan_cmas.csv"), row.names = FALSE)
