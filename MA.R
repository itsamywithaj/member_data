rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("backports")
library(backports)
install.packages(c("tidyverse","readxl","ggplot2"),
                 dependencies = TRUE,
                 repos = c("http://rstudio.org/_packages",
                           "http://cran.rstudio.com"))
library(tidyverse)
library(readxl)

# data from http://profiles.doe.mass.edu/statereport/enrollmentbyracegender.aspx
ma_enrl <- read_excel("raw_data/MA_enrollmentbyracegender.xlsx")
names(ma_enrl) <- tolower(ma_enrl[1,])
ma_enrl <- ma_enrl[-1,]
ma_enrl <- ma_enrl %>% 
  separate(`school name`,c("district","school")," - ",extra = "merge") %>% 
  filter(district == "Boston"|
           district == "Cambridge"|
           school == "Benjamin Banneker Charter Public School"|
           school == "Boston Collegiate Charter School")
str(ma_enrl)
ma_enrl <- ma_enrl %>% 
  mutate(amind = as.numeric(`native american`),
         asian = as.numeric(asian)+as.numeric(`native hawaiian, pacific islander`),
         black = as.numeric(`african american`),
         hispanic = as.numeric(hispanic),
         white = as.numeric(white),
         multiple = as.numeric(`multi-race, non-hispanic`)) %>% 
  select(district, school, amind, asian, black, hispanic, white, multiple)
m_enrl <- ma_enrl %>% 
  group_by(district) %>% 
  filter(district == "Boston"|
           district == "Cambridge") %>% 
  summarize(m_amind = mean(amind),
            sd_amind = sd(amind),
            m_asian = mean(asian),
            sd_asian = sd(asian),
            m_black = mean(black),
            sd_black = sd(black),
            m_hispanic = mean(hispanic),
            sd_hispanic = sd(hispanic),
            m_white = mean(white),
            sd_white = sd(white),
            m_multiple = mean(multiple),
            sd_multiple = sd(multiple),
            n= n())
ma_enrl <- ma_enrl %>% 
  filter(district!= "Boston",
         district!= "Cambridge")
ma_enrl$district <- c("Cambridge","Boston")
ma_stan_enrl <- merge(ma_enrl,m_enrl, by="district")
ma_stan_enrl <- ma_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple) %>% 
  select(district, school, amind, m_amind, comp_amind, asian, m_asian, comp_asian,
         black, m_black, comp_black, hispanic, m_hispanic, comp_hispanic, white,
         m_white, comp_white, multiple, m_multiple, comp_multiple)
write.csv(ma_stan_enrl, file = file.path("output_data/ma_stan_enrl.csv"), row.names = FALSE)

# --- FRPL, SWD, SPED -----
# data from http://profiles.doe.mass.edu/statereport/selectedpopulations.aspx
ma_sub <- read_excel("raw_data/MA_selectedpopulations.xlsx")
names(ma_sub) <- tolower(ma_sub[1,])
names(ma_sub)[16] <- "aaa"
summary(ma_sub)
ma_sub <- ma_sub[-1,]
ma_sub <- ma_sub %>%
  separate(`school name`,c("district","school")," - ",extra = "merge") %>% 
  filter(district == "Boston"|
           district == "Cambridge"|
           school == "Benjamin Banneker Charter Public School"|
           school == "Boston Collegiate Charter School") %>% 
  mutate(frpl = as.numeric(`free lunch %`) + as.numeric(`reduced lunch %`),
         ell = as.numeric(`english language learner %`),
         swd = as.numeric(`students with disabilities %`),
         econ = as.numeric(`economically disadvantaged %`)) %>% 
  select(district, school, econ, ell, swd)
m_sub <- ma_sub %>%
  group_by(district) %>% 
  filter(district == "Boston"|
           district == "Cambridge") %>% 
  summarize(m_econ = mean(econ),
            sd_econ = sd(econ),
            m_ell = mean(ell),
            sd_ell= sd(ell),
            m_swd = mean(swd),
            sd_swd = sd(swd),
            n= n())
ma_sub <- ma_sub %>% 
  filter(district!= "Boston",
         district!= "Cambridge")
ma_sub$district <- c("Cambridge","Boston")
ma_stan_sub <- merge(ma_sub, m_sub,by="district")
ma_stan_sub <- ma_stan_sub %>% 
  mutate(comp_econ = (econ- m_econ)/sd_econ,
         comp_ell = (ell - m_ell)/sd_ell,
         comp_swd = (swd - m_swd)/sd_swd) %>% 
  select(district, school, econ, m_econ, comp_econ,
         ell, m_ell, comp_ell, swd, m_swd, comp_swd)
write.csv(ma_stan_sub, file = file.path("output_data/ma_stan_sub.csv"), row.names = FALSE)

#----- nextgen MCAS -----
mcas <- read_excel("raw_data/MA_NextGenMCAS.xlsx")
names(mcas) <- tolower(mcas[1,])
mcas <- mcas[-1,]
str(mcas)
mcas <- mcas %>% 
  separate(`school name`,c("district","school")," - ",extra = "merge") %>% 
  filter(district == "Boston"|
           district == "Cambridge"|
           school == "Benjamin Banneker Charter Public School"|
           school == "Boston Collegiate Charter School",
         subject == "MATH"|
           subject == "ELA") %>% 
  mutate(me = .01 * as.numeric(`m+e %`),
         n_tested = as.numeric(`no. of students included`)) %>% 
  select(district, school, subject, n_tested, me)

stan_mcas <- mcas %>% 
  group_by(district,subject) %>% 
  summarize(m_me = mean(me),
            sd_me = sd(me),
            n = n()) %>% 
  filter(district == "Boston"|
           district == "Cambridge")
mcas <- mcas %>% 
  filter(district != "Boston",
         district != "Cambridge") %>% 
  select(school, subject, n_tested, me)
mcas$district <- c("Cambridge","Cambridge","Boston","Boston")
ma_stan_acad <- merge(mcas, stan_mcas, by=c("district","subject"))
ma_stan_acad <- ma_stan_acad %>% 
  mutate(comp_me = (me - m_me)/sd_me) %>% 
  select(district, school, subject, n_tested, me, m_me, comp_me)
write.csv(ma_stan_acad, file = file.path('output_data/ma_stan_acad.csv'),row.names = FALSE)
