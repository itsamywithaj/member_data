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
library(ggplot2)
install.packages("data.table")
library(data.table)

ok_enrl <- read_excel("raw_data/OK_Copy of GG_bySITE_2A_EthGen-FY1920_Public_2019-12-09.xlsx")
names(ok_enrl) <- tolower(ok_enrl[1,])
ok_enrl <- ok_enrl[-1,]
str(ok_enrl)
ok_enrl<-ok_enrl %>% 
  filter(fy=="2020") %>% 
  mutate(school = as.factor(`school site`),
         total = as.numeric(total),
         c_amind = as.numeric(`american indian male (non-hispanic)`)+as.numeric(`american indian female (non-hispanic)`),
         c_asian = as.numeric(`asian male (non-hispanic)`)+as.numeric(`asian female (non-hispanic)`)+
           as.numeric(`hawaiian or pacific islander male (non-hispanic)`)+as.numeric(`hawaiian or pacific islander female (non-hispanic)`),
         c_black = as.numeric(`black male (non-hispanic)`)+as.numeric(`black female (non-hispanic)`),
         c_hispanic = as.numeric(`hispanic female`)+as.numeric(`hispanic male`),
         c_white = as.numeric(`white male  (non-hispanic)`)+as.numeric(`white female  (non-hispanic)`),
         c_multiple = as.numeric(`two or more races male (non-hispanic)`)+as.numeric(`two or more races female (non-hispanic)`)) %>% 
  select(district, school, total, c_amind, c_asian, c_black, c_hispanic, c_white, c_multiple)
DT <- data.table(ok_enrl) # sum across all grades
ok_enrl <- DT[, lapply(.SD, sum), by=list(district,school)]
ok_enrl <- ok_enrl %>% 
  mutate(amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total) %>% 
  select(district, school, total, amind, asian, black, hispanic, white, multiple) %>% 
  filter(district == "TULSA")
ok_memb <- ok_enrl %>% 
  filter(str_detect(school,"TULSA SCHL")==TRUE)
ok_enrl <- ok_enrl %>% 
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
ok_enrl <- cbind(ok_memb, ok_enrl)
ok_enrl <- ok_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple) %>% 
  select(district, school, total, amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple)
write.csv(ok_enrl, file = file.path("output_data/ok_stan_enrl.csv"), row.names = FALSE)

ok_frpl <- read_excel("raw_data/OK_Updated FY 20 Low Income Report (Redacted-unlocked) (1)_1.xlsx")
names(ok_frpl)[c(1:3,8)]<- c("county","district","school","c_frpl")
str(ok_frpl)
ok_frpl <- ok_frpl[,-1]
ok_frpl <- ok_frpl %>% 
  mutate(c_frpl = as.numeric(c_frpl)) %>% 
  mutate_if(is.numeric,replace_na, replace =0) %>%
  filter(school!="District On-Site Total"&
           school!="District Total") %>% 
  select(school, c_frpl, Enrollment) %>% 
  mutate(frpl = c_frpl/Enrollment)
ok_frpl <- ok_frpl[c(which(ok_frpl$school == "103-ACADEMY CENTRAL ES"):which(ok_frpl$school == "750-TRAICE"),
                     which(str_detect(ok_frpl$school,"TULSA SCHL") == TRUE)),]
ok_memb <- ok_frpl %>% 
  filter(str_detect(school, "TULSA SCHL")==TRUE)
ok_frpl <- ok_frpl %>% 
  filter(str_detect(school, "TULSA SCHL")==FALSE)
ok_frpl <- ok_frpl %>% 
  summarize(m_frpl = mean(frpl),
            sd_frpl = sd(frpl))
ok_frpl <- merge(ok_memb,ok_frpl)
ok_frpl <- ok_frpl %>% 
  mutate(comp_frpl = (frpl - m_frpl)/sd_frpl) %>% 
  select(school, frpl, m_frpl, comp_frpl)
write.csv(ok_frpl,file = file.path("output_data/ok_stan_frpl.csv"),row.names = FALSE)

#---- el, swd, academics -----
# data from https://escmatrix.com/ok/#matrix
ok<-read.csv("raw_data/OK_Matrix Data.csv")
str(ok)
ok <- ok %>% 
  filter(year == 2019,
         district_name == "Tulsa"|
           str_detect(school_name,"Tulsa School")==TRUE) %>%
  select(district_name, school_name,p_ell,p_sped,p_perf_lvl_ela_adv,p_perf_lvl_ela_prof,
         p_perf_lvl_math_adv,p_perf_lvl_math_prof) %>% 
  mutate_if(is.numeric,replace_na, replace = 0) %>% 
  mutate(p_ela = p_perf_lvl_ela_prof+p_perf_lvl_ela_adv,
         p_math = p_perf_lvl_math_prof+p_perf_lvl_math_adv) %>% 
  select(district_name,school_name, p_ell, p_sped, p_math,p_ela)
ok_m <- ok %>% 
  group_by(district_name) %>% 
  summarize(m_ell = mean(p_ell),
            sd_ell = sd(p_ell),
            m_sped = mean(p_sped),
            sd_sped = sd(p_sped),
            m_ela = mean(p_ela),
            sd_ela = sd(p_ela),
            m_math = mean(p_math),
            sd_math = sd(p_math)) %>% 
  filter(district_name == "Tulsa")
ok<- ok %>% 
  filter(str_detect(school_name,"Tulsa Sch")==TRUE) %>% 
  select(school_name,p_ell,p_sped,p_math,p_ela)
ok <- merge(ok, ok_m)
ok <- ok %>% 
  mutate(comp_ell = (p_ell-m_ell)/sd_ell,
         comp_sped = (p_sped - m_sped)/sd_sped,
         comp_math = (p_math - m_math)/sd_math,
         comp_ela = (p_ela - m_ela)/sd_ela) %>% 
  select(school_name,p_ell,m_ell,comp_ell,p_sped,m_sped,comp_sped,
         p_math,m_math,comp_math,p_ela,m_ela,comp_ela)
write.csv(ok,file = file.path("output_data/ok_stan_elswdacad.csv"),row.names=FALSE)
