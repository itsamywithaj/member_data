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

# --- Enrollment data ----
# zip file of data imported from https://data.nysed.gov/files/enrollment/18-19/enrollment_2019.zip
# mdb pile was a pain in the ass -- thx 4 nothing, Gates
# open in Access and save as Excel

nys_enrl <- read_excel("raw_data/NYS_enrollment_2019/Demographic Factors.xlsx")
names(nys_enrl) <- tolower(names(nys_enrl))
str(nys_enrl)
nys_enrl <- nys_enrl %>% 
  filter(year == "2019")

boces <- read_excel("raw_data/NYS_enrollment_2019/BOCES and N_RC.xlsx")
# boces = board of cooperative educational services
memb_boces <- boces %>% 
  filter(BOCES_NAME == "BOCES MONROE 1"|
           BOCES_NAME == "BOCES ERIE 1",
         YEAR == "2019")
names(memb_boces) <- tolower(names(memb_boces))
str(memb_boces)
memb_boces <- memb_boces %>% 
  mutate(entity_cd = as.factor(entity_cd),
         boces_name = as.factor(boces_name)) %>% 
  select(year,entity_cd, school_name,boces_name)
nys_enrl <- merge(memb_boces,nys_enrl,by=c("year","entity_cd"))
nys_enrl <- nys_enrl %>% 
  mutate(district = boces_name,
         school = school_name,
         total = num_am_ind + num_black + num_asian + num_hisp + num_white + num_multi,
         amind = num_am_ind/total,
         asian = num_asian/total,
         black = num_black/total,
         hispanic = num_hisp/total,
         white = num_white/total,
         multiple = num_multi/total,
         econd = num_ecdis/total,
         ell = num_ell/total,
         swd = num_swd/total) %>% 
  select(district, school, total, amind, asian, black, hispanic, white, multiple, econd, ell, swd)
comp_enrl <- nys_enrl %>% 
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
            sd_white = sd(white),
            m_multiple = mean(multiple),
            sd_multiple = sd(multiple),
            m_econ = mean(econd),
            sd_econ = sd(econd),
            m_ell = mean(ell),
            sd_ell = sd(ell),
            m_swd = mean(swd),
            sd_swd = sd(swd))
memb_enrl <- nys_enrl %>% 
  filter(school == "GENESEE COMMUNITY CHARTER SCHOOL"|
           school == "ELMWOOD VILLAGE CHARTER SCHOOL"|
           school == "ELMWOOD VILLAGE CHARTER - HERTEL")
nys_stan_enrl <- merge(memb_enrl,comp_enrl,by="district")
nys_stan_enrl <- nys_stan_enrl %>% 
  mutate(comp_amind = (amind - m_amind)/sd_amind,
         comp_asian = (asian - m_asian)/sd_asian,
         comp_black = (black - m_black)/sd_black,
         comp_hispanic = (hispanic - m_hispanic)/sd_hispanic,
         comp_white = (white - m_white)/sd_white,
         comp_multiple = (multiple - m_multiple)/sd_multiple,
         comp_econ = (econd - m_econ)/sd_econ,
         comp_ell = (ell - m_ell)/sd_ell,
         comp_swd = (swd - m_swd)/sd_swd) %>% 
  select(district, school, total, amind, m_amind, comp_amind, asian, m_asian, comp_asian,
         black, m_black, comp_black, hispanic, m_hispanic, comp_hispanic,
         white, m_white, comp_white, multiple, m_multiple, comp_multiple,
         econd, m_econ, comp_econ, ell, m_ell, comp_ell, swd, m_swd, comp_swd)
write.csv(nys_stan_enrl,file = file.path("output_data/nys_stan_enrl.csv"),row.names = FALSE)

# --- academic data ----
nys <- read_excel("raw_data/NYS_3-8-2018-19/3-8_ELA_AND_MATH_RESEARCHER_FILE_2019.xlsx")
names(nys) <- tolower(names(nys))
names(nys)[7] <- "school_name"
nys$school_name <- as.factor(nys$school_name)
levels(nys$school_name)[968:969] <- c("ELMWOOD VILLAGE CHARTER SCHOOL","ELMWOOD VILLAGE CHARTER - HERTEL")
# change to match the BOCES naming convention for EVCS
nys_acad <- merge(memb_boces, nys, by="school_name") # filters out rows that don't match a school within comparison BOCES
nys_acad <- nys_acad %>% 
  filter(subgroup_name == "All Students") %>% 
  mutate(n_tested = total_tested,
         c_lev34 = as.numeric(l3_count)+as.numeric(l4_count),
         p_lev34 = as.numeric(`l3-l4_pct`),
         subject = as.factor(item_subject_area)) %>% 
  select(school_name, boces_name, subject, n_tested, #remove item_desc in order to sum up all grades
         c_lev34, p_lev34)
levels(nys_acad$subject) <- c("ela","math")

install.packages("data.table")
library(data.table)
DT <- data.table(nys_acad)
nys_acad <- DT[, lapply(.SD, sum), by=list(school_name, boces_name,subject)]
nys_acad <- nys_acad %>% 
  na.omit() %>% 
  mutate(p_lev34 = c_lev34/n_tested)
acad_stan <- nys_acad %>% 
  group_by(boces_name) %>% 
  summarize(m_lev34 = mean(p_lev34),
            sd_lev34 = sd(p_lev34),
            n = n())
memb_acad <- nys_acad %>% 
  filter(school_name == "GENESEE COMMUNITY CHARTER SCHOOL"|
           str_detect(school_name,"ELMWOOD VILLAGE")==TRUE)
nys_stan_acad <- merge(memb_acad, acad_stan, by="boces_name")
nys_stan_math <-nys_stan_acad %>% 
  filter(subject == "math") %>% 
  mutate(n_math = n_tested,
         p_math = p_lev34,
         comp_math = (p_math - m_lev34)/sd_lev34) %>% 
  select(boces_name,school_name,n_math,p_math, m_lev34,comp_math) %>% 
  arrange(boces_name,school_name)
write.csv(nys_stan_math, file = file.path("output_data/nys_stan_math.csv"),row.names = FALSE)

nys_stan_ela <-nys_stan_acad %>% 
  filter(subject == "ela") %>% 
  mutate(n_ela = n_tested,
         p_ela = p_lev34,
         comp_ela = (p_ela - m_lev34)/sd_lev34) %>% 
  select(boces_name,school_name,n_ela,p_ela,m_lev34,comp_ela) %>% 
  arrange(boces_name,school_name)
write.csv(nys_stan_ela, file = file.path("output_data/nys_stan_ela.csv"),row.names = FALSE)
