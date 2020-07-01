rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

#--- Enrollment by race and ethnicity -----
# data imported from https://oraapp.doe.k12.ga.us/ows-bin/owa/fte_pack_ethnicsex_pub.entry_form
ga_enroll <- read.csv("raw_data/GA_FTE Enrollment by Race_Ethnicity and Gender Fiscal Year2020-3 Data Report.csv", header = FALSE)
ga_enroll <- ga_enroll[-c(1:4),]
ga_enroll$V1 <- as.character(ga_enroll$V1)
ga_enroll$V2 <- as.character(ga_enroll$V2)
ga_enroll$V3 <- as.character(ga_enroll$V3)
ga_enroll$V4 <- as.character(ga_enroll$V4)
ga_enroll$V5 <- as.character(ga_enroll$V5)
ga_enroll$V6 <- as.character(ga_enroll$V6)
ga_enroll$V7 <- as.character(ga_enroll$V7)
ga_enroll$V8 <- as.character(ga_enroll$V8)
ga_enroll$V9 <- as.character(ga_enroll$V9)
ga_enroll$V10 <- as.character(ga_enroll$V10)
ga_enroll$V11 <- as.character(ga_enroll$V11)
colnames(ga_enroll) <- ga_enroll[1,]
ga_enroll <- ga_enroll[-1,]
names(ga_enroll) <- c("dist_code","district","school","gender","c_hispanic","c_amind","c_asian","c_black","c_pi","c_white","c_multiple")
ga_enroll <- ga_enroll %>% 
  filter(district == "Atlanta Public Schools"|
           district == "DeKalb County",
         gender == "Total") %>% 
  mutate(c_hispanic = as.numeric(gsub("[^0-9.-]","",c_hispanic)),
         c_amind = as.numeric(gsub("[^0-9.-]","",c_amind)),
         c_asian = as.numeric(gsub("[^0-9.-]","",c_asian)) + as.numeric(gsub("[^0-9.-]","",c_pi)),
         c_black = as.numeric(gsub("[^0-9.-]","",c_black)),
         c_white = as.numeric(gsub("[^0-9.-]","",c_white)),
         c_multiple = as.numeric(gsub("[^0-9.-]","",c_multiple)))
summary(ga_enroll)
ga_enroll[is.na(ga_enroll)] = 0
ga_enroll <- ga_enroll %>% 
  mutate(total_enr = c_hispanic + c_amind + c_asian + c_black + c_white + c_multiple,
         amind = c_amind/total_enr,
         asian = c_asian/total_enr,
         black = c_black/total_enr,
         hispanic = c_hispanic/total_enr,
         white = c_white/total_enr,
         multiple = c_multiple/total_enr)%>% 
  select(dist_code, district, school, total_enr, amind, asian, black, hispanic, white, multiple,
         c_amind, c_asian, c_black, c_hispanic, c_white, c_multiple) %>% 
  na.omit()
ga_stan <- ga_enroll %>% 
  group_by(district) %>% 
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
            n_schools = n())
ga_enrl <- ga_enroll %>% 
  filter(school == "0215-Museum School Avondale Estates"|
           str_detect(school, "Atlanta Neighborhood")==TRUE)
ga_enrl <- merge(ga_enrl,ga_stan, by="district")
ga_enrl <- ga_enrl %>% 
  mutate(comp_amind = (amind - mean_amind)/sd_amind,
         comp_asian = (asian - mean_asian)/sd_asian,
         comp_black = (black - mean_black)/sd_black,
         comp_hispanic = (hispanic - mean_hispanic)/sd_hispanic,
         comp_white = (white - mean_white)/sd_white,
         comp_multiple = (multiple - mean_multiple)/sd_multiple) %>% 
  select(district, school, total_enr, amind, mean_amind, comp_amind, asian, mean_asian, comp_asian,
         black, mean_black, comp_black, hispanic, mean_hispanic, comp_hispanic,
         white, mean_white, comp_white, multiple, mean_multiple, comp_multiple)
write.csv(ga_enrl, file = file.path("output_data/ga_stan_enrl.csv"),row.names = FALSE)

#---- FRPM ----
# frpm data imported from https://oraapp.doe.k12.ga.us/ows-bin/owa/fte_pack_frl001_public.entry_form
ga_frpm <- read.csv("raw_data/GA_Free Reduced Lunch (FRL) Fiscal Year2020 Data Report.csv", header = FALSE)
ga_frpm$V1 <- as.character(ga_frpm$V1)
ga_frpm$V2 <- as.character(ga_frpm$V2)
ga_frpm$V3 <- as.character(ga_frpm$V3)
ga_frpm$V4 <- as.character(ga_frpm$V4)
ga_frpm <- ga_frpm[-c(1:5),]
names(ga_frpm) <- c("dist_code","district","school","perc_frpm")
ga_frpm <- ga_frpm %>% 
  mutate(perc_frpm = .01* as.numeric(perc_frpm)) %>%
  na.omit() %>% 
  filter(district == "Atlanta Public Schools"|
           district == "DeKalb County")
ga_stan_frpm <- ga_frpm %>% 
  group_by(district) %>% 
  summarize(mean_frpm = mean(perc_frpm),
            sd_frpm = sd(perc_frpm))
ga_memb <- ga_frpm %>% 
  filter(str_detect(school, "Museum School")==TRUE|
           str_detect(school, "Atlanta Neighborhood")==TRUE)
ga_stan_frpm <- merge(ga_memb,ga_stan_frpm, by="district")
ga_stan_frpm <- ga_stan_frpm %>% 
  mutate(comp_frpm = (perc_frpm - mean_frpm)/sd_frpm) %>% 
  select(district, school, perc_frpm, mean_frpm, comp_frpm) %>% 
  arrange(school)
summary(ga_frpm$perc_frpm)
write.csv(ga_stan_frpm,file = file.path("output_data/ga_stan_frpm.csv"),row.names = FALSE)

#---- lep and swd -----
# data from https://download.gosa.ga.gov/2019/ELL_EXIT_RATES_2019_DEC3rd_2019.csv
ga_sub <- read.csv("raw_data/GA/Enrollment_by_Subgroups_Programs_2019_Dec2nd_2019.csv")
names(ga_sub) <- tolower(names(ga_sub))
ga_sub <- ga_sub %>% 
  select(instn_name,school_dstrct_nm, enroll_percent_lep, enroll_percent_swd) %>% 
  filter(school_dstrct_nm == "Atlanta Public Schools"|
           school_dstrct_nm == "DeKalb County") %>% 
  mutate(lep = .01 * as.numeric(enroll_percent_lep),
         swd = .01 * enroll_percent_swd) %>% 
  select(instn_name,school_dstrct_nm,lep, swd)
ga_memb <- ga_sub %>% 
  filter(str_detect(instn_name,"Atlanta Neighborhood")==TRUE|
         str_detect(instn_name,"Museum")==TRUE)
ga_sub <- ga_sub %>% 
  group_by(school_dstrct_nm) %>% 
  summarize(n_schools = n(),
            mean_lep = mean(lep),
            sd_lep = sd(lep),
            mean_swd = mean(swd),
            sd_swd = sd(swd))
ga_memb_sub <- merge(ga_memb, ga_sub, by="school_dstrct_nm")
ga_memb_sub <- ga_memb_sub %>% 
  mutate(comp_lep = (lep - mean_lep)/sd_lep,
         comp_swd = (swd - mean_swd)/sd_swd) %>% 
  select(school_dstrct_nm,instn_name,lep, mean_lep, comp_lep,
         swd, mean_swd, comp_swd)
write.csv(ga_memb_sub, file = file.path("output_data/ga_stan_swdlep.csv"), row.names = FALSE)

#----- math and ela performance ----
ga_acadk8 <- read.csv("raw_data/GA_EOG_2019_By_Grad_FEB_24_2020.csv")
summary(ga_acadk8)
names(ga_acadk8) <- tolower(names(ga_acadk8))
names(ga_acadk8)[c(3,5,6,7,8,9,12,13,16,17)] <- c("district","school","grade","subgroup","subject","n_tested",
                                                  "c_prof","c_ding","p_prof","p_ding")
levels(ga_acadk8$subject)[1:2] <- c("ela","math")
ga_acadk8 <- ga_acadk8 %>% 
  filter(subject == "ela"|
           subject == "math",
         district == "Atlanta Public Schools"|
           district == "DeKalb County",
         subgroup == "All Students") %>% 
  select(district, school, grade, subgroup, subject, n_tested, c_prof, c_ding, p_prof, p_ding)
DT <- data.table(ga_acadk8)
ga_allgrades <- DT[, lapply(.SD, sum), by=list(school, district, subject, subgroup)]
ga_allgrades <- ga_allgrades %>% 
  mutate(p_prof = c_prof/n_tested,
         p_ding = c_ding/n_tested,
         perc_pd = (c_prof + c_ding)/n_tested) %>% # percent of students deemed "proficient" + "distinguished"
  select(district, school, subject, n_tested, perc_pd)

ga_math_stan <- ga_allgrades %>% 
  filter(subject == "math") %>% 
  na.omit() %>% 
  group_by(district) %>% 
  summarize(mean_math = mean(perc_pd),
            sd_math = sd(perc_pd),
            n_schools = n())
ga_memb_math <- ga_allgrades %>% 
  filter(subject == "math",
         str_detect(school, "Atlanta Neighborhood")==TRUE|
           str_detect(school, "Museum")==TRUE)
ga_math_stan <- merge(ga_memb_math, ga_math_stan, by="district")
ga_math_stan <- ga_math_stan %>% 
  mutate(comp_math = (perc_pd - mean_math)/sd_math) %>% 
  select(district, school, subject, n_tested, perc_pd, mean_math, comp_math)
names(ga_math_stan)[4:5] <- c("n_math","perc_math")

ga_ela_stan <- ga_allgrades %>% 
  filter(subject == "ela") %>% 
  na.omit() %>% 
  group_by(district) %>% 
  summarize(mean_ela = mean(perc_pd),
            sd_ela = sd(perc_pd),
            n_schools = n())
ga_memb_ela <- ga_allgrades %>% 
  filter(subject == "ela",
         str_detect(school, "Atlanta Neighborhood")==TRUE|
           str_detect(school, "Museum")==TRUE)
ga_ela_stan <- merge(ga_memb_ela, ga_ela_stan, by="district")
ga_ela_stan <- ga_ela_stan %>% 
  mutate(comp_ela = (perc_pd - mean_ela)/sd_ela) %>% 
  select(district, school, subject, n_tested, perc_pd, mean_ela, comp_ela)
names(ga_ela_stan)[4:5] <- c("n_ela","perc_ela")

ga_stan_acad <- merge(ga_math_stan,ga_ela_stan,by=c("district","school"))
ga_stan_acad <- ga_stan_acad %>% 
  select(district, school, n_math, perc_math, mean_math, comp_math, n_ela, perc_ela, mean_ela, comp_ela)
write.csv(ga_stan_acad, file = file.path("output_data/ga_stan_acad.csv"),row.names = FALSE)
