rm(list = ls()) # clear working environment
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)

#-------  Enrollment --------
tn_enrl <- read_excel("raw_data/TN_201819_membership.xlsx") 
# dataset downloaded from https://www.tn.gov/content/dam/tn/education/data/201819_membership.xlsx
names(tn_enrl) <- tolower(names(tn_enrl))
names(tn_enrl) <- c("dist_num","district","school_num","school","grade","race","gender","count")
summary(tn_enrl)
tn_enrl$district <- as.factor(tn_enrl$district)
tn_enrl$school <- as.factor(tn_enrl$school)
tn_enrl$grade <- as.factor(tn_enrl$grade)
tn_enrl$race <- as.factor(tn_enrl$race)
tn_enrl$gender <- as.factor(tn_enrl$gender)
# pare down to just Nashville, all grades
tn_enrl <- tn_enrl %>% 
  filter(district == "Metro Nashville Public Schools",
         grade == "All Grades") %>% 
  select(district, school, race, gender, count) %>% 
  arrange(district, school)
tn_enrl <- tn_enrl %>% 
  distinct(school, district, gender, race, .keep_all = TRUE) %>% 
  spread(key = race,
         value = count,
         fill = 0) %>% 
  distinct(school, district, gender, .keep_all = TRUE) %>% 
  spread(key = gender,
         value = `All Race/Ethnic Groups`,
         fill = 0)
names(tn_enrl)[3:10] <- c("count_amind","count_asian","count_black","count_hispanic","count_multiple","count_hawpi","count_white","total") 
# recode Hawaii and PI to match other members
tn_enrl<-tn_enrl %>% 
  mutate(count_asian = count_asian + count_hawpi,
         amind = count_amind/total,
         asian = count_asian/total,
         black = count_black/total,
         hispanic = count_hispanic/total,
         white = count_white/total,
         multiple = count_multiple/total) %>%
  select(district, school, total, amind, asian, black, hispanic, white, multiple) %>% 
  filter(total>0)
tn_stan_enroll <- tn_enrl %>% 
  filter(school != "Valor Flagship Academy" &
           school != "Valor Voyager Academy")
tn_stan_enroll <- tn_stan_enroll %>% 
  summarize(mean_amind = mean(amind),
            mean_asian = mean(asian),
            mean_black = mean(black),
            mean_hispanic = mean(hispanic),
            mean_white = mean(white),
            mean_multiple = mean(multiple),
            sd_amind = sd(amind),
            sd_asian = sd(asian),
            sd_black = sd(black),
            sd_hispanic = sd(hispanic),
            sd_white = sd(white),
            sd_multiple = sd(multiple)) %>% 
  mutate(district = "Metro Nashville Public Schools") %>% 
  select(district, mean_amind,sd_amind, mean_asian, sd_asian, mean_black, sd_black, mean_hispanic, sd_hispanic,
         mean_white, sd_white, mean_multiple, sd_multiple)
valor <- tn_enrl %>% 
  filter(school == "Valor Flagship Academy"|
           school == "Valor Voyager Academy")
valor <- merge(valor, tn_stan_enroll, by="district")
valor <- valor %>% 
  mutate(comp_amind = (amind - mean_amind)/sd_amind,
         comp_asian = (asian - mean_asian)/sd_asian,
         comp_black = (black - mean_black)/sd_black,
         comp_hispanic = (hispanic - mean_hispanic)/sd_hispanic,
         comp_white = (white - mean_white)/sd_white,
         comp_multiple = (multiple - mean_multiple)/sd_multiple) %>% 
  select(school,total, amind, mean_amind, comp_amind,
         asian, mean_asian, comp_asian,
         black, mean_black, comp_black,
         hispanic, mean_hispanic, comp_hispanic,
         white, mean_white, comp_white,
         multiple, mean_multiple, comp_multiple)

write.csv(valor, file = file.path('output_data/tn_stan_enroll.csv'), row.names = FALSE)


#-------  ELL --------
tn_ell <- read.csv("raw_data/TN_wida_growth_standard_school_suppressed.csv") 
# dataset downloaded from https://www.tn.gov/content/dam/tn/education/accountability/2019/wida_growth_standard_school_suppressed.csv
summary(tn_ell)
names(tn_ell)[1:6] <- c("sys_numb","system","sch_numb","school","subgroup","count")
tn_ell <- tn_ell %>% 
  filter(subgroup=="All Students") %>% 
  select(sys_numb,sch_numb, school, count) %>% 
  arrange(sys_numb, sch_numb)
district_ell <- tn_enrl %>% 
  select(school, total) # list of schools and total enrollment in Metro Nashville PS District
merger <- tn_ell[tn_ell$school %in% district_ell$school ,]
district_ell <- merge(district_ell, merger)
district_ell <- district_ell %>% 
  select(school, total, count) %>% 
  mutate(perc_ell = count/total)

dist_ell <- district_ell %>% 
  filter(school != "Valor Flagship Academy"&
           school!="Valor Voyager Academy")
tn_stan_ell <- district_ell %>% 
  summarize(mean_ell = mean(perc_ell),
            sd_ell = sd(perc_ell))
valor_ell <- district_ell %>% 
  filter(school == "Valor Flagship Academy"|
           school == "Valor Voyager Academy") %>% 
  mutate(mean_ell = tn_stan_ell$mean_ell,
         sd_ell = tn_stan_ell$sd_ell,
         comp_ell = (perc_ell - mean_ell)/sd_ell) %>% 
  select(school, total, perc_ell, mean_ell, comp_ell)

write.csv(valor_ell, file = file.path("output_data/tn_stan_ell.csv"), row.names = FALSE)

#------- FRPL and SWD ------
tn_subgroups <- read_excel("raw_data/TN_schoolprofile201819 .xlsx")
View(tn_subgroups)
names(tn_subgroups) <- tolower(names(tn_subgroups))
names(tn_subgroups)[c(15,17,19)] <- c("econ_dis_pct","lep_pct","swd_pct")
tn_subgroups <- tn_subgroups %>% 
  select(district_name, school_name, total, econ_dis_pct,lep_pct,swd_pct,african_american_pct,asian_pct,hawaiian_pacisld_pct,
         hispanic_pct,native_american_pct,white_pct)
str(tn_subgroups)
tn_subgroups[c(4:12)] <- tn_subgroups[c(4:12)]*.01 # change percents to a decimal figure
tn_lep <- tn_subgroups %>% 
  filter(district_name == "Metro Nashville Public Schools") %>%  # filter to only comparison districts
  select(school_name, total, lep_pct) %>% 
  filter(school_name!= "Valor Voyager Academy" &
           school_name!= "Valor Flagship Academy")
a<- tn_lep %>% 
  summarize(mean_lep = mean(lep_pct),
            sd_lep = sd(lep_pct))

valor_lep <- tn_subgroups %>% 
  filter(school_name == "Valor Flagship Academy"|
           school_name == "Valor Voyager Academy") %>% 
  mutate(mean_lep = a$mean_lep,
         sd_lep = a$sd_lep,
         comp_lep = (lep_pct - mean_lep)/sd_lep) %>% 
  select(school_name, total, lep_pct, mean_lep, comp_lep)

tn_swd <- tn_subgroups %>% 
  filter(district_name == "Metro Nashville Public Schools") %>%  # filter to only comparison districts
  select(school_name, total, swd_pct) %>% 
  filter(school_name!= "Valor Voyager Academy" &
           school_name!= "Valor Flagship Academy")
a<- tn_swd %>% 
  summarize(mean_swd = mean(swd_pct),
            sd_swd = sd(swd_pct))
valor_swd <- tn_subgroups %>% 
  filter(school_name == "Valor Flagship Academy"|
           school_name == "Valor Voyager Academy") %>% 
  mutate(mean_swd = a$mean_swd,
         sd_swd = a$sd_swd,
         comp_swd = (swd_pct - mean_swd)/sd_swd) %>% 
  select(school_name, total, swd_pct, mean_swd, comp_swd)

write.csv(valor_swd, file = file.path("output_data/tn_stan_swd.csv"))
