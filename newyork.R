rm(list = ls())
library(knitr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("stringr")
library(stringr)
getwd()
setwd("/Users/amyjiravisitcul/Downloads/States/")
# ------ NEW YORK CITY
nyc_enroll <- read_excel("raw_data/NYC_enrollment_2014-2019.xlsx", sheet = "District") # Read NYC's demographic snapshot
names(nyc_enroll) <- tolower(names(nyc_enroll))
names(nyc_enroll)[1] <- "admin_dist"
nyc_enroll$admin_dist <- as.numeric(nyc_enroll$admin_dist)
str(nyc_enroll)
nyc_enroll <- nyc_enroll %>% 
  filter(year == "2018-19",
         (admin_dist > 1 & admin_dist < 10)|
           (admin_dist > 12 & admin_dist < 18)|
           (admin_dist > 20 & admin_dist < 33 & admin_dist != 23 &
              admin_dist != 25 & admin_dist != 26 & admin_dist != 28 & admin_dist != 31))
# ----- NYC districts include members in 2-9, 13-17, 21, 22, 24, 27, 29, 30, 32
View(nyc_enroll)
names(nyc_enroll)[19:38] <- c("perc_f", "count_m", "perc_m","count_asian","perc_asian","count_black","perc_black",
                              "count_hisp","perc_hisp","count_mult","perc_mult","count_white","perc_white",
                              "count_swd","perc_swd","count_ell","perc_ell", "count_poverty","perc_poverty","econ_need")
nyc_enroll_dist <- nyc_enroll %>% 
  select(names(nyc_enroll)[c(1:3,18:38)])
nyc_enroll <- read_excel("raw_data/NYC_enrollment_2014-2019.xlsx", sheet = "School")
names(nyc_enroll) <- tolower(names(nyc_enroll))
nyc_enroll_sch <- nyc_enroll %>% 
  filter(year == "2018-19") %>% 
  separate(dbn, c("dist", "other"),
                  sep = 2)
nyc_enroll_sch %>% 
  filter(dist == "24") %>% 
  View() # confirming that the 1st two characters of "dbn" are, in fact, administrative districts
names(nyc_enroll_sch)[20:40] <- c("count_f","perc_f", "count_m", "perc_m","count_asian","perc_asian",
                                  "count_black","perc_black","count_hisp","perc_hisp","count_mult",
                                  "perc_mult","count_white","perc_white", "count_swd","perc_swd",
                                  "count_ell","perc_ell", "count_poverty","perc_poverty","econ_need")
summary(as.factor(nyc_enroll_sch$dist))
nyc_enroll_sch$dist <- as.numeric(nyc_enroll_sch$dist)
nyc_enroll_summary <- nyc_enroll_sch %>%
  group_by(dist) %>% 
  summarize(n = n(),
            avg_f = mean(perc_f),
          sd_f = sd(perc_f),
          avg_asian = mean(perc_asian),
          sd_asian = sd(perc_asian),
          avg_black = mean(perc_black),
          sd_black = sd(perc_black),
          avg_hisp = mean(perc_hisp),
          sd_hisp = sd(perc_hisp),
          avg_white = mean(perc_white),
          sd_white = sd(perc_white),
          avg_mult = mean(perc_mult),
          sd_mult = sd(perc_mult),
          avg_swd = mean(perc_swd),
          sd_swd = sd(perc_swd),
          avg_ell = mean(perc_ell),
          sd_ell = sd(perc_ell),
          avg_econ = mean(econ_need),
          sd_econ = sd(econ_need)) %>% 
  filter((dist > 1 & dist < 10)|
  (dist > 12 & dist < 18)|
  (dist > 20 & dist < 33 & dist != 23 &
     dist != 25 & dist != 26 & dist != 28 & dist != 31))
# NYC districts include members in 2-9, 13-17, 21, 22, 24, 27, 29, 30, 32
write.csv(nyc_enroll_summary, file = file.path('nyc_comp_enroll.csv'),row.names = FALSE)
nyc_enroll_sch %>% 
  filter(dist == 84) %>% 
  View() # District 84 includes all the NYC charter schools
names(nyc_enroll_sch)[3] <- "school"
nyc_enroll_memb <- nyc_enroll_sch %>% # select the member schools
  filter(school == "Academy of the City Charter School"|
           school == "Brooklyn Prospect Charter School"|
           school == "Brooklyn Prospect Charter School Downtown"|
           school == "Central Queens Academy Charter School"|
           school == "Community Roots Charter School"|
           school == "Compass Charter School"|
           school == "Hebrew Language Academy Charter School"|
           school == "Hebrew Language Academy Charter School 2"|
           school == "Harlem Hebrew Language Charter School"|
           school == "Hellenic Classical Charter School"|
           school == "The International Charter School of New York"|
           school == "New York French American Charter School"|
           school == "Brooklyn Urban Garden Charter School"|
           str_detect(school, "Success Academy") == TRUE) %>% 
  arrange(school)
nyc_enroll_memb$school
nyc_enroll_memb$prox_dist <- c(30,13,15,15,24,13,13,3,22,21,
                               15,30,14,14,21,22,7,9,9,8,
                               32,15,27,17,3,5,4,3,5,5,
                               2,29,13,29,3,6,14,17,13,2,
                               17,2,13) # district values for comparison
names(nyc_enroll_memb)
nyc_enroll_memb <- nyc_enroll_memb %>% 
  arrange(prox_dist, school) %>% 
  select(prox_dist,school,year, `total enrollment`, perc_f,
         perc_asian, perc_black, perc_hisp, perc_white, perc_mult,
         econ_need, perc_ell, perc_swd)
View(nyc_enroll_memb)
nyc_enroll_summary$prox_dist <- nyc_enroll_summary$dist
tmp_data <- merge(nyc_enroll_memb,nyc_enroll_summary, by="prox_dist")
tmp_data <- tmp_data %>% 
  mutate(std_f = (perc_f - avg_f)/sd_f,
         std_asian = (perc_asian - avg_asian)/sd_asian,
         std_black = (perc_black - avg_black)/sd_black,
         std_hisp = (perc_hisp - avg_hisp)/sd_hisp,
         std_white = (perc_white - avg_white)/sd_white,
         std_mult = (perc_mult - avg_mult)/sd_mult,
         std_econ = (econ_need - avg_econ)/sd_econ,
         std_ell = (perc_ell - avg_ell)/sd_ell,
         std_swd = (perc_swd - avg_swd)/sd_swd) %>% 
  select(prox_dist, school, year, `total enrollment`, perc_f, avg_f, std_f,
         perc_asian, avg_asian, std_asian, perc_black, avg_black, std_black,
         perc_hisp, avg_hisp, std_hisp, perc_white, avg_white, std_white,
         perc_mult, avg_mult, std_mult, econ_need, avg_econ, std_econ,
         perc_ell, avg_ell, std_ell, perc_swd, avg_swd, std_swd)
View(tmp_data)
write.csv(tmp_data, file = file.path('nyc_stan_enroll.csv'),row.names = FALSE)
#----------------- NYC Math
nyc_math_all <- read_excel("raw_data/NYC_math_2013-2019.xlsx", sheet = "All")
names(nyc_math_all) <- tolower(names(nyc_math_all))
nyc_math_all <- nyc_math_all %>% 
  separate(dbn, c("dist", "other"),
           sep = 2) %>% 
  filter(year == 2019,
         grade == "All Grades")
nyc_math_all$dist <- as.numeric(nyc_math_all$dist)
nyc_math_all$`% level 3+4` <- as.numeric(nyc_math_all$`% level 3+4`)
nyc_math_all$`mean scale score` <- as.numeric(nyc_math_all$`mean scale score`)
nyc_math_all <- nyc_math_all %>%
  filter((dist > 1 & dist < 10)|
  (dist > 12 & dist < 18)|
  (dist > 20 & dist < 33 & dist != 23 &
     dist != 25 & dist != 26 & dist != 28 & dist != 31))
nyc_math_stan <- nyc_math_all %>% # NYC comparison district math proficiency among all students
  group_by(dist) %>% 
  summarize(n_schools = n(),
            avg_tested = mean(`number tested`),
            sd_tested = sd(`number tested`),
            avg_score = mean(`mean scale score`),
            sd_score = sd(`mean scale score`),
            avg_perc_lev34 = mean(`% level 3+4`),
            sd_perc_lev34 = sd(`% level 3+4`))
write.csv(nyc_math_stan, file = file.path('nyc_stan_math.csv'),row.names = FALSE)

nyc_math_memb <- read_excel("raw_data/NYC_charter_2013-2019.xlsx", sheet = "Math")
names(nyc_math_memb) <- tolower(names(nyc_math_memb))
nyc_math_memb$category <- as.factor(nyc_math_memb$category)
str(nyc_math_memb$category) # Factor w/ 1 level "All Students"
names(nyc_math_memb)[3] <- "school"
nyc_math_memb$school <- as.factor(nyc_math_memb$school)
nyc_math_memb$school <- str_to_title(nyc_math_memb$school, locale = "en") # Match title case for filter
nyc_math_memb <- nyc_math_memb %>%  # filter to DCSC members
  filter(grade == "All Grades",
         year == "2019",
         school == "Academy Of The City Charter School"|
           school == "Brooklyn Prospect Charter School"|
           school == "Brooklyn Prospect Charter School Downtown"|
           school == "Central Queens Academy Charter School"|
           school == "Community Roots Charter School"|
           school == "Compass Charter School"|
           school == "Hebrew Language Academy Charter School"|

           school == "Harlem Hebrew Language Charter School"|
           school == "Hellenic Classical Charter School"|
           school == "The International Charter School Of New York"|
           school == "New York French American Charter School"|
           school == "Brooklyn Urban Garden Charter School"|
           str_detect(school, "Success Academy") == TRUE) %>% 
  arrange(school)
nyc_math_memb$school
nyc_math_memb$prox_dist <- c(30,13,15,15,24,13,13,3,22,15,
                             30,14,14,21,22,7,9,9,8,32,
                             15,27,17,3,5,4,3,5,29,27,
                             29,3,6,14,17,13,2,17,2,13) # district values for comparison
nyc_math_memb <- nyc_math_memb %>% 
  arrange(prox_dist, school) %>% 
  select(prox_dist, school, year, grade,
         category, `number tested`,`mean scale score`,`% level 3+4`)
names(nyc_math_memb)[1] <- "dist"
tmp_data <- merge(nyc_math_memb,nyc_math_stan, by="dist")
names(tmp_data)
tmp_data$`mean scale score` <- as.numeric(tmp_data$`mean scale score`)
tmp_data$`% level 3+4` <- as.numeric(tmp_data$`% level 3+4`)
tmp_data <- tmp_data %>% 
  mutate(std_n = (`number tested` - avg_tested)/sd_tested,
         std_score = (`mean scale score` - avg_score)/sd_score,
         std_perc_lv34 = (`% level 3+4` - avg_perc_lev34)/sd_perc_lev34) %>% 
  select(dist, school, year, `number tested`, avg_tested, std_n,
         `mean scale score`, avg_score, std_score,
         `% level 3+4`, avg_perc_lev34,std_perc_lv34)
write.csv(tmp_data, file.path('nyc_stan_math.csv'), row.names = FALSE)
#----------------- NYC ELA
nyc_ela_all <- read_excel("raw_data/NYC_ELA_2013-2019.xlsx", sheet = "All")
names(nyc_ela_all) <- tolower(names(nyc_ela_all))
nyc_ela_all <- nyc_ela_all %>% 
  separate(dbn, c("dist", "other"),
           sep = 2) %>% 
  filter(year == 2019,
         grade == "All Grades")
nyc_ela_all$dist <- as.numeric(nyc_ela_all$dist)
nyc_ela_all$`% level 3+4` <- as.numeric(nyc_ela_all$`% level 3+4`)
nyc_ela_all$`mean scale score` <- as.numeric(nyc_ela_all$`mean scale score`)
nyc_ela_all <- nyc_ela_all %>%
  filter((dist > 1 & dist < 10)|
           (dist > 12 & dist < 18)|
           (dist > 20 & dist < 33 & dist != 23 &
              dist != 25 & dist != 26 & dist != 28 & dist != 31))
nyc_ela_stan <- nyc_ela_all %>% # NYC comparison district math proficiency among all students
  group_by(dist) %>% 
  summarize(n_schools = n(),
            avg_tested = mean(`number tested`),
            sd_tested = sd(`number tested`),
            avg_score = mean(`mean scale score`),
            sd_score = sd(`mean scale score`),
            avg_perc_lev34 = mean(`% level 3+4`),
            sd_perc_lev34 = sd(`% level 3+4`))
write.csv(nyc_ela_stan, file = file.path('nyc_stan_ela.csv'),row.names = FALSE)

nyc_ela_memb <- read_excel("raw_data/NYC_charter_2013-2019.xlsx", sheet = "ELA")
names(nyc_ela_memb) <- tolower(names(nyc_ela_memb))
nyc_ela_memb$category <- as.factor(nyc_ela_memb$category)
str(nyc_ela_memb$category) # Factor w/ 1 level "All Students"
names(nyc_ela_memb)[3] <- "school"
nyc_ela_memb$school <- as.factor(nyc_ela_memb$school)
nyc_ela_memb$school <- str_to_title(nyc_ela_memb$school, locale = "en") # Match title case for filter
nyc_ela_memb <- nyc_ela_memb %>%  # filter to DCSC members
  filter(grade == "All Grades",
         year == "2019",
         school == "Academy Of The City Charter School"|
           school == "Brooklyn Prospect Charter School"|
           school == "Brooklyn Prospect Charter School Downtown"|
           school == "Central Queens Academy Charter School"|
           school == "Community Roots Charter School"|
           school == "Compass Charter School"|
           school == "Hebrew Language Academy Charter School"|
           
           school == "Harlem Hebrew Language Charter School"|
           school == "Hellenic Classical Charter School"|
           school == "The International Charter School Of New York"|
           school == "New York French American Charter School"|
           school == "Brooklyn Urban Garden Charter School"|
           str_detect(school, "Success Academy") == TRUE) %>% 
  arrange(school)
nyc_ela_memb$school
nyc_ela_memb$prox_dist <- c(30,13,15,15,24,13,13,3,22,15,
                             30,14,14,21,22,7,9,9,8,32,
                             15,27,17,3,5,4,3,5,29,27,
                             29,3,6,14,17,13,2,17,2,13) # district values for comparison
nyc_ela_memb <- nyc_ela_memb %>% 
  arrange(prox_dist, school) %>% 
  select(prox_dist, school, year, grade,
         category, `number tested`,`mean scale score`,`% level 3+4`)
names(nyc_ela_memb)[1] <- "dist"
tmp_data <- merge(nyc_ela_memb,nyc_ela_stan, by="dist")
names(tmp_data)
tmp_data$`mean scale score` <- as.numeric(tmp_data$`mean scale score`)
tmp_data$`% level 3+4` <- as.numeric(tmp_data$`% level 3+4`)
tmp_data <- tmp_data %>% 
  mutate(std_n = (`number tested` - avg_tested)/sd_tested,
         std_score = (`mean scale score` - avg_score)/sd_score,
         std_perc_lv34 = (`% level 3+4` - avg_perc_lev34)/sd_perc_lev34) %>% 
  select(dist, school, year, `number tested`, avg_tested, std_n,
         `mean scale score`, avg_score, std_score,
         `% level 3+4`, avg_perc_lev34,std_perc_lv34)
write.csv(tmp_data, file.path('nyc_stan_ela.csv'), row.names = FALSE)

nyc_enroll_summary$sd_swd[11]
