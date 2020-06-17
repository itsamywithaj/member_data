rm(list = ls())
setwd("~/Downloads/States/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("ggplot2")
library(ggplot2)
getwd()
ca_enrl <- read.csv("raw_data/CA_raw_filesenr_19-20.csv")
#-------- clean data originally loaded from https://www.cde.ca.gov/ds/sd/sd/fsenr.asp ----
View(ca_enrl)
names(ca_enrl) <- tolower(names(ca_enrl))
ca_enrl <- ca_enrl %>% 
  select(school, county, district, gender, ethnic, enr_total)
ca_enrl$ethnic <- as.factor(ca_enrl$ethnic)
levels(ca_enrl$ethnic) <- c("unreported","amind","asian","hawpi","filipino","hispanic","black","white","multiple") # recode this variable based on file structure notes for raceial designation
levels(ca_enrl$ethnic)[4:5] <- "asian" # code to match categories in other states
ca_enrl <- ca_enrl %>% # reshape data to have a column for each ethnic category
  distinct(school, district, gender, ethnic, .keep_all = TRUE) %>% 
  spread(key = ethnic,
         value = enr_total,
         fill = 0) %>% 
  mutate(total = unreported + amind + asian + hispanic + black + white + multiple)
levels(ca_enrl$gender) <- c("F","F","F") # combine genders to sum it up
ca_enrl <- ca_enrl %>% 
  select(school, district, unreported, amind, asian, hispanic, black, white, multiple, total)
install.packages("data.table")
library(data.table)
DT <- data.table(ca_enrl)
ca_enrl <- DT[, lapply(.SD, sum), by=list(school, district)] # Summing the rows for all genders
ca_enrl <- ca_enrl %>% # from counts to percentages
  mutate(perc_white = white / total,
         perc_amind = amind / total,
         perc_asian = asian / total,
         perc_black = black / total,
         perc_hispanic = hispanic / total,
         perc_multiple = multiple / total,
         perc_unreported = unreported / total)
y <- ca_enrl %>% # means and SDs of subgroups by district
  group_by(district) %>% 
  summarize(mean_stud = mean(total),
            sd_stud = sd(total),
            mean_amind = mean(perc_amind),
            sd_amind = sd(perc_amind),
            mean_asian = mean(perc_asian),
            sd_asian = sd(perc_asian),
            mean_black = mean(perc_black),
            sd_black = sd(perc_black),
            mean_hispanic = mean(perc_hispanic),
            sd_hispanic = sd(perc_hispanic),
            mean_white = mean(perc_white),
            sd_white = sd(perc_white),
            mean_multiple = mean(perc_multiple),
            sd_multiple = sd(perc_multiple),
            mean_unreported = mean(perc_unreported),
            sd_unreported = sd(perc_unreported),
            n_schools = n()) %>% 
  distinct(district, .keep_all = TRUE) %>% 
  arrange(district)
View(y)
# ------- member demographic enrollment -----
ca_memb <- ca_enrl %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           str_detect(school, "Lashon Academy") == TRUE|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter") %>% 
  arrange(district, school) %>% 
  select(school, district, total, perc_amind, perc_asian, perc_black, perc_hispanic, perc_white, perc_multiple, perc_unreported)
ca_memb$district[28:36]<- c("Chula Vista Elementary", # comparison districts for HTH
                            "San Diego Unified",
                            "San Marcos Unified",
                            "Sweetwater Union High",
                            "San Diego Unified",
                            "San Marcos Unified",
                            "Chula Vista Elementary",
                            "San Diego Unified",
                            "San Marcos Unified")
ca_memb <- merge(ca_memb, y, by="district") # columns for percentages in comparison district
ca_memb <- ca_memb %>% 
  mutate(comp_asian = (perc_asian - mean_asian)/sd_asian,
         comp_amind = (perc_amind - mean_amind)/sd_amind,
         comp_black = (perc_black - mean_black)/sd_black,
         comp_hisp = (perc_hispanic - mean_hispanic)/sd_hispanic,
         comp_white = (perc_white - mean_white)/sd_white,
         comp_mult = (perc_multiple - mean_multiple)/sd_multiple,
         comp_unrep = (perc_unreported - mean_unreported)/sd_unreported) %>% 
  select(school, district, total, perc_amind, mean_amind, comp_amind,
         perc_asian, mean_asian, comp_asian,
         perc_black, mean_black, comp_black,
         perc_hispanic, mean_hispanic, comp_hisp,
         perc_white, mean_white, comp_white,
         perc_multiple, mean_multiple, comp_mult,
         perc_unreported, mean_unreported, comp_unrep) %>% 
  arrange(school)
# ------ file export for racial demographic enrollment ------
write.csv(ca_memb, file = file.path('ca_stan_enroll.csv'), row.names = FALSE)
# ------ FRPL ------------
ca_frpm <- read_excel("raw_data/CA_frpm1920.xlsx", sheet = "FRPM School-Level Data ") # Read FRPM data
# load Excel sheet from https://www.cde.ca.gov/ds/sd/sd/filessp.asp
colnames(ca_frpm) <- ca_frpm[1,]
ca_frpm <- ca_frpm[-1,]
names(ca_frpm) <- tolower(names(ca_frpm))
names(ca_frpm)[7] <- "school"
names(ca_frpm)[22] <- "perc_elig"
names(ca_frpm)[6] <- "district"
ca_frpm <- ca_frpm %>% 
  select(school, district, perc_elig) %>% 
  arrange(district, school)
View(ca_frpm)
ca_frpm$perc_elig <- as.numeric(ca_frpm$perc_elig)
ca_frpm$district <- as.factor(ca_frpm$district)
z <- ca_frpm %>% 
  group_by(district) %>% 
  summarize(dist_mean = mean(perc_elig),
            dist_sd = sd(perc_elig))
ca_memb_frpm <- ca_frpm %>% 
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
           school == "The City" |
           school == "City Language Immersion Charter"|
           str_detect(school, "Lashon Academy") == TRUE|
           str_detect(school, "High Tech") == TRUE|
           school == "Larchmont Charter"|
           school == "Odyssey Charter"|
           str_detect(school, "Summit Public") == TRUE|
           school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school == "Valley Charter Elementary" |
           school == "Valley Charter Middle"|
           school == "Yu Ming Charter") %>% 
  arrange(district, school)
ca_memb_frpm$district[28:36]<- c("Chula Vista Elementary", # comparison districts for HTH
                            "San Diego Unified",
                            "San Marcos Unified",
                            "Sweetwater Union High",
                            "San Diego Unified",
                            "San Marcos Unified",
                            "Chula Vista Elementary",
                            "San Diego Unified",
                            "San Marcos Unified")
ca_memb_frpm <- merge(ca_memb_frpm, z, by="district")
ca_memb_frpm <- ca_memb_frpm %>% 
  mutate(comp_frpm = (perc_elig - dist_mean)/dist_sd) %>% 
  arrange(school) %>% 
  select(school, district, perc_elig, dist_mean, comp_frpm)

write.csv(ca_memb_frpm, file = file.path('ca_stan_frpm.csv'), row.names = FALSE)
# ------ English Learners -------
ca_ell <- read.csv("raw_data/fileselsch.csv")
# raw data downloaded from https://www.cde.ca.gov/ds/sd/sd/fileselsch.asp
names(ca_ell) <- tolower(names(ca_ell))
ca_ell <- ca_ell %>% 
  select(school, district, total_el)
DT <- data.table(ca_ell)
ca_ell <- DT[, lapply(.SD, sum), by=list(school, district)]
ca_ell <- ca_ell %>% # combine data frames to get ELLs by percentage of total enrollment
  mutate(unique = paste(school, district, sep = "_"))
ca_totals <- ca_enrl %>% 
  select(school, district, total) %>% 
  mutate(unique = paste(school, district, sep = "_"))
ca_ell <- merge(ca_ell,ca_totals, by="unique")
names(ca_ell)[2:3] <- c("school","district")
ca_ell <- ca_ell %>% 
  select(school, district, total_el,total) %>% 
  mutate(perc_el = total_el/total)
a <- ca_ell %>% 
  group_by(district) %>% 
  summarize(dist_mean = mean(perc_el),
            dist_sd = sd(perc_el),
            n_schools = n())
ca_memb_ell <- ca_ell %>% #filter to just members
  filter(str_detect(school, "Citizens of the World Charter School") == TRUE|
            school == "The City" |
            school == "City Language Immersion Charter"|
            str_detect(school, "Lashon Academy") == TRUE|
            str_detect(school, "High Tech") == TRUE|
            school == "Larchmont Charter"|
            school == "Odyssey Charter"|
            str_detect(school, "Summit Public") == TRUE|
            school == "Tomorrow's Leadership Collaborative (TLC) Charter"|
            school == "Valley Charter Elementary" |
            school == "Valley Charter Middle"|
            school == "Yu Ming Charter") %>% 
  arrange(district, school)

ca_memb_ell$district[28:36]<- c("Chula Vista Elementary", # comparison districts for HTH
                                "San Diego Unified",
                                "San Marcos Unified",
                                "Sweetwater Union High",
                                "San Diego Unified",
                                "San Marcos Unified",
                                "Chula Vista Elementary",
                                "San Diego Unified",
                                "San Marcos Unified")
ca_memb_ell <- merge(ca_memb_ell,a,by="district")
ca_memb_ell <- ca_memb_ell %>% 
  mutate(comp_el = (perc_el - dist_mean)/dist_sd) %>% 
  select(school, district, total_el,total, perc_el, dist_mean, comp_el)

write.csv(ca_memb_ell, file = file.path('ca_stan_ell.csv'), row.names = FALSE)

# ------ CAASPP math and ELA --------
# downloaded research files from https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList?ps=true&lstTestYear=2019&lstTestType=B&lstCounty=00&lstDistrict=00000&lstSchool=0000000
caaspp <- read_csv("raw_data/sb_ca2019_1_csv_v4/sb_ca2019_1_csv_v4.csv")
caaspp_schools <- read_csv("raw_data/sb_ca2019_1_csv_v4/sb_ca2019entities_csv.csv")
names(caaspp) <- tolower(names(caaspp))
names(caaspp)[c(7:11,13,17)] <- c("test_type","n_tested_school","n_tested_wscores",
                        "grade","test_id","n_tested","perc_metabove")
names(caaspp)[2:3] <- c("dist_code","school_code")
str(caaspp)
caaspp$test_id <- as.factor(caaspp$test_id)
caaspp$perc_metabove <- as.numeric(caaspp$perc_metabove)*.01
levels(caaspp$test_id) <- c("ela","math")
caaspp$dist_code <- as.factor(caaspp$dist_code)
caaspp$school_code <- as.factor(caaspp$school_code)

names(caaspp_schools) <- tolower(names(caaspp_schools))
names(caaspp_schools)[c(2,3,6,8,9)] <- c("dist_code", "school_code","type_id","dist_name","school_name")
str(caaspp_schools)
caaspp_schools$type_id <- as.factor(caaspp_schools$type_id)
levels(caaspp_schools$type_id) <- c("state","county","dist","school","charter_direct","charter_local")
# renaming based on record definitions in https://caaspp-elpac.cde.ca.gov/caaspp/research_fixfileformat19
summary(caaspp_schools$type_id)
# to see number of rows for each category and check my understanding of the records

caaspp_memb_codes <- caaspp_schools %>% 
  select(type_id, dist_code, school_code, dist_name,school_name) %>% 
  filter(type_id == "dist" | type_id == "school"| # save the codes for just members
           type_id == "charter_direct" | type_id == "charter_local",
         str_detect(school_name, "Citizens of the World Charter School") == TRUE|
           school_name == "The City" |
           school_name == "City Language Immersion Charter"|
           str_detect(school_name, "Lashon Academy") == TRUE|
           str_detect(school_name, "High Tech") == TRUE|
           school_name == "Larchmont Charter"|
           school_name == "Odyssey Charter"|
           str_detect(school_name, "Summit Public") == TRUE|
           school_name == "Tomorrow's Leadership Collaborative (TLC) Charter"|
           school_name == "Valley Charter Elementary" |
           school_name == "Valley Charter Middle"|
           school_name == "Yu Ming Charter") %>% 
  arrange(dist_name, school_name) %>% 
  select(school_code, school_name)
dist_list <- ca_memb_ell %>% 
  select(district, school)
dist_list <- unique(dist_list) # list of comparison districts and members
caaspp_schools <- caaspp_schools[caaspp_schools$dist_name %in% dist_list$district ,] # index to only include coding for comparison districts
caaspp_members <- subset(caaspp,school_code %in% caaspp_memb_codes$school_code)
# index of CAASPP data to only include members
caaspp_members <- merge(caaspp_members,caaspp_memb_codes, by="school_code") # matching caaspp data by school name
names(caaspp_members)
caaspp_members <- caaspp_members %>% 
  select(school_code, school_name, grade, test_id, n_tested, perc_metabove) %>% 
  arrange(school_name,test_id,grade)
names(caaspp_members)[c(2)] <- c("school")
dist_codes <- caaspp_schools %>% 
  select(dist_code, dist_name) %>% 
  unique() # comparison district codes and names to match with members
names(dist_list)[1] <- "dist_name" # rename to match coding list
dist_codes <- merge(dist_codes, dist_list, by="dist_name")
View(dist_codes)
caaspp_members <- merge(caaspp_members, dist_codes, by="school") # added districts to match summative data
names(caaspp_members)
caaspp_members <- caaspp_members %>% 
  select(school, school_code,dist_name,dist_code, grade, test_id, n_tested, perc_metabove) %>% 
  unique() %>% 
  filter(grade == "13") # grade 13 = all the grades summed together
  

caaspp <- caaspp[caaspp$dist_code %in% dist_codes$dist_code ,] # subset relevant district codes
caaspp <- caaspp %>% 
  select(dist_code, school_code, grade, n_tested, test_id, perc_metabove)
summary(caaspp)
caaspp$n_tested <- as.numeric(caaspp$n_tested)
caaspp <- caaspp %>% 
  filter(school_code!= "0000000",
         grade == 13) %>% #filter out the rows reporting on district or state
  arrange(dist_code, school_code, test_id, n_tested) %>% 
  select(dist_code, school_code, grade, test_id, n_tested, perc_metabove) %>%  
  na.omit(caaspp$n_metabove) # remove rows with missing values
c <- dist_codes %>% 
  select(dist_name, dist_code)
caaspp <- merge(caaspp,c, by="dist_code") # add district names to the correct code

caaspp_stan_math <- caaspp %>% 
  filter(test_id == "math") %>% 
  unique() %>% 
  group_by(dist_name) %>% 
  summarize(n_schools = n(),
            mean_perc_me = mean(perc_metabove),
            sd_perc_me = sd(perc_metabove))

caaspp_stan_ela <- caaspp %>% 
  filter(test_id == "ela") %>% 
  unique() %>% 
  group_by(dist_name) %>% 
  summarize(n_schools = n(),
            mean_perc_me = mean(perc_metabove),
            sd_perc_me = sd(perc_metabove))

caaspp_members$n_tested <- as.numeric(caaspp_members$n_tested)
caaspp_members <- caaspp_members %>% 
  select(dist_code, dist_name, school, grade, test_id, n_tested, perc_metabove) %>% 
  arrange(dist_name, school, test_id)

ca_stan_math <- caaspp_members %>% 
  filter(test_id == "math")
ca_stan_math <- merge(ca_stan_math, caaspp_stan_math,by="dist_name")
ca_stan_math <- ca_stan_math %>% 
  mutate(comp_me = (perc_metabove - mean_perc_me) / sd_perc_me) %>% 
  select(dist_name, school, n_tested,perc_metabove, mean_perc_me, comp_me) %>% 
  arrange(school)

ca_stan_ela <- caaspp_members %>% 
  filter(test_id == "ela")
ca_stan_ela <- merge(ca_stan_ela, caaspp_stan_ela, by="dist_name")
ca_stan_ela <- ca_stan_ela %>% 
  mutate(comp_me = (perc_metabove - mean_perc_me) / sd_perc_me) %>% 
  select(dist_name, school, n_tested,perc_metabove, mean_perc_me, comp_me) %>% 
  arrange(school)

write.csv(ca_stan_ela, file = file.path("ca_stan_ela.csv"), row.names = FALSE)
write.csv(ca_stan_math, file = file.path("ca_stan_math.csv"), row.names = FALSE)
