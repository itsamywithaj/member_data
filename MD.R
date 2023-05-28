# --- Amy Jiravisitcul. 13 May 2023 ----
rm(list = ls()) # clear working environment
setwd("~/Documents/DCSC/member_data/")
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages('segregation')
library(segregation)
getwd()

# ----- ENROLLMENT BY RACE --------
# Data downloaded from https://reportcard.msde.maryland.gov/DataDownloads/FileDownload/408
enrl <- read.csv('raw data/MD_Enrollment_2023/Enrollment_By_Race_2023.csv')
names(enrl) <- tolower(names(enrl))
enrl <- enrl %>% 
  mutate(enrolled.count = as.numeric(enrolled.count)) # change format from character to number

agg <- enrl %>% 
  filter(school.name == "All Baltimore City Schools"|
           school.name == "All Baltimore County Schools") %>% 
  spread(race, enrolled.count)
names(agg)[c(3:5,7:14)] <- c("district","school_code","school","total","amind",
                             "asian","black","hisp","nhpi","mult","white")
agg <- agg %>% # percentages by race subgroup of both district and county
  mutate(total = as.numeric(total),
         amind = as.numeric(amind)/total,
         asian = (as.numeric(asian) + as.numeric(nhpi))/total,
         black = as.numeric(black)/total,
         hisp = as.numeric(hisp)/total,
         white = as.numeric(white)/total,
         mult = as.numeric(mult)/total) %>% 
  select(district, school, total:hisp, white, mult)

wide_enrl <- enrl %>% 
  spread(race, enrolled.count) # Reshape to data set with 1,419 rows; 1 for each school
names(wide_enrl)[c(3:5,7:14)] <- c("district","school_code","school","total","amind",
                                   "asian","black","hisp","nhpi","mult","white")
wide_enrl <- wide_enrl %>% 
  filter(lea == "03"| # only include Baltimore City and County
           lea == "30") %>% 
  mutate(total = as.numeric(total),
         amind_p = case_when(lea == "03" ~ agg$amind[1], # estimate where data is suppressed
                             lea == "30" ~ agg$amind[2]),
         amind = case_when(as.numeric(amind)>0 ~ as.numeric(amind),
                           (amind == "*"|is.na(amind)) ~ round(total * amind_p)),
         asian_p = case_when(lea == "03" ~ agg$asian[1],
                             lea == "30" ~ agg$asian[2]),
         asian = case_when(as.numeric(asian)>0 ~ as.numeric(asian),
                           (asian == "*"|is.na(asian)) ~ round(total * asian_p)),
         black_p = case_when(lea == "03" ~ agg$black[1],
                             lea == "30" ~ agg$black[2]),
         black = case_when(as.numeric(black)>0 ~ as.numeric(black),
                           (black == "*"|is.na(black)) ~ round(total * black_p)),
         hisp_p = case_when(lea == "03" ~ agg$hisp[1],
                            lea == "30" ~ agg$hisp[2]),
         hisp = case_when(as.numeric(hisp)>0 ~ as.numeric(hisp),
                          (hisp == "*"|is.na(hisp)) ~ round(total * hisp_p)),
         white_p = case_when(lea == "03" ~ agg$white[1],
                             lea == "30" ~ agg$white[2]),
         white = case_when(as.numeric(white)>0 ~ as.numeric(white),
                           (white == "*"|is.na(white)) ~ round(total * white_p)),
         mult_p = case_when(lea == "03" ~ agg$mult[1],
                            lea == "30" ~ agg$mult[2]),
         mult = case_when(as.numeric(mult)>0 ~ as.numeric(mult),
                          (mult == "*"|is.na(mult)) ~ round(total * mult_p))) %>% 
  select(lea, district, school, total, amind, asian, black, hisp,white, mult) %>% na.omit()

memb_enrl <- wide_enrl %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")|
           str_detect(school,"Watershed")) %>% 
  mutate(county = "Baltimore County + City",
         amind = amind/total,
         asian = asian/total,
         black = black/total,
         hisp = hisp/total,
         white = white/total,
         mult = mult/total)

a <- agg %>% 
  filter(district == "Baltimore City"|
           district == "Baltimore County") %>% 
  mutate(dist_total = total,
         dist_amind = amind,
         dist_asian = asian,
         dist_black = black,
         dist_hisp = hisp,
         dist_white = white,
         dist_mult = mult) %>% 
  select(district, dist_total:dist_mult)

memb_enrl <- merge(memb_enrl,a)

b <- agg %>% 
  filter(district == "Baltimore County"|
           district == "Baltimore City") %>% 
  select(-district) %>%
  mutate(county = "Baltimore County + City",
         amind = amind * total,
         asian = asian * total,
         black = black * total,
         hisp = hisp * total,
         white = white * total,
         mult = mult* total) %>%
  group_by(county) %>% 
  summarize(cty_total = sum(total),
            cty_amind = sum(amind),
            cty_asian = sum(asian),
            cty_black = sum(black),
            cty_hisp = sum(hisp),
            cty_white = sum(white),
            cty_mult = sum(mult),
            .groups = 'drop') %>% 
  mutate(cty_amind = cty_amind/cty_total,
         cty_asian = cty_asian/cty_total,
         cty_black = cty_black/cty_total,
         cty_hisp = cty_hisp/cty_total,
         cty_white = cty_white/cty_total,
         cty_mult = cty_mult/cty_total)

memb_enrl <- merge(memb_enrl,b)

# ---- LOCAL RACIAL SEGREGATION BY DISTRICT AND COUNTY ------
locseg_cty <- wide_enrl %>% 
  select(-total)  %>% 
  filter(school != "All Baltimore County Schools",
         school != "All Baltimore City Schools") %>% 
  gather(race, n, amind:mult) %>%
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")|
           str_detect(school, "Watershed")) %>% 
  mutate(ls_cty = ls) %>% 
  select(school, ls_cty)
memb_enrl <- merge(memb_enrl,locseg_cty)

locseg_dist <- wide_enrl %>% 
  select(-total) %>% 
  filter(lea == "30",
         school != "All Baltimore City Schools") %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school", weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)
c <- wide_enrl %>% 
  select(-total) %>% 
  filter(lea == "03",
         school != "All Baltimore County Schools") %>% 
  gather(race, n, amind:mult) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  filter(str_detect(school, "Watershed")) %>% 
  mutate(ls_dist = ls) %>% 
  select(school, ls_dist)
locseg_dist <- rbind(locseg_dist,c)

memb_enrl <- merge(memb_enrl, locseg_dist)

# ------ FRPL, SWD, ELL ------
subgroups <- read.csv('raw data/MD_2022_Special_Services.csv')
names(subgroups) <- tolower(names(subgroups))
names(subgroups)[c(2:5,25,11,15,29)] <- c("lss","district","school_code","school","ecdis",
                                          "ell","swd","total")
subgroups <- subgroups %>% 
  select(lss, district, school.type, school_code, school, ecdis, ell, swd) %>% 
  filter(school.type == "All"|
           str_detect(school,"Watershed")|
           str_detect(school, "City Neighbors High"),
         str_detect(school, "All Baltimore")|
           str_detect(school, "City Neighbors")|
           str_detect(school, "Baltimore Montessori")|
           str_detect(school,"Watershed")) %>% 
  mutate(ecdis = as.numeric(ecdis)*.01,
         ell= as.numeric(ell) * .01,
         swd = as.numeric (swd) * .01)

memb_sub <- subgroups %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Montessori")|
           str_detect(school, "Watershed")) %>% 
  mutate(ell = 0) %>% 
  select(school, ecdis, ell, swd)

memb_enrl <- merge(memb_enrl, memb_sub)

dist_sub <- subgroups %>% 
  filter(school.type=="All",
         school == "All Baltimore City Schools"|
           school == "All Baltimore County Schools") %>%
  mutate(dist_ecdis = ecdis,
         dist_ell = ell,
         dist_swd = swd) %>% 
  select(district, dist_ecdis, dist_ell, dist_swd)

memb_enrl <- merge(memb_enrl, dist_sub)

cty_sub <- subgroups %>% 
  filter(school.type=="All",
         school == "All Baltimore County Schools"|
           school == "All Baltimore City Schools") %>%
  mutate(county = "Baltimore County + City",
         cty_total = 187078,
         cty_ecdis = ecdis * cty_total,
         cty_ell = ell * cty_total,
         cty_swd = swd * cty_total) %>% 
  select(county, cty_ecdis:cty_swd) %>% 
  group_by(county) %>% 
  summarize(cty_ecdis = sum(cty_ecdis),
            cty_ell = sum(cty_ell),
            cty_swd = sum(cty_swd),
            .groups = 'drop')

memb_enrl <- merge(memb_enrl, cty_sub)

names(memb_enrl)
memb_enrl <- memb_enrl %>% 
  select(school, total:mult, ecdis:swd, ls_dist, ls_cty, district, dist_total:dist_mult,
         dist_ecdis:dist_swd, county, cty_total:cty_mult, cty_ecdis:cty_swd)

write.csv(memb_enrl, file = file.path('output data/md_enrl.csv'), row.names = FALSE)

# ----- ASSESSMENT DATA ------
ela <- read_excel('raw data/MD_MCAP_ELA_Math_2022/2022_MCAP_ELA_Administrative_Data_Report_Card.xlsx')
names(ela) <- tolower(names(ela))
str(ela)
memb_ela <- ela %>% 
  filter(`lea name` == "Baltimore City"|
           `lea name` == "Baltimore County") %>% 
  mutate(district = as.factor(`lea name`),
         school = as.factor(`school name`),
         n_tested = as.numeric(`tested count`),
         n_prof = as.numeric(`proficient count`),
         p_prof = as.numeric(`proficient pct`) *.01) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  select(district, school, n_tested:p_prof)%>% 
  group_by(school) %>% 
  summarize(n_ela = sum(n_tested),
            prof_ela = sum(n_prof)) %>% 
  mutate(prof_ela = prof_ela/n_ela) %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Watershed")|
           str_detect(school, "Montessori"))
dist_ela <- ela %>% 
  filter(`lea name` == "Baltimore City"|
           `lea name` == "Baltimore County") %>% 
  mutate(district = as.factor(`lea name`),
         n_tested = as.numeric(`tested count`),
         n_prof = as.numeric(`proficient count`)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  select(district, n_tested, n_prof) %>% 
  group_by(district) %>% 
  summarize(dist_n = sum(n_tested),
            dist_ela = sum(n_prof)) %>% 
  mutate(dist_ela = dist_ela/dist_n)
memb_ela$district <- c("Baltimore City","Baltimore City","Baltimore City","Baltimore City","Baltimore County")
memb_ela <- merge(memb_ela, dist_ela)

math <- read_excel('raw data/MD_MCAP_ELA_Math_2022/2022_MCAP_MATH_Administrative_Data_Report_Card.xlsx')
names(math) <- tolower(names(math))
str(math)
memb_math <- math %>% 
  filter(`lea name` == "Baltimore City"|
           `lea name` == "Baltimore County") %>% 
  mutate(district = as.factor(`lea name`),
         school = as.factor(`school name`),
         n_tested = as.numeric(`tested count`),
         n_prof = as.numeric(`proficient count`),
         p_prof = as.numeric(`proficient pct`) *.01) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  select(district, school, n_tested:p_prof)%>% 
  group_by(school) %>% 
  summarize(n_math = sum(n_tested),
            prof_math = sum(n_prof)) %>% 
  mutate(prof_math = prof_math/n_math) %>% 
  filter(str_detect(school, "City Neighbors")|
           str_detect(school, "Watershed")|
           str_detect(school, "Montessori"))
memb_math$district <- c("Baltimore City","Baltimore City","Baltimore City","Baltimore City","Baltimore County")
dist_math <- math %>% 
  filter(`lea name` == "Baltimore City"|
           `lea name` == "Baltimore County") %>% 
  mutate(district = as.factor(`lea name`),
         n_tested = as.numeric(`tested count`),
         n_prof = as.numeric(`proficient count`)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  select(district, n_tested, n_prof) %>% 
  group_by(district) %>% 
  summarize(dist_n = sum(n_tested),
            dist_math = sum(n_prof)) %>% 
  mutate(dist_math = dist_math/dist_n)
memb_math <- merge(memb_math, dist_math)

memb_acad <- merge(memb_ela,memb_math,by=c("district","school")) %>% 
  select(district, school, prof_math,dist_math,prof_ela,dist_ela,n_math,n_ela)
write.csv(memb_acad, file = file.path('output data/md_acad.csv'),row.names = FALSE)
