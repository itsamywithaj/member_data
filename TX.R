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
# list of Austin ISD schools https://rptsvr1.tea.texas.gov/adhocrpt/adg19227.html
# school-level data look-up https://rptsvr1.tea.texas.gov/adhocrpt/adste.html
# sub populations https://rptsvr1.tea.texas.gov/cgi/sas/broker 
aisd_enrl <- data.frame(district = "Austin",
                        c_frpl = c(1684,470,68,472,350,496,429,0,299,97,125,497,25,
                                   77,739,242,292,650,267,276,115,263,284,36,924,159,384,39,29,489,513,258,1012,
                                   273,0,158,246,646,132,375,728,503,372,
                                   90,82,339,64,49,524,635,40,505,637,
                                   43,115,491,347,575,183,448,39,400,240,
                                   498,1223,133,625,11,85,333,26,127,500,
                                   118,443,313,589,203,58,269,122,217,368,
                                   222,347,254,509,705,372,658,235,41,346,
                                   488,11,492,360,384,
                                   1039,137,403,80,463,
                                   61,310,250,170,351,
                                   314,246,265,0,0,
                                   36,954,334,278,646,
                                   637,206,434,346,218,
                                   518,427,231,88),
                        c_el = c(525,267,17,129,259,140,25,0,67,106,56,396,13,
                                 76,266,52,185,281,46,62,37,104,211,13,609,22,119,20,39,370,135,80,246,
                                 64,0,149,79,405,138,155,332,351,184,
                                 0,37,134,33,17,385,542,13,349,525,
                                 13,93,311,351,384,70,129,74,101,79,
                                 300,801,0,240,0,25,219,0,28,179,
                                 39,360,45,346,87,91,133,32,82,312,
                                 36,185,115,371,600,192,239,197,0,124,
                                 309,0,421,246,196,
                                 449,126,67,64,256,
                                 26,193,203,58,154,
                                 197,276,170,0,0,
                                 16,429,172,185,530,
                                 450,181,218,120,148,
                                 417,355,85,17),
                        c_swd = c(352,88,19,192,52,225,11,16,129,73,117,29,40,
                                  24,187,47,50,142,94,285,94,71,44,16,171,34,92,60,102,59,158,103,240,
                                  83,0,90,86,103,70,72,144,99,92,
                                  0,148,79,10,0,74,69,78,61,104,
                                  59,92,77,0,81,48,116,99,79,124,
                                  87,241,12,128,0,63,43,0,69,122,
                                  43,85,147,129,22,109,149,12,118,109,
                                  33,45,50,59,70,64,151,104,21,58,
                                  96,0,37,62,81,
                                  162,29,12,36,62,
                                  211,46,21,19,163,
                                  33,105,72,0,0,
                                  20,202,61,41,82,
                                  107,17,102,95,40,
                                  79,85,59,60),
                        campus = c(227901017,227901101,227901012,227901009,227901102,227901002,227901194,227901250,227901059,227901187,227901182,227901149,227901103,
                                   227901104,227901054,227901105,227901106,227901185,227901170,227901013,227901107,227901108,227901109,227901110,227901046,227901111,227901173,227901112,227901184,227901161,227901057,227901183,227901008,
                                   227901113,227901115,227901179,227901114,227901055,227901154,227901019,227901043,227901176,227901064,
                                   227901015,227901062,227901116,227901026,227901025,227901159,227901186,227901117,227901118,227901163,
                                   227901119,227901155,227901162,227901029,227901178,227901120,227901044,227901180,227901172,227901045,
                                   227901168,227901004,227901018,227901014,227901035,227901121,227901160,227901197,227901122,227901051,
                                   227901123,227901165,227901005,227901058,227901124,227901181,227901052,227901150,227901047,227901148,
                                   227901125,227901156,227901126,227901189,227901188,227901171,227901061,227901143,227901128,227901129,
                                   227901190,227901032,227901164,227901151,227901130,
                                   227901006,227901132,227901028,227901133,227901174,
                                   227901251,227901065,227901127,227901139,227901060,
                                   227901136,227901138,227901158,227901027,227901036,
                                   227901030,227901007,227901140,227901177,227901141,
                                   227901053,227901193,227901175,227901166,227901157,
                                   227901152,227901144,227901145,227901146),
                        c_amind = c(10,0,0,10,0,10,0,0,10,10,10,10,0,10,0,0,10,10,10,10,0,0,0,0,10,0,0,0,10,10,10,10,10,
                                    10,0,10,0,0,0,0,0,10,10,10,10,10,0,0,10,0,0,0,10,0,10,0,0,10,10,10,10,10,10,0,0,10,10,0,10,0,10,0,0,
                                    0,0,10,10,0,10,10,0,10,10,10,0,0,10,0,10,0,10,0,0,
                                    0,0,0,10,10,
                                    10,0,10,10,0,
                                    10,10,0,0,10,
                                    0,0,0,0,0,
                                    0,10,0,0,0,
                                    0,0,10,0,10,
                                    0,10,0,10),
                        c_asian = c(64+10,10,0,193+10,13,50+10,10+10,0,34+10,92,38+10,10,10,20,10,10,10+10,23+10,20,177+10,14,0,20,15+10,10,10,17,30,134+10,10,10+10,48,18+10,
                                    10+10,0,104,10,20,103+10,10,34,14,10,10,88+10,0,10,0,10,26+10,14,20,21,33+10,66,10,10,10+10,10,189,143,10+10,19+10,10,20,298+10,10+10,10,35,29,10,10,10,
                                    26,10,30,10,14,96,116+10,10,20,33+10,10,10,10,10,10,10,20,176,10,10,
                                    0,0,10+10,38+10,10,
                                    20,18+10,40,10,10,
                                    14,10,14,0,42+10,
                                    10,199+10,10,0,0,
                                    0,18+10,43+10,10+10,40,
                                    10,0,10,15,10+10,
                                    10+10,10,10,11),
                        c_black = c(157,20,20,129,62,92,112,10,44,10,22,29,10,11,51,97,30,52,18,66,21,31,19,10,71,91,27,10,10,43,56,40,106,
                                    35,0,40,39,61,30,58,58,25,86,15,18,57,10,10,61,31,14,78,50,10,40,33,13,124,10,129,20,55,59,34,114,27,301,10,30,36,10,46,86,
                                    35,18,123,43,10,20,55,59,37,23,67,16,40,89,31,21,59,40,20,147,
                                    30,10,47,55,54,
                                    162,10,39,10,45,
                                    21,55,10,78,38,
                                    14,50,20,10,10,
                                    10,103,22,22,49,
                                    53,10,39,23,58,
                                    33,25,40,10),
                        c_hispanic = c(2138,457,60,675,283,851,287,10,481,167,281,450,105,220,721,186,337,688,238,843,177,245,232,64,862,49,409,85,111,450,439,334,1122,
                                       240,0,169,240,561,144,339,731,479,289,77,300,296,70,57,498,575,95,423,558,90,172,488,332,468,165,443,184,323,379,469,1321,270,507,10,110,275,14,134,420,
                                       139,426,533,559,185,215,348,59,339,433,149,366,213,415,687,373,727,262,93,201,
                                       485,20,447,309,352,
                                       913,165,520,194,414,
                                       99,262,246,91,515,
                                       283,203,344,10,0,
                                       36,976,256,261,529,
                                       580,202,391,309,179,
                                       473,407,196,144),
                        c_white = c(320,20,14,1120,10,1257,18,10,365,453,633,16,283,192,56,88,165,110,242,1621,449,20,26,300,22,22,108,583,503,13,271,333,248,
                                    103,10,402,70,25,502,16,255,27,16,50,880,10,10,10,31,21,443,20,28,494,669,14,10,15,90,442,723,146,716,12,52,613,20,10,256,20,0,268,24,
                                    188,11,1008,10,10,469,741,10,492,325,10,22,22,10,10,22,82,410,103,20,
                                    29,13,10,71,26,
                                    30,57,248,135,10,
                                    77,10,10,10,568,
                                    40,350,193,10,0,
                                    10,70,164,10,54,
                                    22,10,10,60,28,
                                    19,17,10,306),
                        c_multiple = c(66,0,10,93,10,103,11,0,56,48,46,10,19,19,20,40,25,35,30,133,33,10,10,18,10,20,16,35,43,10,38,54,43,
                                       17,0,40,20,10,49,10,38,10,10,10,66,10,10,0,12,10,30,10,10,32,61,10,0,10,10,70,54,25,61,10,15,75,13,0,36,10,0,32,10,
                                       29,10,78,10,10,64,89,10,26,42,10,10,10,10,10,10,24,60,14,10,
                                       10,0,10,20,10,
                                       20,12,34,20,10,
                                       10,10,0,10,58,
                                       20,49,18,10,0,
                                       10,10,20,10,10,
                                       10,10,10,14,11,
                                       10,10,10,27))
aisd_enrl <- aisd_enrl %>% 
  mutate(total = c_amind + c_asian + c_black + c_hispanic + c_multiple + c_white,
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total,
         frpl = c_frpl/total,
         el = c_el/total,
         swd = c_swd/total)
austin_summary <- aisd_enrl %>% 
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
            m_frpl = mean(frpl),
            sd_frpl = sd(frpl),
            m_el = mean(el),
            sd_el = sd(el),
            m_swd = mean(swd),
            sd_swd = sd(swd))
mfa <- data.frame(amind = 0,
                  asian = 11,
                  black = 58,
                  hispanic = 233,
                  white = 157,
                  multiple = 19,
                  c_frpl = 243,
                  c_el = 123,
                  c_swd = 32)
mfa<- mfa %>% 
  mutate(total = amind + asian + black + hispanic + white + multiple,
         amind = amind/total,
         asian = asian/total,
         black = black/total,
         hispanic = hispanic/total,
         white = white/total,
         multiple = multiple/total,
         frpl = c_frpl/total,
         el = c_el/total,
         swd = c_swd/total,
         school = "Montessori For All",
         district = "Austin") %>% 
  select(district, school, total, amind, asian, black, hispanic, white, multiple, frpl, el, swd)
austin_summary$district <- "Austin"
mfa <- merge(austin_summary,mfa,by="district")
mfa <- mfa %>% 
  mutate(comp_amind = (amind-m_amind)/sd_amind,
         comp_asian = (asian-m_asian)/sd_asian,
         comp_black = (black-m_black)/sd_black,
         comp_hispanic = (hispanic-m_hispanic)/sd_hispanic,
         comp_white = (white-m_white)/sd_white,
         comp_multiple = (multiple-m_multiple)/sd_multiple,
         comp_frpl = (frpl - m_frpl)/sd_frpl,
         comp_el = (el-m_el)/sd_el,
         comp_swd = (swd-m_swd)/sd_swd) %>% 
  select(district, school, total,amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple,frpl,m_frpl,comp_frpl,
         el,m_el,comp_el,swd,m_swd,comp_swd)
write.csv(mfa, file=file.path("output_data/mfa_stan_enrl.csv"),row.names = FALSE)
idea <- data.frame(school = c("IDEA Health Professions College Prep",
                              "IDEA Health Professions Academy",
                              "IDEA Montopolis College Prep",
                              "IDEA Montopolis Academy",
                              "IDEA Bluff Springs College Prep",
                              "IDEA Bluff Springs Academy",
                              "IDEA Rundberg College Prep",
                              "IDEA Rundberg Academy",
                              "IDEA Parmer Park College Prep",
                              "IDEA Parmer Park Academy",
                              "IDEA Pflugerville College Prep",
                              "IDEA Pflugerville Academy"),
                   c_amind = c(0,10,10,0,0,0,10,0,10,10,10,10),
                   c_asian = c(0,10+10,0,10,10+10,10+10,10+10,10+10,10,20,18+10,23+10),
                   c_black = c(30,44,27,40,16,58,21,47,15,85,25,84),
                   c_hispanic = c(69,128,689,623,387,557,537,581,93,172,174,277),
                   c_white = c(10,18,16,25,20,58,20,38,10,34,23,42),
                   c_multiple = c(0,18,10,10,10,11,0,10,10,14,10,14),
                   c_frpl = c(88,180,655,609,387,586,568,649,103,257,184,317),
                   c_el = c(34,62,373,416,217,318,408,483,59,86,137,182),
                   c_swd = c(10,0,61,43,54,59,67,43,13,14,34,38))
idea <- idea %>% 
  mutate(total = c_amind + c_asian + c_black + c_hispanic + c_white + c_multiple,
         amind = c_amind/total,
         asian = c_asian/total,
         black = c_black/total,
         hispanic = c_hispanic/total,
         white = c_white/total,
         multiple = c_multiple/total,
         frpl = c_frpl/total,
         el = c_el/total,
         swd = c_swd/total,
         district = "Austin") %>% 
  select(district, school, total, amind, asian, black, hispanic, white, multiple,frpl,el,swd)
idea <- merge(idea, austin_summary,by="district")
idea <- idea %>% 
  mutate(comp_amind = (amind-m_amind)/sd_amind,
         comp_asian = (asian-m_asian)/sd_asian,
         comp_black = (black-m_black)/sd_black,
         comp_hispanic = (hispanic-m_hispanic)/sd_hispanic,
         comp_white = (white-m_white)/sd_white,
         comp_multiple = (multiple-m_multiple)/sd_multiple,
         comp_frpl = (frpl - m_frpl)/sd_frpl,
         comp_el = (el-m_el)/sd_el,
         comp_swd = (swd-m_swd)/sd_swd) %>% 
  select(district, school, total,amind, m_amind, comp_amind,
         asian, m_asian, comp_asian, black, m_black, comp_black,
         hispanic, m_hispanic, comp_hispanic, white, m_white, comp_white,
         multiple, m_multiple, comp_multiple,frpl,m_frpl,comp_frpl,
         el,m_el,comp_el,swd,m_swd,comp_swd)
write.csv(idea, file = file.path('output_data/idea_stan_enrl.csv'),row.names = FALSE)

#----- STAAR? -----
staar <- read.csv("raw_data/TX_CAMPSTAAR1.dat") 
# documentation https://rptsvr1.tea.texas.gov/perfreport/tapr/2019/download/campstaar2a.html
# data from https://rptsvr1.tea.texas.gov/perfreport/tapr/2019/download/DownloadData.html
summary(staar)
staar <- staar[, c(1,which(str_detect(substr(as.character(names(staar)),start = 11, stop = 12),"19")==TRUE)) ] #2019 only
staar <- staar[, c(1,which(str_detect(substr(as.character(names(staar)),start = 1, stop = 3),"CDA")==TRUE))] # all students only
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 13, stop = 13),"R")==TRUE))] # numerators and demoniators only
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 6, stop = 8),"ASC")==TRUE))] #remove science
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 6, stop = 8),"ASC")==TRUE))] #remove science
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 6, stop = 8),"AWR")==TRUE))] #remove writing
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 6, stop = 8),"ASS")==TRUE))] #remove non math and non ela
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 9, stop = 12),"1319")==TRUE))] #remove master and approaching standards
staar <- staar[, -c(which(str_detect(substr(as.character(names(staar)),start = 9, stop = 12),"1S19")==TRUE))] #remove master and approaching standards


names(staar)
aisd <- data.frame(CAMPUS = as.character(aisd_enrl$campus))
staar_aus <- merge(staar,aisd, by="CAMPUS")
names(staar_aus)<- c("campus","ela_gr3_n","ela_gr3_me","ela_gr4_n","ela_gr4_me","ela_gr5_n","ela_gr5_me",
                     "math_gr3_n","math_gr3_me","math_gr4_n","math_gr4_me","math_gr5_n","math_gr5_me","ela_gr6_n",
                     "ela_gr6_me","ela_gr7_n","ela_gr7_me","ela_gr8_n","ela_gr8_me","math_gr6_n","math_gr6_me",
                     "math_gr7_n","math_gr7_me","math_gr8_n","math_gr8_me")
staar_aus <- staar_aus %>% 
  mutate(ela_me_c = as.numeric(gsub("[^0-9-]+",0,ela_gr3_me))+as.numeric(gsub("[^0-9-]+",0,ela_gr4_me))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr5_me))+as.numeric(gsub("[^0-9-]+",0,ela_gr6_me))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr7_me))+as.numeric(gsub("[^0-9-]+",0,ela_gr8_me)),
         n_ela = as.numeric(gsub("[^0-9-]+",0,ela_gr3_n))+as.numeric(gsub("[^0-9-]+",0,ela_gr4_n))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr5_n))+as.numeric(gsub("[^0-9-]+",0,ela_gr6_n))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr7_n))+as.numeric(gsub("[^0-9-]+",0,ela_gr8_n)),
         math_me_c = as.numeric(gsub("[^0-9-]+",0,math_gr3_me))+as.numeric(gsub("[^0-9-]+",0,math_gr4_me))+
           as.numeric(gsub("[^0-9-]+",0,math_gr5_me))+as.numeric(gsub("[^0-9-]+",0,math_gr6_me))+
           as.numeric(gsub("[^0-9-]+",0,math_gr7_me))+as.numeric(gsub("[^0-9-]+",0,math_gr8_me)),
         n_math = as.numeric(gsub("[^0-9-]+",0,math_gr3_n))+as.numeric(gsub("[^0-9-]+",0,math_gr4_n))+
           as.numeric(gsub("[^0-9-]+",0,math_gr5_n))+as.numeric(gsub("[^0-9-]+",0,math_gr6_n))+
           as.numeric(gsub("[^0-9-]+",0,math_gr7_n))+as.numeric(gsub("[^0-9-]+",0,math_gr8_n)),
         ela_me = ela_me_c/n_ela,
         math_me = math_me_c/n_math) %>% 
  select(campus,n_math,math_me,n_ela,ela_me)%>%na.omit()
staar_aus <- staar_aus[-nrow(staar_aus),]
staar_sum <- staar_aus %>% 
  summarize(m_ela_me = mean(ela_me),
            sd_ela_me = sd(ela_me),
            m_math_me = mean(math_me),
            sd_math_me = sd(math_me))

memb_code <- data.frame(school = c("Montessori for All","IDEA Health Professions College Prep",
                                   "IDEA Health Professions Academy",
                                   "IDEA Montopolis College Prep",
                                   "IDEA Montopolis Academy",
                                   "IDEA Bluff Springs College Prep",
                                   "IDEA Bluff Springs Academy",
                                   "IDEA Rundberg College Prep",
                                   "IDEA Rundberg Academy",
                                   "IDEA Parmer Park College Prep",
                                   "IDEA Parmer Park Academy",
                                   "IDEA Pflugerville College Prep",
                                   "IDEA Pflugerville Academy"),
                        CAMPUS = c(227826101,108807091,108807191,108807035,108807135,
                                   108807037,108807137,108807036,108807136,
                                   108807090,108807190,108807038,108807138))
tx_memb <- merge(memb_code,staar,by="CAMPUS")
names(tx_memb)<- c("campus","school","ela_gr3_n","ela_gr3_me","ela_gr4_n","ela_gr4_me","ela_gr5_n","ela_gr5_me",
                     "math_gr3_n","math_gr3_me","math_gr4_n","math_gr4_me","math_gr5_n","math_gr5_me","ela_gr6_n",
                     "ela_gr6_me","ela_gr7_n","ela_gr7_me","ela_gr8_n","ela_gr8_me","math_gr6_n","math_gr6_me",
                     "math_gr7_n","math_gr7_me","math_gr8_n","math_gr8_me")
tx_memb <- tx_memb %>% 
  mutate(ela_me_c = as.numeric(gsub("[^0-9-]+",0,ela_gr3_me))+as.numeric(gsub("[^0-9-]+",0,ela_gr4_me))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr5_me))+as.numeric(gsub("[^0-9-]+",0,ela_gr6_me))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr7_me))+as.numeric(gsub("[^0-9-]+",0,ela_gr8_me)),
         n_ela = as.numeric(gsub("[^0-9-]+",0,ela_gr3_n))+as.numeric(gsub("[^0-9-]+",0,ela_gr4_n))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr5_n))+as.numeric(gsub("[^0-9-]+",0,ela_gr6_n))+
           as.numeric(gsub("[^0-9-]+",0,ela_gr7_n))+as.numeric(gsub("[^0-9-]+",0,ela_gr8_n)),
         math_me_c = as.numeric(gsub("[^0-9-]+",0,math_gr3_me))+as.numeric(gsub("[^0-9-]+",0,math_gr4_me))+
           as.numeric(gsub("[^0-9-]+",0,math_gr5_me))+as.numeric(gsub("[^0-9-]+",0,math_gr6_me))+
           as.numeric(gsub("[^0-9-]+",0,math_gr7_me))+as.numeric(gsub("[^0-9-]+",0,math_gr8_me)),
         n_math = as.numeric(gsub("[^0-9-]+",0,math_gr3_n))+as.numeric(gsub("[^0-9-]+",0,math_gr4_n))+
           as.numeric(gsub("[^0-9-]+",0,math_gr5_n))+as.numeric(gsub("[^0-9-]+",0,math_gr6_n))+
           as.numeric(gsub("[^0-9-]+",0,math_gr7_n))+as.numeric(gsub("[^0-9-]+",0,math_gr8_n)),
         ela_me = ela_me_c/n_ela,
         math_me = math_me_c/n_math) %>% 
  select(campus,school,n_math,math_me,n_ela,ela_me)%>%na.omit()
tx_memb <- tx_memb %>% 
  mutate(comp_math = (math_me - staar_sum$m_math_me)/staar_sum$sd_math_me,
         comp_ela = (ela_me - staar_sum$m_ela_me)/staar_sum$sd_ela_me,
         m_math = staar_sum$m_math_me,
         m_ela = staar_sum$m_ela_me) %>% 
  select(campus, school, n_math,math_me, m_math,comp_math,n_ela,ela_me,m_ela,comp_ela)
write.csv(tx_memb, file = file.path('output_data/tx_stan_acad.csv'),row.names = FALSE)
