#### packages needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("girdExtra", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(sjlabelled)) install.packages("sjlabelled", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(doBy)) install.packages("doBy", repos = "http://cran.us.r-project.org")
if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")

install.packages('devtools')
devtools::install_github('bbc/bbplot') # 

library(tidyverse) # several tools for data manipulation
library(haven) # read SPSS, Stata, and SAS data
library(gridExtra) # arrange multiple grid-based plots on a page
library(corrr) # for correlations correlate()
library(Hmisc) # many functions useful for data analysis
library(sjmisc) # useful to set Na's
library(sjlabelled) # get_labels function
library(ggthemes) # themes for ggplot2
library(ggrepel) # help with the position of the labels on ggplot2 graph
library(doBy) # 'do' something to data stratified 'by' some variables
library(bbplot) # BBC style plots

#### Output configurations
options(digits = 3) # decimal points to 3

#### Labour Force Survey Data 2020 - UKDS End User Licence

LFS20_JM_w1 <- read_spss("./Datasets_UKDS/1-Jan-Mar-W1/UKDA-8639-spss/spss/spss25/lfsp_jm20_eul_pwt18.sav") # January to March (wave 1)
LFS20_FA_c1 <- read_spss("./Datasets_UKDS/2-Feb-Apr-Corona/UKDA-8646-spss/spss/spss25/lfsp_fa20_eul_pwt18.sav") # February to April (covid)
LFS20_MM_c2 <- read_spss("./Datasets_UKDS/3-Mar-May-Corona/UKDA-8659-spss/spss/spss25/lfsp_mm20_eul_pwt18.sav") # March to May (covid)
LFS20_AJ_w2 <- read_spss("./Datasets_UKDS/4-Apr-Jun-W2/UKDA-8671-spss/spss/spss25/lfsp_aj20_eul_pwt18.sav") # April to June (wave 2)
LFS20_MJ_c3 <- read_spss("./Datasets_UKDS/5-May-Jul-Corona/UKDA-8707-spss/spss/spss25/lfsp_mj20_eul_pwt18.sav") # May to July (covid)
LFS20_JA_c4 <- read_spss("./Datasets_UKDS/6-Jun-Aug-Corona/UKDA-8706-spss/spss/spss25/lfsp_ja20_eul_pwt18.sav") # June to August (covid)
LFS20_JS_w3 <- read_spss("./Datasets_UKDS/7-Jul-Sep-W3/UKDA-8720-spss/spss/spss25/lfsp_js20_eul_pwt18.sav") # July to September (wave 3)
LFS20_AO_c5 <- read_spss("./Datasets_UKDS/8-Aug-Oct-Corona/UKDA-8738-spss/spss/spss25/lfsp_ao20_eul_pwt18.sav") # August to October (covid)
LFS20_SN_c6 <- read_spss("./Datasets_UKDS/9-Sep-Nov-Corona/UKDA-8760-spss/spss/spss25/lfsp_sn20_eul_pwt18.sav") # September to November (covid)

#### Creating the variable "quarter" in each dataset and an empty "PIWT18" for all covid datasets

LFS20_JM_w1 <- LFS20_JM_w1 %>% mutate(QUARTER = "Q1")
LFS20_FA_c1 <- LFS20_FA_c1 %>% mutate(QUARTER = "Q2", PIWT18 = NA)
LFS20_MM_c2 <- LFS20_MM_c2 %>% mutate(QUARTER = "Q3", PIWT18 = NA)
LFS20_AJ_w2 <- LFS20_AJ_w2 %>% mutate(QUARTER = "Q4")
LFS20_MJ_c3 <- LFS20_MJ_c3 %>% mutate(QUARTER = "Q5", PIWT18 = NA)
LFS20_JA_c4 <- LFS20_JA_c4 %>% mutate(QUARTER = "Q6", PIWT18 = NA)
LFS20_JS_w3 <- LFS20_JS_w3 %>% mutate(QUARTER = "Q7")
LFS20_AO_c5 <- LFS20_AO_c5 %>% mutate(QUARTER = "Q8", PIWT18 = NA)
LFS20_SN_c6 <- LFS20_SN_c6 %>% mutate(QUARTER = "Q9", PIWT18 = NA)


#### Extracting labels and questions

# Labels

l_LFS20_JM_w1 <- get_labels(LFS20_JM_w1, values = "n") #labels January to March (wave 1)
l_LFS20_FA_c1 <- get_labels(LFS20_FA_c1, values = "n") #labels February to April (covid)
l_LFS20_MM_c2 <- get_labels(LFS20_MM_c2, values = "n") #labels March to May (covid)
l_LFS20_AJ_w2 <- get_labels(LFS20_AJ_w2, values = "n") #labels April to June (wave 2)
l_LFS20_MJ_c3 <- get_labels(LFS20_MJ_c3, values = "n") #labels May to July (covid)
l_LFS20_JA_c4 <- get_labels(LFS20_JA_c4, values = "n") #labels June to August (covid)
l_LFS20_JS_w3 <- get_labels(LFS20_JS_w3, values = "n") #labels July to September (wave 3)
l_LFS20_AO_c5 <- get_labels(LFS20_AO_c5, values = "n") #labels August to October (covid)
l_LFS20_SN_c6 <- get_labels(LFS20_SN_c6, values = "n") #labels September to November (covid)

# Questions

q_LFS20_JM_w1 <- as.data.frame(label(LFS20_JM_w1))
q_LFS20_FA_c1 <- as.data.frame(label(LFS20_FA_c1))
q_LFS20_MM_c2 <- as.data.frame(label(LFS20_MM_c2))
q_LFS20_AJ_w2 <- as.data.frame(label(LFS20_AJ_w2))
q_LFS20_MJ_c3 <- as.data.frame(label(LFS20_MJ_c3))
q_LFS20_JA_c4 <- as.data.frame(label(LFS20_JA_c4))
q_LFS20_JS_w3 <- as.data.frame(label(LFS20_JS_w3))
q_LFS20_AO_c5 <- as.data.frame(label(LFS20_AO_c5))
q_LFS20_SN_c6 <- as.data.frame(label(LFS20_SN_c6))


#### Selecting variables

# common to all datasets and without weights


variables <- c(
        # case identifiers
        "QUARTER", "IOUTCOME", # interview outcome
        "COUNTRY", "URESMC", # Country within UK and Region of usual residence
        
        # respondent characteristics
        "SEX", # 1 male, 2 female
        "AGE", "AGEEUL", # age in number and categories
        "MARSTA", # marital status
        "RELIG11", # religion GB level
        "NATOX7_EUL_main", "NATOX7_EUL_Sub", # nationality (main and subdivisions)
        "ETHUKEUL", # ethnic group
        "CRYOX7_EUL_Main", "CRYOX7_EUL_Sub", # country of birth variables       
        "QUAL_1", "QUAL_9", "HIGHO", # if they have a degree, or below degree levels, and what highest level.
        "TEN1", # Accommodation details
        
        # general employment characteristics 
        "ILODEFR", # Basic economic activity
        "FTPT", # Whether working full or part time (only for people in employment)
        "DURUN2", # Duration of unemployment
        
        # main job in reference week 
        "STAT",  # Employment status
        "WRKING", # Whether in paid job in reference week, either as employee or as self-employed
        "JBAWAY", # Whether temporary away from paid job (if "no" in WRKING)
        "INDE07M", # Industry sectors in main job
        "PUBLICR", # Whether working in public or private sector 
        "SC10MMJ", # Major occupation group (main job)
        "MANAGER", # managerial status
        "HOME", # Whether working from home in main job
        "GORWKR", # Region of place of work
        "SAMELAD", # Whether lives and works in same Local Authority District
   
        # second job
        "SECJOB", # second job in the past week
        "STAT2", # Employment status in second job (reported)
        "Y2JOB", # Whether had two jobs because of a change of job in reference week
        "SC10SMJ", # Major occupation group (second job)
        "INDS07S", # Industry section in second job 
        "MANAG2", # Did you have any managerial duties?
        "HOME2", # work location second job   
        "GORWK2R", # Region of workplace for second job (reported)

        # Total hours for everyone
        "BUSHR", # Total usual hours for everyone excluding overtime in main job 
        "TTUSHR", # Total usual hours for everyone including overtime in main job 
        "SUMHRS", # Total hours worked in reference week in main and second jobs

        # Hours only for people who work overtime
        "TOTUS2", # Total usual hours including overtime  
        "POTHR", #  Usual hours of paid overtime
        "UOTHR", # Usual hours of unpaid overtime
                        
        # earnings if in work in reference week
        "GROSS99", "GRSEXP", "GRSPRD", # amount gross paid, gross expected, period covered
        "NET99", "NETPRD", # take home pay, period covered
        "HOURLY", "HRRATE", # paid by hour? and, if yes, rate
        
        # earnings in second job in reference week      
        "SECGRO", "SECGA", #  gross paid and period covered
        "SECEX", "SECGB", #  gross expected and period covered
        "SECNET", "SCNTGA", # take home pay, period covered
        "HOURLY2", "HRRATE2", # paid by hour? and, if yes, rate
        
        
        # redundancy in the last 3 months  
        "REDUND", #  Whether made redundant in last three months (filter for 1 in each of the next ones)
        "REDPAID", # Have you left any paid job in the last three months?
        "REDYL13", # Could you tell me the reason you left your last job?
        "REDCLOS",  # Reason for leaving job left in last three months
        "INDE07R", #  Industry sectors in job made redundant from
        "REDOCC", # Whether occupation made redundant from is same as previously stated        
        
        # weights
        "PWT18", "PIWT18" # data for PIWT18 is not available for the new covid series
) 






#### selecting the variables in all dataset and creating temporary datasets

t1 <- LFS20_JM_w1 %>% select(variables) 
t2 <- LFS20_FA_c1 %>% select(variables)
t3 <- LFS20_MM_c2 %>% select(variables)
t4 <- LFS20_AJ_w2 %>% select(variables)
t5 <- LFS20_MJ_c3 %>% select(variables)
t6 <- LFS20_JA_c4 %>% select(variables)
t7 <- LFS20_JS_w3 %>% select(variables)
t8 <- LFS20_AO_c5 %>% select(variables)
t9 <- LFS20_SN_c6 %>% select(variables)

#### combine all temporary datasets

LFS20_all <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9) # working database

q_LFS20_all <- as.data.frame(label(LFS20_all)) # variable labels

rm(t1, t2, t3, t4, t5, t6, t7, t8, t9) # delete temporary datasets

LFS20_all <- as_tibble(LFS20_all)

### inspecting working data

class(LFS20_all)
head(LFS20_all)
lapply(LFS20_all, class)

### cleaning the LFS20_all dataset 

LFS20_all_clean <- LFS20_all

LFS20_all_clean <- LFS20_all_clean %>% 
        set_na(na = c(-8, -9), drop.levels = TRUE, as.tag = FALSE) %>% 
        as_data_frame(.) # NAs for: does not apply(-9), no answer(-8)

LFS20_all_clean <- LFS20_all_clean %>% 
        set_na(na = c(USUHR = 99, POTHR = 99, UOTHR = 99, GROSS99 = c(99996, 99998, 99999), 
                      GRSEXP = c(99998, 9999), NET99 = c(99998, 99999), HRRATE = c(998, 999), 
                      SECGRO = c(99996, 99998, 99999), SECEX = c(99998, 99999), SECNET = c(99998, 99999),
                      HRRATE2 = c(998, 999)), 
               drop.levels = TRUE, as.tag = FALSE) %>% 
        as_data_frame(.) # NAs for specific variable labels


l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # value labels 



#### Analysis 




### descriptives

LFS20_all_clean %>% mean(AGE)


### graphs 










