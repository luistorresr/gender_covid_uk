#############################################################################################
########## Labour Force Survey - all datasets for 2020 with COVID-related variables #########
#############################################################################################

### R version
# R 4.0 or above

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
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(descr)) install.packages("descr", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")

library(descr) # contingency table
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
library(plotly) # interactive plots
library(formattable) # formatting tables 
library(scales) # add scale symbols to axis
library(survey) # for survey analysis

#### Output configurations
options(digits = 3) # decimal points to 3

#### Secure Access with COVID-19 related variables available from April-June Quarter

LFS20_AJ <- read_spss("./") # April to June (wave 2)
LFS20_MJ <- read_spss("./") # May to July (covid)
LFS20_JA <- read_spss("./") # June to August (covid)
LFS20_JS <- read_spss("./") # July to September (wave 3)
LFS20_AO <- read_spss("./") # August to October (covid)
LFS20_SN <- read_spss("./") # September to November (covid)
LFS20_OD <- read_spss("./") # October to December (wave 4)

#### Creating the variable with the quarter and for those not present in all datasets

LFS20_AJ <- LFS20_AJ %>% mutate(QUARTER = 4, COROROLE = NA, SJOBCORO = NA, COROUC = NA, CORUCBF = NA) # present from JS
LFS20_MJ <- LFS20_MJ %>% mutate(QUARTER = 5, COROROLE = NA, SJOBCORO = NA, COROUC = NA, CORUCBF = NA) # present from JS
LFS20_JA <- LFS20_JA %>% mutate(QUARTER = 6, COROROLE = NA, SJOBCORO = NA, COROUC = NA, CORUCBF = NA) # present from JS
LFS20_JS <- LFS20_JS %>% mutate(QUARTER = 7)
LFS20_AO <- LFS20_AO %>% mutate(QUARTER = 8)
LFS20_SN <- LFS20_SN %>% mutate(QUARTER = 9)
LFS20_OD <- LFS20_OD %>% mutate(QUARTER = 10)

#### Extracting labels and questions

# Value labels

l_LFS20_AJ <- get_labels(LFS20_AJ, values = "n") #labels April to June (wave 2)
l_LFS20_MJ <- get_labels(LFS20_MJ, values = "n") #labels May to July (covid)
l_LFS20_JA <- get_labels(LFS20_JA, values = "n") #labels June to August (covid)
l_LFS20_JS <- get_labels(LFS20_JS, values = "n") #labels July to September (wave 3)
l_LFS20_AO <- get_labels(LFS20_AO, values = "n") #labels August to October (covid)
l_LFS20_SN <- get_labels(LFS20_SN, values = "n") #labels September to November (covid)
l_LFS20_OD <- get_labels(LFS20_OD, values = "n") #labels October to December (wave 4)


# Extracting questions

q_LFS20_AJ <- as.data.frame(label(LFS20_AJ)) 
q_LFS20_MJ <- as.data.frame(label(LFS20_MJ))
q_LFS20_JA <- as.data.frame(label(LFS20_JA))
q_LFS20_JS <- as.data.frame(label(LFS20_JS))
q_LFS20_AO <- as.data.frame(label(LFS20_AO))
q_LFS20_SN <- as.data.frame(label(LFS20_SN))
q_LFS20_OD <- as.data.frame(label(LFS20_OD))


#### Selecting variables

LFS20_variables <- c(
        
        "QUARTER",       
        
        # WEIGHTS
        "PWT18", "PIWT18", 
        
        # Geographical variables  
        
        "COUNTRY", # Country within UK
        "GOVTOF", # Region (13 options)

        # BASIC FILTERS
        
        "ILODEFR", # Basic economic activity (ILO definition) (not include 4 = under 16)
        "INECAC05", # Basic economic activity (ILO definition) (extended)
        "STAT",  # Employment status (respondents currently in work or who have worked in the last eight years)
        "NOPENFLG", # (1) 16+ but not of pensionable age / (2) Pensionable age / (-9) Under 16

        # INDIVIDUAL DEMOGRAPHICS
        
        "SEX", # 1 male, 2 female
        "AGE", "AGEEUL", # age and age bands
        "ETHUK11", # Ethnicity (11 categories) UK level
        "HIQUL15D", # Highest qualification
        "NSECMJ10", # NS-SEC major group (SOC2010 based) - National Statistics Socio-economic Classification      
        
        # MAIN JOB
        
        "WRKING", # Whether in paid job in reference week, either as employee or as self-employed
        "JBAWAY", # Whether temporary away from paid job (if "no" in WRKING)
        "INDS07M", # Industry section in main job - 21 categories to link with BICS
        "MANAGER", # managerial status in current job (filter by STAT = 1)  
        "REGWKR", # Region of place of work (reported)

        
        ## Home workers (MAIN JOB)
        
        "HOME", # Whether working from home in main job (WRKING = 1 or JBAWAY = 1)
        "EVHM98", # Do you ever do any paid or unpaid work at home for your (main) job? (filter by those who do not woak at home: HOME = 2, 3 or 4)
        
        ## Teleworkers
        "TELEQA", # Used both telephone and computer to carry out work at home (HOME = 1 or 3 OR HOMED = 1 or 3)
        "TELEQB", # Whether possible to work at home without using both telephone and computer (TELEQA = 1)
        
        
        # MAIN JOB - PRECARIOUS WORK INDICATORS
        
        ## full vs part time
        
        "FTPTWK", # Whether full or part time in main job (filter for the required economics activity)
        "YPTJOB", # Reason for part time job (filter by FTPTWK = 2) 
        "PTNCRE7", # Reasons for part-time work (filter by YPTJOB = 4) - multi response variable
        "YNOTFT", # Reason not wanting full time job (filter by YPTJOB = 4)
        "YPTCIA", # Reason part-time work (filter by YNOTFT = 3, 4, or 6).
        
        ## Permanent/Temporary Employment
        
        "JOBTYP", # Whether job permanent (filter by Stat=1 AND EverWk<1)
        "WHYTMP6", # Reason for taking non-permanent job (filter by JOBTYP = 2) 
        "RESTMR6", # Reason job is temporary
        
        ## Self Employment
        
        "MSELFA", # Why respondent self employed (filter by WRKING=1 OR JBAWAY=1 AND STAT=2)
        "MSELFB", # Main reason for working self employed (if provide more than one response to MSELFA)
        
        # SECOND JOB
        
        "SECJOB", # Whether had second job in reference week
        "Y2JOB", # Whether had 2 jobs because of a change of job in reference week (this is mainly for filtering in some variables)
        "STAT2", # Employment status for those in regular second jobs
        "INDS07S", # Industry section in second job - 21 categories
        "MANAG2", # Did you have any managerial duties?
        "JOBTYP2", # Permanency of second job
        "REGWK2R", # Region of workplace for second job (reported)
        
        "HOME2", # wWhether working from home in additional job  (filter by Y2Job=2)
        "TELQA2", # Whether respondent uses both a telephone and a computer to carry out work at home ((HOME2=1 or 3 OR HOMED2=1 or 3).)
        "TELQB2", # Whether possible to work at home without using both telephone and computer (TELQA2=1)

        # HOURS
        "EVEROT", # Whether ever work paid or unpaid overtime (filter use only)
        "POTHR", # Usual hours of paid overtime (filter by EVEROT = 1)
        "UOTHR", # Usual hours of unpaid overtime (filter by EVEROT = 1)
        
        "SUMHRS", # Total hours worked in reference week in main and second jobs
                
        ## Main job 
        
        "BUSHR", # Total usual hours excluding overtime
        "TTUSHR", # Total usual hours including overtime

        ## Second job
        
        "ACTHR2", # Actual hours in second job including overtime (filter by Y2Job=2)
        
        # EARNINGS           
        "GRSSWK", # Gross weekly pay in main job
        "GRSSWK2", # Gross weekly pay in second job

        # REDUNDANCY WITHIN LAST 3 MONTHS
        
        "REDUND", #  Whether made redundant in last three months (filter for 1 in each of the next ones)
        "REDPAID", # Have you left any paid job in the last three months?
        "REDYL13", # Could you tell me the reason you left your last job? (filter by REDPAID=Yes))
        "REDYRS", # Reason regarding care services for leaving job (Filter by REDYL13 = 9 and 11)
        "REDANY", # Whether made redundant from any other job in last 3 months (filter by REDYL13 = 1, 4, 5, 6, 7, 8, 9, 10, 11 AND REDPAID = 1))
        "REDCLOS",  # Reason for leaving job left in last three months (REDYL13 = 2 OR 3 AND REDPAID = 1) OR (REDANY = 1))     
        "IN9207ER", # Conversion for industry made redundant from, sector level
        
        # COVID-19 questions
       
        # Main job changes
        "COROROLE", # (from JS20) (MAIN JOB) Has role changed substantially within current job because of the coronavirus (1 = yes / 2 = no)
        
        # second job 
        "SJOBSIX", # was the second job obtained in the last six months (only for filtering the next questions)
        "SJOBCORO", # (from JS20) Did you obtain the second job for reasons linked to coronavirus (COVID-19)?        
        
        # Redundancy 
        "CORORED", # Whether made redundant from your job for reasons related to coronavirus (filter by REDUND = 1, Redyl13 = 2, 3,  and respondent left job in last 3 months OR Redany = yes)
        
        # Sickness
        "CORO20A1", # (from AJ20 (updated in JS20)) / Whether sickness absence linked to coronavirus (Applies to those that coded minor illness or respiratory conditions, or other, at ILNE20, ILLFST20, ILLNXT20)
        "CORO20B1", # (from AJ20) / Coronavirus reason for sickness absence (CORO20A1=Yes)
        
        # Working fewer hours
        "CORO20A2", # (from AJ20 (updated in JS20)) / Whether reason worked fewer hours linked to coronavirus
        "CORO20B2", # (from AJ20 (updated in JS20)) / Reason worked fewer hours due to coronavirus (filter by CORO20A2 = 1)
        "CORFUREM", # (from AJ20 (updated in JS20)) / Whether still being paid whilst job in on hold/affected by coronavirus (Coro20a2=Yes and Stat =employee)
        "CORFUSEM", # (from AJ20 (updated in JS20)) / Whether applied for coronavirus (COVID-19) Self-employment Income Support Scheme (Coro20a2=Yes AND STAT= Self-employed)
        
        # Working more hours 
        "CORO20A3", # (from AJ20 (updated in JS20)) / Whether working more hours was linked to coronavirus
        "CORO20B3", # (from AJ20 (updated in JS20)) / Coronavirus reason for working more hours (CORO20A3=1)
        
        # Benefits 
        "BENFTS", # Whether claiming any State Benefits/Tax credits (filter by 1 for next questions)
        "UCREDIT", # Reason for claiming Universal Credit (filter for next question using all)
        "COROUC", # (from JS20) Was the reason for financial support from Universal Credit linked to coronavirus? (IF UCREDIT = all options)
        "CORUCBF", # (from JS20)Was Universal Credit being claimed before the Coronavirus (COVID-19) pandemic?

        # Earnings 
        "YPAYL20a", # Reason for pay being less than usual (only to use with CORO20A4)
        "CORO20A4", # (from AJ20 (updated in JS20)) / Was the reason for less pay linked to coronavirus (YPAYL20=1, 2, 3, 5, 9, 11, or 12)
        "CORO20B4", # (from AJ20 (updated in JS20)) / How coronavirus affected pay
        
        # SUBJECTIVE WELL-BEING
        "SATIS", # Overall, how satisfied are you with your life nowadays?
        "WORTH", # Overall, to what extent do you feel that the things you do in your life are worthwhile?
        "HAPPY", # How happy did you feel yesterday? 
        "ANXIOUS", # How anxious did you feel yesterday?
 
        ) 

#### selecting the variables in all dataset and creating temporary datasets

t4 <- LFS20_AJ %>% select(LFS20_variables)
t5 <- LFS20_MJ %>% select(LFS20_variables)
t6 <- LFS20_JA %>% select(LFS20_variables)
t7 <- LFS20_JS %>% select(LFS20_variables)
t8 <- LFS20_AO %>% select(LFS20_variables)
t9 <- LFS20_SN %>% select(LFS20_variables)
t10 <- LFS20_OD %>% select(LFS20_variables)

#### combine all temporary datasets

LFS20_all <- rbind(t4, t5, t6, t7, t8, t9, t10) # working database
q_LFS20_all <- as.data.frame(label(LFS20_all)) # variable labels
l_LFS20_all <- get_labels(LFS20_all, values = "n") # value labels 

LFS20_all <- as_tibble(LFS20_all) # creating a tibble for data manipulation


### inspecting the consolidated dataset

class(LFS20_all)
head(LFS20_all)
lapply(LFS20_all, class)

### cleaning the LFS20_all dataset 

LFS20_all_clean <- LFS20_all

LFS20_all_clean <- LFS20_all_clean %>% 
        set_na(na = c(-8, -9), drop.levels = TRUE, as.tag = FALSE) %>% 
        as_tibble(.) # NAs for: does not apply(-9), no answer(-8)

l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # check value labels 

LFS20_all_clean <- LFS20_all_clean %>% 
        set_na(na = c(POTHR = 99, UOTHR = 99, ACTHR2 = 99), 
               drop.levels = TRUE, as.tag = FALSE) %>% 
        as_tibble(.) # NAs for specific variable labels

l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # check value labels 


#### delete individual databases, questions and labels

rm(t4, t5, t6, t7, t8, t9, t10) # delete temporary datasets
rm(LFS20_AJ, LFS20_MJ, LFS20_JA, LFS20_JS, LFS20_AO, LFS20_SN, LFS20_OD) # delete databases
rm(l_LFS20_AJ, l_LFS20_MJ, l_LFS20_JA, l_LFS20_JS, l_LFS20_AO, l_LFS20_SN, l_LFS20_OD) # delete value labels
rm(q_LFS20_AJ, q_LFS20_MJ, q_LFS20_JA, q_LFS20_JS, q_LFS20_AO, q_LFS20_SN, q_LFS20_OD) # delete questions
rm(LFS20_variables) # delete the variable selection file
rm(l_LFS20_all) # delete the combined dataset labels
rm(LFS20_all) # delete the combined dataset 


### Recording and creating variables based on existing ones

## Replacing sex and ILO unemployment labels

LFS20_all_clean$SEX <- replace_labels(LFS20_all_clean$SEX, labels = c("Men" = 1, "Women" = 2)) # changing male and female

LFS20_all_clean$ILODEFR <- replace_labels(LFS20_all_clean$ILODEFR, labels = c("Not employed" = 2)) # replace ILO unemployment

l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # check value labels 


## adding labels to QUARTER

LFS20_all_clean$QUARTER <- set_labels(LFS20_all_clean$QUARTER, labels = c("Apr-Jun" = 4, "May-Jul" = 5, "Jun-Aug" = 6,
                                                                              "Jul-Sep" = 7, "Aug-Oct" = 8, "Sep-Nov" = 9,
                                                                              "Oct-Dec" = 10))

l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # check value labels 


## Recoding RESTMR6 - Reason job is temporary

LFS20_all_clean <- LFS20_all_clean %>% 
        mutate(RESTMR6_2 = recode(RESTMR6,
                                `1` = 1, 
                                `2` = 2, `3` = 2, `4` = 2, `5` = 2, `6` = 2, `7` = 2, 
                                `8` = 3, `9` = 3, `10` = 3, `11` = 3, `12` = 3, `13` = 3,
                                `14` = 4, `15` = 4, `16` = 4, `17` = 4, `18` = 4, `19` = 4,
                                `20` = 5, `21` = 5, `22` = 5, `23` = 5, `24` = 5, `25` = 5,
                                `26` = 6, `27` = 6, `28` = 6, `29` = 6, `30` = 6, `31` = 6,
                                `32` = 7))

LFS20_all_clean$RESTMR6_2 <- set_labels(LFS20_all_clean$RESTMR6_2, labels = c("Permanent job" = 1, "Seasonal job" = 2, "Fixed contract" = 3,
                                                                              "Agency temp" = 4, "Casual job" = 5, "Other temporary job" = 6,
                                                                              "Not stated" = 7))

l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # check value labels 


#### ANALYSIS

## Calculate sample size by quarter and sex

LFS20_all_clean %>%
        group_by(QUARTER, as_label(SEX)) %>%
        summarise(n = n()) 

## Calculate population estimates by quarter and sex

LFS20_all_clean %>%
        group_by(QUARTER, as_label(SEX)) %>%
        summarise(POPEST = sum(PWT18))


## QUESTION 1: Are there differences among women and men in their levels of unemployment?

# Sex and ethnicity

econ_act <- LFS20_all_clean %>%
                group_by(QUARTER, ILODEFR,ETHUK11, SEX, ) %>%
                summarise(Total = sum(PWT18)) # estimates of economic activity 


# Sex and age



# Sex and socio-economic classification







