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

#### Output configurations
options(digits = 3) # decimal points to 3

#### Secure Access with COVID-19 related variables available from April-June Quarter

LFS20_AJ_w2 <- read_spss("./") # April to June (wave 2)
LFS20_MJ_c3 <- read_spss("./") # May to July (covid)
LFS20_JA_c4 <- read_spss("./") # June to August (covid)
LFS20_JS_w3 <- read_spss("./") # July to September (wave 3)
LFS20_AO_c5 <- read_spss("./") # August to October (covid)
LFS20_SN_c6 <- read_spss("./") # September to November (covid)
LFS20_OD_w4 <- read_spss("./") # October to December (wave 4)

#### Creating the variable with the quarter number in each dataset

LFS20_AJ_w2 <- LFS20_AJ_w2 %>% mutate(QUARTER = 4)
LFS20_MJ_c3 <- LFS20_MJ_c3 %>% mutate(QUARTER = 5)
LFS20_JA_c4 <- LFS20_JA_c4 %>% mutate(QUARTER = 6)
LFS20_JS_w3 <- LFS20_JS_w3 %>% mutate(QUARTER = 7)
LFS20_AO_c5 <- LFS20_AO_c5 %>% mutate(QUARTER = 8)
LFS20_SN_c6 <- LFS20_SN_c6 %>% mutate(QUARTER = 9)
LFS20_OD_w4 <- LFS20_OD_w4 %>% mutate(QUARTER = 10)


#### Extracting labels and questions

# Value labels

l_LFS20_AJ_w2 <- get_labels(LFS20_AJ_w2, values = "n") #labels April to June (wave 2)
l_LFS20_MJ_c3 <- get_labels(LFS20_MJ_c3, values = "n") #labels May to July (covid)
l_LFS20_JA_c4 <- get_labels(LFS20_JA_c4, values = "n") #labels June to August (covid)
l_LFS20_JS_w3 <- get_labels(LFS20_JS_w3, values = "n") #labels July to September (wave 3)
l_LFS20_AO_c5 <- get_labels(LFS20_AO_c5, values = "n") #labels August to October (covid)
l_LFS20_SN_c6 <- get_labels(LFS20_SN_c6, values = "n") #labels September to November (covid)
l_LFS20_OD_w4 <- get_labels(LFS20_OD_w4, values = "n") #labels October to December (wave 4)


# Extracting questions

q_LFS20_AJ_w2 <- as.data.frame(label(LFS20_AJ_w2)) 
q_LFS20_MJ_c3 <- as.data.frame(label(LFS20_MJ_c3))
q_LFS20_JA_c4 <- as.data.frame(label(LFS20_JA_c4))
q_LFS20_JS_w3 <- as.data.frame(label(LFS20_JS_w3))
q_LFS20_AO_c5 <- as.data.frame(label(LFS20_AO_c5))
q_LFS20_SN_c6 <- as.data.frame(label(LFS20_SN_c6))
q_LFS20_OD_w4 <- as.data.frame(label(LFS20_OD_w4))


#### Selecting variables

variables <- c(
        
        "QUARTER",       
        
        # WEIGHTS
        "PWT18", "PIWT18", # data for PIWT18 is not available for the new covid series
        
        # Geographical variables  
        
        "COUNTRY", # Country within UK
        "GOVTOF", # Region (13 options)
        "URESMC", # Region of usual residence (20 options)

        # BASIC FILTERS
        
        "ILODEFR", # Basic economic activity (ILO definition) (not include 4 = under 16)
        "INECAC05", # Basic economic activity (ILO definition) (extended)
        "STAT",  # Employment status (respondents currently in work or who have worked in the last eight years)
        "FTPT", # Whether working full or part time (only for people in employment)
        "NOPENFLG", # (1) 16+ but not of pensionable age / (2) Pensionable age / (-9) Under 16

        # INDIVIDUAL DEMOGRAPHICS
        
        "SEX", # 1 male, 2 female
        "AGE", "AGEEUL", # age and age bands
        "MARSTA", # marital status
        "ETHUK11", # Ethnicity (11 categories) UK level
        "HIQUL15D", # Highest qualification
        
        # MAIN JOB
        
        "WRKING", # Whether in paid job in reference week, either as employee or as self-employed
        "JBAWAY", # Whether temporary away from paid job (if "no" in WRKING)
        "INDE07M", # Industry sectors in main job
        "MPNR02", # Number of employees at workplace
        "SC10MMJ", # Major occupation group
        "MANAGER", # managerial status in current job (filter by STAT = 1)  
        "REGWKR", # Region of place of work (reported)
        "TRVTME", # Usual home to work travel time in minutes (Those working outside the UK are recorded as ‘0’)
        
        ## Home workers (MAIN JOB)
        
        "HOME", # Whether working from home in main job (WRKING = 1 or JBAWAY = 1 or OWNBUS = 1 or RELBUS = 1)
        "EVHM98", # Whether doing paid or unpaid work at home in main job (HOME = 2, 3 or 4)
        "HOMED", # Worked at least one FULL day at home in reference week in main job (WRKING = 1 or OWNBUS = 1 or RELBUS = 1).
        "TELEQA", # Used both telephone and computer to carry out work at home (HOME = 1 or 3 OR HOMED = 1 or 3)
        "TELEQB", # Whether possible to work at home without using both telephone and computer (TELEQA = 1)
        
        # MAIN JOB - PRECARIOUS WORK INDICATORS
        
        ## full vs part time
        
        "FTPTWK", # Whether full or part time in main job (filter by STAT)
        "YPTJOB", # Reason for part time job (filter by FTPTWK = 2)
        "PTNCRE7", # Reasons for part-time work (filter by YPTJOB = 4)
        "YNOTFT", # Reason not wanting full time job (filter by YPTJOB = 4)
        "YPTCIA", # Reason part-time work (filter by YNOTFT = 3, 4, or 6).
        
        ## Permanent/Temporary Employment
        
        "JOBTYP", # Whether job permanent (filter by Stat=Emp AND EverWk<Yes)
        "JBTP10", # Way in which job was not permanent (filter by JOBTYP=2)
        "WHYTMP6", # Reason for taking non-permanent job (filter by JOBTYP = 2)
        "TEMLEN", # Length of non-permanent job (filter by JOBTYP = 2)
        "AGWRK", # Whether agency worker (filter by STAT= 1 OR EVERWK <Yes AND JOBTYP=1)
        
        ## Self Employment
        
        "MSELFA", # Why respondent self employed (filter by WRKING=1 OR JBAWAY=1 AND STAT=2)
        "MSELFB", # Main reason for working self employed (if provide more than one response to MSELFA)
        
        # SECOND JOB
        
        "SECJOB", # Whether had second job in reference week
        "Y2JOB", # Whether had 2 jobs because of a change of job in reference week (this is mainly for filtering in some variables)
        "STAT2", # Employment status for those in regular second jobs
        "SC10SMJ", # Major occupation group 
        "INDS07S", # Industry section in second job 
        "MANAG2", # Did you have any managerial duties?
        "MPNSR02", # Number of employees second job
        "JOBTYP2", # Permanency of second job
        "REGWK2R", # Region of workplace for second job (reported)
        
        "HOME2", # wWhether working from home in additional job  (filter by Y2Job=2)
        "HOMED2", # Work at least one day at home in reference week (filter by Y2Job=2)
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
        "INDE07R", #  Industry sectors in job made redundant from
        
        # COVID-19
       
        # Main job changes
        "COROROLE", # (from JS20) (MAIN JOB) Has role changed substantially within current job because of the coronavirus (1 = yes / 2 = no)
        
        # second job 
        "SJOBSIX", # was the second job obtained in the last six months (only for filtering the next questions)
        "SJOBCORO", # Did you obtain the second job for reasons linked to coronavirus (COVID-19)?        
        
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
        "COROUC", # Was the reason for financial support from Universal Credit linked to coronavirus? (IF UCREDIT = all options)
        "CORUCBF", # Was Universal Credit being claimed before the Coronavirus (COVID-19) pandemic?

        # Earnings 
        "YPAYL20a", # Reason for pay being less than usual (only to use with CORO20A4)
        "CORO20A4", # (from AJ20 (Waves 1 and 5 only) updated in JS20) / Was the reason for less pay linked to coronavirus (YPAYL20=1, 2, 3, 5, 9, 11, or 12)
        "CORO20B4", # (from AJ20 (Waves 1 and 5 only) updated in JS20) / How coronavirus affected pay
        
        # SUBJECTIVE WELL-BEING
        "SATIS", # Overall, how satisfied are you with your life nowadays?
        "WORTH", # Overall, to what extent do you feel that the things you do in your life are worthwhile?
        "HAPPY", # How happy did you feel yesterday? 
        "ANXIOUS", # How anxious did you feel yesterday?
 
        ) 


