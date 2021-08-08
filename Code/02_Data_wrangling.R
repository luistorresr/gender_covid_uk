##################################################################################################
#########################Labour Force Survey - all datasets from 2019 #############################
################ /###################
####################################################################################################

# There is no need to run this script as the resulted database is made available for the analysis.
# To run this script, the UK LFS datasets from 2019 onwards are needed. They are available at https://ukdataservice.ac.uk

### Data files

LFS19_JM <- read_spss("./Data/LFS_UKDS/a1-Jan-Mar19/UKDA-8485-spss/spss/spss24/lfsp_jm19_eul_pwt18.sav") # January to March (wave 1)
LFS19_AJ <- read_spss("./Data/LFS_UKDS/a2-Apr-Jun19/UKDA-8563-spss/spss/spss24/lfsp_aj19_eul_pwt18.sav") # April to June (wave 2)
LFS19_JS <- read_spss("./Data/LFS_UKDS/a3-Jul-Sep19/UKDA-8588-spss/spss/spss24/lfsp_js19_eul.sav") # July to September (wave 3)
LFS19_OD <- read_spss("./Data/LFS_UKDS/a4-Oct-Dec19/UKDA-8614-spss/spss/spss25/lfsp_od19_eul.sav") # October to December (wave 4)

LFS20_JM <- read_spss("./Data/LFS_UKDS/1-Jan-Mar-W1/UKDA-8639-spss/spss/spss25/lfsp_jm20_eul_pwt18.sav") # January to March (wave 1)
LFS20_FA <- read_spss("./Data/LFS_UKDS/2-Feb-Apr-Corona/UKDA-8646-spss/spss/spss25/lfsp_fa20_eul_pwt18.sav") # February to April (covid)
LFS20_MM <- read_spss("./Data/LFS_UKDS/3-Mar-May-Corona/UKDA-8659-spss/spss/spss25/lfsp_mm20_eul_pwt18.sav") # March to May (covid)
LFS20_AJ <- read_spss("./Data/LFS_UKDS/4-Apr-Jun-W2/UKDA-8671-spss/spss/spss25/lfsp_aj20_eul_pwt18.sav") # April to June (wave 2)
LFS20_MJ <- read_spss("./Data/LFS_UKDS/5-May-Jul-Corona/UKDA-8707-spss/spss/spss25/lfsp_mj20_eul_pwt18.sav") # May to July (covid)
LFS20_JA <- read_spss("./Data/LFS_UKDS/6-Jun-Aug-Corona/UKDA-8706-spss/spss/spss25/lfsp_ja20_eul_pwt18.sav") # June to August (covid)
LFS20_JS <- read_spss("./Data/LFS_UKDS/7-Jul-Sep-W3/UKDA-8720-spss/spss/spss25/lfsp_js20_eul_pwt18.sav") # July to September (wave 3)
LFS20_AO <- read_spss("./Data/LFS_UKDS/8-Aug-Oct-Corona/UKDA-8738-spss/spss/spss25/lfsp_ao20_eul_pwt18.sav") # August to October (covid)
LFS20_SN <- read_spss("./Data/LFS_UKDS/9-Sep-Nov-Corona/UKDA-8760-spss/spss/spss25/lfsp_sn20_eul_pwt18.sav") # September to November (covid)
LFS20_OD <- read_spss("./Data/LFS_UKDS/10-Oct-Dec-W4/UKDA-8777-spss/spss/spss25/lfsp_od20_eul_pwt18.sav") # October to December (wave 4)

LFS21_NJ <- read_spss("./Data/LFS_UKDS/11-Nov-Jan-Corona/UKDA-8788-spss/spss/spss25/lfsp_n20j_eul_pwt18.sav") # November 2020 to January 2021
LFS21_DF <- read_spss("./Data/LFS_UKDS/12-Dec-Feb-Corona/UKDA-8794-spss/spss/spss25/lfsp_d20f_eul_pwt18.sav") # December 2020 to February 2021
LFS21_JM <- read_spss("./Data/LFS_UKDS/13-Jan-Mar21/UKDA-8806-spss/spss/spss25/lfsp_jm21_eul_pwt18.sav") # January to March 2021
LFS21_FA <- read_spss("./Data/LFS_UKDS/14-Feb-Apr21/UKDA-8813-spss/spss/spss25/lfsp_fa21_eul_pwt18.sav") # February to April 2021


#### Extracting labels and questions for inspection

# Labels

l_LFS19_JM <- get_labels(LFS19_JM, values = "n")
l_LFS19_AJ <- get_labels(LFS19_AJ, values = "n")
l_LFS19_JS <- get_labels(LFS19_JS, values = "n")
l_LFS19_OD <- get_labels(LFS19_OD, values = "n")

l_LFS20_JM <- get_labels(LFS20_JM, values = "n") #labels January to March (wave 1)
l_LFS20_FA <- get_labels(LFS20_FA, values = "n") #labels February to April (covid)
l_LFS20_MM <- get_labels(LFS20_MM, values = "n") #labels March to May (covid)
l_LFS20_AJ <- get_labels(LFS20_AJ, values = "n") #labels April to June (wave 2)
l_LFS20_MJ <- get_labels(LFS20_MJ, values = "n") #labels May to July (covid)
l_LFS20_JA <- get_labels(LFS20_JA, values = "n") #labels June to August (covid)
l_LFS20_JS <- get_labels(LFS20_JS, values = "n") #labels July to September (wave 3)
l_LFS20_AO <- get_labels(LFS20_AO, values = "n") #labels August to October (covid)
l_LFS20_SN <- get_labels(LFS20_SN, values = "n") #labels September to November (covid)
l_LFS20_OD <- get_labels(LFS20_OD, values = "n") #labels October to December (wave 4)

l_LFS21_NJ <- get_labels(LFS21_NJ, values = "n") #labels Nov 2020 to Jan 2021
l_LFS21_DF <- get_labels(LFS21_DF, values = "n") #labels Dec 2020 to Feb 2021
l_LFS21_JM <- get_labels(LFS21_JM, values = "n") #labels Jan to Mar 2021
l_LFS21_FA <- get_labels(LFS21_FA, values = "n") #labels Feb to Apr 2021


# Questions

q_LFS19_JM <- as.data.frame(label(LFS19_JM))
q_LFS19_AJ <- as.data.frame(label(LFS19_AJ))
q_LFS19_JS <- as.data.frame(label(LFS19_JS))
q_LFS19_OD <- as.data.frame(label(LFS19_OD))

q_LFS20_JM <- as.data.frame(label(LFS20_JM))
q_LFS20_FA <- as.data.frame(label(LFS20_FA))
q_LFS20_MM <- as.data.frame(label(LFS20_MM))
q_LFS20_AJ <- as.data.frame(label(LFS20_AJ))
q_LFS20_MJ <- as.data.frame(label(LFS20_MJ))
q_LFS20_JA <- as.data.frame(label(LFS20_JA))
q_LFS20_JS <- as.data.frame(label(LFS20_JS))
q_LFS20_AO <- as.data.frame(label(LFS20_AO))
q_LFS20_SN <- as.data.frame(label(LFS20_SN))
q_LFS20_OD <- as.data.frame(label(LFS20_OD))

q_LFS21_NJ <- as.data.frame(label(LFS21_NJ))
q_LFS21_DF <- as.data.frame(label(LFS21_DF))
q_LFS21_JM <- as.data.frame(label(LFS21_JM))
q_LFS21_FA <- as.data.frame(label(LFS21_FA))

        
## creating weeks

LFS19_JM <- LFS19_JM %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 1, `2`= 2, `3`=3, `4`=4, 
                               `5`=5, `6`=6, `7`= 7, `8`= 8, `9`= 9, 
                               `10`= 10, `11`= 11, `12`= 12, `13`= 13)) # calendar quarter 1 2019

LFS19_AJ <- LFS19_AJ %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK), 
                               `1`= 14, `2`= 15, `3`=16, `4`=17, 
                               `5`=18, `6`=19, `7`=20, `8`= 21, `9`= 22, 
                               `10`= 23, `11`= 24, `12`= 25, `13`= 26)) # calendar quarter 2 2019


LFS19_JS <- LFS19_JS %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK), 
                               `1`= 27, `2`= 28, `3`= 29, `4`= 30, 
                               `5`= 31, `6`= 32, `7`= 33, `8` = 34, `9`= 35, 
                               `10`= 36, `11`= 37, `12`= 38, `13`= 39))  # calendar quarter 3 2019

LFS19_OD <- LFS19_OD %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK), 
                               `1`= 40, `2`= 41, `3`= 42, `4`= 43, 
                               `5`= 44, `6`= 45, `7`= 46, `8` = 47, `9`= 48,
                               `10`= 49, `11`= 50, `12`= 51, `13`= 52)) # calendar quarter 4 2019

LFS20_JM <- LFS20_JM %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 1, `2`= 2, `3`=3, `4`=4, 
                               `5`=5, `6`=6, `7`= 7, `8`= 8, `9`= 9, 
                               `10`= 10, `11`= 11, `12`= 12, `13`= 13)) # calendar quarter 1 2020

LFS20_FA <- LFS20_FA %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`=5, `2`=6, `3`= 7, `4`= 8, `5`= 9,
                               `6`= 10, `7`= 11, `8`= 12, `9`= 13, 
                               `10`=14, `11`=15, `12`= 16, `13`= 17)) # non calendar quarter 

LFS20_MM <- LFS20_MM %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 10, `2`= 11, `3`= 12, `4`= 13, 
                               `5`=14, `6`=15, `7`= 16, `8`= 17,
                                `9`= 18, `10`= 19, `11`= 20, `12`= 21, `13`= 22)) # non calendar quarter 

LFS20_AJ <- LFS20_AJ %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK), 
                               `1`= 14, `2`= 15, `3`=16, `4`=17, 
                               `5`=18, `6`=19, `7`=20, `8`= 21, `9`= 22, 
                               `10`= 23, `11`= 24, `12`= 25, `13`= 26)) # calendar quarter 2
 
LFS20_MJ <- LFS20_MJ %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`=18, `2`=19, `3`=20, `4`= 21, `5`= 22, 
                               `6`= 23, `7`= 24, `8`= 25, `9`= 26,
                               `10`= 27, `11`= 28, `12`= 29, `13`= 30)) # non calendar quarter  

LFS20_JA <- LFS20_JA %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 23, `2`= 24, `3`= 25, `4`= 26,
                               `5`= 27, `6`= 28, `7`= 29, `8`= 30,
                               `9`= 31, `10`= 32, `11`= 33, `12`= 34, `13`= 35)) # non calendar quarter 

LFS20_JS <- LFS20_JS %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK), 
                               `1`= 27, `2`= 28, `3`= 29, `4`= 30, 
                               `5`= 31, `6`= 32, `7`= 33, `8` = 34, `9`= 35, 
                               `10`= 36, `11`= 37, `12`= 38, `13`= 39))  # calendar quarter 3

LFS20_AO <- LFS20_AO %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 31, `2`= 32, `3`= 33, `4` = 34, `5`= 35, 
                               `6`= 36, `7`= 37, `8`= 38, `9`= 39,
                               `10`= 40, `11`= 41, `12`= 42, `13`= 43)) # non calendar quarter 

LFS20_SN <- LFS20_SN %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 36, `2`= 37, `3`= 38, `4`= 39,
                               `5`= 40, `6`= 41, `7`= 42, `8`= 43,
                               `9` = 44, `10`= 45, `11`= 46, `12`= 47, `13`= 48)) # non calendar quarter 

LFS20_OD <- LFS20_OD %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK), 
                               `1`= 40, `2`= 41, `3`= 42, `4`= 43, 
                               `5`= 44, `6`= 45, `7`= 46, `8` = 47, `9`= 48,
                               `10`= 49, `11`= 50, `12`= 51, `13`= 52)) # calendar quarter 4

LFS21_NJ <- LFS21_NJ %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 44, `2`= 45, `3`= 46, `4` = 47, `5`= 48,
                               `6`= 49, `7`= 50, `8`= 51, `9`= 52,
                               `10`= 1, `11`= 2, `12`= 3, `13`= 4)) # non calendar quarter  

LFS21_DF  <- LFS21_DF %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 49, `2`= 50, `3`= 51, `4`= 52,
                               `5`= 1, `6`= 2, `7`= 3, `8`= 4,
                               `9`= 5, `10`= 6, `11`= 7, `12`= 8, `13`= 9)) # non calendar quarter 

LFS21_JM <- LFS21_JM %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`= 1, `2`= 2, `3`= 3, `4`= 4, 
                               `5`= 5, `6`= 6, `7`= 7, `8`= 8, `9`= 9, 
                               `10`= 10, `11`= 11, `12`= 12, `13`= 13)) # calendar quarter 1 2020

LFS21_FA <- LFS21_FA %>% 
        mutate(YWEEKS = recode(as.numeric(WEEK),  
                               `1`=5, `2`=6, `3`= 7, `4`= 8, `5`= 9,
                               `6`= 10, `7`= 11, `8`= 12, `9`= 13, 
                               `10`=14, `11`=15, `12`= 16, `13`= 17)) # non calendar quarter 


#### Creating the variable "quarter", and empty "PIWT18" as well as YWEEKS for all covid datasets

LFS19_JM <- LFS19_JM %>% mutate(QUARTER = 1, YEAR = 2019, FLED10 = NA)
LFS19_AJ <- LFS19_AJ %>% mutate(QUARTER = 2, YEAR = 2019)
LFS19_JS <- LFS19_JS %>% mutate(QUARTER = 3, YEAR = 2019, FLED10 = NA)
LFS19_OD <- LFS19_OD %>% mutate(QUARTER = 4, YEAR = 2019)

LFS20_JM <- LFS20_JM %>% mutate(QUARTER = 5, YEAR = 2020, FLED10 = NA)
LFS20_FA <- LFS20_FA %>% mutate(QUARTER = 6, YEAR = 2020, PIWT18 = NA)
LFS20_MM <- LFS20_MM %>% mutate(QUARTER = 7, YEAR = 2020, PIWT18 = NA)
LFS20_AJ <- LFS20_AJ %>% mutate(QUARTER = 8, YEAR = 2020)
LFS20_MJ <- LFS20_MJ %>% mutate(QUARTER = 9, YEAR = 2020, PIWT18 = NA)
LFS20_JA <- LFS20_JA %>% mutate(QUARTER = 10, YEAR = 2020, PIWT18 = NA)
LFS20_JS <- LFS20_JS %>% mutate(QUARTER = 11, YEAR = 2020, FLED10 = NA)
LFS20_AO <- LFS20_AO %>% mutate(QUARTER = 12, YEAR = 2020, PIWT18 = NA)
LFS20_SN <- LFS20_SN %>% mutate(QUARTER = 13, YEAR = 2020, PIWT18 = NA)
LFS20_OD <- LFS20_OD %>% mutate(QUARTER = 14, YEAR = 2020)

LFS21_NJ <- LFS21_NJ %>% mutate(QUARTER = 15, YEAR = 2020, PIWT18 = NA)
LFS21_DF <- LFS21_DF %>% mutate(QUARTER = 16, YEAR = 2020, PIWT18 = NA)

LFS21_JM <- LFS21_JM %>% mutate(QUARTER = 17,YEAR = 2021, 
                                NSECMJ10 = NSECMJ20, # new variable to standardise to the old name
                                SC10MMJ = SC20MMJ, # new variable to standardise to the old name
                                SC10SMJ = SC20SMJ) # new variable to standardise to the old name

LFS21_FA <- LFS21_FA %>% mutate(QUARTER = 18, YEAR = 2021, PIWT18 = NA, 
                                NSECMJ10 = NSECMJ20, # new variable to standardise to the old name
                                SC10MMJ = SC20MMJ, # new variable to standardise to the old name
                                SC10SMJ = SC20SMJ) # new variable to standardise to the old name

#### Selecting variables

# common to all datasets

variables <- c(
       
         # Case identifiers
        "YEAR",
        "QUARTER",
        "WEEK",
        "YWEEKS",
        "IOUTCOME", # interview outcome
        
        # BASIC FILTERS
        
        "ILODEFR", # Basic economic activity (ILO definition) (not include 4 = under 16)
        "INECAC05", # Basic economic activity (ILO definition) (extended)
        "STAT",  # Employment status (respondents currently in work or who have worked in the last eight years)

        # DEMOGRAPHICS
        
        "COUNTRY",  # Country within UK
        "GOVTOF2", # gov region
        
        # Respondent characteristics
        
        "SEX", # 1 male, 2 female
        "AGE", # number
        "AGEEUL", # age bands
        "ETHUKEUL", # ethnic group
        "DISEA", # Disability: equality act
        "NSECMJ10", # NS-SEC major group (SOC2010 based) - National Statistics Socio-economic Classification   


        # MAIN JOB
        
        "STATR", # Employment status in main job (reported)
        "WRKING", # Whether in paid job in reference week, either as employee or as self-employed
        "JBAWAY", # Whether temporary away from paid job (if "no" in WRKING)
        "PUBLICR", # Whether working in public or private sector (reported)
        "SECTOR", # Whether working for private firm or business
        "SECTRO03", # Type of non-private organisation
        "INDE07M", # Industry section in main job 
        "SC10MMJ", # Major occupation group
        "MANAGER", # managerial status in current job (filter by STAT = 1)  
        "REGWKR", # Region of place of work (reported)

        # MAIN JOB - PRECARIOUS WORK INDICATORS
        
        ## full vs part time (complement with hours data)
        
        "FTPTWK", # Whether full or part time in main job (filter for the required economics activity)        
        "YPTJOB", # Reason for part time job (filter by FTPTWK = 2) 

        ## Permanent/Temporary Employment
        
        "JOBTYP", # Whether job permanent (filter by Stat=1 AND EverWk<1)
        "AGWRK", # Whether agency worker (applies to JOBTYP = 1)
        "WHYTMP6", # Reason for taking non-permanent job (filter by JOBTYP = 2) 
        "RESTMR6", # Reason job is temporary - Way in which job was not permanent
        "TEMLEN", # Length of non-permanent job
        "EMPLEN", # Length of time continuously employed
        "EMPMON", # Months continuously employed      
        
        ## work arrangements
        
        "FLED10", # Type of agreed work arrangement

        # SECOND JOB
        
        "SECJOB", # Whether had second job in reference week
        "Y2JOB", # Whether had 2 jobs because of a change of job in reference week (this is mainly for filtering in some variables)
        "STAT2", # Employment status for those in regular second jobs
        "INDS07S", # Industry section in second job - 21 categories
        "MANAG2", # Did you have any managerial duties?
        "JOBTYP2", # Permanency of second job
        "REGWK2R", # Region of workplace for second job (reported)
        "SC10SMJ", # Major occupation group (second job)
        
        # HOURS

        "EVEROT", # Whether ever work paid or unpaid overtime (filter use only)
        "POTHR", # Usual hours of paid overtime (filter by EVEROT = 1)
        "UOTHR", # Usual hours of unpaid overtime (filter by EVEROT = 1)
        
        "SUMHRS", # Total hours worked in reference week in main and second jobs

        "VARYHR", # Whether weekly hours tend to vary
        
        ## Main job 
        
        "BUSHR", # Total usual hours excluding overtime
        "TTUSHR", # Total usual hours including overtime
        "PAIDHRU", # Paid hours - based on usual hours per week, including paid overtime only
        "POTHR", # Usual hours of paid overtime
        "UOTHR", # Usual hours of unpaid overtime
        
        "BACTHR", # Basic actual hours in main job (per week)- It does not include overtime (paid or unpaid).
        "TTACHR", # Total actual hours worked in main job in reference week includes any paid or unpaid overtime
        "PAIDHRA", # Paid Hours - based on actual hours per week
        
        ## Second job
        
        "ACTHR2", # Actual hours in second job including overtime (filter by Y2Job=2)

        # EARNINGS           
        "GRSSWK", # Gross weekly pay in main job
        "GRSSWK2", # Gross weekly pay in second job
        
        # REDUNDANCY WITHIN LAST 3 MONTHS
        
        "REDUND", #  Whether made redundant in last three months (filter for 1 in each of the next ones)
        "REDPAID", # Have you left any paid job in the last three months?
        "REDSTAT", # Status in job (made redundant from) - applies to (((REDYL13 = 2 OR 3) AND REDPAID = 1) OR (REDANY = 1)).
        "REDYL13", # Could you tell me the reason you left your last job? (filter by REDPAID=Yes))
        "REDANY", # Whether made redundant from any other job in last 3 months (filter by REDYL13 = 1, 4, 5, 6, 7, 8, 9, 10, 11 AND REDPAID = 1))
        "REDCLOS",  # Reason for leaving job left in last three months (REDYL13 = 2 OR 3 AND REDPAID = 1) OR (REDANY = 1))     
        "INDE07R", #  Industry sectors in job made redundant from
        
        # self-employment
        
        ### NOTE: partime, redundancy, hours, income, seeking also applies
        
        "SELF1", # Other methods of payment aside from receiving a salary or wage direct from an employer
        "SELF2", # Other methods of payment aside from receiving a salary or wage direct from an employer
        "SELF3", # Other methods of payment aside from receiving a salary or wage direct from an employer
        "SELF4", # Other methods of payment aside from receiving a salary or wage direct from an employer
        "SOLOR", # Self-employed with or without employees (reported)
        "ONETEN", # Number (1 – 10) of employees working for self employed person. Filter by "SOLOR" ==2
        "CONSEY", # Year started as continuously self employed
        "SOLO2", # Working alone or employees in second job (reported)
        
        ## Seeking for work
        "DIFJOB", # Whether looking for different or additional paid job or business
        "TYEMPS", # Type of employment sought (DIFJOB = 1)
        "LKSELA", # Whether looking for work as employee or self employed
        "ADDJOB", # Whether seeking replacement or additional job (DIFJOB = 1)
        "UNDEMP", # Whether would like to work longer hours, at current basic rate of pay, given the opportunity
        "UNDHRS", # Number of extra hours would like to work
        
        # weights
        "PWT18", "PIWT18" # data for PIWT18 is not available for the new covid series
        ) 

#### selecting the variables in all dataset and creating temporary datasets

t1a <- LFS19_JM %>% select(variables) # FLED10 does not exist 
t2a <- LFS19_AJ %>% select(variables) # `                     
t3a <- LFS19_JS %>% select(variables) # FLED10 does not exist 
t4a <- LFS19_OD %>% select(variables) #                       

t1 <- LFS20_JM %>% select(variables) # FLED10 does not exist t
t2 <- LFS20_FA %>% select(variables)
t3 <- LFS20_MM %>% select(variables)
t4 <- LFS20_AJ %>% select(variables)
t5 <- LFS20_MJ %>% select(variables) #                        
t6 <- LFS20_JA %>% select(variables) #                        
t7 <- LFS20_JS %>% select(variables) # FLED10 does not exist  
t8 <- LFS20_AO %>% select(variables) #                        
t9 <- LFS20_SN %>% select(variables) #                       
t10 <- LFS20_OD %>% select(variables)#                        

t11 <- LFS21_NJ %>% select(variables)
t12 <- LFS21_DF %>% select(variables)

t13 <- LFS21_JM %>% select(variables)
t14 <- LFS21_FA %>% select(variables)

#### combine all temporary datasets

LFS_all <- rbind(t1a, t2a, t3a, t4a, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) # working database
LFS_all <- as_tibble(LFS_all) # creating a tibble for data manipulation

save(LFS_all, file = "./Data_clean/LFS_all.rda") # load the saved dataset for data manipulation

rm(list = ls()) # clean the global environment to save memory

load("./Data_clean/LFS_all.rda") # this is the file for analysis

#### Create a working dataset to clean

LFS_clean <- LFS_all # working dataset

### Cleaning the working dataset 

LFS_clean <- LFS_clean %>% 
        set_na(na = c(-8, -9), drop.levels = TRUE, as.tag = FALSE) %>% 
        as_tibble(.) # NAs for: does not apply(-9), no answer(-8)

l_LFS_clean <- get_labels(LFS_clean, values = "n") # check value labels 

LFS_clean <- LFS_clean %>% 
        set_na(na = c(POTHR = 99, UOTHR = 99, ACTHR2 = 99), 
               drop.levels = TRUE, as.tag = FALSE) %>% 
        as_tibble(.) # NAs for specific variable labels

l_LFS_clean <- get_labels(LFS_clean, values = "n") # check value labels 


### Recording and creating variables based on existing ones

## Replacing sex and ILO unemployment labels

LFS_clean$SEX <- replace_labels(LFS_clean$SEX, labels = c("Men" = 1, "Women" = 2)) # changing male and female

LFS_clean$ILODEFR <- replace_labels(LFS_clean$ILODEFR, labels = c("Not employed" = 2)) # replace ILO unemployment

l_LFS_clean <- get_labels(LFS_clean, values = "n") # check value labels 


## recoding "Scotland North Caledonian Canal" for Scotland

LFS_clean <- LFS_clean %>% mutate(COUNTRY2 = recode_factor(factor(COUNTRY), 
                                                   `1` = "England",
                                                   `2` = "Wales",
                                                   `3` = "Scotland", 
                                                   `4` = "Scotland",
                                                   `5` = "Northern Ireland")) %>% 
                                                as_numeric(., keep.labels = TRUE)

## recoding age bands

LFS_clean <- LFS_clean %>% 
        mutate(AGEEUL_2 = recode_factor(factor(AGEEUL),
                                        `1`="Below 18", `2`="Below 18", `3`="Below 18", 
                                        `4`="Below 18", `5`="Below 18", `6`="Below 18",
                                        `7`="Aged 18-24", `8`="Aged 18-24", `9`="Aged 18-24",
                                        `10`="Aged 18-24", `11`="Aged 18-24", `12`="Aged 18-24", `13`="Aged 18-24", 
                                        `14`="Aged 25-29", 
                                        `15`="Aged 30-39", `16`="Aged 30-39", 
                                        `17`="Aged 40-49", `18`="Aged 40-49",
                                        `19`="Aged 50-64", `20`= "Aged 50-64", `21`= "Aged 50-64",
                                        `22`="Above 64", `23` = "Above 64", `24`= "Above 64", 
                                        `25`= "Above 64", `26` = "Above 64", `27`= "Above 64", 
                                        `28`= "Above 64")) %>% 
        as_numeric(., keep.labels = TRUE)


## recoding occupations

LFS_clean <- LFS_clean %>% 
        mutate(NSECMJ10_2 = recode_factor(factor(NSECMJ10), 
                          `1` = "Management & professional",
                          `2` = "Management & professional",
                          `3` = "Intermediate", 
                          `4` = "Small employers & own account",
                          `5` = "Lower supervisory & technical",
                          `6` = "Routine & semi-routine",
                          `7` = "Routine & semi-routine", 
                          `8` = "Never worked, unemployed, and nec")) %>% 
       as_numeric(., keep.labels = TRUE)


## adding labels to QUARTER
LFS_clean$QUARTER <- set_labels(LFS_clean$QUARTER, labels = c("Jan/Mar19" = 1, "Apr/Jun19" = 2, "Jul/Sep19" = 3, "Oct/Dec19" = 4,
                                                              "Jan/Mar20" = 5, "Feb/Apr20" = 6, "Mar/May20" = 7,
                                                              "Apr/Jun20" = 8, "May/Jul20" = 9, "Jun/Aug20" = 10,
                                                              "Jul/Sep20" = 11, "Aug/Oct20" = 12, "Sep/Nov20" = 13,
                                                             "Oct/Dec20" = 14, "Nov20/Jan21" = 15,"Dec20/Feb21" = 16,
                                                             "Jan/Mar21" = 17, "Feb/Apr21" = 18))

## adding labels to FLED10
LFS_clean$FLED10 <- set_labels(LFS_clean$FLED10, labels = c("Flexitime" = 1, 
                                                            "Annualised hours" = 2, 
                                                            "Term time working" = 3, 
                                                            "Job sharing" = 4, 
                                                            "Nine day fortnight" = 5, 
                                                            "Four and a half day week" = 6, 
                                                            "Zero hours contract" = 7,
                                                            "On-Call Working" = 8, 
                                                            "None of these" = 9, 
                                                            "Don’t know" = 10))
l_LFS_clean <- get_labels(LFS_clean, values = "n") # value labels 

### saving the clean dataset

save(LFS_clean, file = "./Data_clean/LFS_clean.rda")

rm(list = ls()) # clean the global environment

######################### END WORKING WITH ORIGINAL DATASETS ####################################