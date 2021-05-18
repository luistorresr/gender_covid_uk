#############################################################################################
########## Labour Force Survey - all datasets from 2020 #####################################
#############################################################################################

### R version
# R version 4.0.4

### R version
# R 4.0 or above

#### packages needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("girdExtra", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(sjlabelled)) install.packages("sjlabelled", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggtext)) install.packages("ggtext", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(doBy)) install.packages("doBy", repos = "http://cran.us.r-project.org")
if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(flextable)) install.packages("flextable", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(ggalt)) install.packages("ggalt", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(ggalt) # contingency table
library(tidyverse) # several tools for data manipulation
library(haven) # read SPSS, Stata, and SAS data
library(gridExtra) # arrange multiple grid-based plots on a page
library(corrr) # for correlations correlate()
library(Hmisc) # many functions useful for data analysis
library(sjmisc) # useful to set Na's
library(sjlabelled) # get_labels function
library(ggthemes) # themes for ggplot2
library(ggtext) # fancy text
library(ggrepel) # help with the position of the labels on ggplot2 graph
library(doBy) # 'do' something to data stratified 'by' some variables
library(plotly) # interactive plots
library(flextable) # formatting tables 
library(scales) # add scale symbols and colour to axis
library(janitor) # add totals to tables
library(caret) # machine learning algorithms 
library(rpart) # decision tree 
library(mltools) # one hot encoding 
library(party)
library(tree)
library(randomForest)

#### Output configurations
options(digits = 3) # decimal points to 3

####################################################################################################
###################### THIS SECTION IS ONLY TO WORK WITH THE ORIGINAL DATASETS ####################
###################################################################################################
#########################Labour Force Survey - all datasets from 2020 #############################
################ UKDS End User Licence available at https://ukdataservice.ac.uk/###################
####################################################################################################

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
LFS21_NJ <- read_spss("./Data/LFS_UKDS/11-Nov-Jan-Corona/UKDA-8788-spss/spss/spss25/lfsp_n20j_eul_pwt18.sav") # November to January
LFS21_DF <- read_spss("./Data/LFS_UKDS/12-Dec-Feb-Corona/UKDA-8794-spss/spss/spss25/lfsp_d20f_eul_pwt18.sav") # December to February

#### Extracting labels and questions for inspection

# Labels

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

# Questions

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


## creating weeks

LFS20_JM <- LFS20_JM %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 1, `2`= 2, `3`=3, `4`=4, 
                               `5`=5, `6`=6, `7`= 7, `8`= 8, `9`= 9, 
                               `10`= 10, `11`= 11, `12`= 12, `13`= 13)) # calendar quarter 1

LFS20_FA <- LFS20_FA %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`=5, `2`=6, `3`= 7, `4`= 8, `5`= 9,
                               `6`= 10, `7`= 11, `8`= 12, `9`= 13, 
                               `10`=14, `11`=15, `12`= 16, `13`= 17)) # non calendar quarter 

LFS20_MM <- LFS20_MM %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 10, `2`= 11, `3`= 12, `4`= 13, 
                               `5`=14, `6`=15, `7`= 16, `8`= 17,
                                `9`= 18, `10`= 19, `11`= 20, `12`= 21, `13`= 22)) # non calendar quarter 

LFS20_AJ <- LFS20_AJ %>% 
        mutate(YWEEKS = recode(WEEK, 
                               `1`= 14, `2`= 15, `3`=16, `4`=17, 
                               `5`=18, `6`=19, `7`=20, `8`= 21, `9`= 22, 
                               `10`= 23, `11`= 24, `12`= 25, `13`= 26)) # calendar quarter 2
 
LFS20_MJ <- LFS20_MJ %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`=18, `2`=19, `3`=20, `4`= 21, `5`= 22, 
                               `6`= 23, `7`= 24, `8`= 25, `9`= 26,
                               `10`= 27, `11`= 28, `12`= 29, `13`= 30)) # non calendar quarter  

LFS20_JA <- LFS20_JA %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 23, `2`= 24, `3`= 25, `4`= 26,
                               `5`= 27, `6`= 28, `7`= 29, `8`= 30,
                               `9`= 31, `10`= 32, `11`= 33, `12`= 34, `13`= 35)) # non calendar quarter 

LFS20_JS <- LFS20_JS %>% 
        mutate(YWEEKS = recode(WEEK, 
                               `1`= 27, `2`= 28, `3`= 29, `4`= 30, 
                               `5`= 31, `6`= 32, `7`= 33, `8` = 34, `9`= 35, 
                               `10`= 36, `11`= 37, `12`= 38, `13`= 39))  # calendar quarter 3

LFS20_AO <- LFS20_AO %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 31, `2`= 32, `3`= 33, `4` = 34, `5`= 35, 
                               `6`= 36, `7`= 37, `8`= 38, `9`= 39,
                               `10`= 40, `11`= 41, `12`= 42, `13`= 43)) # non calendar quarter 

LFS20_SN <- LFS20_SN %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 36, `2`= 37, `3`= 38, `4`= 39,
                               `5`= 40, `6`= 41, `7`= 42, `8`= 43,
                               `9` = 44, `10`= 45, `11`= 46, `12`= 47, `13`= 48)) # non calendar quarter 

LFS20_OD <- LFS20_OD %>% 
        mutate(YWEEKS = recode(WEEK, 
                               `1`= 40, `2`= 41, `3`= 42, `4`= 43, 
                               `5`= 44, `6`= 45, `7`= 46, `8` = 47, `9`= 48,
                               `10`= 49, `11`= 50, `12`= 51, `13`= 52)) # calendar quarter 4

LFS21_NJ <- LFS21_NJ %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 44, `2`= 45, `3`= 46, `4` = 47, `5`= 48,
                               `6`= 49, `7`= 50, `8`= 51, `9`= 52,
                               `10`= 53, `11`= 54, `12`= 55, `13`= 56)) # non calendar quarter  

LFS21_DF  <- LFS21_DF %>% 
        mutate(YWEEKS = recode(WEEK,  
                               `1`= 49, `2`= 50, `3`= 51, `4`= 52,
                               `5`= 53, `6`= 54, `7`= 55, `8`= 56,
                               `9`= 57, `10`= 58, `11`= 59, `12`= 60, `13`= 61)) # non calendar quarter 


#### Creating the variable "quarter", and empty "PIWT18" as well as YWEEKS for all covid datasets

LFS20_JM <- LFS20_JM %>% mutate(QUARTER = 1)
LFS20_FA <- LFS20_FA %>% mutate(QUARTER = 2, PIWT18 = NA)
LFS20_MM <- LFS20_MM %>% mutate(QUARTER = 3, PIWT18 = NA)
LFS20_AJ <- LFS20_AJ %>% mutate(QUARTER = 4)
LFS20_MJ <- LFS20_MJ %>% mutate(QUARTER = 5, PIWT18 = NA)
LFS20_JA <- LFS20_JA %>% mutate(QUARTER = 6, PIWT18 = NA)
LFS20_JS <- LFS20_JS %>% mutate(QUARTER = 7)
LFS20_AO <- LFS20_AO %>% mutate(QUARTER = 8, PIWT18 = NA)
LFS20_SN <- LFS20_SN %>% mutate(QUARTER = 9, PIWT18 = NA)
LFS20_OD <- LFS20_OD %>% mutate(QUARTER = 10)
LFS21_NJ <- LFS21_NJ %>% mutate(QUARTER = 11, PIWT18 = NA)
LFS21_DF <- LFS21_DF %>% mutate(QUARTER = 12, PIWT18 = NA)


#### Selecting variables

# common to all datasets

variables <- c(
       
         # Case identifiers
        
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
        "NSECMJ10", # NS-SEC major group (SOC2010 based) - National Statistics Socio-economic Classification   


        # MAIN JOB
       
        "WRKING", # Whether in paid job in reference week, either as employee or as self-employed
        "JBAWAY", # Whether temporary away from paid job (if "no" in WRKING)
        "INDS07M", # Industry section in main job - 21 categories to link with BICS
        "MANAGER", # managerial status in current job (filter by STAT = 1)  
        "REGWKR", # Region of place of work (reported)
        "SC10MMJ", # Major occupation group
        "SAMELAD", # Whether lives and works in same Local Authority District
        
        ## Home workers (MAIN JOB)
        
        "HOME", # Whether working from home in main job (WRKING = 1 or JBAWAY = 1)
        
        # MAIN JOB - PRECARIOUS WORK INDICATORS
        
        ## full vs part time
        
        "FTPTWK", # Whether full or part time in main job (filter for the required economics activity)
        "YPTJOB", # Reason for part time job (filter by FTPTWK = 2) 

        ## Permanent/Temporary Employment
        
        "JOBTYP", # Whether job permanent (filter by Stat=1 AND EverWk<1)
        "WHYTMP6", # Reason for taking non-permanent job (filter by JOBTYP = 2) 
        "RESTMR6", # Reason job is temporary
        
        
        # SECOND JOB
        
        "SECJOB", # Whether had second job in reference week
        "Y2JOB", # Whether had 2 jobs because of a change of job in reference week (this is mainly for filtering in some variables)
        "STAT2", # Employment status for those in regular second jobs
        "INDS07S", # Industry section in second job - 21 categories
        "MANAG2", # Did you have any managerial duties?
        "JOBTYP2", # Permanency of second job
        "REGWK2R", # Region of workplace for second job (reported)
        "SC10SMJ", # Major occupation group (second job)
        "GORWK2R", # Region of workplace for second job (reported)
        
        "HOME2", # whether working from home in additional job  (filter by Y2Job=2)
       
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
        "REDANY", # Whether made redundant from any other job in last 3 months (filter by REDYL13 = 1, 4, 5, 6, 7, 8, 9, 10, 11 AND REDPAID = 1))
        "REDCLOS",  # Reason for leaving job left in last three months (REDYL13 = 2 OR 3 AND REDPAID = 1) OR (REDANY = 1))     
        "INDE07R", #  Industry sectors in job made redundant from
        
        # weights
        "PWT18", "PIWT18" # data for PIWT18 is not available for the new covid series
        ) 

#### selecting the variables in all dataset and creating temporary datasets

t1 <- LFS20_JM %>% select(variables) 
t2 <- LFS20_FA %>% select(variables)
t3 <- LFS20_MM %>% select(variables)
t4 <- LFS20_AJ %>% select(variables)
t5 <- LFS20_MJ %>% select(variables)
t6 <- LFS20_JA %>% select(variables)
t7 <- LFS20_JS %>% select(variables)
t8 <- LFS20_AO %>% select(variables)
t9 <- LFS20_SN %>% select(variables)
t10 <- LFS20_OD %>% select(variables)
t11 <- LFS21_NJ %>% select(variables)
t12 <- LFS21_DF %>% select(variables)

#### combine all temporary datasets

LFS_all <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) # working database
LFS_all <- as_tibble(LFS_all) # creating a tibble for data manipulation

save(LFS_all, file = "./Data_clean/LFS_all.rda") 

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
LFS_clean$QUARTER <- set_labels(LFS_clean$QUARTER, labels = c("Jan-Mar20" = 1, "Feb-Apr20" = 2, "Mar-May20" = 3,
                                                              "Apr-Jun20" = 4, "May-Jul20" = 5, "Jun-Aug20" = 6,
                                                              "Jul-Sep20" = 7, "Aug-Oct20" = 8, "Sep-Nov20" = 9,
                                                             "Oct-Dec20" = 10, "Nov-Jan21" = 11,"Dec-Feb21" = 12 ))

### saving the clean dataset

save(LFS_clean, file = "./Data_clean/LFS_clean.rda")

rm(list = ls()) # clean the global environment

######################### END WORKING WITH ORIGINAL DATASETS ####################################

################################################################################################
############################### Working with the clean dataset #################################
################################################################################################

### load clean working dataset

load("./Data_clean/LFS_all.rda") # in case modification to the original dataset are needed. Recoding of variables should be implemented.

load("./Data_clean/LFS_clean.rda") # this is the file for analysis

q_LFS_clean <- as.data.frame(label(LFS_clean)) # variable labels
l_LFS_clean <- get_labels(LFS_clean, values = "n") # value labels 


#### Analysis 

#### NOTE: We consider only the personal and proxy responses (IOUTCOME 1 and 2) of people between 18 and 64 years old 

### Calculate sample/population size 

N_total <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64) %>%
        group_by(as_label(QUARTER)) %>%
        summarise(Sample = n(), Population = sum(PWT18)) 

N_active <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & ILODEFR <=2) %>%
        group_by(as_label(QUARTER)) %>%
        summarise(Econ_active = sum(PWT18))

N_total <- left_join(N_total, N_active, by = "as_label(QUARTER)")  

N_total %>% flextable() %>% theme_vanilla() %>%
        set_caption(caption = "Sample size and population estimates", style = "table") %>%
        bold(bold = TRUE, part = "header") %>%
        set_header_labels(`as_label(QUARTER)` = "Quarter",
                          Population = "Population estimates")  %>%
        autofit() %>%
        align_nottext_col(align = "center", header = TRUE, footer = FALSE) %>%
        add_footer_lines("Source: UK Labour Force Survey (Person)") %>%
        color(part = "footer", color = "#666666") %>%
        print(.) # final table for reporting


### Are there differences among women and men in their levels of unemployment?

## Unemployment - tables and graphs 

# Unemployment per sex

unem_sex <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & ILODEFR <=2) %>%
        group_by(QUARTER, as_label(ILODEFR), SEX) %>%
        summarise(Active = sum(PWT18))  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total) # unemployment rates per gender 

unem_sex %>% 
        ggplot(aes(x = QUARTER, y = Rate, col = as_label(SEX))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) + 
        geom_vline(xintercept = 3, linetype="dotted") +
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.06, 0.01), limits = c(0, 0.06), labels = scales::percent_format(accuracy = 1L)) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                         labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                  "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                  "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 1: Men’s unemployment rose most steeply",
             subtitle  = "Unemployment rate of UK population  2020-2021",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "text", x = 12, y = 0.053, label = "Men", color= "#DF9216") +
        annotate(geom = "text", x = 12, y = 0.040, label = "Women", color= "#791F83") +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle=90),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 11),
              legend.position = "none",
              legend.title = element_blank())  


# Unemployment per sex and week for calendar waves
# When weekly data from the four calendar quarters in 2020 are considered,
# sample levels estimates show high variability. However, the trend shows 
# that unemployment increased inmediatelly after the first lockdown and 
# recover by the time when the second lockdown was in place.


unem_sex_week <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & ILODEFR <=2) %>%
        group_by(YWEEKS, as_label(ILODEFR), SEX) %>%
        summarise(Active = n())  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total) # unemployment rates per gender 

unem_sex_week %>% 
        ggplot(aes(x = YWEEKS, y = Rate, col = as_label(SEX))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.06, 0.01), limits = c(0,0.06), labels = label_percent()) +
        scale_x_continuous(breaks = seq(1,61, 1), limits = c(1, 62)) + 
        
        # annotation
        labs(x = "", 
             y = "",
             title ="Unemployment Rate for Men and Women",
             subtitle  = "Weekly unemployment trend in 2020",
             caption = c("Note: Sample level estimates for four calendar quarters
                         Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "text", x = 52, y = 0.051, label = "Men", color= "#DF9216") +
        annotate(geom = "text", x = 52, y = 0.032, label = "Women", color= "#791F83") +
        annotate(geom = "rect", xmin = 13,  xmax = 23, ymin = 0,  ymax = 0.06, alpha = 0.2) +
        annotate(geom = "rect", xmin = 44, xmax = 48, ymin = 0, ymax = 0.06, alpha = 0.2) +
        annotate(geom = "rect", xmin = 54, xmax = 61, ymin = 0, ymax = 0.06, alpha = 0.2) +        
        annotate(geom = "text", x = 18, y = 0.058, label = "Lockdown 1") +
        annotate(geom = "text", x = 46, y = 0.058, label = "Lockdown 2") +
        annotate(geom = "text", x = 58, y = 0.058, label = "Lockdown 3") +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle = 0),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 8),
              legend.position = "none",
              legend.title = element_blank()) 


## Unemployment per age and sex

unem_age <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & ILODEFR <=2) %>%
        group_by(QUARTER, as_label(ILODEFR), SEX, AGEEUL_2) %>%
        summarise(Active = sum(PWT18))  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%        
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total)

age_select <- unem_age %>% filter(AGEEUL_2 == 2)

unem_age %>%       
        # select data
        select(QUARTER, AGEEUL_2, SEX, Rate) %>%  
        filter(!is.na(Rate))  %>% 
        arrange(desc(Rate)) %>%

        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col= as_label(AGEEUL_2))) +
        geom_point(aes(size=Rate)) +  
        geom_encircle(data = age_select, aes(x = QUARTER, y = Rate)) +
        geom_vline(xintercept = 3, linetype="dotted") +
        facet_wrap(.~as_label(SEX)) +
        
        #scales
        scale_y_continuous(breaks = seq(0,0.20, 0.02), limits = c(0,0.20), labels = scales::percent_format(accuracy = 1L)) +
        scale_x_continuous(breaks=c(1:12),
                           labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) + 
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 2: Highest job loss for young people",
             subtitle  = "Unemployment rate of UK population, 2020-2021",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle=90),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 11),
              legend.position = "top",
              legend.key.width = unit(0.5,"cm"),
              legend.margin = ggplot2::margin(1, 0, 1, 0, "cm"),
              legend.title = element_blank(),
              legend.text = element_text(size=10),
              strip.text = element_text(face = "bold", hjust = 0, size = 11)) +
        guides(size = FALSE, color = guide_legend(override.aes = list(size = 5))) 


# Unemployment per ethnicity and sex

unem_eth <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & ILODEFR <=2) %>%
        group_by(QUARTER, as_label(ILODEFR), SEX, ETHUKEUL) %>%
        summarise(Active = sum(PWT18))  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total)

unem_eth %>% 
        # select data
        select(QUARTER, ETHUKEUL, SEX, Rate) %>% 
        filter(!is.na(Rate & ETHUKEUL)) %>% 
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col = as_label(SEX))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 3, linetype="dotted") +
        facet_wrap(.~as_label(ETHUKEUL), ncol = 3) +
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0,0.18), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:12),
                         labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                  "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                  "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 3: The steepest increases in unemployment were among minority ethnic groups",
             subtitle  = "Unemployment rate of UK population, 2020-2021",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle=90),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 9),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size=11),
              strip.text = element_text(face = "bold", hjust = 0, size = 10)) +
        guides(color = guide_legend(override.aes = list(size = 3))) 


## redundancies 

### who has been made redundant in the last three months? 

## overall estimates for everyone made redundant in the last three months 

# covers the number of people who were not in employment during the reference week 
# and who reported that they had been made redundant in the month of the reference 
# week or in the two calendar months prior to this; plus the number of people who were 
# in employment during the reference week who started their job in the same calendar month as,
# or the two calendar months prior to, the reference week, and who reported that they had 
# been made redundant in the past three months.

sex_est <- LFS20_all_clean %>%
        filter(ILODEFR2 == 1 & 2) %>%
        group_by(QUARTER, SEX) %>%
        summarise(SEXEST = sum(PWT18))

red_sex <- LFS20_all_clean %>%
        filter(REDUND == 1) %>%
        group_by(QUARTER, SEX) %>%
        summarise(SEXRED = sum(PWT18)) 

red_sex_est <- left_join(red_sex, sex_est, by = c("QUARTER", "SEX"))

red_sex_est <- red_sex_est %>%
        mutate(PERSEX = SEXRED / SEXEST) # population ratio


red_sex_est %>% ggplot(aes(x = QUARTER, y = SEXRED, col = as_label(SEX))) +
        geom_line() +
        geom_point(shape = 21) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
        scale_y_continuous(breaks = breaks_extended(10)) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        theme(axis.text.x = element_text(angle = 90), legend.position = "top") +
        xlab("Quarter") + 
        ylab("Estimated Total (weighted)") +
        labs(subtitle = "Made redundant in the last three months",
             col = "") # population estimates 

red_sex_est %>% ggplot(aes(x = QUARTER, y = PERSEX, col = as_label(SEX))) +
        geom_line() +
        geom_point(shape = 21) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
        scale_y_continuous(labels = label_percent(accuracy = 0.1), breaks = breaks_extended(11)) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        theme(axis.text.x = element_text(angle = 90), legend.position = "top") +
        xlab("Quarter") + 
        ylab("Redundancy vs population ratio") +
        labs(col = "") # redundancy vs population ratio


## Reason for redundancy 

redundancy_reason <- LFS20_all_clean %>%
        filter(REDUND == 1 & REDCLOS != 3) %>%
        group_by(QUARTER, SEX, REDCLOS) %>%
        summarise(REDREAS = sum(PWT18)) 

redundancy_reason %>% 
        ggplot(aes(x = QUARTER, y = REDREAS, col = as_label(SEX))) +
        geom_line() +
        geom_point(shape = 21) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
        scale_y_continuous(breaks = breaks_extended(10)) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9","10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        theme(axis.text.x = element_text(angle = 90), legend.position = "top") +
        xlab("Quarter") + 
        ylab("Estimated Total (weighted)") +
        labs(subtitle = "Reason made redundant in last three months",
             col = "") + facet_wrap(~as_label(REDCLOS))

## Redundancy by industry

est_ind <- LFS20_all_clean %>%
        filter(ILODEFR2 == 1 & 2) %>%
        group_by(QUARTER, SEX, INDE07R) %>%
        summarise(ESTIND = sum(PWT18))

red_ind <- LFS20_all_clean %>%
        filter(REDUND == 1) %>%
        group_by(QUARTER, SEX, INDE07R) %>%
        summarise(REDIND = sum(PWT18))

red_ind_est <- left_join(red_ind, est_ind, by = c("QUARTER", "SEX", "INDE07R"))


red_ind_est <- red_ind_est %>%
        mutate(PERIND = REDIND / ESTIND) # population ratio


red_ind_est %>% drop_na(INDE07R) %>% 
        ggplot(aes(x = QUARTER, y = REDIND, col = as_label(SEX))) +
        geom_line() +
        geom_point(shape = 21) +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
        scale_y_continuous(breaks = breaks_extended(6)) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9","10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        theme(axis.text.x = element_text(angle = 90), legend.position = "top") +
        xlab("Quarter") + 
        ylab("Estimated Total (weighted)") +
        labs(subtitle = "Made redundant by industry",
             col = "") + facet_wrap(.~as_label(INDE07R))


# redundancy by ethnicity 

est_eth <- LFS20_all_clean %>%
        filter(ILODEFR2 == 1 & 2) %>%
        group_by(QUARTER, SEX, ETHUKEUL) %>%
        summarise(ESTETH = sum(PWT18))

red_eth <- LFS20_all_clean %>%
        filter(REDUND == 1) %>%
        group_by(QUARTER, SEX, ETHUKEUL) %>%
        summarise(REDETH = sum(PWT18))

red_eth_est <- left_join(red_eth, est_eth, by = c("QUARTER", "SEX", "ETHUKEUL"))

red_eth_est <- red_eth_est %>%
        mutate(PERETC = REDETH / ESTETH) # population ratio

red_eth_est %>%  filter(ETHUKEUL %in% c(1, 2, 3, 8)) %>%
        ggplot(aes(x = QUARTER, y = PERETC, col = as_label(SEX))) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        scale_y_continuous(labels = label_percent(accuracy = 0.1), breaks = breaks_extended(11)) +
        scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9","10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        theme(axis.text.x = element_text(angle = 90), legend.position = "top") +
        xlab("Quarter") + 
        ylab("Redundancy vs population ratio") +
        labs(col = "") + facet_wrap(.~as_label(ETHUKEUL))

red_eth_est %>% filter(ETHUKEUL == 1) %>% 
        group_by(QUARTER, SEX) %>%
        summarise(MEAN = mean(PERETC)*100)





## Employment per occupation and sex

LFS_clean %>% 
        filter(AGE >= 16 & AGE <= 64) %>%
        group_by(QUARTER) %>%
        summarise(pop = sum(PWT18), empl = sum(PWT18[ILODEFR ==1])) %>%
        group_by(QUARTER) %>%
        mutate(Rate = empl / pop)

em_class <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64) %>%
        group_by(QUARTER, SEX, NSECMJ10_2) %>%
        summarise(pop = sum(PWT18), empl = sum(PWT18[ILODEFR ==1])) %>%
        mutate(Rate = empl / pop) %>%
        filter(NSECMJ10_2 != 6)

em_class %>% 
        # select data
        select(QUARTER, NSECMJ10_2, SEX, Rate) %>%  
        filter(NSECMJ10_2 <=5)  %>% 
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col = as_label(NSECMJ10_2))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 3, linetype="dotted") +
        facet_wrap(.~as_label(SEX)) +
        
        #scales
        scale_y_continuous(breaks = seq(0.7,1, 0.02), limits = c(0.7,1), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                         labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                  "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                  "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 4: Employment has not recovered equally for everyone",
             subtitle  = "Unemployment rate of UK population, 2020-2021",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle=90),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 9),
              legend.position = "top",
              legend.key.width = unit(0.5,"cm"),
              legend.margin = ggplot2::margin(1, 0, 1, 0, "cm"),
              legend.title = element_blank(),
              legend.text = element_text(size=11),
              strip.text = element_text(face = "bold", hjust = 0, size = 10)) +
        guides(color = guide_legend(override.aes = list(size = 5)))         
        

