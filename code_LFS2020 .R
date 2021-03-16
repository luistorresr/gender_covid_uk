### R version
# R version 4.0.4

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
library(plotly) # interactive plots
library(formattable) # formatting tables 
library(scales) # add scale symbols to axis

#### Output configurations
options(digits = 3) # decimal points to 3


#### Labour Force Survey - all datasets for 2020 
#### UKDS End User Licence available at https://ukdataservice.ac.uk/
#### No COVID-19 related variables in these datasets 

LFS20_JM_w1 <- read_spss("./Datasets_UKDS/1-Jan-Mar-W1/UKDA-8639-spss/spss/spss25/lfsp_jm20_eul_pwt18.sav") # January to March (wave 1)
LFS20_FA_c1 <- read_spss("./Datasets_UKDS/2-Feb-Apr-Corona/UKDA-8646-spss/spss/spss25/lfsp_fa20_eul_pwt18.sav") # February to April (covid)
LFS20_MM_c2 <- read_spss("./Datasets_UKDS/3-Mar-May-Corona/UKDA-8659-spss/spss/spss25/lfsp_mm20_eul_pwt18.sav") # March to May (covid)
LFS20_AJ_w2 <- read_spss("./Datasets_UKDS/4-Apr-Jun-W2/UKDA-8671-spss/spss/spss25/lfsp_aj20_eul_pwt18.sav") # April to June (wave 2)
LFS20_MJ_c3 <- read_spss("./Datasets_UKDS/5-May-Jul-Corona/UKDA-8707-spss/spss/spss25/lfsp_mj20_eul_pwt18.sav") # May to July (covid)
LFS20_JA_c4 <- read_spss("./Datasets_UKDS/6-Jun-Aug-Corona/UKDA-8706-spss/spss/spss25/lfsp_ja20_eul_pwt18.sav") # June to August (covid)
LFS20_JS_w3 <- read_spss("./Datasets_UKDS/7-Jul-Sep-W3/UKDA-8720-spss/spss/spss25/lfsp_js20_eul_pwt18.sav") # July to September (wave 3)
LFS20_AO_c5 <- read_spss("./Datasets_UKDS/8-Aug-Oct-Corona/UKDA-8738-spss/spss/spss25/lfsp_ao20_eul_pwt18.sav") # August to October (covid)
LFS20_SN_c6 <- read_spss("./Datasets_UKDS/9-Sep-Nov-Corona/UKDA-8760-spss/spss/spss25/lfsp_sn20_eul_pwt18.sav") # September to November (covid)
LFS20_OD_w4 <- read_spss("./Datasets_UKDS/10-Oct-Dec-W4/UKDA-8777-spss/spss/spss25/lfsp_od20_eul_pwt18.sav") # October to December (wave 4)

#### Creating the variable "quarter" in each dataset and an empty "PIWT18" for all covid datasets

LFS20_JM_w1 <- LFS20_JM_w1 %>% mutate(QUARTER = 1)
LFS20_FA_c1 <- LFS20_FA_c1 %>% mutate(QUARTER = 2, PIWT18 = NA)
LFS20_MM_c2 <- LFS20_MM_c2 %>% mutate(QUARTER = 3, PIWT18 = NA)
LFS20_AJ_w2 <- LFS20_AJ_w2 %>% mutate(QUARTER = 4)
LFS20_MJ_c3 <- LFS20_MJ_c3 %>% mutate(QUARTER = 5, PIWT18 = NA)
LFS20_JA_c4 <- LFS20_JA_c4 %>% mutate(QUARTER = 6, PIWT18 = NA)
LFS20_JS_w3 <- LFS20_JS_w3 %>% mutate(QUARTER = 7)
LFS20_AO_c5 <- LFS20_AO_c5 %>% mutate(QUARTER = 8, PIWT18 = NA)
LFS20_SN_c6 <- LFS20_SN_c6 %>% mutate(QUARTER = 9, PIWT18 = NA)
LFS20_OD_w4 <- LFS20_OD_w4 %>% mutate(QUARTER = 10)


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
l_LFS20_OD_w4 <- get_labels(LFS20_OD_w4, values = "n") #labels October to December (wave 4)

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
q_LFS20_OD_w4 <- as.data.frame(label(LFS20_OD_w4))

#### Selecting variables

# common to all datasets and without weights

variables <- c(
        # case identifiers
        "QUARTER", "IOUTCOME", # interview outcome
        "COUNTRY", "GOVTOF2", "URESMC", # Country within UK, gov region, and region of usual residence
        
        # respondent characteristics
        "SEX", # 1 male, 2 female
        "AGE", "AGEEUL", # age in number and categories
        "MARSTA", # marital status
        "RELIG11", # religion GB level
        "NATOX7_EUL_Main", "NATOX7_EUL_Sub", # nationality (main and subdivisions)
        "ETHUKEUL", # ethnic group
        "CRYOX7_EUL_Main", "CRYOX7_EUL_Sub", # country of birth variables       
        "TEN1", # Accommodation details        
        "QUAL_1", # Whether Degree level qualification including foundation degrees, graduate
                  #membership of a professional institute, PGCE, or higher obtained
        
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
                        
        # earnings if main job in reference week
        "GRSSWK", # Gross weekly pay in main job
        "HOURPAY", # Average gross hourly pay
        "NETWK", # Net weekly pay in main job
        "YPAYL20", # Reason for pay being less than usual
        
        # earnings in second job in reference week      
        "GRSSWK2", # Gross weekly pay in second job
        "NETWK2", # Net weekly pay in 2nd job
        
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
t10 <- LFS20_OD_w4 %>% select(variables)

#### combine all temporary datasets

LFS20_all <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) # working database
q_LFS20_all <- as.data.frame(label(LFS20_all)) # variable labels
rm(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) # delete temporary datasets
LFS20_all <- as_tibble(LFS20_all) # creating a tibble for data manipulation

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
        set_na(na = c(TOTUS2= 99, POTHR = 99, UOTHR = 99), 
               drop.levels = TRUE, as.tag = FALSE) %>% 
        as_data_frame(.) # NAs for specific variable labels

LFS20_all_clean$SEX <- replace_labels(LFS20_all_clean$SEX, labels = c("Men" = 1, "Women" = 2)) # changing male and female

l_LFS20_all_clean <- get_labels(LFS20_all_clean, values = "n") # value labels 



#### Analysis 

### Descriptive

## Calculate sample size by quarter and sex

sample_size <- LFS20_all_clean %>%
        group_by(QUARTER, as_label(SEX)) %>%
        summarise(SEXSAMPLE = n()) # sample size per quarter and sex

sample_size <- sample_size %>% 
        group_by(QUARTER) %>%
        mutate(SAMPLE = sum(SEXSAMPLE)) # adding the total sample size

## Calculate population estimates by quarter and sex

pop_est <- LFS20_all_clean %>%
        group_by(QUARTER, SEX) %>%
        summarise(POPEST = sum(PWT18))

pop_est$POPEST <- round(pop_est$POPEST, -3) # Round the estimates

## Are there differences among women and men in their levels of unemployment?

LFS20_all_clean <- LFS20_all_clean %>% mutate(ILODEFR2 = ILODEFR) # duplicate ILODEFR

LFS20_all_clean$ILODEFR2 <- replace_labels(LFS20_all_clean$ILODEFR2, labels = c("Not employed" = 2)) # replace ILO unemployment

econ_act <- LFS20_all_clean %>%
        group_by(QUARTER, SEX, ILODEFR2) %>%
        summarise(ESTATUS = sum(PWT18)) # estimates of economic activity 

econ_act %>% filter(ILODEFR2 == 2) %>%
        ggplot(aes(x = QUARTER, y = ESTATUS, col = as_label(SEX))) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = 0, size = 1, colour="#333333") +
        scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Quarter") + 
        ylab("Not employed") +
        labs(title="Unemployment trend 2020",
             subtitle = "Population estimates (weighted)",
             col = "Sex")


### are there difference among women and men in their hours worked?


SUMHRS # Total hours worked in reference week in main and second jobs
BUSHR # Total usual hours for everyone excluding overtime in main job 
TTUSHR # Total usual hours for everyone including overtime in main job 

# Hours only for people who work overtime
TOTUS2 # Total usual hours including overtime  
POTHR #  Usual hours of paid overtime
UOTHR # Usual hours of unpaid overtime



### are there difference among women and men in their earnings?

## Sample level gross weekly pay in main job (employees) - unweighted

earnings <-  LFS20_all_clean %>% filter(STAT == 1) %>% 
        select("QUARTER", "URESMC", "SEX", "AGE", "AGEEUL", "ETHUKEUL",   "SC10MMJ", 
               "MANAGER", "GRSSWK", "YPAYL20") %>% 
        drop_na(GRSSWK)

earnings %>% 
        ggplot(aes(x = as_factor(QUARTER), y = GRSSWK, fill = as_label(SEX))) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") +
        scale_y_continuous(limits = c(50, 1000), breaks = breaks_extended(10)) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10"),
                         labels=c("Jan-Mar", "Feb-Apr", "Mar-May", "Apr-Jun", "May-Jul", 
                                  "Jun-Aug", "Jul-Sep","Aug-Oct", "Sep-Nov", "Oct-Dec")) +
        xlab("Quarter") + 
        ylab("Gross salary per week (£)") +
        labs(subtitle = "Gross salary per week by sex in 2020",
             fill = "")

gap_sex <- earnings %>% group_by(QUARTER, SEX) %>%
        summarise(MEDIAN = median(GRSSWK)) %>%
        spread(as_label(SEX), MEDIAN) %>% 
        mutate(median_gap = 1-(`2`/`1`))

mean(gap_sex$median_gap)

gap_level <- earnings %>% group_by(QUARTER, SEX, MANAGER) %>%
        summarise(MEDIAN = median(GRSSWK)) %>%
        spread(as_label(SEX), MEDIAN) %>%
        drop_na(MANAGER) %>%
        mutate(median_gap = 1-(`2`/`1`))

level_summary <- summaryBy(median_gap ~ QUARTER + MANAGER, data = gap_level, FUN = mean)

summary(level_summary %>% spread(MANAGER, median_gap.mean))


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


# end
