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
if(!require(descr)) install.packages("descr", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")

library(descr) # contingency table
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

#### Selecting variables

# common to all datasets

variables <- c(
       
         # Case identifiers
        
        "QUARTER", 
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

#### Creating the variable "quarter" in each dataset and an empty "PIWT18" for all covid datasets

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

LFS_clean <- LFS_clean %>% mutate(COUNTRY2 = recode_factor(COUNTRY, 
                                                           `1` = "England",
                                                           `2` = "Wales",
                                                           `3` = "Scotland", 
                                                           `4` = "Scotland",
                                                           `5` = "Northern Ireland" 
))                               


## adding labels to QUARTER

LFS_clean$QUARTER <- set_labels(LFS_clean$QUARTER, labels = c("Jan-Mar20" = 1, "Feb-Apr20" = 2, "Mar-May20" = 3,
                                                              "Apr-Jun20" = 4, "May-Jul20" = 5, "Jun-Aug20" = 6,
                                                              "Jul-Sep20" = 7, "Aug-Oct20" = 8, "Sep-Nov20" = 9,
                                                              "Oct-Dec20" = 10, "Nov-Jan21" = 11,"Dec-Feb21" = 12 ))


### saving the clean dataset

save(LFS_clean, file = "./Data/LFS_clean.rda")

rm(list = ls()) # clean the global environment

######################### END WORKING WITH ORIGINAL DATASETS ####################################

################################################################################################
############################### Working with the clean dataset #################################
################################################################################################

### load clean working dataset

load("./Data/LFS_clean.rda")

q_LFS_clean <- as.data.frame(label(LFS_clean)) # variable labels
l_LFS_clean <- get_labels(LFS_clean, values = "n") # value labels 


#### Analysis 

#### NOTE: We consider only the personal and proxy responses (IOUTCOME 1 and 2) of people between 18 and 64 years old 

### Calculate sample size 

N_total <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64 & IOUTCOME <= 2) %>%
        group_by(as_label(QUARTER)) %>%
        summarise(Sample = n(), Population = sum(PWT18)) 

N_rolled <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64 & IOUTCOME == 6) %>%
        group_by(as_label(QUARTER)) %>%
        summarise(Non_Reponse = sum(PWT18))

N_active <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & IOUTCOME <= 2 & ILODEFR <=2) %>%
        group_by(as_label(QUARTER)) %>%
        summarise(Econ_active = sum(PWT18))

N_total <- left_join(N_total, N_rolled, by = "as_label(QUARTER)")

N_total <- left_join(N_total, N_active, by = "as_label(QUARTER)")        
        
N_total %>% flextable() %>% theme_vanilla() %>%
        set_caption(caption = "Table 1: Sample size and population estimates", style = "table") %>%
        bold(bold = TRUE, part = "header") %>%
        set_header_labels(`as_label(QUARTER)` = "Quarter",
                          Population = "Total Population",
                            Non_Reponse = "Non Responses",
                            Econ_active = "Economically Active") %>%
        autofit() %>%
        align_nottext_col(align = "center", header = TRUE, footer = FALSE) %>%
        add_footer_lines("Source: UK Labour Force Survey (Person)") %>%
        color(part = "footer", color = "#666666") %>%
        print(.) # final table for reporting


### Are there differences among women and men in their levels of unemployment?

## Unemployment - tables and graphs 


# Unemployment per sex

unem_sex <- LFS_clean %>% 
                filter(AGE >= 18 & AGE <= 64 & IOUTCOME <= 2 & ILODEFR <=2) %>%
                group_by(QUARTER, SEX, as_label(ILODEFR)) %>%
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
        scale_y_continuous(breaks = seq(0,0.06, 0.01), limits = c(0,0.06), labels = label_percent()) +
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                         labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                  "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                  "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "Quarter", 
             y = "Rate",
             title ="Unemployment Rate for Men and Women",
             subtitle  = "UK Propulation between 18-64 years old",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "text", x = 12, y = 0.053, label = "Men", color= "#DF9216") +
        annotate(geom = "text", x = 12, y = 0.040, label = "Women", color= "#791F83") +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle = 90),
              axis.title.x = element_text(margin = margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 10),
              legend.position = "none",
              legend.title = element_blank()) 


# Unemployment per ethnicity and sex

unem_eth <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & IOUTCOME <= 2 & ILODEFR <=2) %>%
        group_by(QUARTER, ETHUKEUL, SEX, as_label(ILODEFR)) %>%
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
        scale_y_continuous(breaks = seq(0,0.2, 0.03), limits = c(0,0.2), labels = label_percent()) + 
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                         labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                  "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                  "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "Quarter", 
             y = "Rate",
             title ="Unemployment Rate for Men and Women by Ethnic Group",
             subtitle  = "UK Population between 18-64 years old ",
             caption = c("Source: UK Labour Force Survey (Person)")) +

        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_blank(),
              axis.title.x = element_text(margin = margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 8),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size=10),
              strip.text = element_text(face = "bold", hjust = 0, size = 9)) +
        guides(color = guide_legend(override.aes = list(size = 3))) 


# Unemployment per occupation and sex

LFS_clean <- LFS_clean %>% mutate(NSECMJ10_2 = recode_factor(NSECMJ10, 
                                                           `1` = "Higher managerial & professional",
                                                           `2` = "Lower managerial & professional",
                                                           `3` = "Intermediate occupations", 
                                                           `4` = "Small employers & self-employed",
                                                           `5` = "Lower supervisory & technical",
                                                           `6` = "Routine/semi-routine occupations",
                                                           `7` = "Routine/semi-routine occupations", 
                                                           `8` = "Never worked, unemployed, and nec"))  
unem_class <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & IOUTCOME <= 2 & ILODEFR <=2) %>%
        group_by(QUARTER, NSECMJ10_2, SEX, as_label(ILODEFR)) %>%
        summarise(Active = sum(PWT18))  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total) 
        

unem_class %>% 
        # select data
        select(QUARTER, NSECMJ10_2, SEX, Rate) %>%  
        filter(!is.na(Rate & as.integer(NSECMJ10_2)) & as.integer(NSECMJ10_2) != 7)  %>% 
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col = as_label(SEX))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 3, linetype="dotted") +
        facet_wrap(.~as_label(NSECMJ10_2), ncol = 3) +
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0,0.08), labels = label_percent()) + 
        scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                         labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                  "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                  "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "Quarter", 
             y = "Rate",
             title ="Unemployment Rate for Men and Women by Occupation",
             subtitle  = "UK Population between 18-64 years old ",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_blank(),
              axis.title.x = element_text(margin = margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 8),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size=10),
              strip.text = element_text(face = "bold", hjust = 0, size = 9)) +
        guides(color = guide_legend(override.aes = list(size = 3))) 



# Unemployment per income and sex

unem_income <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & IOUTCOME <= 2 & ILODEFR <=2) %>%
        group_by(QUARTER, SEX, as_label(ILODEFR)) %>%
        summarise(Active = sum(PWT18))  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total) 


unem_class %>% 
        # select data
        select(QUARTER, NSECMJ10_2, SEX, Rate) %>%  
        filter(!is.na(Rate & as.integer(NSECMJ10_2)) & as.integer(NSECMJ10_2) != 7)  %>% 
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col = as_label(SEX))) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 3, linetype="dotted") +
        facet_wrap(.~as_label(NSECMJ10_2), ncol = 3) +
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0,0.08), labels = label_percent()) + 
        scale_x_continuous(breaks=c(1:12),
                           labels=c("Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        
        # annotation
        labs(x = "Quarter", 
             y = "Rate",
             title ="Unemployment Rate for Men and Women by Occupation",
             subtitle  = "UK Population between 18-64 years old ",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle = 90),
              axis.title.x = element_text(margin = margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 8),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size=10),
              strip.text = element_text(face = "bold", hjust = 0, size = 9)) +
        guides(color = guide_legend(override.aes = list(size = 3))) 




### are there difference among women and men in their hours worked?






