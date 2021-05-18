#############################################################################################
##################### Business Impact of COVID-19 Survey (BICS) #############################
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

#### Accessing the datasets for all waves in 2020

BICS_w1 <- read_spss("./") 
BICS_w2 <- read_spss("./") 
BICS_w3 <- read_spss("./") 
BICS_w4 <- read_spss("./") 
BICS_w5 <- read_spss("./") 

BICS_w6 <- read_spss("./") 
BICS_w7 <- read_spss("./") 
BICS_w8 <- read_spss("./") 
BICS_w9 <- read_spss("./") 
BICS_w10 <- read_spss("./") 
BICS_w11 <- read_spss("./") 
BICS_w12 <- read_spss("./") 
BICS_w13 <- read_spss("./") 
BICS_w14 <- read_spss("./") 
BICS_w15 <- read_spss("./") 
BICS_w16 <- read_spss("./") 
BICS_w17 <- read_spss("./") 
BICS_w18 <- read_spss("./") 
BICS_w19 <- read_spss("./") 

#### Extracting labels and questions

# Value labels

l_BICS_w1 <- get_labels(LFS20_AJ_w2, values = "n") #labels April to June (wave 2)


# Extracting questions

q_BICS_w1 <- as.data.frame(label(LFS20_AJ_w2)) 



#### Selecting variables

variables <- c(
  
  # Classification
 
   "a_wave_num", # Wave number of survey
  "a_NUTS1_region_code", # region
  "a_NUTS1_region_description", # region name
  "a_RUSIC2007",  # Reporting Unit SIC2007 industry code
  "a_INDsec", # Industrial Sector
  
  # waves 6 to 10
  
  "data_097", # Is your business providing top-ups to any furloughed workers pay, on top of the Coronavirus Job Retention Scheme (CJRS) payments?
  "data_098", # Of those workers on furlough leave, approximately what percentage have their pay topped up?
  "data_1001", # Return to the workplace from furlough
  "data_1002", # Return to the workplace from remote working
  "data_1003", # Be made permanently redundant
  "data_101", # How many external job vacancies is your business actively recruiting for?
  "data_102", # Of the number of external job vacancies, how many were new in the last two weeks? 
  
  )


  
  
  
  