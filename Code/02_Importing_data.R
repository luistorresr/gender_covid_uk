
# IMPORTING DATA

## This and the following scripts are based on the Labour Force Survey - all datasets from 2019 


# IMPORTANT: There is no need to run this script as the resulted database is made available for the analysis.

# To run this script, the UK LFS datasets from 2019 onwards are needed. They are available at https://ukdataservice.ac.uk
# You can also download the latest available datasets on the UK Data Service Website to continue tracking trends. 

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

        
######################## END IMPORTING DATA ####################################