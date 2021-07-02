
###################################### Report 1 ##################################################
################################# Unemployment and job loss ######################################

# 02_Data_wrangling.R has to be run before this script

### Load clean working dataset

load("./Data_clean/LFS_clean.rda") # this is the file for analysis

q_LFS_clean <- as.data.frame(label(LFS_clean)) # variable labels
l_LFS_clean <- get_labels(LFS_clean, values = "n") # value labels 

#### Analysis 

### Calculate sample/population size 

N_total <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64) %>%
        group_by(as_label(QUARTER)) %>%
        summarise(Sample = n(), Population = sum(PWT18), Econ_active = sum(PWT18[ILODEFR <=2])) 

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
        geom_vline(xintercept = 7, linetype="dotted") +
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.06, 0.01), limits = c(0, 0.06), labels = scales::percent_format(accuracy = 1L)) +
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 1: Men’s unemployment rose most steeply",
             subtitle  = "Unemployment rate of UK population 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "text", x = 16, y = 0.053, label = "Men", color= "#DF9216") +
        annotate(geom = "text", x = 16, y = 0.040, label = "Women", color= "#791F83") +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.06, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.055, label = "2019") +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle=90),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 10),
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
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_wrap(.~as_label(SEX)) +
        
        #scales
        scale_y_continuous(breaks = seq(0,0.20, 0.02), limits = c(0,0.20), labels = scales::percent_format(accuracy = 1L)) +
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 2: Highest unemployment for young people",
             subtitle  = "Unemployment rate of UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.20, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.17, label = "2019") +
        
        # theme
        theme_economist_white(gray_bg = FALSE) +
        theme(plot.title = element_text(face = "bold",
                                        margin = ggplot2::margin(10, 0, 10, 0),
                                        size = 13),
              axis.text.x=element_text(angle=90),
              axis.title.x = element_text(margin = ggplot2::margin(t = 5), vjust = 0, size = 10),
              axis.title.y = element_text(margin = ggplot2::margin(r = 3), vjust = 2, size = 10), 
              axis.text = element_text(size = 10),
              legend.position = "top",
              legend.key.width = unit(0.5,"cm"),
              legend.margin = ggplot2::margin(1, 0, 1, 0, "cm"),
              legend.title = element_blank(),
              legend.text = element_text(size=10),
              strip.text = element_text(face = "bold", hjust = 0, size = 11)) +
        guides(size = FALSE, color = guide_legend(override.aes = list(size = 5))) 


# Unemployment per ethnicity

LFS_clean <- LFS_clean %>% mutate(ETHUKEUL_2 = recode_factor(factor(ETHUKEUL), 
                                                           `1` = "White",
                                                           `2` = "Non-White", `3` = "Non-White", 
                                                           `4` = "Non-White", `5` = "Non-White",
                                                           `6` = "Non-White",`7` = "Non-White",
                                                           `8` = "Non-White",`9` = "Non-White")) %>% 
                                                as_numeric(., keep.labels = TRUE)

unem_eth_gen <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64 & ILODEFR <=2) %>%
        group_by(QUARTER, as_label(ILODEFR), ETHUKEUL_2) %>%
        summarise(Active = sum(PWT18))  %>% 
        pivot_wider(names_from = `as_label(ILODEFR)`, values_from = Active) %>%
        adorn_totals("col") %>%
        mutate(Rate = `Not employed` / Total) 

unem_eth_gen  %>% 
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col=as_label(ETHUKEUL_2))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 7, linetype="dotted") +
        
        #scales
        scale_color_manual(values = c("#DF9216", "#6a51a3")) +
        scale_y_continuous(breaks = seq(0,0.1, 0.02), limits = c(0,0.1), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Unemployment has hit non-white ethnics groups the worst",
             subtitle  = "Unemployment rate UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.1, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.09, label = "2019", size = 3) +
        
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
        guides(color = guide_legend(override.aes = list(size = 5))) 


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
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_wrap(.~as_label(ETHUKEUL), ncol = 3) +
        
        #scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0,0.18), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 3: The steepest increases in unemployment were among minority ethnic groups",
             subtitle  = "Unemployment rate of UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.18, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.15, label = "2019", size = 3) +
        
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
        guides(color = guide_legend(override.aes = list(size = 5))) 


## Employment per occupation and sex

em_class <- LFS_clean %>% 
        filter(AGE >= 18 & AGE <= 64) %>%
        group_by(QUARTER, SEX, NSECMJ10_2) %>%
        summarise(pop = sum(PWT18), empl = sum(PWT18[ILODEFR ==1])) %>%
        mutate(Rate = empl / pop) %>%
        filter(NSECMJ10_2 != 6)

em_class %>% 
        # select data
        select(QUARTER, NSECMJ10_2, SEX, Rate) %>%  
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Rate, col = as_label(NSECMJ10_2))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_wrap(.~as_label(SEX)) +
        
        #scales
        scale_y_continuous(breaks = seq(0.7,1, 0.02), limits = c(0.7,1), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "Percentage of people in employment",
             title ="Figure 4: Employment has not recovered equally for everyone",
             subtitle  = "Employment rate of UK population per occupation, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0.7, ymax = 1, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.97, label = "2019", size = 3) +
        
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
              legend.margin = ggplot2::margin(0.5, 0, 0.5, 0, "cm"),
              legend.title = element_blank(),
              legend.text = element_text(size=11),
              strip.text = element_text(face = "bold", hjust = 0, size = 10)) +
        guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4)))



## Redundancies 

### who has been made redundant in the last three months? 

## overall estimates for everyone made redundant in the last three months 

# covers the number of people who were not in employment during the reference week 
# and who reported that they had been made redundant in the month of the reference 
# week or in the two calendar months prior to this; plus the number of people who were 
# in employment during the reference week who started their job in the same calendar month as,
# or the two calendar months prior to, the reference week, and who reported that they had 
# been made redundant in the past three months.


## Redundancy rate per sex

red_rate <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64 & REDUND == 1) %>%
        group_by(QUARTER, SEX) %>%
        summarise(Redund = sum(PWT18)) 

red_rate %>% 
        ggplot(aes(x = QUARTER, y = Redund, col = as_label(SEX))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +
        geom_vline(xintercept = 7, linetype="dotted") +
        
        # scales
        scale_colour_manual(values = c("#DF9216", "#791F83")) +
        scale_y_continuous(breaks = seq(0,225000, 25000), limits = c(0,225000)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19", "Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +

        # annotation
        labs(x = "", 
        y = "Number of people made redundant",
        title ="Up to four times more employees were made redundant in 2020",
        subtitle  = "Redundancy rate of UK population, 2019/20/21",
        caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 225000, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 180000, label = "2019", size = 3) +
        
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


## Reason for redundancy - percentage out of the total redundancies per quarter

red_reason <- LFS_clean %>%
        filter(REDUND == 1) %>%
        group_by(QUARTER, SEX, REDCLOS) %>%
        summarise(Reason = sum(PWT18)) %>%
        group_by(QUARTER) %>%
        mutate(Percent = Reason / sum(Reason))

red_reason %>% filter(!is.na(REDCLOS)) %>% 
        ggplot(aes(x = QUARTER, y = Percent, col = as_label(REDCLOS))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_wrap(~as_label(SEX)) + 
        
        #scales
        scale_y_continuous(breaks = seq(0,0.50, 0.05), limits = c(0,0.50), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        
        # annotation
        labs(x = "", 
             y = "% of employees made redundant",
             title ="Employers are not being able to keep their staff",
             subtitle  = "Redundancies of UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.5, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.43, label = "2019", size = 3) +

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
              legend.title.align=0.5,
              legend.text = element_text(size=11),
              strip.text = element_text(face = "bold", hjust = 0, size = 10)) +
        guides(color = guide_legend(title="Made redundant because:", title.position = "top", override.aes = list(size = 5))) 


## redundancy per reason and ethnicity

red_reason_eth <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64 & REDUND == 1) %>%
        group_by(QUARTER, SEX, ETHUKEUL_2, REDCLOS) %>%
        summarise(Reason = sum(PWT18)) %>%
        group_by(QUARTER) %>%
        mutate(Percent = Reason / sum(Reason))

red_reason_eth %>% 
        # filter data
        filter(!is.na(ETHUKEUL_2) & !is.na(REDCLOS)) %>% 
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Percent, col = as_label(REDCLOS))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_grid(as_label(SEX)~as_label(ETHUKEUL_2)) +
        
        #scales
        scale_y_continuous(breaks = seq(0,0.4, 0.05), limits = c(0,0.4), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 3: The steepest increases in unemployment were among minority ethnic groups",
             subtitle  = "Unemployment rate of UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.18, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.15, label = "2019", size = 3) +
        
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
        guides(color = guide_legend(override.aes = list(size = 5))) 


# redundancy by class

red_class <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64 & REDUND == 1) %>%
        group_by(QUARTER, SEX, NSECMJ10_2) %>%
        summarise(Redund = sum(PWT18)) %>%
        filter(!is.na(NSECMJ10_2) & NSECMJ10_2 != 6)

red_class %>% 
        ggplot(aes(x = QUARTER, y = Redund, col = as_label(NSECMJ10_2))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_wrap(~as_label(SEX)) + 
        
        #scales
        scale_y_continuous(breaks = seq(0,90000, 10000), limits = c(0,90000)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        
        # annotation
        labs(x = "", 
             y = "Number of workers",
             title ="Redundancies were concentrated in managerial and professionals as well as rutine occupations",
             subtitle  = "Redundancies of UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 90000, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 75000, label = "2019", size = 3) +
        
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
        guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 5))) 




## redundancy per personal reason

get_labels(LFS_clean$REDYL13)

LFS_clean <- LFS_clean %>% mutate(REDYL13_2 = recode_factor(factor(REDYL13), 
                                                            `1` = "Dismissal",
                                                            `2` = "Made redundant",
                                                            `3` = "Voluntary redundancy", 
                                                            `4` = "Temporary job", 
                                                             `5` = "Resignation",
                                                             `6` = "Personal or Health reasons",
                                                            `7` = "Retirement",
                                                             `8` = "Retirement",
                                                            `9` = "Personal or Health reasons",
                                                            `10` = "Education",
                                                            `11` = "Other reason")) %>% 
        as_numeric(., keep.labels = TRUE)




red_reason_1 <- LFS_clean %>%
        filter(AGE >= 18 & AGE <= 64 & REDUND == 1) %>%
        group_by(QUARTER, REDYL13_2, NSECMJ10_2) %>%
        summarise(Reason = sum(PWT18)) %>%
        group_by(QUARTER) %>%
        mutate(Percent = Reason / sum(Reason))

red_reason_1 %>% 
        # filter data
        filter(!is.na(NSECMJ10_2) & !is.na(REDYL13_2) & NSECMJ10_2 %in% c(1, 5) & REDYL13_2 %in% c(1, 2, 3, 4, 5, 6, 7)) %>% 
        
        # graphic type and variables 
        ggplot(aes(x = QUARTER, y = Percent, col = as_label(REDYL13_2))) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(shape = 19) +  
        geom_vline(xintercept = 7, linetype="dotted") +
        facet_wrap(.~as_label(NSECMJ10_2)) +
        
        #scales
        scale_y_continuous(breaks = seq(0,0.4, 0.05), limits = c(0,0.4), labels = scales::percent_format(accuracy = 1L)) + 
        scale_x_continuous(breaks=c(1:16), 
                           labels=c("Jan-Mar19","Apr-Jun19","Jul-Sep19","Oct-Dec19",
                                    "Jan-Mar20", "Feb-Apr20", "Mar-May20", "Apr-Jun20", "May-Jul20", 
                                    "Jun-Aug20", "Jul-Sep20","Aug-Oct20", "Sep-Nov20", "Oct-Dec20",
                                    "Nov20-Jan21", "Dec20-Feb21")) +
        # annotation
        labs(x = "", 
             y = "",
             title ="Figure 3: The steepest increases in unemployment were among minority ethnic groups",
             subtitle  = "Unemployment rate of UK population, 2019/20/21",
             caption = c("Source: UK Labour Force Survey (Person)")) +
        annotate(geom = "rect", xmin = 1, xmax = 4, ymin = 0, ymax = 0.18, alpha = 0.2) +        
        annotate(geom = "text", x = 2.5, y = 0.15, label = "2019", size = 3) +
        
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
        guides(color = guide_legend(override.aes = list(size = 5))) 




###  Predictive analytics 

## forest for unemployment

tree_data <- LFS_clean %>%
        filter(QUARTER >= 7 & 
               AGE >= 18 & AGE <= 64 & 
               ILODEFR %in% c("1","2") & # Employed/unemployed
               IOUTCOME %in% c("1","2")) %>% # personal/proxy response
        select(QUARTER, SEX, AGEEUL_2, ETHUKEUL, ILODEFR, REDUND, NSECMJ10_2) %>%
        mutate_at(c("QUARTER", "REDUND", "AGEEUL_2", "ETHUKEUL", "ILODEFR", "SEX", "NSECMJ10_2"), ~as.factor(.)) %>% 

        na.omit() %>% 
        droplevels()


# Partitioning the data intro training and test - Employment

set.seed(300)
index <- createDataPartition(y = tree_data$ILODEFR, times = 1, p = 0.3, list = FALSE)
train <- tree_data[-index,]
test <- tree_data[index,]
rm(index, LFS_clean)# free up memory

rf <- randomForest(ILODEFR ~ AGEEUL_2 +  SEX + ETHUKEUL + QUARTER, 
                   data = train, ntree = 200, importance = TRUE, #change number of trees and mtry to optimize OOB
                   mtry = 2)
rf
varImpPlot(rf)
importance(rf)

pred <- predict(rf, newdata = test)
# confusion matrix
table(test$ILODEFR,pred)

# random forest with oversampling

set.seed(42)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "up")

rf_up <- caret::train(ILODEFR ~ AGEEUL_2 + SEX + ETHUKEUL + QUARTER, 
                      data = train,
                      method = "rf",
                      ntree = 100,
                      trControl = ctrl)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "down")

rf_down <- caret::train(ILODEFR ~ AGE +  SEX + ETHUKEUL + QUARTER, 
                        data = train,
                        method = "rf",
                        ntree = 100,
                        trControl = ctrl)


