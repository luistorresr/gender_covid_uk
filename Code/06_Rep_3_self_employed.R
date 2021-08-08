
###################################### Report 3 #############################
######################  COVID-19 and self-employment ########################

# Load dataset

load("./Data_clean/LFS_clean.rda") # this is the base file

## CREATING A SUB-DATASET FOR ANALYSIS

d_self <- LFS_clean %>% 
  # set variable types
  mutate_at(c("QUARTER", "PWT18", "PIWT18"), ~as.numeric(.)) %>%  
  # Apply basic filters
  filter(AGE >= 18 & AGE <= 64) %>% 
  filter(ILODEFR == 1) # only people in employment 

## Sample and population   
  
N_total_3 <- d_self %>%
  group_by(as_label(QUARTER)) %>%
  summarise(Sample = n(), In_employment = sum(PWT18), Self_employed = sum(PWT18[INECAC05 == 2]))

N_total_3 %>% flextable() %>% theme_vanilla() %>%
  set_caption(caption = "Table 1: Sample size and population estimates", style = "Table Caption") %>%
  bold(bold = TRUE, part = "header") %>%
  set_header_labels(`as_label(QUARTER)` = "Quarter",
                    In_employment = "In employment",
                    Self_employed = " Self-employed")  %>%
  autofit() %>%
  align_nottext_col(align = "center", header = TRUE, footer = FALSE) %>%
  add_footer_lines("Note: Workers between 18-64 years old") %>%
  add_footer_lines("Source: UK Labour Force Survey (Person)") %>%
  color(part = "footer", color = "#666666")  


# Analysis 

# What is the self-employed representation in employment? 

## totals

t_self_total <- d_self %>% # Totals
  group_by(QUARTER) %>% 
  summarise(self = sum(PWT18[INECAC05 == 2]))

p_self_total <- t_self_total %>% 
  ggplot(aes(x = QUARTER, y = self)) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_vline(xintercept = 7, linetype="dotted") +
  geom_bar(stat="identity", width=0.5, fill = c("#367BB8")) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,5000000, 500000), limits = c(0, 5000000), labels = comma) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 4800000, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 4800000, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 4800000, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size=11))

p_self_total # print 


## by sex

t_self_rate <- d_self %>% # rate to in employment population
  # Apply basic filters
  group_by(QUARTER, SEX) %>% 
  summarise(empl = sum(PWT18[ILODEFR ==1]), self = sum(PWT18[INECAC05 == 2])) %>% 
  group_by(QUARTER, SEX) %>% 
  # rate compared to the 
  mutate(rate_self = self / empl)

p_self_rate <- t_self_rate %>% 
  ggplot(aes(x = QUARTER, y = rate_self, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.208), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 17, y = 0.145, label = "Men", color= "#DF9216", size = 4) +
  annotate(geom = "text", x = 17, y = 0.085, label = "Women", color= "#791F83", size = 4) +
  annotate(geom = "text", x = 7.1, y = 0.03, label = "Start of the COVID-19\nfirst lockdown", hjust = 0, size = 3.1) +
  annotate(geom = "text", x = 2, y = 0.207, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.207, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.207, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") 

p_self_rate # plot



## in main and second jobs by sex

t_self_rate_sex_emain <- d_self %>% # main as an employee with a second job as self-employee
  filter(STATR == 1 & STAT2 == 2) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(total = sum(PWT18)) %>% 
  mutate(worker_type = 1)

t_self_rate_sex_smain <- d_self %>% # main as self-employee with a second job as  employee
  filter(STATR == 2 & STAT2 == 1) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(total = sum(PWT18)) %>% 
  mutate(worker_type = 2)

t_self_rate_sex_smains <- d_self %>% # main as self-employee with a second job as self-employee
  filter(STATR == 2 & STAT2 == 2) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(total = sum(PWT18)) %>% 
  mutate(worker_type = 3)

t_self_rate_sex_main_second <- rbind(t_self_rate_sex_emain, t_self_rate_sex_smain, t_self_rate_sex_smains)

t_self_rate_sex_main_second <- t_self_rate_sex_main_second %>% set_labels(worker_type, 
                                                                          labels = c("Main job: Employee | Second job: Self-employed" = 1,
                                                                                     "Main job: Self-employed | Second job: Employee" = 2,
                                                                                     "Main job: Self-employed | Second job: Self-employed" = 3))

t_self_rate_sex_main_second <- t_self_rate_sex_main_second %>% set_labels(SEX, 
                                                                          labels = c("Men" = 1,
                                                                                     "Women" = 2))

p_self_rate_sex_main_second  <- t_self_rate_sex_main_second  %>% #filter(worker_type != 1) %>%
  ggplot(aes(x = QUARTER, y = total, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(worker_type)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  #scale_y_continuous(breaks = seq(0,100000, 10000), limits = c(0, 100000), labels = comma) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 95000, label = "2019", size = 3.1) +  
  annotate(geom = "text", x = 9.5, y = 95000, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 95000, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size)  

p_self_rate_sex_main_second # plot



### by ethnicity and sex

d_self <- d_self %>% mutate(ETHUKEUL_2 = recode_factor(factor(ETHUKEUL), 
                                                             `1` = "White",
                                                             `2` = "Minority-ethnic", `3` = "Minority-ethnic", 
                                                             `4` = "Minority-ethnic", `5` = "Minority-ethnic",
                                                             `6` = "Minority-ethnic",`7` = "Minority-ethnic",
                                                             `8` = "Minority-ethnic",`9` = "Minority-ethnic")) %>% as_numeric(., keep.labels = TRUE)

t_self_rate_eth <- d_self %>% # rate to in employment population
  # Apply basic filters
  group_by(QUARTER, SEX, ETHUKEUL_2) %>% 
  summarise(empl = sum(PWT18[ILODEFR ==1]), self = sum(PWT18[INECAC05 == 2])) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2) %>% 
  # rate compared to the 
  mutate(rate = self / empl) %>%
  filter(!is.na(ETHUKEUL_2))

p_self_rate_eth <- t_self_rate_eth %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_grid(.~ as_label(ETHUKEUL_2)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.22, 0.02), limits = c(0, 0.225), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 17, y = 0.165, label = "Men", color= "#DF9216", size = 4) +
  annotate(geom = "text", x = 17, y = 0.105, label = "Women", color= "#791F83", size = 4) +
  annotate(geom = "text", x = 7.1, y = 0.03, label = "Start of the COVID-19\nfirst lockdown", hjust = 0, size = 3.1) +
  annotate(geom = "text", x = 2, y = 0.224, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.224, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.224, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_rate_eth # plot


### Where do they work by sex, ethnicity 

#"COUNTRY",  # Country within UK

## by sex

t_self_rate_sex_country <- d_self %>% # rate to in employment population
  # Apply basic filters
  group_by(QUARTER, COUNTRY2, SEX) %>% 
  summarise(empl = sum(PWT18[ILODEFR ==1]), self = sum(PWT18[INECAC05 == 2])) %>% 
  group_by(QUARTER, COUNTRY2, SEX) %>% 
  # rate compared to the 
  mutate(rate_self = self / empl)

p_self_rate_sex_country <- t_self_rate_sex_country %>% 
  ggplot(aes(x = QUARTER, y = rate_self, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(COUNTRY2)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.25, 0.02), limits = c(0, 0.25), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 9.5, y = 0.244, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.244, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size)  

p_self_rate_sex_country # plot


### by ethnicity 

t_self_rate_eth_country <- d_self %>% # rate to in employment population
  # Apply basic filters
  group_by(QUARTER, COUNTRY2, ETHUKEUL_2) %>% 
  summarise(empl = sum(PWT18[ILODEFR ==1]), self = sum(PWT18[INECAC05 == 2])) %>% 
  group_by(QUARTER, COUNTRY2, ETHUKEUL_2) %>% 
  # rate compared to the 
  mutate(rate = self / empl) %>%
  filter(!is.na(ETHUKEUL_2))

p_self_rate_eth_country <- t_self_rate_eth_country %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(ETHUKEUL_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(COUNTRY2)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.20, 0.02), limits = c(0, 0.207), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.207, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.207, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.207, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_rate_eth_country # plot


#"INDE07M", # Industry section in main job 

get_labels(d_self$INDE07M)

d_self <- d_self %>% mutate(INDE07M_2 = recode_factor(factor(INDE07M), 
                                                       `1` = "Agriculture, forestry and fishing",
                                                       `2` = "Energy and water", `3` = "Manufacturing", 
                                                       `4` = "Construction", `5` = "Distribution, hotels and restaurants",
                                                       `6` = "Transport and communication",`7` = "Banking and finance",
                                                       `8` = "Public admin, education and health",`9` = "Other services")) %>% as_numeric(., keep.labels = TRUE)



## by sex 

t_self_rate_sex_industry <- d_self %>% # rate to in employment population
  # Apply basic filters
  group_by(QUARTER, INDE07M_2, SEX) %>% 
  summarise(empl = sum(PWT18[ILODEFR ==1]), self = sum(PWT18[INECAC05 == 2])) %>% 
  group_by(QUARTER, INDE07M_2, SEX) %>% 
  # rate compared to the 
  mutate(rate_self = self / empl) %>%
  filter(!is.na(INDE07M_2))

p_self_rate_sex_industry <- t_self_rate_sex_industry %>% 
  ggplot(aes(x = QUARTER, y = rate_self, colour = as_label(INDE07M_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(SEX)) + 
  
  # scale and colour

  scale_y_continuous(breaks = seq(0,0.60, 0.05), limits = c(0, 0.60), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 9.5, y = 0.244, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.244, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size)  

p_self_rate_sex_industry # plot


### by ethnicity 

t_self_rate_eth_industry <- d_self %>% # rate to in employment population
  # Apply basic filters
  group_by(QUARTER, ETHUKEUL_2, INDE07M_2) %>% 
  summarise(empl = sum(PWT18[ILODEFR ==1]), self = sum(PWT18[INECAC05 == 2])) %>% 
  group_by(QUARTER, ETHUKEUL_2, INDE07M_2) %>% 
  # rate compared to the 
  mutate(rate = self / empl) %>%
  filter(!is.na(ETHUKEUL_2) & !is.na(INDE07M_2))

p_self_rate_eth_industry <- t_self_rate_eth_industry %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(INDE07M_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_grid(.~ as_label(ETHUKEUL_2)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.55, 0.05), limits = c(0, 0.56), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.56, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.56, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.56, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_rate_eth_industry # plot


## How formal they are?

### sex 

t_self_main <- d_self %>% 
  filter(STATR == 2) %>%
  group_by(QUARTER, SEX) %>% 
  summarise(total = sum(PWT18)) # total self employment in main job 

t_self_main_employees <- d_self %>% 
  filter(STATR == 2 & SOLOR == 2) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(with_employees = sum(PWT18)) # total self-employed with employees in main job

t_self_main_employees_rate <- left_join(t_self_main, t_self_main_employees, by = c("QUARTER", "SEX"))
t_self_main_employees_rate <- t_self_main_employees_rate  %>% mutate(rate = with_employees / total, category = 1)

t_self_second <- d_self %>% 
  filter(STAT2 == 2) %>%
  group_by(QUARTER, SEX) %>% 
  summarise(total = sum(PWT18)) # total self employment in second job

t_self_second_employees <- d_self %>% 
  filter(STAT2 == 2 & SOLO2 == 2) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(with_employees = sum(PWT18)) # total self-employed with employees in second job

t_self_second_employees_rate <- left_join(t_self_second, t_self_second_employees, by = c("QUARTER", "SEX"))
t_self_second_employees_rate <- t_self_second_employees_rate  %>% mutate(rate = with_employees / total, category = 2)

t_self_employees_rate <- rbind(t_self_main_employees_rate, t_self_second_employees_rate)

t_self_employees_rate <- t_self_employees_rate %>% set_labels(category, 
                                                                labels = c("Self-employment as main job" = 1,
                                                                           "Self-employment as second job" = 2))


p_self_employees_rate <- t_self_employees_rate %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(category)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.208), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.207, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.207, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.207, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_employees_rate # plot


### ethnicity 

t_self_main_eth <- d_self %>% 
  filter(STATR == 2) %>%
  group_by(QUARTER, ETHUKEUL_2) %>% 
  summarise(total = sum(PWT18)) # total self employment in main job 

t_self_main_employees_eth <- d_self %>% 
  filter(STATR == 2 & SOLOR == 2) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  summarise(with_employees = sum(PWT18)) # total self-employed with employees in main job

t_self_main_employees_rate_eth <- left_join(t_self_main_eth, t_self_main_employees_eth, by = c("QUARTER", "ETHUKEUL_2"))
t_self_main_employees_rate_eth <- t_self_main_employees_rate_eth  %>% mutate(rate = with_employees / total, category = 1)

t_self_second_eth <- d_self %>% 
  filter(STAT2 == 2) %>%
  group_by(QUARTER, ETHUKEUL_2) %>% 
  summarise(total = sum(PWT18)) # total self employment in second job

t_self_second_employees_eth <- d_self %>% 
  filter(STAT2 == 2 & SOLO2 == 2) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  summarise(with_employees = sum(PWT18)) # total self-employed with employees in second job

t_self_second_employees_rate_eth <- left_join(t_self_second_eth, t_self_second_employees_eth, by = c("QUARTER", "ETHUKEUL_2"))
t_self_second_employees_rate_eth <- t_self_second_employees_rate_eth %>% mutate(rate = with_employees / total, category = 2)

t_self_employees_rate_eth <- rbind(t_self_main_employees_rate_eth, t_self_second_employees_rate_eth) %>% filter(!is.na(ETHUKEUL_2))

t_self_employees_rate_eth <- t_self_employees_rate_eth %>% set_labels(category, 
                                                              labels = c("Self-employment as main job" = 1,
                                                                         "Self-employment as second job" = 2))
t_self_employees_rate_eth <- t_self_employees_rate_eth %>% set_labels(ETHUKEUL_2, 
                                                                      labels = c("White" = 1,
                                                                                 "Minority-ethnic" = 2))

p_self_employees_rate_eth <- t_self_employees_rate_eth %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(ETHUKEUL_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(category)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.25, 0.02), limits = c(0, 0.25), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.207, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.207, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.207, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_employees_rate_eth # plot


## Self-employment hours

### part-time vs full time

#"FTPTWK", # Whether full or part time in main job (filter for the required economics activity)  

t_self <- d_self %>% 
  filter(INECAC05 == 2) %>%
  group_by(QUARTER, SEX) %>% 
  summarise(total = sum(PWT18))

t_self_ft <- d_self %>% # rate to in employment population
  filter(INECAC05 == 2 & FTPTWK == 1) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(subtotal= sum(PWT18)) %>%
  mutate(jobtype = 1)

t_self_pt <- d_self %>% # rate to in employment population
  filter(INECAC05 == 2 & FTPTWK == 2) %>% 
  group_by(QUARTER, SEX) %>% 
  summarise(subtotal= sum(PWT18)) %>%
  mutate(jobtype = 2)

t_self_ft_rate <- left_join(t_self, t_self_ft, by = c("QUARTER", "SEX"))
t_self_ft_rate <- t_self_ft_rate %>% mutate(rate = subtotal / total)

t_self_pt_rate <- left_join(t_self, t_self_pt, by = c("QUARTER", "SEX"))
t_self_pt_rate <- t_self_pt_rate %>% mutate(rate = subtotal / total)

t_self_ftpt_rate <- rbind(t_self_ft_rate, t_self_pt_rate) 

t_self_ftpt_rate <- t_self_ftpt_rate %>% set_labels(jobtype, 
                                                           labels = c("Full-time" = 1,
                                                                      "Part-time" = 2))

p_self_ftpt_rate <- t_self_ftpt_rate %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_grid(.~ as_label(jobtype)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +  
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.56, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.56, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.56, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_ftpt_rate # plot


#"VARYHR", # Whether weekly hours tend to vary / not much information
#"UNDEMP", # Whether would like to work longer hours, at current basic rate of pay, given the opportunity / not much information

t_self_varyhr <- d_self %>% 
  filter(INECAC05 == 2 & UNDEMP == 1) %>%
  group_by(QUARTER, SEX) %>% 
  summarise(hours_vary = sum(PWT18))

t_self_varyhr_rate <- left_join(t_self, t_self_varyhr, by = c("QUARTER", "SEX"))
t_self_varyhr_rate <- t_self_varyhr_rate  %>% mutate(rate = hours_vary / total)

p_self_varyhr_rate <- t_self_varyhr_rate %>% 
  ggplot(aes(x = QUARTER, y = rate, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +  
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.56, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.56, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 0.56, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size) 

p_self_varyhr_rate# plot


# "PAIDHRU", # Paid hours - based on usual hours per week, including paid overtime only

## sex

mean(d_self$PAIDHRU[d_self$INECAC05 == 2], na.rm = TRUE)

hours <- d_self %>% select(QUARTER, INECAC05, SEX, ETHUKEUL_2, PAIDHRU, EVEROT, POTHR, UOTHR) %>%
  filter(INECAC05 %in% c(1, 2))

p_hours <- hours %>%  filter(!is.na(PAIDHRU) & INECAC05 ==2) %>%
  ggplot(aes(x = as_factor(QUARTER), y = PAIDHRU, fill = as_label(SEX))) + 
  geom_boxplot(aes(ymin=..lower.., ymax=..upper..), alpha=0.2, outlier.shape=NA, show.legend = FALSE) +
  stat_summary(aes(col = as_label(SEX)), fun = mean, geom = "point", shape=19, size = 3, show.legend = TRUE, alpha=1) + 
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +  
  scale_fill_manual(values = c("#DF9216", "#791F83")) + 
  scale_y_continuous(breaks = seq(0,80, 5), limits = c(-1, 82)) + 
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                   labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                            "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                            "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                            "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "Paid hours including overtime",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 82, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 82, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 82, label = "2021", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = -1, label = "Circles represent averages", size = 4) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", hjust = 0, size = 11)) + # change the face_wrap title
  guides(col = guide_legend(override.aes = list(size = 5)))

p_hours# plot

ggplotly(p_hours) %>%
  layout(boxmode = "group",
         yaxis = list(fixedrange = TRUE),
         xaxis = list(fixedrange = TRUE),
         margin = list(l = 0)) %>%
  config(displayModeBar = FALSE) %>% 
  layout(xaxis = list(showticklabels = TRUE, autotick = TRUE, tickfont = list(size = 11)),
         yaxis = list(tickfont = list(size = 11)), 
         legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       yanchor = "top",
                       x = 0.5, y= 1.2,
                       font  = list(size = 13))) 

## ethnicity

p_hours_eth <- hours %>%  filter(!is.na(ETHUKEUL_2) & !is.na(PAIDHRU) & INECAC05 == 2) %>% 
  ggplot(aes(x = as_factor(QUARTER), y = PAIDHRU, fill = as_label(ETHUKEUL_2))) + 
  geom_boxplot(alpha=0.5, outlier.shape=NA, show.legend = FALSE) +
  stat_summary(aes(col = as_label(ETHUKEUL_2)), fun = mean, geom = "point", shape=19, size = 3, show.legend = TRUE, alpha=1) + 
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_vline(xintercept = 7, linetype="dotted") + 
  #facet_wrap(.~ as_label(INECAC05)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +  
  scale_fill_manual(values = c("#DF9216", "#791F83")) + 
  scale_y_continuous(breaks = seq(0,80, 5), limits = c(-1, 82)) + 
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                   labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                            "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                            "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                            "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "Paid hours including overtime",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 82, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 82, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 82, label = "2021", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = -1, label = "Circles represent averages", size = 4) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", hjust = 0, size = 11)) + # change the face_wrap title
  guides(col = guide_legend(override.aes = list(size = 5)))

p_hours_eth # plot

ggplotly(p_hours_eth) %>%
  layout(boxmode = "group",
         yaxis = list(fixedrange = TRUE),
         xaxis = list(fixedrange = TRUE),
         margin = list(l = 0)) %>%
  config(displayModeBar = FALSE) %>% 
  layout(xaxis = list(showticklabels = TRUE, autotick = TRUE, tickfont = list(size = 11)),
         yaxis = list(tickfont = list(size = 11)), 
         legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       yanchor = "top",
                       x = 0.5, y= 1.2,
                       font  = list(size = 13))) 

# "POTHR", # Usual hours of paid overtime
# "UOTHR", # Usual hours of unpaid overtime

## sex

t_povertime <- hours %>% filter(INECAC05 == 2 & EVEROT == 1 & POTHR >= 1 & !is.na(POTHR)) %>% 
  mutate(overtime = POTHR, overtime_type = 1) %>% 
  select(QUARTER, SEX, ETHUKEUL_2, overtime, overtime_type)

t_nonpovertime <- hours %>% filter(INECAC05 == 2 & EVEROT == 1 & UOTHR >= 1 & !is.na(UOTHR)) %>% 
  mutate(overtime = UOTHR, overtime_type = 2) %>% 
  select(QUARTER, SEX, ETHUKEUL_2, overtime, overtime_type)

t_overtime <- rbind(t_povertime, t_nonpovertime)

t_overtime <- t_overtime %>% set_labels(overtime_type, 
                                           labels = c("Usual hours of PAID overtime" = 1,
                                                      "Usual hours of UNPAID overtime" = 2))

p_overtime <- t_overtime %>%  
  ggplot(aes(x = as_factor(QUARTER), y = overtime, fill = as_label(SEX))) + 
  geom_boxplot(aes(ymin=..lower.., ymax=..upper..), alpha=0.3, outlier.shape=NA, show.legend = TRUE) +
  stat_summary(aes(col = as_label(SEX)), fun = mean, geom = "point", shape=19, size = 3, show.legend = TRUE, alpha=1) + 
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_vline(xintercept = 7, linetype="dotted") + 
  facet_wrap(.~ as_label(overtime_type)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +  
  scale_fill_manual(values = c("#DF9216", "#791F83")) + 
  coord_cartesian(ylim = c(1, 15)) + 
  scale_y_continuous(breaks = seq(1,20, 1)) + 
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                   labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                            "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                            "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                            "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "Hours",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 20.5, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 20.5, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 20.5, label = "2021", size = 3.1) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", hjust = 0, size = 11)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 5))) 

p_overtime  # plot


## ethnicity   
  
p_overtime_eth <- t_overtime %>% filter(!is.na(ETHUKEUL_2)) %>%
  ggplot(aes(x = as_factor(QUARTER), y = overtime, fill = as_label(ETHUKEUL_2))) + 
  geom_boxplot(alpha=0.5, outlier.shape=NA, show.legend = FALSE) +
  stat_summary(aes(col = as_label(ETHUKEUL_2)), fun = mean, geom = "point", shape=19, size = 3, show.legend = TRUE, alpha=1) + 
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_vline(xintercept = 7, linetype="dotted") + 
  facet_wrap(.~ as_label(overtime_type)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +  
  scale_fill_manual(values = c("#DF9216", "#791F83")) + 
  scale_y_continuous(breaks = seq(1,20, 1), limits = c(0, 20.5)) + 
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                   labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                            "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                            "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                            "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "Hours",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 20.5, label = "2019", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 20.5, label = "2020", size = 3.1) +
  annotate(geom = "text", x = 17, y = 20.5, label = "2021", size = 3.1) +
  annotate(geom = "text", x = 9.5, y = 0.2, label = "Circles represent averages", size = 4) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(face = "bold", hjust = 0, size = 11)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 5))) 

p_overtime_eth  # plot  
