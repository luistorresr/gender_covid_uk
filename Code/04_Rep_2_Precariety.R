
###################################### Report 2 ##################################################
################################# Precarious work ################################################

# 01_load_packahes.R has to be run before this script

### Load clean working dataset (output from the data wrangling)

load("./Data_clean/LFS_clean.rda") # this is the file for analysis

q_LFS_clean <- as.data.frame(label(LFS_clean)) # variable labels
l_LFS_clean <- get_labels(LFS_clean, values = "n") # value labels 

## CREATING A SUB-DATASET

d_precariety <- LFS_clean %>% 
  # set variable types
  mutate_at(c("QUARTER", "PWT18", "PIWT18"), ~as.numeric(.)) %>%  
  # Apply basic filters
  filter(AGE >= 18 & AGE <= 64) %>% 
  filter(ILODEFR == 1) %>% # only people in employment
  filter(STAT <= 2) %>% # only employees and self-employed
  filter(STATR <= 2) # only employees and self-employed current job

### data table 

N_total_2 <- LFS_clean %>%
  filter(AGE >= 18 & AGE <= 64) %>% 
  filter(ILODEFR == 1) %>% # only people in employment
  filter(STAT <= 2) %>% # only employees and self-employed
  filter(STATR <= 2) %>% # only employees and self-employed current job
  group_by(as_label(QUARTER)) %>%
  summarise(Sample = n(), Population = sum(PWT18)) 

N_total_2 %>% flextable() %>% theme_vanilla() %>%
  set_caption(caption = "Table 1: Sample size and population estimates", style = "Table Caption") %>%
  bold(bold = TRUE, part = "header") %>%
  set_header_labels(`as_label(QUARTER)` = "Quarter",
                    Population = "Population estimates")  %>%
  autofit() %>%
  align_nottext_col(align = "center", header = TRUE, footer = FALSE) %>%
  add_footer_lines("Source: UK Labour Force Survey (Person)") %>%
  color(part = "footer", color = "#666666") 


### PART-TIME WORK (employees and self employed people only) 

#### by sex 

t_ptwrk <- d_precariety %>% # summary table
  filter(!is.na(FTPTWK)) %>%  # omit no answers and no applly
  group_by(QUARTER, SEX, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2)

p_ptwrk <- t_ptwrk %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_factor(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.5, 0.05), limits = c(0, 0.5), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 17, y = 0.12, label = "Men", color= "#DF9216", size = 5) +
  annotate(geom = "text", x = 17, y = 0.38, label = "Women", color= "#791F83", size = 5) +
  annotate(geom = "text", x = 7.1, y = 0.23, label = "Start of the COVID-19\nfirst lockdown", hjust = 0, size = 3.5) +
  annotate(geom = "text", x = 2, y = 0.03, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.03, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.03, label = "2021", size = 3.5) +
  
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
        legend.position = "none") 

p_ptwrk # print the plot


#### by ethnicity

d_precariety <- d_precariety %>% 
  mutate(ETHUKEUL_2 = recode_factor(factor(ETHUKEUL), # recode ethinicity 
                      `1` = "White",
                      `2` = "Minority-ethnic", `3` = "Minority-ethnic", 
                      `4` = "Minority-ethnic", `5` = "Minority-ethnic",
                      `6` = "Minority-ethnic",`7` = "Minority-ethnic",
                      `8` = "Minority-ethnic",`9` = "Minority-ethnic")) %>% as_numeric(., keep.labels = TRUE)

t_ptwrk_eth <- d_precariety %>% # table summary 
  filter(!is.na(FTPTWK)) %>% # omit no answers and no apply
  group_by(QUARTER, ETHUKEUL_2, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2 & !is.na(ETHUKEUL_2))

p_ptwrk_eth <- t_ptwrk_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(ETHUKEUL_2))) + ## line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.5, 0.05), limits = c(0, 0.5), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 7.1, y = 0.13, label = "Start of the COVID-19\nfirst lockdown", hjust = 0) +
  annotate(geom = "text", x = 2, y = 0.03, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.03, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.03, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 4))) # change the legend box size

p_ptwrk_eth # print the plot


#### by ethnicity and sex

t_ptwrk_seth <- d_precariety %>% # table summary 
  filter(!is.na(FTPTWK)) %>% # omit no answers and no apply
  group_by(QUARTER, SEX, ETHUKEUL_2, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2 & !is.na(ETHUKEUL_2))

p_ptwrk_seth <- t_ptwrk_seth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(ETHUKEUL_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(SEX)) +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.5, 0.05), limits = c(0, 0.5), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 7.1, y = 0.23, label = "Start of the COVID-19\nfirst lockdown", hjust = 0, size = 3.5) +
  annotate(geom = "text", x = 2, y = 0.03, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.03, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.03, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 4))) # change the legend box size

p_ptwrk_seth # print the plot


# by class

t_ptwrk_class <- d_precariety %>% # table summary 
  filter(!is.na(FTPTWK) & NSECMJ10_2 <= 5) %>% # # omit no answers and no apply / never worked
  group_by(QUARTER, NSECMJ10_2, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2 & !is.na(NSECMJ10_2) )

p_ptwrk_class <- t_ptwrk_class %>%
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.5, 0.05), limits = c(0, 0.5), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 7.1, y = 0.48, label = "Start of the COVID-19\nfirst lockdown", hjust = 0, size = 3.5) +
  annotate(geom = "text", x = 2, y = 0.03, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.03, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.03, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_ptwrk_class # print the plot


#### by class and sex

t_ptwrk_sclass <- d_precariety %>% # table summary 
  filter(!is.na(FTPTWK) & NSECMJ10_2 <= 5) %>% # omit no answers and no apply
  group_by(QUARTER, SEX, NSECMJ10_2, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, NSECMJ10_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2 & !is.na(NSECMJ10_2))

p_ptwrk_sclass <- t_ptwrk_sclass %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.6, 0.05), limits = c(0, 0.6), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.025, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.025, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.025, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_ptwrk_sclass # print the plot


#### sex, ethnicity and class 

t_ptwrk_mix <- d_precariety %>% # table summary 
  filter(!is.na(FTPTWK) & NSECMJ10_2 <= 5) %>% # # omit no answers and no apply
  group_by(QUARTER, SEX, ETHUKEUL_2, NSECMJ10_2, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2, NSECMJ10_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2 & !is.na(NSECMJ10_2) & !is.na(ETHUKEUL_2))

p_ptwrk_mix <- t_ptwrk_mix %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(as_label(ETHUKEUL_2) ~ as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.7, 0.1), limits = c(0, 0.7), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.65, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.65, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.65, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_ptwrk_mix # print the plot


### Reasons for going part-time

#### by sex 

t_yptwrk <- d_precariety %>% # table summary 
  filter(FTPTWK == 2) %>% # only those in par-time work
  group_by(QUARTER, SEX, YPTJOB) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(YPTJOB %in% c("3", "4")) # could not find / did not want ft job

p_yptwrk <- t_yptwrk %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(YPTJOB)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% reasons for part-time work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.02, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.02, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.02, label = "2021", size = 3.5) +

  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 4))) # change the legend box size

p_yptwrk # print graph


#### by ethnicity 

t_yptwrk_eth <- d_precariety %>% # table summary
  filter(FTPTWK == 2) %>% # only those in par-time work / ETHUKEUL_2 omit no answers and no apply
  group_by(QUARTER, ETHUKEUL_2, YPTJOB) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(YPTJOB %in% c("3", "4") & !is.na(ETHUKEUL_2)) # could not find / did not want ft job

p_yptwrk_eth <- t_yptwrk_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(ETHUKEUL_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(YPTJOB)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title =" ",
       subtitle  = "Part-time work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.02, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.02, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.02, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 4))) # change the legend box size

p_yptwrk_eth


#### by class

t_yptwrk_class <- d_precariety %>% # table summary 
  filter(FTPTWK == 2 & NSECMJ10_2 <= 5) %>% # only those in par-time work
  group_by(QUARTER, NSECMJ10_2, YPTJOB) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(YPTJOB %in% c("3", "4")) # could not find / did not want ft job

p_yptwrk_class <- t_yptwrk_class %>% # line graph
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) +
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(YPTJOB)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "Part-time work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.02, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.02, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.02, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_yptwrk_class


#### sex, ethinicity and class 

t_yptwrk_mix <- d_precariety %>% # table summary
  filter(FTPTWK == 2 & NSECMJ10_2 <= 5) %>% # only those in par-time work
  group_by(QUARTER, SEX, ETHUKEUL_2, NSECMJ10_2, YPTJOB) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2, NSECMJ10_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(YPTJOB %in% c("3") & !is.na(NSECMJ10_2) & !is.na(ETHUKEUL_2)) # could not find

p_yptwrk_mix <- t_yptwrk_mix %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_grid(as_label(SEX)~ as_label(ETHUKEUL_2)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","","Jul-\nSep","",
                              "Jan-\nMar", "", "", "Apr-\nJun", "",
                              "", "Jul-\nSep","", "", "Oct-\nDec",
                              "", "", "Jan-\nMar", "")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "Part-time work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +

  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 10),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_yptwrk_mix # print the plot


### TEMPORARY EMPLOYMENT

# "JOBTYP", # Whether job permanent (filter by Stat=1)

#### by sex 

t_jobtyp <- d_precariety %>% # summary table
  filter(!is.na(JOBTYP)) %>%  # omit no answer and no apply
  group_by(QUARTER, SEX, JOBTYP) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(JOBTYP == 2)

p_jobtyp <- t_jobtyp %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_factor(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.1, 0.01), limits = c(0, 0.1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 17, y = 0.043, label = "Men", color= "#DF9216", size = 5) +
  annotate(geom = "text", x = 17, y = 0.062, label = "Women", color= "#791F83", size = 5) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
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
        legend.position = "none") 

p_jobtyp # print the plot

#### ethinicity

t_jobtyp_eth <- d_precariety %>% # table summary 
  filter(!is.na(JOBTYP)) %>% # # omit no answer and no apply
  group_by(QUARTER, SEX, ETHUKEUL_2, JOBTYP) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(JOBTYP == 2 & !is.na(ETHUKEUL_2))

p_jobtyp_eth <- t_jobtyp_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(ETHUKEUL_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(SEX)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.1, 0.01), limits = c(0, 0.1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 17, y = 0.048, label = "White", color= "#DF9216", size = 5) +
  annotate(geom = "text", x = 16, y = 0.075, label = "Minority-ethnic", color= "#791F83", size = 5) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_jobtyp_eth # print the plot

#### class

t_jobtyp_class <- d_precariety %>% # table summary 
  filter(!is.na(JOBTYP) & NSECMJ10_2 <= 5) %>% # remove NA's
  group_by(QUARTER, NSECMJ10_2, JOBTYP) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(JOBTYP == 2 & !is.na(NSECMJ10_2))

p_jobtyp_class <- t_jobtyp_class %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.6, 0.05), limits = c(0, 0.6), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.009, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.009, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.009, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_jobtyp_class # print the plot


# "RESTMR6", # type of temporary job 

d_precariety <- d_precariety %>% 
  mutate(RESTMR6_2 = recode_factor(factor(RESTMR6), # recode categories in RESTMR6
                                   `1` = "Permanent",
                                   `2` = "Seasonal", `3` = "Seasonal", `4` = "Seasonal", `5` = "Seasonal", `6` = "Seasonal", `7` = "Seasonal", 
                                   `8` = "Fixed contract", `9` = "Fixed contract", `10` = "Fixed contract", `11` = "Fixed contract", `12` = "Fixed contract", `13` = "Fixed contract",
                                   `14` = "Agency", `15` = "Agency", `16` = "Agency", `17` = "Agency", `18` = "Agency", `19` = "Agency",
                                   `20` = "Casual", `21` = "Casual", `22` = "Casual", `23` = "Casual", `24` = "Casual", `25` = "Casual",
                                   `26` = "Other", `27` = "Other", `28` = "Other", `29` = "Other", `30` = "Other", `31` = "Other",
                                   `32` = "Type not stated")) %>% 
  as_numeric(., keep.labels = TRUE) 


#### by sex 

t_restmr2 <- d_precariety %>% # summary table
  filter(JOBTYP == 2) %>%  
  group_by(QUARTER, SEX, RESTMR6_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage 
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(RESTMR6_2) & RESTMR6_2 != 6) # remove "other"

p_restmr2 <- t_restmr2 %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(RESTMR6_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.6, 0.05), limits = c(0, 0.6), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_restmr2 # print the plot

#### ethinicity

t_restmr2_eth <- d_precariety %>% # table summary 
  filter(JOBTYP == 2) %>% # remove NA's
  group_by(QUARTER, RESTMR6_2, ETHUKEUL_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  # percentage
  mutate(percentage = count/sum(count)) %>% 
  filter(RESTMR6_2 != 6 & !is.na(ETHUKEUL_2) & !is.na(RESTMR6_2)) # remove "other"

p_restmr2_eth <- t_restmr2_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(RESTMR6_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(ETHUKEUL_2)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.6, 0.05), limits = c(0, 0.6), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_restmr2_eth # print the plot

#### class

t_restmr2_class <- d_precariety %>% # table summary 
  filter(JOBTYP == 2 &  NSECMJ10_2 <= 5) %>% # remove NA's
  group_by(QUARTER, RESTMR6_2, NSECMJ10_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(RESTMR6_2) & !is.na(NSECMJ10_2) & RESTMR6_2 != 6 & NSECMJ10_2 != 3) # remove "other" and self-employed

p_restmr2_class <- t_restmr2_class %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(RESTMR6_2)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0, 1), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.95, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.95, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.95, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_restmr2_class # print the plot


#### "TEMLEN", # Length of non-permanent job by type

d_precariety <- d_precariety %>% 
  mutate(TEMLEN_2 = recode_factor(factor(TEMLEN), # recode lengh of temporary job 
                                  `1` = "Less than 1 month",
                                  `2` = "Less than 3 months", 
                                  `3` = "Less than 6 months", 
                                  `4` = "Less than 12 months", 
                                  `5` = "Less than 18 months",
                                  `6` = "Less than 24 months",
                                  `7` = "2 years or more", `8` = "2 years or more", `9` = "2 years or more",
                                  `10` = "2 years or more", 
                                  `11` = "Time not fixed yet")) %>% as_numeric(., keep.labels = TRUE) 

##### by sex 

t_temlen <- d_precariety %>% # summary table
  filter(JOBTYP == 2) %>%  
  group_by(QUARTER, TEMLEN_2, SEX) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count))  %>% 
  filter(!is.na(TEMLEN_2) & TEMLEN_2 %in% c("1", "2", "3", "4")) # only contracts below 12 months

p_temlen <- t_temlen %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(TEMLEN_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(. ~ as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_temlen # print the plot


##### by ethnicity 

t_temlen_eth <- d_precariety %>% # summary table
  filter(JOBTYP == 2) %>%  
  group_by(QUARTER, SEX, ETHUKEUL_2, TEMLEN_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>%
  filter(!is.na(TEMLEN_2) & TEMLEN_2 %in% c("1", "2", "3", "4") & !is.na(ETHUKEUL_2))

p_temlen_eth <- t_temlen_eth %>%  
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(TEMLEN_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(ETHUKEUL_2) + as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_temlen_eth # print the plot

##### by class

t_temlen_class <- d_precariety %>% # summary table
  filter(JOBTYP == 2 & NSECMJ10_2 <= 5 ) %>%  
  group_by(QUARTER, NSECMJ10_2, TEMLEN_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(TEMLEN_2) & !is.na(NSECMJ10_2) & NSECMJ10_2 != 3)

p_temlen_class <- t_temlen_class %>% filter(TEMLEN_2 %in% c("1", "2", "3", "4")) %>%
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(TEMLEN_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(NSECMJ10_2)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.3, 0.02), limits = c(0, 0.3), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_temlen_class # print the plot


### "WHYTMP6", # Reason for taking non-permanent job (filter by JOBTYP = 2) 
### "RESTMR6", # type of temporary job and reason

### by sex 

#### Overall

t_whytem <- d_precariety %>% # table summary 
  # only temp
  filter(JOBTYP == 2) %>% # only those in par-time work
  group_by(QUARTER, SEX, WHYTMP6) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(WHYTMP6 %in% c("3", "4")) # could not find / did not want 

p_whytem <- t_whytem %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(SEX))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(WHYTMP6)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.4, 0.05), limits = c(0, 0.4), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "Reasons for temporary work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.02, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.02, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.02, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 4))) # change the legend box size

p_whytem # print graph


##### cannot find permanent job by contract type

t_restmr <- d_precariety %>% # table summary 
  filter(JOBTYP == 2) %>% # only those in part-time work
  group_by(QUARTER, RESTMR6_2, RESTMR6, SEX) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(RESTMR6) & RESTMR6 %in% c("4", "10", "16", "22")) # could not find

p_restmr <- t_restmr %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(RESTMR6_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_restmr # print the plot


#### by ethnicity 

##### overall 

t_whytem_eth <- d_precariety %>% # table summary
  # only temp
  filter(JOBTYP == 2) %>% # only those in par-time work
  group_by(QUARTER, ETHUKEUL_2, WHYTMP6) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(WHYTMP6 %in% c("3") & !is.na(ETHUKEUL_2)) # could not find 

p_whytem_eth <- t_whytem_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(ETHUKEUL_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.4, 0.05), limits = c(0, 0.4), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title =" ",
       subtitle  = "Reasons temporary work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.02, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.02, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.02, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(override.aes = list(size = 4))) # change the legend box size

p_whytem_eth

##### cannot find permanenet job by contract type

t_restmr_eth <- d_precariety %>% # table summary 
  filter(JOBTYP == 2) %>% # remove NA's
  group_by(QUARTER, RESTMR6_2, RESTMR6, ETHUKEUL_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, ETHUKEUL_2) %>% 
  # percentage
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(ETHUKEUL_2) & !is.na(RESTMR6) & RESTMR6 %in% c("4", "10", "16", "22")) # could not find

p_restmr_eth <- t_restmr_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(RESTMR6_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(ETHUKEUL_2)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_restmr_eth # print the plot


#### by class

t_whytem_class <- d_precariety %>% # table summary 
  filter(JOBTYP == 2 & NSECMJ10_2 <= 5) %>% # only temporary
  group_by(QUARTER, NSECMJ10_2, WHYTMP6) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(NSECMJ10_2 != 3 & WHYTMP6 %in% c("3")) # could not find 

p_whytem_class <- t_whytem_class %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(NSECMJ10_2))) +
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.5, 0.05), limits = c(0, 0.5), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "Reasons temporary work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.02, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.02, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.02, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_whytem_class


##### cannot find permanenet job by contract type

t_restmr_class <- d_precariety %>% # table summary 
  filter(JOBTYP == 2 & NSECMJ10_2 <= 5) %>% # remove NA's
  group_by(QUARTER, RESTMR6_2, RESTMR6, NSECMJ10_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(RESTMR6) & RESTMR6 %in% c("4", "10", "16", "22") & NSECMJ10_2 != 3 & !is.na(NSECMJ10_2)) # remove "other" and self-employed

p_restmr_class <- t_restmr_class %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(RESTMR6_2))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(NSECMJ10_2)) + 
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.3, 0.05), limits = c(0, 0.3), labels =
                       scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.29, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.29, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.29, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_restmr_class # print the plot


## Work arrangements

### "FLED10", # Type of agreed work arrangement

get_labels(d_precariety$FLED10) %>% as_tibble()

# 1 Flexitime               
# 2 Annualised hours        
# 3 Term time working       
# 4 Job sharing             
# 5 Nine day fortnight      
# 6 Four and a half day week
# 7 Zero hours contract     
# 8 On-Call Working         
# 9 None of these           
# 10 Don’t know   

# quarters in which the variable is not present = 1, 3, 5, 11

#### by sex 

t_fled <- d_precariety %>% # table summary 
  filter(QUARTER %in% c(2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18)) %>% 
  group_by(QUARTER, FLED10, SEX) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(FLED10) & FLED10 %in% c(1, 4, 7, 8)) # omit NAs and include only certain categories

p_fled <- t_fled %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(FLED10))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_fled # print the plot


#### ethnicity

t_fled_eth <- d_precariety %>% # table summary 
  filter(QUARTER %in% c(2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18)) %>% 
  group_by(QUARTER, FLED10, SEX, ETHUKEUL_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX, ETHUKEUL_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(FLED10) & !is.na(ETHUKEUL_2) & FLED10 %in% c(1, 2, 4, 7, 8)) # remove NA and include only certain categories

p_fled_eth <- t_fled_eth %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(FLED10))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(ETHUKEUL_2) + as_label(SEX)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_fled_eth # print the plot


#### class

t_fled_class <- d_precariety %>% # table summary 
  filter(QUARTER %in% c(2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18) & NSECMJ10_2 <= 5) %>% 
  group_by(QUARTER, FLED10, NSECMJ10_2) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, NSECMJ10_2) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(!is.na(FLED10) & FLED10 %in% c(1, 2, 4, 7, 8) & !is.na(NSECMJ10_2)) # only certain categories

p_fled_class <- t_fled_class %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = as_label(FLED10))) + # line graph
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~ as_label(NSECMJ10_2)) +
  
  # scale and colour
  scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.2), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="",
       subtitle  = "% of temporary workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 2, y = 0.005, label = "2019", size = 3.5) +
  annotate(geom = "text", x = 9.5, y = 0.005, label = "2020", size = 3.5) +
  annotate(geom = "text", x = 17, y = 0.005, label = "2021", size = 3.5) +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 9),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", hjust = 0, size = 12)) + # change the face_wrap title
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) # change the legend box size

p_fled_class # print the plot


###"VARYHR", # Whether weekly hours tend to vary




