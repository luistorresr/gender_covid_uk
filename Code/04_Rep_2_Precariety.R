
###################################### Report 2 ##################################################
################################# Precarious work ################################################

# 01_load_packahes.R has to be run before this script

### Load clean working dataset (output from the data wrangling)

load("./Data_clean/LFS_clean.rda") # this is the file for analysis

q_LFS_clean <- as.data.frame(label(LFS_clean)) # variable labels
l_LFS_clean <- get_labels(LFS_clean, values = "n") # value labels 


## Creating a sub-dataset

data_precariety <- LFS_clean %>% 
  # set variable types
  mutate_at(c("AGEEUL_2","ETHUKEUL", "SEX", "NSECMJ10_2"), ~as.factor(.)) %>%
  mutate_at(c("QUARTER", "PWT18", "PIWT18"), ~as.numeric(.)) %>%  
  # Apply basic filters
  filter(AGE >= 18 & AGE <= 64) %>% 
  filter(ILODEFR == 1) %>% # only people in employment
  filter(STAT <= 2) # only employees and self-employed


### Part time work trend (employees and self employed people only) 

### by sex 

ptwrk <- data_precariety %>% 
  filter(!is.na(FTPTWK)) %>% 
  group_by(QUARTER, SEX, FTPTWK) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage part-time per quarter
  mutate(percentage = count/sum(count)) %>% 
  filter(FTPTWK == 2)

# line graph
ptwrk_plot <- ptwrk %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = SEX)) +
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.5, 0.05), limits = c(0, 0.5), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="Figure 1: Slight decline in part-time work for women ",
       subtitle  = "% of part-time workers in the UK, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 18, y = 0.12, label = "Men", color= "#DF9216") +
  annotate(geom = "text", x = 18, y = 0.38, label = "Women", color= "#791F83") +
  annotate(geom = "text", x = 7.1, y = 0.06, label = "Start of the COVID-19\nfirst lockdown", hjust = 0) +
  annotate(geom = "text", x = 2, y = 0.01, label = "2019") +
  annotate(geom = "text", x = 9.5, y = 0.01, label = "2020") +
  annotate(geom = "text", x = 17, y = 0.01, label = "2021") +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        legend.position = "none") 

ptwrk_plot # print the plot








### Reasons for going part-time

yptwrk <- data_precariety %>% 
  # only partimers
  filter(FTPTWK == 2) %>% 
  group_by(QUARTER, SEX, YPTJOB) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER, SEX) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)) %>% 
  filter(YPTJOB %in% c("3", "4")) # could not find / did not want ft job

# line graph
yptwrk_plot <- yptwrk %>% 
  ggplot(aes(x = QUARTER, y = percentage, colour = SEX)) +
  geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
  geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(shape = 19) +
  geom_vline(xintercept = 7, linetype="dotted") +
  facet_wrap(.~as_label(YPTJOB)) + 
  
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous(breaks = seq(0,0.8, 0.05), limits = c(0, 0.8), labels =
                       scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks=c(1:18),
                     labels=c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
  # annotation
  labs(x = "", 
       y = "",
       title ="Figure 1: Slight decline in part-time work for women ",
       subtitle  = "Part-time work UK population, 2019/20/21",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  annotate(geom = "text", x = 18, y = 0.12, label = "Men", color= "#DF9216") +
  annotate(geom = "text", x = 18, y = 0.38, label = "Women", color= "#791F83") +
  annotate(geom = "text", x = 7.1, y = 0.06, label = "Start of the COVID-19\nfirst lockdown", hjust = 0) +
  annotate(geom = "text", x = 2, y = 0.01, label = "2019") +
  annotate(geom = "text", x = 9.5, y = 0.01, label = "2020") +
  annotate(geom = "text", x = 17, y = 0.01, label = "2021") +
  
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(plot.margin = unit(c(1, 5, 1, 1), "lines"),
        plot.title = element_text(margin = ggplot2::margin(10, 0, 10, 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 5),
                                    vjust = 0, size = 12),
        axis.title.y = element_text(margin = ggplot2::margin(r = 3),
                                    vjust = 2, size = 12),
        legend.position = "none") 

yptwrk_plot





# Ethnicity and part-time
yptwrk3 <- lfs %>% 
  # only partimers
  filter(FTPTWK == "2") %>%  # employed
  group_by(QUARTER, ETHUKEUL, SEX, YPTJOB) %>% 
  summarise(count = sum(PWT18)) %>% 
  group_by(QUARTER) %>% 
  # percentage reason
  mutate(percentage = count/sum(count)*100) %>% 
  filter(YPTJOB == "3") %>% # Did not find ft job
  filter(!is.na(ETHUKEUL))

# stacked graph
ggplot(yptwrk3, aes(x = QUARTER, y = percentage, colour = SEX)) +
  geom_line(size = 1, show.legend = FALSE) +
  # scale and colour
  scale_colour_manual(values = c("#DF9216", "#791F83")) +
  scale_y_continuous("%") +
  scale_x_continuous(breaks = c(1, 4, 8, 12),
                     labels = c("Jan-Mar20", "Apr-Jun20", "Aug-Oct20", "Dec20-Feb21")) +
  facet_wrap(~ETHUKEUL, labeller = labeller(ETHUKEUL = c("1" = "White",
                                                         "2" = "Mixed/Multiple",
                                                         "3" = "Indian",
                                                         "4" = "Pakistani",
                                                         "5" = "Bangladeshi",
                                                         "6" = "Chinese",
                                                         "7" = "Other Asian",
                                                         "8" = "Black/African/Caribbean/Black British",
                                                         "9" = "Other")),
             ncol = 3) +
  # annotation
  labs(x = NULL, y = NULL,
       title ="",
       subtitle = "Percentage of <span style = 'color:#DF9216;'>men</span> and <span style = 'color:#791F83;'>women</span>  that could not find a full-time job",
       caption = c("Source: UK Labour Force Survey (Person)")) +
  # theme
  theme_economist_white(gray_bg = FALSE) +
  theme(legend.position = "none",
        plot.margin = unit(c(1,3,1,1),units = "lines"),
        plot.title = element_text(vjust = 3),
        plot.subtitle =  element_markdown(hjust = 0),
        axis.text.x = element_text(angle = 90)) 