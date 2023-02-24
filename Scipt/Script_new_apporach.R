# Prepare the directory and load the libraries ----------------------------

###Set working directory
setwd("D:/Documents/LDP/Productivity and Reproducibility/Git/Cloud-Survey-Data-Analysis-/Data")
###load libraries
library(tidyverse)
library(stringr)
library(maps)
library(mapdata)
library(ggmap)
library(mapcan)
library(moonBook)
library(webr)
library(ggthemes)
library(RColorBrewer)
library(rmarkdown)
library(gtsummary)
library(gt)
library(RColorBrewer)
library(likert)
library(ggthemes)

###Load the data

#survey data, english and french versions
survey_en <- read.csv("Final_CloudSurvey_EN_20230221.csv",
                   header = T,
                   encoding = "UTF-8",
                   na.strings=c("","NA"))

survey_fr <- read.csv("Final_CloudSurvey_FR_20230221.csv",
                       header = T,
                       encoding = "UTF-8",
                       na.strings=c("","NA"))

#Questions
questions_n <- read.csv("Questions_n_EN_20230223.csv")#this table has the questions in rows (n=47)
#To add the french version

# Organizing data ----------------------------------------------------------------

###Re-organize the data
survey_organized <- 
  survey_en %>%
  gather("Question",
         "Answer", 
         -Internal.ID) %>%
  extract(Question, 
          c("Ques_num", "Ques"), 
          "([[:alnum:]]+)..([[:graph:]]+)") %>% #This function separates question number and question (e.g., X1..Question = X1 | Question)
  drop_na(Answer)

#Clean the questions: join the questions from table "questions_n" using the question's number "X.."
survey_organized_clean <- 
  survey_organized %>% 
  left_join(questions_n, by = "Ques_num")

survey_organized_clean1 <- 
  survey_organized_clean %>% 
  select(Internal.ID, Ques_num, Answer)

survey_organized_spread <- pivot_wider(survey_organized_clean1, 
                                       names_from = Ques_num,
                                       values_from = Answer,
                                       values_fn = list)



# test2 <- test1 %>% unnest(X1)
# test3 <- test2 %>% unnest(X3)
# X7 <- survey_organized_spread %>% unnest(X7)
# x7_x3 <- X7 %>% unnest(X3) %>% select(Respond_ID, X3, X7)

# Demographic of respondents ----------------------------------------------

survey_x1.x2<- 
  survey_organized_spread %>% 
  select(Internal.ID, X1, X2) %>% 
  unnest(c(X1, X2)) %>% 
  rename(Geography = X2, Role = X1)

survey_x1.x2$Role <- as.factor(survey_x1.x2$Role)
survey_x1.x2$Role[survey_x1.x2$Role == "Other (please specify)"] <- "NULL"


#Print demographical table
demographic %>% 
  group_by(Geography)%>% 
  summarize(n = n()) %>% 
  arrange(desc(n),.by_group = T) %>% 
  mutate("%" = (n / sum(n))*100) %>%
  gt()


### Ontario ######
Ontario <-filter(demographic, Geography %in% "Ontario") %>% 
  select( Geography, Affiliation) %>%
  group_by(Affiliation)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()
