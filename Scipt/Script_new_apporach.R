# Prepare the directory and load the libraries ----------------------------

###Set working directory
setwd("C:/Users/fsdha/Desktop/Cloud-Survey-Data-Analysis-Fares/Data")
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

# Demographic of respondents - data prep----------------------------------------------

#Roles and geograpgy
survey_x1.x2<- 
  survey_organized_spread %>% 
  select(Internal.ID, X1, X2) %>% 
  unnest(c(X1, X2)) %>% 
  rename(Geography = X2, Role = X1)

roles <- read.csv("Survey_roles_20230223.csv")

#Delete answers in "other (please specify)
survey.x1.x2.no <- 
  survey_x1.x2 %>% 
  left_join(roles, by = "Role") %>% 
  filter(Need_to_be_changed == "no")

#Check that the number of others = the number of indep answers
survey.x1.x2.no %>% 
  group_by(Role) %>% 
  count() #other = 39

survey.x1.x2.yes <- 
  survey_x1.x2 %>% 
  left_join(roles, by = "Role") %>% 
  filter(Need_to_be_changed == "yes")

no <- survey.x1.x2.yes %>% 
  group_by(Role) %>% 
  count()

sum(no$n) # = 38

###produce nested table: roles per demography
survey.x1.x2 <- 
  survey.x1.x2.no %>% 
  select(Internal.ID, Role, Geography)


#Print demographical table
demographic <- 
  survey.x1.x2 %>% 
  select(-Role) %>% 
  unique() %>% 
  drop_na()

demographic %>% 
  group_by(Geography)%>% 
  summarize(n = n()) %>% 
  arrange(desc(n),.by_group = T) %>% 
  mutate("%" = (n / sum(n))*100) %>%
  gt()


### Ontario ######
Ontario <-filter(survey.x1.x2, Geography %in% "Ontario") %>% 
  select( Geography, Role) %>%
  group_by(Role)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()

Ontario$Role <- factor(Ontario$Role, levels=unique(Ontario$Role))


#### Bar graph for Role - Ontario #####
ggplot(Ontario, aes(x = Role)) +
  geom_col(aes(y=n, fill=Role)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Ontario") +
  xlab("Role") + 
  ylab("Number of respondents")

### Quebec ######
Quebec <-filter(survey.x1.x2, Geography %in% "Quebec") %>% 
  select( Geography, Role) %>%
  group_by(Role)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()


Quebec$Role <- factor(Quebec$Role, levels=unique(Quebec$Role))


#### Bar graph for Role - Quebec ######
ggplot(Quebec, aes(x = Role)) +
  geom_col(aes(y=n, fill=Role)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Québec") +
  xlab("Role") + 
  ylab("Number of respondents")


### West ####
West <-filter(survey.x1.x2, Geography %in% c("Alberta", "British Columbia", "Saskatchewan","Manitoba")) %>% 
  select( Geography, Role) %>%
  group_by(Role)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()


West$Role <- factor(West$Role, levels=unique(West$Role))


#### Bar graph for Role - Western Canada ####
ggplot(West, aes(x = Role)) +
  geom_col(aes(y=n, fill=Role)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Western Canada") +
  xlab("Role") + 
  ylab("Number of respondents")


### East #####
East <-filter(survey.x1.x2, Geography %in% c("New Brunswick", "Nova Scotia", "Newfoundland and Labrador", "Prince Edward Island")) %>% 
  select( Geography, Role) %>%
  group_by(Role)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()

East$Role <- factor(East$Role, levels=unique(East$Role))


#### Bar graph for Role - Eastern Canada ####
ggplot(East, aes(x = Role)) +
  geom_col(aes(y=n, fill=Role)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Eastern Canada") +
  xlab("Role") + 
  ylab("Number of respondents")


### All Provinces ####
Canada <- filter(survey.x1.x2, !Geography %in% c("National", "Regional", "International")) %>% 
  group_by(Geography) %>% 
  summarize(props = n()) %>%   
  arrange(props) 

Canada$Geography <- factor(Canada$Geography, levels = unique(Canada$Geography))


#### Pie chart - All Canada ####
PieDonut(Canada, aes(Geography, count= props), ratioByGroup = FALSE, showPieName=FALSE, r0=0.25,r1=1,r2=1.4,start=pi/2,labelpositionThreshold=1, showRatioThreshold = F, title= "Respondents by Region", titlesize = 5) #+ 
  # scale_fill_manual(values =  cbp1)

