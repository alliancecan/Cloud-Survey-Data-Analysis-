# Prepare the directory and load the libraries ----------------------------

###Set working directory
setwd("C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/Cloud survey")

###load libraries
library(tidyverse)
library(stringr)

###Load the data

#survey data
survey <- read.csv("Cloud_Survey2023_UTF.csv",
                   header = T,
                   encoding = "UTF-8",
                   na.strings=c("","NA")) %>% 
  select(-First.name, -Last.name, -Email)

###
questions_n <- read.csv("questions_n.csv")#this table has the questions in rows (n=47)


# Organizing data ----------------------------------------------------------------

###Re-organize the data
survey_organized <- 
  survey %>%
  gather("Question",
         "Answer", 
         -X.U.FEFF.Internal.ID, 
         -Status) %>%
  extract(Question, 
          c("Ques_num", "Ques"), 
          "([[:alnum:]]+)..([[:graph:]]+)") %>% #This function separates question number and question (e.g., X1..Question = X1 | Question)
  drop_na(Answer)

#Clean the questions: join the questions from table "questions_n" using the question's number "X.."
survey_organized_clean <- 
  survey_organized %>% 
  left_join(questions_n, by = "Ques_num") %>% 
  rename(Respond_ID = X.U.FEFF.Internal.ID)

survey_organized_clean1 <- 
  survey_organized_clean %>% 
  select(Respond_ID, Ques_num, Answer)

survey_organized_spread <- pivot_wider(survey_organized_clean1, 
                                       names_from = Ques_num,
                                       values_from = Answer,
                                       values_fn = list)


test1 <- head(survey_organized_spread)

test2 <- test1 %>% unnest(X1)
test3 <- test2 %>% unnest(X3)

X7 <- survey_organized_spread %>% unnest(X7)
x7_x3 <- X7 %>% unnest(X3) %>% select(Respond_ID, X3, X7)

survey$TC3 <- recode_factor(survey$TC3, CIHR = "Health Research", 
                            NSERC = "Sciences and Engineering", SSHRC = "Social Sciences and Humanities")

# getting number of respondents in this question.

Workflow <- select(survey, CaseId, TC3, starts_with("G1M"), -ends_with("O")) %>%
  gather(Option, Source, 3:32) %>% filter(!Source == "")


nHR <- filter(Workflow, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #240
nSE <- filter(Workflow, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric()#644
nSSH <- filter(Workflow, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #262

Workflow_Health <- filter(Workflow, TC3=="Health Research") %>%
  group_by(TC3, Source) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow, TC3=="Sciences and Engineering") %>%
  group_by(TC3, Source) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, Source) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 


Workflow_Tri <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  


# test4 <- list()#create an empty list
# for(i in 2:ncol(test2)){
#   test4[i] <- unnest(test2[i]) %>% unique()
# }
#    
# 
# 
# test4 <- list()#create an empty list
# for(i in 2:ncol(test2)){
#   test4[i] <- unnest(test2[i])
# }
# 
# sapply(test2, function(i) unnest(test2[i]))
