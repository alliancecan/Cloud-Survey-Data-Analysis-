# Prepare the directory and load the libraries ----------------------------

#Set working directory
setwd("C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/Cloud survey")

#load libraries
library(tidyverse)
library(stringr)

#Load the data
survey <- read.csv("Cloud_Survey2023_UTF.csv", header = T,  encoding = "UTF-8",
                   na.strings=c("","NA")) %>% 
  select(-First.name, -Last.name, -Email)

questions_n <- read.csv("questions_n.csv")#this table has the questions in rows (n=47)
# Prepare the data --------------------------------------------------------
glimpse(survey)

#Re-organize the data
survey_organized <- 
  survey %>%
  gather("Question", "Answer", -X.U.FEFF.Internal.ID, -Status) %>%
  extract(Question, c("Ques_num", "Ques"), "([[:alnum:]]+)..([[:graph:]]+)") %>% #This function separates question number and question (e.g., X1..Question = X1 | Question)
  drop_na(Answer)

#Clean the questions: join the questions from table "questions_n" using the question's number "X.."
option2_clean <- 
  survey_organized %>% 
  left_join(questions_n, by = "Ques_num") %>% 
  select(-Ques) %>% 
  rename(Respond_ID = X.U.FEFF.Internal.ID)

#Create a unique ID per row to spread the data = each question in a separate column
unique_id <- c(1:nrow(option2_clean))

#combine unique Id and option2_clean table
option2_select_spread <- 
  cbind(unique_id, option2_clean) #%>% 
  # select(-Ques_num)


survey_spread_questions <- spread(option2_select_spread, Question, Answer)

test2 <- pivot_wider(option2_select_spread, 
                     names_from = Question,
                     values_from = Answer,
                     values_fn = list)
# Analyze the data --------------------------------------------------------

