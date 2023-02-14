
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


# Prepare the data --------------------------------------------------------
glimpse(survey)

##Step1: extract a list of question-numbers
#Gather all the question columns
survey_q_gathererd <- 
  survey %>% 
  gather("question", "answer", 3:189) 

#Extract the first charctacters = "X1.", "X2.", ..., "X47"
n_question <- 
  unique(stringr::str_sub(survey_q_gathererd$question,
                          1, 3))

#add the number of questions in a list
list_questions <- list(n_question)

##Step2: Group the answers by question. E.g., all Questions 1's answers together
id_status <- 
  select(survey, X.U.FEFF.Internal.ID, Status)#select first two columns to use later for cbind

#option1
my_list <- list()#create an empty list

for(i in seq_along(n_question)) {
  my_list[[i]] <- select(survey, contains(n_question[i]))#for loop to spread the questions into independent dataframes (dfs)
}

option1 <- lapply(seq_along(my_list), function(i) cbind(id_status, my_list[[i]])) #bind ID and status to the different dfs
length(option1)
#Option 1 is a list of questions

# lapply(seq_along(option1), function(i) gather("aa","bb", -Status))

#option2
option2 <- 
  survey %>%
  gather("Question", "Answer", -X.U.FEFF.Internal.ID, -Status) %>%
  extract(Question, c("Ques_num", "Ques"), "([[:alnum:]]+)..([[:graph:]]+)") #%>%
# spread(Ques_num, Answer)

option2_select <- 
  option2 %>% 
  subset(Ques_num == "X1" | Ques_num =="X2") %>% #select only questions X1 and X2
  drop_na(Answer) %>% #Delete all empty rows
  rename(ID = X.U.FEFF.Internal.ID) %>%
  select(-Ques) %>% #delete column Ques
  mutate(question = if_else(Ques_num == "X1", "Role", "Province")) %>% #Create a new colum based on a condition
  select(-Ques_num) #delete column Ques_num

unique_id <- c(1:569) #Create unique ID for the function "spread"

option2_select_spread <- 
  cbind(unique_id, option2_select)


test1 <- spread(option2_select_spread, question, Answer)
test2 <- pivot_wider(option2_select, 
                     names_from = question,
                     values_from = Answer,
                     values_fn = list)
# Analyze the data --------------------------------------------------------


