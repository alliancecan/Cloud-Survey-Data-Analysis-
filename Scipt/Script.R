
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

option1 <- 
  lapply(seq_along(my_list), 
         function(i) cbind(id_status, 
                           my_list[[i]])) #bind ID and status to the different dfs
length(option1)
#Option 1 is a list of questions

# lapply(seq_along(option1), function(i) gather("aa","bb", -Status))

#option2
option2 <- 
  survey %>%
  gather("Question", "Answer", -X.U.FEFF.Internal.ID, -Status) %>%
  extract(Question, c("Ques_num", "Ques"), "([[:alnum:]]+)..([[:graph:]]+)") #%>%
# spread(Ques_num, Answer)

questions_n <- read.csv("questions_n.csv")

option2_clean <- 
  option2 %>% 
  left_join(questions_n, by = "Ques_num") %>% 
  select(-Ques)

unique_id <- c(1:nrow(option2_clean)) #Create unique ID for the function "spread"

option2_select_spread <- 
  cbind(unique_id, option2_clean)


test1 <- spread(option2_select_spread, question, Answer)
test2 <- pivot_wider(option2_select, 
                     names_from = question,
                     values_from = Answer,
                     values_fn = list)
# Analyze the data --------------------------------------------------------

option2 <- drop_na(option2, Answer)

table <- reshape(option2, idvar = "X.U.FEFF.Internal.ID", timevar = "Ques_num", direction = "wide")

############## different approach

df <- as_tibble(drop_na(option2, Answer))

df_sep <- within(df, Ques<-data.frame(do.call('rbind', strsplit(as.character(Ques), '..', fixed=TRUE))))
df_sep <- df_sep %>% select()

df_replace_dots <-
  gsub("[.]", "-", df$Ques) 
df_replace_dots <- as.character(df_replace_dots)


separate(df_replace_dots ,x, c("aa", "bb"), "--")

aa <- df %>% 
extract(Ques, c("aa", "bb"), "([[:graph:]]+)..([[:graph:]])")
