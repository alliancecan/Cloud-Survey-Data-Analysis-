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


# Option 1 ----------------------------------------------------------------
#Re-organize the data
survey_organized <- 
  survey %>%
  gather("Question", "Answer", -X.U.FEFF.Internal.ID, -Status) %>%
  extract(Question, c("Ques_num", "Ques"), "([[:alnum:]]+)..([[:graph:]]+)") %>% #This function separates question number and question (e.g., X1..Question = X1 | Question)
  drop_na(Answer)

#Clean the questions: join the questions from table "questions_n" using the question's number "X.."
survey_organized_clean <- 
  survey_organized %>% 
  left_join(questions_n, by = "Ques_num") %>% 
  rename(Respond_ID = X.U.FEFF.Internal.ID)

#Create a unique ID per row to spread the data = each question in a separate column
unique_id <- c(1:nrow(survey_organized_clean))

#combine unique Id and survey_organized_clean table
option2_select_spread <- 
  cbind(unique_id, survey_organized_clean) %>% 
  select(-Ques_num)

#Spread the table to have questions = columns
survey_spread_questions <- spread(option2_select_spread, Question, Answer)
##-> not very accurate - need to work more on this option


# Option 2 ----------------------------------------------------------------

#Create each column separetly
survey_questions_manually <- 
  survey_organized %>% 
  rename(ID = X.U.FEFF.Internal.ID) %>%  #select only questions X1 and X2
  drop_na(Answer) %>% #Delete all empty rows
  select(-Ques) %>% #delete column Ques
  mutate(question = if_else(Ques_num == "X1", "Please describe your role(s).",
                            if_else(Ques_num == "X2", "Please select which province or territory you are located in to access cloud services.",
                                    if_else(Ques_num == "X3", "Please choose your research domain based on the Canadian Research and Development Classification (CRDC) 2020 associated with your cloud experience.",
                                            if_else(Ques_num == "X4", "Do you currently, or have you in the past, use(d) cloud resources (see options in Question 5) to support your research?",
                                                    if_else(Ques_num == "X5", "Please indicate how important these cloud services are to support your research.",
                                                            if_else(Ques_num == "X6", "Feel free to specify other services not listed in Question 5:",
                                                                    if_else(Ques_num == "X7", "For what purpose(s) do you use cloud services to support your research?",
                                                                            if_else(Ques_num == "X8", "Please describe how you use cloud services to support your research. For example, what software, type of analysis, workflows and data movement into and out of the cloud is involved with your research using cloud computing in the cloud?",
                                                                                    if_else(Ques_num == "X9", "Which method(s) of interacting with cloud resources are you familiar with?",
                                                                                            if_else(Ques_num == "X10", "What factors do you consider in choosing which cloud platform to use (Alliance or commercial)?",
                                                                                                    if_else(Ques_num == "X11", "Do your cloud needs include storing or processing controlled or sensitive data (e.g., data owned by First Nations, personal data, data subject to a data sharing agreement or specific security requirements)?",
                                                                                                            if_else(Ques_num == "X12", "Have you used a commercial cloud provider?",
                                                                                                                    if_else(Ques_num == "X13", "Approximately how many dollars (CDN) in cloud credits or vendor in-kind funds did your research group consume over the last calendar year on commercial cloud resources?",
                                                                                                                            if_else(Ques_num == "X14", "Approximately how many dollars (CDN) of research funds did your research group spend over the last calendar year on commercial cloud resources?",
                                                                                                                                    if_else(Ques_num == "X15", "How acis your commercial cloud budget funded?",
                                                                                                                                            if_else(Ques_num == "X16", "What is/was your primary reason for using a commercial cloud?",
                                                                                                                                                    if_else(Ques_num == "X17", "What components of the commercial cloud do you use?",
                                                                                                                                                            if_else(Ques_num == "X18", "Are there particular platforms or software services you currently deploy or wish to deploy in your commercial cloud environments which you feel would be valuable to be offered as a national service for all researchers to access?",
                                                                                                                                                                    if_else(Ques_num == "X19", "Which of the following functions have you used the commercial cloud for?",
                                                                                                                                                                            if_else(Ques_num == "X20", "Is any of the content stored or shared on the commercial cloud controlled, high risk or sensitive data?",
                                                                                                                                                                                    if_else(Ques_num == "X21", "Do you need to securely share data stored on the commercial cloud with specific collaborators?",
                                                                                                                                                                                            if_else(Ques_num == "X22", "How do you transfer the data stored on the commercial cloud?",
                                                                                                                                                                                                    if_else(Ques_num == "X23", "Do you have any concerns about storing data on the commercial cloud (e.g., safety/security, backups, costs of downloading or moving data)?",
                                                                                                                                                                                                            if_else(Ques_num == "X24", "At the end of your research project, where do you transfer data stored on the commercial cloud?",
                                                                                                                                                                                                                    if_else(Ques_num == "X25", "Have you lost access to data stored on the commercial cloud due to lack of continuity of funding for your research?",
                                                                                                                                                                                                                            if_else(Ques_num == "X26", "Do you get support when using the commercial cloud?",
                                                                                                                                                                                                                                    if_else(Ques_num == "X27", "Would you like support from the Alliance when using the commercial cloud?",
                                                                                                                                                                                                                                            if_else(Ques_num == "X28", "Have you used the Alliance Community Cloud?",
                                                                                                                                                                                                                                                    if_else(Ques_num == "X29", "What is/was your primary reason for using the Alliance Cloud?",
                                                                                                                                                                                                                                                            if_else(Ques_num == "X30", "What components of the Alliance Cloud do you use?",
                                                                                                                                                                                                                                                                    if_else(Ques_num == "X31", "If you selected “other,” please describe how you use the Alliance Community Cloud:",
                                                                                                                                                                                                                                                                            if_else(Ques_num == "X32", "Are there cloud services which you would like to be able to access on the Alliance Cloud but currently cannot (e.g., Managed Kubernetes, Galaxy, Docker)?",
                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X33", "Are there particular platforms or software services you currently deploy or wish to deploy in your own cloud environments which you feel would be valuable to be offered as a national service for all researchers to access?",
                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X34", "How have you used the Alliance Cloud to share data (if at all)?",
                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X35", "Is any of the content stored or shared on the Alliance Cloud controlled, high risk or sensitive data?",
                                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X36", "Do you need to securely share data stored on the Alliance cloud-specific collaborators?",
                                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X37", "How do you transfer the data stored on the Alliance Cloud?",
                                                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X38", "Do you have any concerns about storing data on the Alliance Cloud (e.g., safety/security, backups)?",
                                                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X39", "Would it be helpful to have tools that track your cloud data storage and remind you to back up or migrate content?",
                                                                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X40", "On top of the existing resources, which of the following technical support methods would you like to access?",
                                                                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X41", "What cloud-related training topics would be of interest to you?",
                                                                                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X42", "Should the Alliance allocate part of its budget to provide commercial cloud credits as an alternative to a Rapid Access Service, Resource Allocation Competitions or Research Platforms and Portals award? If you answered “yes”, feel free to provide additional comments.",
                                                                                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X43", "What other communication channels/groups/communities discussing/supporting cloud services should we be aware of and engage with?",
                                                                                                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X44", "How often do you store research outputs on cloud platforms (i.e., NextCloud, Dropbox, Google Drive, One Drive etc.)?",
                                                                                                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X45", "Do you keep content at the end of your research project stored in cloud platforms (e.g., Next Cloud, Dropbox, Google Drive, OneDrive etc.)?",
                                                                                                                                                                                                                                                                                                                                                                                            if_else(Ques_num == "X46", "How often is the content stored in cloud platforms related to a publication?",
                                                                                                                                                                                                                                                                                                                                                                                                    if_else(Ques_num == "X47", "Is there anything else not covered in this survey, such as future planning around how the Alliance can improve cloud services for Canadian researchers?", "NA",
                                                                                                                                                                                                                                                                                                                                                                                                    )))))))))))))))))))))))))))))))))))))))))))))))) %>% #Create a new colum based on a condition
  select(-Ques_num) #delete column Ques_num

survey_questions_manually_spread <- pivot_wider(survey_questions_manually, 
                                                names_from = question,
                                                values_from = Answer,
                                                values_fn = list) #this option create a list, not ideal for data anlysis

df <- apply(survey_questions_manually_spread,2,as.character)#unlist the df so I can save it in csv
# write.csv(df, "df.csv")

#Load the table = this table has one respondant per row, and one question per column. A cell can contains more than one answer, if the question is multiple choice.

df <- read.csv("df.csv")


# Data analyzes - option 1: tidy data -------------------------------------
#Use: cleaned version of survey_organized_clean
survey_organized_clean_V1 <- read.csv("survey_organized_clean_V1.csv")

aa <- survey_organized_clean_V1 %>% 
  group_by(Ques_num, Answer) %>% 
  count()

