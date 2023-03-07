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
survey <- read.csv("FINAL_Cloud_Survey_EN-FR_20220228.csv",
                   header = T,
                   encoding = "UTF-8",
                   na.strings=c("","NA"))

survey <- survey[c(-10,-12, -20, -21)] #delete columns "Other" and old "old please specify" have been reclassified into the "NEW_Other" column

#Questions
questions_n <- read.csv("Questions_n_EN_20230223.csv")#this table has the questions in rows (n=47)
#To add the french version

# Organizing data ----------------------------------------------------------------

###Re-organize the data
survey_organized <- 
  survey %>%
  gather("Question",
         "Answer", 
         -X.U.FEFF.Internal.ID) %>%
  extract(Question, 
          c("Ques_num", "Ques"), 
          "([[:alnum:]]+)..([[:graph:]]+)") %>% #This function separates question number and question (e.g., X1..Question => col1 = X1 | col2 = Question)
  drop_na(Answer)

#Clean the questions: join the questions from table "questions_n" using the question's number "X.."
survey_organized_clean <- 
  survey_organized %>% 
  left_join(questions_n, by = "Ques_num")

#Delete extra columns by selecting important ones:
survey_organized_clean1 <- 
  survey_organized_clean %>% 
  select(X.U.FEFF.Internal.ID, Ques_num, Answer) %>% 
  rename (Internal.ID = X.U.FEFF.Internal.ID)

#Spread the table: the output will have one column per question (in a list)
survey_organized_spread <- pivot_wider(survey_organized_clean1, 
                                       names_from = Ques_num,
                                       values_from = Answer,
                                       values_fn = list)


# General Survey Questions ############################################################################################
### Q1 - Please describe your role(s). ######

#### data cleaning & preparation####
#select questions X1 and X2
survey_x1.x2_v1<- 
  survey_organized_spread %>% 
  select(Internal.ID, X1, X2) %>% 
  unnest(c(X1, X2)) %>% 
  rename(Geography = X2, Role = X1)

survey_x1.x2_v2 <- 
  survey_x1.x2_v1 %>% 
  mutate(Role_n = ifelse(
    Role == "Professor ", "Professor", ifelse(
      Role == "Researcher", Role, ifelse(
        Role == "Student (Undergraduate/Graduate) ", "Student (Undergraduate/Graduate)", ifelse(
          Role == "Administrator ", "Administrator", ifelse(
            Role == "Research Staff", Role, ifelse(
              Role == "Other", Role, ifelse( #add delete to say that all "Other (please specifiy) rows should be deleted
                Role == "Post-Doctoral Fellow", Role, ifelse(
                  Role == "Research Associate", Role, ifelse(
                    Role == "Government", Role, ifelse(
                      Role == "Librarian", Role, ifelse(
                        Role == "Otjer", "Other", ifelse(
                          Role == "Researcher ", "Researcher", Role))))))))))))) # n = 12 unique roles
survey_x1.x2_v2 <- unique(survey_x1.x2_v2)

#### summary table - Roles ####
roles_summary <- 
  survey_x1.x2_v2 %>% 
  select(-Geography,) %>% 
  group_by(Role_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

#### Pie chart - Roles ####

PieDonut(roles_summary, 
         aes(Role_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.25,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5) #+ 

### Q2 - Please select which province or territory you are located in to access cloud services.. ######
#### data cleaning & preparation - Demography ####

###produce nested table: roles per demography

#Print demographical table
demographic <- 
  survey_x1.x2_v2 %>% 
  select(-Role) %>% 
  unique() %>% 
  drop_na()

demographic %>% 
  group_by(Geography)%>% 
  summarize(n = n()) %>% 
  arrange(desc(n),.by_group = T) %>% 
  mutate("%" = (n / sum(n))*100) %>%
  gt()


#### Ontario ######
Ontario <-filter(survey_x1.x2_v2, Geography %in% "Ontario") %>% 
  select(Geography, Role_n) %>%
  group_by(Role_n)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()

Ontario$Role_n <- factor(Ontario$Role_n, levels=unique(Ontario$Role_n))
Ontario <- drop_na(Ontario)

#### Bar graph for Role_n - Ontario #####
ggplot(Ontario, aes(x = Role_n)) +
  geom_col(aes(y=n, fill=Role_n)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Ontario") +
  xlab("Role_n") + 
  ylab("Number of respondents")

#### Quebec ######
Quebec <-filter(survey_x1.x2_v2, Geography %in% "Québec") %>% 
  select( Geography, Role_n) %>%
  group_by(Role_n)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()


Quebec$Role_n <- factor(Quebec$Role_n, levels=unique(Quebec$Role_n))

Quebec <- drop_na(Quebec)

#### Bar graph for Role_n - Quebec ######
ggplot(Quebec, aes(x = Role_n)) +
  geom_col(aes(y=n, fill=Role_n)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Québec") +
  xlab("Role_n") + 
  ylab("Number of respondents")


#### West ####
West <-filter(survey_x1.x2_v2, Geography %in% c("Alberta", "British Columbia", "Saskatchewan","Manitoba")) %>% 
  select( Geography, Role_n) %>%
  group_by(Role_n)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()


West$Role_n <- factor(West$Role_n, levels=unique(West$Role_n))


#### Bar graph for Role_n - Western Canada ####
ggplot(West, aes(x = Role_n)) +
  geom_col(aes(y=n, fill=Role_n)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Western Canada") +
  xlab("Role_n") + 
  ylab("Number of respondents")


#### East #####
East <-filter(survey_x1.x2_v2, Geography %in% c("New Brunswick", "Nova Scotia", "Newfoundland and Labrador", "Prince Edward Island")) %>% 
  select( Geography, Role_n) %>%
  group_by(Role_n)  %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  data.frame()

East$Role_n <- factor(East$Role_n, levels=unique(East$Role_n))


#### Bar graph for Role_n - Eastern Canada ####
ggplot(East, aes(x = Role_n)) +
  geom_col(aes(y=n, fill=Role_n)) + 
  # scale_fill_manual(values =  cbp1) +
  geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
  coord_flip() + 
  theme_linedraw() +
  theme(legend.position = "none") +
  ggtitle("Eastern Canada") +
  xlab("Role_n") + 
  ylab("Number of respondents")


#### All Provinces ####
Canada <- filter(survey_x1.x2_v2, !Geography %in% c("National", "Regional", "International")) %>% 
  group_by(Geography) %>% 
  summarize(props = n()) %>%   
  arrange(props) 

Canada$Geography <- factor(Canada$Geography, levels = unique(Canada$Geography))


#### Pie chart - All Canada ####
#Comment: group ester provinces
PieDonut(Canada, aes(Geography, count= props), ratioByGroup = FALSE, showPieName=FALSE, r0=0.25,r1=1,r2=1.4,start=pi/2,labelpositionThreshold=1, showRatioThreshold = F, title= "Respondents by Region", titlesize = 5) #+ 
  # scale_fill_manual(values =  cbp1)


### Q3 - Please choose your research domain ######

Domain_Breakdown <- survey_organized_spread %>% 
  select(Internal.ID, X3) %>% 
  unnest(X3) %>% 
  rename(Domain = X3)

#### summary table - Domain ####
domain_summary <- 
  Domain_Breakdown %>% 
  group_by(Domain) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

#### Pie chart - ####
PieDonut(domain_summary, 
         aes(Domain, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5)

# Nested Domain-Role
roles_df <- 
  survey_x1.x2_v2 %>% 
  select(-Geography, -Role,) %>% #select ony roles and ID
  unique()

#join roles and domains based on the IDs
roles_domain <- 
  full_join(roles_df, domain_new_table, by = "Internal.ID")

roles_domain_summary <- 
  roles_domain %>% 
  group_by(Role_n, Domain) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()
 
#This piechart is a mess
# PieDonut(roles_domain, aes(Role_n, Domain_n), color= "white",addPieLabel = TRUE, showPieName=F, r0=0.0,r1=0.8,r2=1.4,start=pi/2, showRatioThreshold = F, title= "Breakdown of Respondents by academic position", donutLabelSize = 4, titlesize =6) 

ggplot(roles_domain_summary, aes(fill=Role_n, y=n, x=Domain)) + 
  geom_bar(position="stack", stat="identity")

### Q4 - Do you currently, or have you in the past, use(d) cloud resources ######
q4 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X4) %>% 
  unnest(X4)

q4_summay <- 
  q4 %>% 
  group_by(X4) %>% 
  count()

#### Pie chart ####
PieDonut(q4_summay, 
         aes(X4, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5)


### Q5 - Please indicate how important these cloud services are to support your research ######
glimpse(survey_organized_clean)

#First, extract the possible answers in a new column
q5 <- 
  survey_organized_clean %>% 
  filter(Ques_num == "X5")

#Second, replace "." by "//"
q5 <- 
  q5 %>% 
  mutate(Ques_num1 = gsub('[.]', '/', q5$Ques))

#Then separate the answer from the question
q5_ord <- 
  q5 %>% 
  separate(Ques_num1, 
           into = c("a", "b"),
           sep = "//") 

#Finaly replace "/" by " " 
q5_ord <-
  q5_ord %>% 
  mutate(b = gsub('[/]', ' ', q5_ord$b)) %>% #replace "/" by a space
  select(-a)

#Now it data cleaning time!
q5_ord_cs <- 
  q5_ord %>% 
  mutate(cloud_service = ifelse(b =="Alliance Cloud", "Alliance Cloud (formerly Compute Canada Cloud)",
                                ifelse(b =="International Community Cloud", "International Community Cloud (e.g., EU-based cloud services)",
                                       ifelse(b =="Amazon Web Services", "Amazon Web Services (AWS)",
                                              ifelse(b =="Google Cloud Platform", "Google Cloud Platform (GCP)",
                                                     ifelse(b =="Microsoft Azure", "Microsoft Azure",
                                                            ifelse(b=="Oracle Cloud","Oracle Cloud",
                                                            ifelse(b=="Regional cloud offering", "Regional cloud offering (e.g., Calcul Québec Juno Cloud)",
                                                                   ifelse(b=="Provincial cloud offering", "Provincial cloud offering (e.g.., BCNET EduCloud)",
                                                                          ifelse(b=="Institutional cloud offering", "Institutional cloud offering", NA))))))))))
#Select most important variables
q5_ord_cs <- 
  q5_ord_cs %>% select(X.U.FEFF.Internal.ID, Question, cloud_service, Answer) %>% 
  rename(Internal.ID = X.U.FEFF.Internal.ID)

#Add scaling 2 1 0 -1 -2
q5_ord_cs_clean <- 
  q5_ord_cs %>% 
  filter(!Answer == 0) %>% # 0 is not a valid answer
  mutate(Answer_clean = ifelse(
    Answer == 5, 2,ifelse(
      Answer == 1, -2, ifelse(
        Answer == 4, 1, ifelse(
          Answer == 3, 0, ifelse(
            Answer == 2, -1, "Verify")))))) #Added verify to see if I missed info, but no missed info

q5_ord_cs_clean <- 
  q5_ord_cs_clean %>% 
  select(Internal.ID, cloud_service, Answer_clean)


#### Add TC3 ####
# 
# 
# #delete "\n" that was used to one can read the TC3 on the piechart on Q3
# domain_summary$Domain[domain_summary$Domain == "Agricultural and\n Veterinary Sciences"] <- "Agricultural and Veterinary Sciences"
# domain_summary$Domain[domain_summary$Domain == "Medical, Health and\nLife Sciences"] <- "Medical, Health and Life Sciences"

#add the TC3

domain_summary1 <- 
  domain_summary %>% 
  mutate(TC3 = ifelse( Domain == "Natural Sciences ", "CIHR", ifelse(
    Domain == "Humanities and the Arts", "SSHRC", ifelse(
      Domain == "Social Sciences ", "SSHRC", ifelse(
        Domain == "Engineering and Technology ", "SSHRC", ifelse(
          Domain == "Medical, Health and Life Sciences ", "CIHR", "NSERC"
      ))))))

#join the domain summary table to the domain table = domain_summary1 contains the total n per domain
domain_new_table1 <- 
  domain_new_table %>% 
  left_join(domain_summary1, by = "Domain")


domain.cloud.s <- 
  q5_ord_cs_clean %>% 
  left_join(domain_new_table1, by = "Internal.ID") %>% 
  select(-n)

#Not sure if this need to be reused as Domain column has been cleaned. try: "unique(domain.cloud.s$Domain)" = 7 unique domains vs three in the next function
# domain.cloud.s$TC3 <- recode_factor(domain.cloud.s$TC3, CIHR = "Health Research",
#                             NSERC = "Sciences and Engineering", SSHRC = "Social Sciences and Humanities")

domain.cloud.s <- as_tibble(domain.cloud.s)

Workfl <- 
  domain.cloud.s %>% 
  select(Internal.ID, cloud_service, Answer_clean, TC3) %>% 
  rename(cloud = cloud_service, answer = Answer_clean) %>% 
  group_by(TC3, cloud, answer)%>%
  summarize(n= n()) %>% 
  drop_na() %>% 
  as.tibble() %>% 
  print()

# Add number of people that answered to each question
group_n <-
  Workfl %>% 
  group_by(TC3, cloud) %>% 
  summarise(group_n = sum(n)) %>% 
  as.tibble() %>% 
  unique() %>% 
  print()

Workfl1 <- 
  Workfl %>% 
  left_join(group_n, by = c("cloud", "TC3"))


#add %
TC3_Needs_sub <- mutate(Workfl1, "%" = (n/group_n)*100)

#Need to replace A, B, D, D and E
TC3_Needs_sub1 <- arrange(TC3_Needs_sub, TC3, cloud, answer)
TC3_Needs_sub1 <- TC3_Needs_sub1 %>% 
  mutate(answer2 = ifelse(answer == 2, "A", ifelse(
  answer == 1, "B", ifelse(
    answer == 0, "C", ifelse(
      answer == -1, "D", "E")))))

TC3_Needs_sub1 <- 
  TC3_Needs_sub1 %>% 
  mutate(`%` = round(`%`))

#Note: add as factor = answer


### Likert Graph on Cloud importance by TRC ####
likert_color <- c("#2166AC", "#92C5DE", "#d3d3d3","#F4A582", "#B2182B")

ggplot(TC3_Needs_sub1, aes(x=cloud, y=`%`, fill=answer2))+geom_col()+facet_grid(rows=vars(TC3)) + 
  scale_fill_manual(values =  likert_color) + 
  geom_hline(yintercept = 50, linetype="dotted", color = "black", size=.75) +
  coord_flip() +
  geom_text(aes(label = round(`%`, digits = 2)), position = position_stack(vjust = .5)) +
  theme_linedraw(base_size = 18) +
  ggtitle("Importance of cloud service to support research") +
  xlab("Services") + 
  ylab("") 

### Q7 - For what purpose(s) do you use cloud services to support your research? ######
#### data cleaning & preparation ####

glimpse(survey_organized_clean)

#extract question 7
q7 <- survey_organized_spread %>% 
  select(Internal.ID, X7) %>% 
  unnest(X7) %>% 
  rename(purpose = X7)

#Create a table with domain for TC3 by ID
domain <- 
  domain_new_table1 %>% 
  select(-Domain, -n)

domain1 <- domain

domain1$TC3 <-
  recode_factor(domain1$TC3, CIHR = "Health Research",
                NSERC = "Sciences and Engineering", SSHRC = "Social Sciences and Humanities")


#Organize q7 by adding all "other" answers together
q7_orga <- 
  q7 %>% 
  filter(!purpose == "Other") %>% #Other is an invalid answer, keeping it will add extra false answers as they are previsouly "Other (Please specify)"
  mutate(answer =
           ifelse(purpose == "Cloud storage (i.e., OneDrive, Dropbox, Google Drive, Sync, etc.)", "Cloud storage", ifelse(
             purpose == "High-performance computing capabilities", purpose, ifelse(
               purpose == "Analysis", purpose, ifelse(
                 purpose == "Accessing cloud-based software", purpose, ifelse(
                   purpose == "Interactive computing", purpose, ifelse(
                     purpose == "Object storage ", "Object storage", ifelse(
                       purpose == "Accessing specialized hardware ", "Accessing specialized hardware", ifelse(
                         purpose == "SaaS (Software as a Service)", purpose, ifelse(
                           purpose == "PaaS (Platform as a Service)", purpose, ifelse(
                             purpose == "IaaS (Infrastructure as a Service)", purpose, "Other" #Here I'm not sure if there is a cleaned column for "other (please specify)"
                               )))))))))))

#summarize the data
q7_summary <- 
  q7_orga %>% 
  group_by(answer) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#link TC3 to q7 IDs
q7.domain <- 
  q7_orga %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  select(-purpose) %>% 
  print() ## n = 362 = unique(Internal.ID)


Workflow.q7 <- q7.domain

Workflow.q7 <- 
  q7.domain %>% 
  mutate(TC3 = replace_na(Workflow.q7$TC3, "Other")) %>% 
  unique()

nHR <- filter(Workflow.q7, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #202
nSE <- filter(Workflow.q7, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#66
nSSH <- filter(Workflow.q7, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #165



Workflow_Health <- filter(Workflow.q7, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q7, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q7, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_Other, Workflow_SSH, Workflow_SciEng, Workflow_Health)  

### Stacked Bar Graph on Cloud uses by TRC ####
cbp1 <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 100)
                       
ggplot(Workflow_Tri1, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "none")+
ggtitle("Use of cloud services") +
  xlab("") + 
  ylab("Use of cloud services")
  # fixed_plot_aspect()

### Q9 - Which method(s) of interacting with cloud resources are you familiar with? ######
#### data cleaning & preparation ####

glimpse(survey_organized_clean)

#extract question 7
q9 <- survey_organized_spread %>% 
  select(Internal.ID, X9) %>% 
  unnest(X9) %>% 
  rename(method = X9) # n = 278

#Create a table with domain for TC3 by ID
domain # n = 481 = unique(Internal.ID)

#Organize q7 by adding all "other" answers together
q9_orga <- 
  q9 %>% 
  filter(!method == "Other") %>% #Delete other
  mutate(answer =
           ifelse(method == "A remote connection to a Graphical desktop (e.g., virtual desktop, remote desktop, etc)", "A remote connection to a Graphical desktop", ifelse(
             method == "Software (e.g., OneDrive, Dropbox, Google Drive, Sync, Jupyter, etc.,)", "Software", ifelse(
               method == "Web browser", method, ifelse(
                 method == "SSH connection and/or terminal", method, ifelse(
                   method == "API (code-level access for applications)", "API" , ifelse(
                     method == "Not applicable",method , ifelse(
                       method == "Not sure", method, "Other")))))))) # the last "delete" is to delete "not applicable"

#delete "Other (please specify)" = changed into delete in previous function
q9_orga <- 
  q9_orga %>% 
  filter(!answer == "Not applicable")

#summarize the data
q9_summary <- 
  q9_orga %>% 
  group_by(answer) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#link TC3 to q7 IDs
q9.domain <- 
  q9_orga %>% 
  left_join(domain, by = "Internal.ID") %>% 
  select(-method) %>% 
  print() ## n = 358 = unique(Internal.ID)


Workflow.q9 <- 
  q9.domain %>% 
  unique()

nHR <- filter(Workflow.q9, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #113
nSE <- filter(Workflow.q9, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#192
nSSH <- filter(Workflow.q9, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #103
nOther <- filter(Workflow.q9, TC3 == "Other") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #20


Workflow_Health <- filter(Workflow.q9, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q9, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q9, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Other <- filter(Workflow.q9, TC3=="Other") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 


Workflow_Tri2 <- rbind(Workflow_Other, Workflow_SSH, Workflow_SciEng, Workflow_Health)  

### Stacked Bar Graph on Cloud uses by TRC ####

ggplot(Workflow_Tri2, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "none")+
ggtitle("Use of cloud services") +
  xlab("Usage") + 
  ylab("Percentage")
  # fixed_plot_aspect()

### Q10 - What factors do you consider in choosing which cloud platform to use (Alliance or commercial)? ######
#### data cleaning & preparation ####
  
  glimpse(survey_organized_clean)
  
  #extract question 7
  q10 <- survey_organized_spread %>% 
    select(Internal.ID, X10) %>% 
    unnest(X10) %>% 
    rename(factor = X10) # n = 278
  
  #Create a table with domain for TC3 by ID
  domain # n = 366
  
  #Organize q7 by adding all "other" answers together
  q10_orga <- 
    q10 %>%
    filter(!factor == "Other") %>% 
    mutate(answer =
             ifelse(factor == "Ease of use ", "Ease of use", ifelse(
               factor == "Cost ", "Cost", ifelse(
                 factor == "Scalability ", "Scalability", ifelse(
                     factor == "Vendor-agnostic features ", "Vendor-agnostic features" , ifelse(
                       factor == "security","Privacy and security" , ifelse(
                         factor == "privacy and security", "Privacy and security", ifelse(
                           factor == "Security", "Privacy and security", ifelse(
                             factor == "Privacy", "Privacy and security", ifelse(
                               factor == "safety-confidentiality", "Privacy and security", ifelse(
                                 factor == "privacy", "Privacy and security", ifelse(
                                   factor == "digital safety, privacy", "Privacy and security", ifelse(
                                     factor == "Security; Popularity (Which platform others in my discipline are using for ease of collaboration)", "Privacy and security", ifelse(
                                       factor == "Security, privacy, laws", "Privacy and security", ifelse(
                                         factor == "Security, accessibility in other countries (e.g., China)", "Privacy and security", ifelse(
                                           factor == "Security standards", "Privacy and security", ifelse(
                                             factor == "Security for sensitive health data", "Privacy and security", ifelse(
                                               factor == "Security and the ethics of the corporation producing it. If you're pushing the WEF agenda, take a hike.", "Privacy and security", "Other"
                                             )))))))))))))))))) # the last "delete" is to delete "not applicable"
  
  
  #summarize the data
  q10_summary <- 
    q10_orga %>% 
    group_by(answer) %>% 
    count() %>% 
    arrange(-n) %>% 
    print()
  
  
  #link TC3 to q7 IDs
  q10.domain <- 
    q10_orga %>% 
    left_join(domain, by = "Internal.ID") %>% 
    select(-factor) %>% 
    print() ## n = 356
  
  
  Workflow.q10 <- 
    q10.domain %>% 
    unique()
  
  
  nHR <- filter(Workflow.q10, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #111
  nSE <- filter(Workflow.q10, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#191
  nSSH <- filter(Workflow.q10, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #105
  nOther <- filter(Workflow.q10, TC3 == "Other") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #19
  
  
  Workflow_Health <- filter(Workflow.q10, TC3=="Health Research") %>%
    group_by(TC3, answer) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR)*100)
  
  Workflow_SciEng <- filter(Workflow.q10, TC3=="Sciences and Engineering") %>%
    group_by(TC3, answer) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE)*100)
  
  Workflow_SSH <- filter(Workflow.q10, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, answer) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH)*100) 
  
  Workflow_Other <- filter(Workflow.q10, TC3=="Other") %>%
    group_by(TC3, answer) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH)*100) 
  
  
  Workflow_Tri2 <- rbind(Workflow_Other, Workflow_SSH, Workflow_SciEng, Workflow_Health)  
  
### Stacked Bar Graph on Cloud uses by TRC ####
  
  ggplot(Workflow_Tri2, aes(x=reorder(answer,`%`))) + 
    geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
    scale_fill_manual(values =  cbp1) + 
    coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
    theme_linedraw(base_size = 18) +
    theme(legend.position = "none")+
    ggtitle("Use of cloud services") +
    xlab("Usage") + 
    ylab("Percentage")
  fixed_plot_aspect()

### Q11 - Do your cloud needs include storing or processing controlled or sensitive data (e.g., data owned by First Nations, personal data, data subject to a data sharing agreement or specific security requirements)?? ######
q11 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X11) %>% 
  unnest(X11)
  
q11_summay <- 
  q11 %>% 
  group_by(X11) %>% 
  count()
  
#### Pie chart ####
PieDonut(q11_summay, 
           aes(X11, count= n), 
           ratioByGroup = FALSE, 
           showPieName=FALSE, 
           r0=0.0,r1=1,r2=1.4,start=pi/2,
           labelpositionThreshold=1, 
           showRatioThreshold = F, 
           title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
           titlesize = 5)
  
  
  
### Q12 - Have you used a commercial cloud provider?? ######
q12 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X12) %>% 
  unnest(X12)

q12_summay <- 
  q12 %>% 
  group_by(X12) %>% 
  count()

#### Pie chart ####
PieDonut(q12_summay, 
         aes(X12, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5)



### Q13 - HApproximately how many dollars (CDN) in cloud credits or vendor in-kind funds did your research group consume over the last calendar year on commercial cloud resources? ######
q13 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X13) %>% 
  unnest(X13) # n = 218

q13_org <- 
  q13 %>% 
  mutate(credits = ifelse(
    X13 == "$0 ","$0", X13))

q13_summay <- 
  q13_org %>% 
  group_by(credits) %>% 
  count() %>% 
  arrange(-n) %>% 
  print() # n = 218

#Link it to the TC3 = data is "domain_new_table"

q13_org_domain <- 
  q13_org %>% 
  left_join(domain_new_table, by = "Internal.ID") %>% 
  select(-X13, -Domain)


q13_org_domain_summary <- 
  q13_org_domain %>% 
  group_by(credits, Domain_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

#### plot ####
ggplot(q13_org_domain_summary, aes(fill=Domain_n, y=n, x=credits)) + 
  geom_bar(position="stack", stat="identity") #A comment, we should change "NA" in "Domain" into "Not specified", something like that


### Q14 - HApproximately how many dollars (CDN) in cloud credits or vendor in-kind funds did your research group consume over the last calendar year on commercial cloud resources? ######
q14 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X14) %>% 
  unnest(X14) # n = 218

q14_org <- 
  q14 %>% 
  mutate(credits = ifelse(
    X14 == "$0 ","$0", X14))

q14_summay <- 
  q14_org %>% 
  group_by(credits) %>% 
  count() %>% 
  arrange(-n) %>% 
  print() # n = 218

#Link it to the TC3 = data is "domain_new_table"

q14_org_domain <- 
  q14_org %>% 
  left_join(domain_new_table, by = "Internal.ID") %>% 
  select(-X14, -Domain)


q14_org_domain_summary <- 
  q14_org_domain %>% 
  group_by(credits, Domain_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

#### plot ####
ggplot(q14_org_domain_summary, aes(fill=Domain_n, y=n, x=credits)) + 
  geom_bar(position="stack", stat="identity") #A comment, we should change "NA" in "Domain" into "Not specified", something like that


### Q15 - How is your commercial cloud budget funded? ######
q15 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X15) %>% 
  unnest(X15) # n = 218

