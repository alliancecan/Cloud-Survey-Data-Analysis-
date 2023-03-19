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
                   na.strings=c("","NA")) %>% 
  rename(Internal.ID = X.U.FEFF.Internal.ID)

survey <- survey[c(-10,-12, -20, -21)] #delete columns "Other" and old "old please specify" have been reclassified into the "NEW_Other" column <<<<<<<< we need to name the columns, otherwise it is a bit confusing as to why or which

#Questions <<<<< I don't think this is relevant.
questions_n <- read.csv("Questions_n_EN_20230223.csv")#this table has the questions in rows (n=47)
#To add the french version

# Organizing data ----------------------------------------------------------------

###Re-organize the data
survey_organized <- 
  survey %>%
  gather("Question",
         "Answer", 
         -Internal.ID) %>%
  extract(Question, 
          c("Ques_num", "Ques"), 
          "([[:alnum:]]+)..([[:graph:]]+)") %>% #This function separates question number and question (e.g., X1..Question => col1 = X1 | col2 = Question)
  drop_na(Answer)

#Clean the questions: join the questions from table "questions_n" using the question's number "X.."
survey_organized_clean <- 
  survey_organized %>% 
  left_join(questions_n, by = "Ques_num")

#Delete extra columns by selecting important ones: <<<<<<<<< not needed
survey_organized_clean1 <- 
  survey_organized_clean %>% 
  select(Internal.ID, Ques_num, Answer) #@%>% 
#rename (Internal.ID = X.U.FEFF.Internal.ID)

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


#Colour Scheme
cbp1 <- rep(c("#B7B6B3", "#D6AB00","#00DBA7", "#56B4E9",
                       "#32322F", "#FBFAFA", "#D55E00", "#CC79A7"), 100)
                       
## <<<<< NEed to create a new pie colour scheme for when we have many elements. This works well for yes, no, don't know                 
cb_pie <- rep(c("#32322F","#FBFAFA", "#D6AB00","#00DBA7", "#B7B6B3",
                         "#0072B2", "#D55E00", "#CC79A7"), 100)
                         

#### summary table - Roles ####
roles_summary <- 
  survey_x1.x2_v2 %>% 
  select(-Geography,) %>% 
  group_by(Role_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  drop_na() %>%
  print()

#### Pie chart - Roles #### <<< need to play with the colours

PieDonut(roles_summary, 
         aes(Role_n, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.25,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Respondents' roles", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie) #+ 

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
  arrange(props) %>%
  drop_na()

Canada$Geography <- factor(Canada$Geography, levels = unique(Canada$Geography))


#### Pie chart - All Canada ####
#Comment: group ester provinces
PieDonut(Canada, aes(Geography, count= props), ratioByGroup = FALSE, showPieName=FALSE, r0=0.25,r1=1,r2=1.4,start=pi/2,labelpositionThreshold=1, showRatioThreshold = F, title= "Respondents by Region", titlesize = 5) #+ 

#### Descending Bargraph
ggplot(Canada, aes(x = reorder(Geography, -props), y= props)) +
  geom_bar(stat="identity", aes(fill=Geography))
# scale_fill_manual(values =  cbp1)


### Q3 - Please choose your research domain ######

Domain_Breakdown <- survey_organized_spread %>% 
  select(Internal.ID, X3) %>% 
  unnest(X3) %>% 
  rename(Domain = X3)


domain_new_table <- Domain_Breakdown



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

#### Bar Graph ####

ggplot(domain_summary, aes(x = reorder(Domain, -n), y= n)) +
  geom_bar(stat="identity", aes(fill=Domain))


# Nested Domain-Role
roles_df <- 
  survey_x1.x2_v2 %>% 
  select(-Geography, -Role,) %>% #select ony roles and ID
  unique()

#join roles and domains based on the IDs
roles_domain <- 
  full_join(roles_df, domain_new_table, by = "Internal.ID") ### <<<<<<<<<<<<< No new table?

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
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie)


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
  q5_ord_cs %>% select(Internal.ID, Question, cloud_service, Answer) #%>% 
# rename(Internal.ID = Internal.ID)

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

#add the TC3 <<<<<<<<<<<<<<<<<<<<<< Engineering is NSERC // Natural Sciences is CIHR //  added a . in humanities. Also NSERC at the end for veterinary sciences.
#
domain_summary1 <- 
  domain_summary %>% 
  mutate(TC3 = ifelse(Domain == "Natural Sciences ", "Sciences and Engineering", ifelse(
    Domain == "Humanities and the Arts ", "Social Sciences and Humanities", ifelse(
      Domain == "Social Sciences ", "Social Sciences and Humanities", ifelse(
        Domain == "Engineering and Technology ", "Sciences and Engineering", ifelse(
          Domain == "Medical, Health and Life Sciences ", "Health Research", "Sciences and Engineering"
        ))))))

#join the domain summary table to the domain table = domain_summary1 contains the total n per domain. <<<<<<<<<<<< no domain_new_table
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


### Q6 - Feel free to specify other services not listed in Question 5: ######

#First, extract the possible answers in a new column
q6 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X6) %>% 
  unnest(X6)

q6.specify <- read.csv("q6_clean.csv") %>% 
  drop_na()

#summarise
q6.specify.summary <- 
  q6.specify %>% group_by(answer_clean) %>% count() %>% 
  drop_na() %>% arrange(-n)

#to be used to reorder the plot values
order <- as.data.frame(c(46:1))

#add total n (sum) = to be used to calculate proportions
sum <- sum(q6.specify.summary$n)

#add proportions
q6.specify.summary <- 
  cbind(q6.specify.summary, order) %>% 
  rename(order = 3) %>% 
  mutate(sum = sum, Percentage = (n/sum)*100)

#### Plot - yes - specify ####
ggplot(q6.specify.summary, aes(x= reorder(answer_clean, order))) + 
  geom_bar(aes(y=Percentage), stat= "identity") +
  scale_fill_manual(values =  "#D6AB00") + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=Percentage, label= round(Percentage, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Support source for using Commercial Cloud") +
  xlab("Source") + 
  ylab("Proportion (%)")


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

#### domain1$TC3 <-
####  recode_factor(domain1$TC3, CIHR = "Health Research",
###            NSERC = "Sciences and Engineering", SSHRC = "Social Sciences and Humanities")


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

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Use of cloud services") +
  xlab("") + 
  ylab("")
# fixed_plot_aspect()

### Q9 - Which method(s) of interacting with cloud resources are you familiar with? ######
#### data cleaning & preparation ####

glimpse(survey_organized_clean)

#extract question 7
q9 <- survey_organized_spread %>% 
  select(Internal.ID, X9) %>% 
  unnest(X9) %>% 
  rename(method = X9) # n = 363

#Create a table with domain for TC3 by ID
domain1 # n = 474 = unique(Internal.ID)

#Organize q7 by adding all "other" answers together
q9_orga <- 
  q9 %>% 
  filter(!method == "Other") %>% #Delete other - but if "other" is a new added answer by F, then delete this line
  mutate(answer =
           ifelse(method == "A remote connection to a Graphical desktop (e.g., virtual desktop, remote desktop, etc)", "A remote connection to a Graphical desktop", ifelse(
             method == "Software (e.g., OneDrive, Dropbox, Google Drive, Sync, Jupyter, etc.,)", "Software", ifelse(
               method == "Web browser", method, ifelse(
                 method == "SSH connection and/or terminal", method, ifelse(
                   method == "API (code-level access for applications)", "API" , ifelse(
                     method == "Not applicable",method , ifelse(
                       method == "Not sure", method, "Other")))))))) # the last "delete" is to delete "not applicable"

#delete "Not applicable"
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
  left_join(domain1, by = "Internal.ID") %>% 
  select(-method) %>% 
  print() ## n = 358 = unique(Internal.ID)


Workflow.q9 <- 
  q9.domain %>% 
  unique()

nHR <- filter(Workflow.q9, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #200
nSE <- filter(Workflow.q9, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#66
nSSH <- filter(Workflow.q9, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #164

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

Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Stacked Bar Graph on Cloud uses by TRC #### 

ggplot(Workflow_Tri2, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "right", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Methods to interact with Cloud Services") +
  xlab("") +
  ylab("")
# fixed_plot_aspect()

### Q10 - What factors do you consider in choosing which cloud platform to use (Alliance or commercial)? ######
#### data cleaning & preparation ####

glimpse(survey_organized_clean)

#extract question 7
q10 <- survey_organized_spread %>% 
  select(Internal.ID, X10) %>% 
  unnest(X10) %>% 
  rename(factor = X10) # n = 357

#Create a table with domain for TC3 by ID
domain1 # n = 474

#Organize q10 by adding all "other" answers together ##### 
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
  left_join(domain1, by = "Internal.ID") %>% 
  select(-factor) %>% 
  print() ## n = 356


Workflow.q10 <- 
  q10.domain %>% 
  unique()


nHR <- filter(Workflow.q10, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #111
nSE <- filter(Workflow.q10, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#191
nSSH <- filter(Workflow.q10, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #105


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


Workflow_Tri2 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

### Stacked Bar Graph on Cloud uses by TRC #### 

ggplot(Workflow_Tri2, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Factors influencing choice of Cloud Platform") +
  xlab("") + 
  ylab("")
# fixed_plot_aspect()

### Q11 - Do your cloud needs include storing or processing controlled or sensitive data (e.g., data owned by First Nations, personal data, data subject to a data sharing agreement or specific security requirements)?? ######
q11 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X11) %>% 
  unnest(X11)

q11_summay <- 
  q11 %>% 
  group_by(X11) %>% 
  count()

#add domain
q11.domain <- 
  q11 %>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q11.domain.summary <- 
  q11.domain %>% 
  group_by(TC3, X11) %>% count() %>% drop_na()

#calculate sum and add it
q11.sum <- 
  q11.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q11.domain.summary <- 
  q11.domain.summary %>% 
  left_join(q11.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X11)

#### Pie chart #### 
PieDonut(q11_summay, 
         aes(X11, count= n), 
         ratioByGroup = FALSE, 
         showPieName=F, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie)

#### plot - domain #### 
cb_pie_3 <- rep(c("#32322F","#B7B6B3", "#D6AB00"), 100)

ggplot(q11.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie_3)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Do your cloud needs include storing or processing controlled or sensitive data?")


### Q12 - Have you used a commercial cloud provider?? ######
q12 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X12) %>% 
  unnest(X12)

q12_summay <- 
  q12 %>% 
  group_by(X12) %>% 
  count()

#add domain
q12.domain <- 
  q12 %>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q12.domain.summary <- 
  q12.domain %>% 
  group_by(TC3, X12) %>% count() %>% drop_na()

#calculate sum and add it
q12.sum <- 
  q12.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q12.domain.summary <- 
  q12.domain.summary %>% 
  left_join(q12.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X12)

#### Pie chart ####
PieDonut(q12_summay, 
         aes(X12, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie)

#### plot - domain #### 

ggplot(q12.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie_3)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Have you used a commercial cloud provider?")




#### combine Q12 & Q28 ####
q12.28 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X12, X28) %>% 
  unnest(c(X12, X28)) %>% 
  gather("Question", "answer", 2:3)

q12.28.summary <- 
  q12.28 %>% group_by(Question, answer) %>% count() %>% 
  drop_na()

q12.28.summary$Question[q12.28.summary$Question == "X12"] <- "Have you used a commercial cloud provider?"
q12.28.summary$Question[q12.28.summary$Question == "X28"] <- "Have you used the Alliance Community Cloud?"
q12.28.summary$answer[q12.28.summary$answer == "No "] <- "No"
q12.28.summary$answer[q12.28.summary$answer == "Yes "] <- "Yes"
q12.28.summary$answer[q12.28.summary$answer == "Not sure "] <- "Not sure"

#sum
q12.28.sum <- 
  q12.28.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q12.28.sum.merged <- 
  q12.28.summary %>% 
  left_join(q12.28.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q12.28.summary.flip <- q12.28.sum.merged %>% 
  mutate(new_n = ifelse(Question == "Have you used a commercial cloud provider?",
                        -1*proportion, proportion))
  
#ggplot comparing answers
cb_pie1 <- rep(c("#32322F", "#D6AB00"), 100)
cb_pie2 <- rep(c("#D6AB00","#32322F"), 100)

ggplot(q12.28.summary.flip, aes(fill=Question, y=new_n, x=answer)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("Commercial cloud vs Alliance Community cloud")+
  scale_fill_manual(values =  cb_pie1)
  


# #ggplot comparing questions
# ggplot(q12.28.summary, aes(fill=answer, y=n, x=Question)) + 
#   geom_bar(position="fill", stat="identity")+ scale_fill_manual(values =  cb_pie)
# 
# 
# ggplot(q12.28.summary, aes(x=answer, y=n, fill=Question)) + 
#   geom_bar(stat="identity", position="identity")+
#   coord_flip()


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
  left_join(domain_new_table1, by = "Internal.ID") %>% 
  select(-X13)


q13_org_domain_summary <- 
  q13_org_domain %>% 
  group_by(credits, TC3) %>% 
  count() %>% 
  arrange(-n) %>% 
  drop_na() %>% 
  mutate(sort = ifelse(
    credits == "$0", 1, ifelse(
      credits == "< $1000", 2, ifelse(
        credits == "$1000 - $5000", 3, ifelse(
          credits == "$5000 - $50000", 4, ifelse(
            credits == "$50000 - $100000", 5, ifelse(
              credits == "> $100000", 6, 7))))))) %>%  #arrange the data
  
  print()

#### plot ####

ggplot(q13_org_domain_summary, aes(fill=TC3, y= n, x=reorder(credits, sort))) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cbp1)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Approximately how many dollars (CDN) in cloud credits or vendor in-kind funds\ndid your research group consume over the last calendar year on commercial\ncloud resources?")+
  xlab("Funds ($)") + ylab("Number of responses")


### Q14 - Approximately how many dollars (CDN) in cloud credits or vendor in-kind funds did your research group consume over the last calendar year on commercial cloud resources? ######
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
  left_join(domain_new_table1, by = "Internal.ID") %>% 
  select(-X14)


q14_org_domain_summary <- 
  q14_org_domain %>% 
  group_by(credits, TC3) %>% 
  count() %>% 
  arrange(-n) %>% 
  drop_na() %>% 
  mutate(sort = ifelse(
    credits == "$0", 1, ifelse(
      credits == "< $1000", 2, ifelse(
        credits == "$1000 - $5000", 3, ifelse(
          credits == "$5000 - $50000", 4, ifelse(
            credits == "$50000 - $100000", 5, ifelse(
              credits == "> $100000", 6, 7))))))) %>%  #arrange the data
  print()

#### plot ####
ggplot(q14_org_domain_summary, aes(fill=TC3, y=n, x=reorder(credits, sort))) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cbp1)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Approximately how many dollars (CDN) of research funds did your research group\nspend over the last calendar year on commercial cloud resources?")+
  xlab("Funds ($)") + ylab("Number of responses")

#### plot Q13 & Q14####
# q13.14 <- 
#   survey_organized_spread %>% # n = 507
#   select(Internal.ID, X13, X14) %>% 
#   unnest(c(X13, X14))
# 
# q13.14_org <- 
#   q13.14 %>% 
#   mutate(credits.x13 = ifelse(
#     X14 == "$0 ","$0", X13), 
#     credits.x14 = ifelse(
#       X14 == "$0 ","$0", X14)) %>% 
#   select(-X13, -X14) # n = 218
# 
# q13.14_summay <- 
#   q13.14_org %>% 
#   group_by(credits.x13, credits.x14) %>% 
#   count() %>% 
#   arrange(-n) %>% 
#   print() # n = 218
# 
# #bar plot
# ggplot(q13.14_summay, aes(fill=answer, y=n, x=Question)) + 
#   geom_bar(position="fill", stat="identity")+
#   ggtitle("")

### Q15 - How is your commercial cloud budget funded? ######
q15 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X15) %>% 
  unnest(X15) %>% 
  filter(!X15 == "Other") # n = 197

#Clean "Other" answers:
q15$X15[q15$X15 == "Bourse / stage universitaire"] <- "Research grants "
q15$X15[q15$X15 == "out of my own pocket" ] <- "Personal funding "
q15$X15[q15$X15 == "I'm not sure."] <- NA
q15$X15[q15$X15 == "Not funded"] <- NA
q15$X15[q15$X15 == "Startup grant"] <- "Industry grants or funding "
q15$X15[q15$X15 == "aucun"] <- NA
q15$X15[q15$X15 == "ne s'applique pas"] <- NA
q15$X15[q15$X15 == "N/A"] <- NA
q15$X15[q15$X15 == "vendor inkinds, philanthropy"] <- "Industry grants or funding "
q15$X15[q15$X15 == "vendor inkinds, philanthropy"] <- "Industry grants or funding "
q15$X15[q15$X15 == "Its bundled in with our overall storage for the University"] <- "Institutionally "

q15 <- q15 %>% drop_na()
  
#link TC3 to q15 IDs
q15.domain <- 
  q15 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  print() ## n = 192


Workflow.q15 <- q15.domain

Workflow.q15 <- 
  q15.domain %>% 
  mutate(TC3 = replace_na(Workflow.q15$TC3, "Other")) %>% 
  rename(answer = X15) %>% 
  unique()

nHR <- filter(Workflow.q15, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #72
nSE <- filter(Workflow.q15, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#106
nSSH <- filter(Workflow.q15, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #59



Workflow_Health <- filter(Workflow.q15, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q15, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q15, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Commercial Cloud Budget") +
  xlab("") + 
  ylab("")


### Q16 - How is your commercial cloud budget funded? ######
q16 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X16) %>% 
  unnest(X16) %>% 
  filter(!X16 == "Other") # n = 210

#add other unique answer (other) together
to_filter <- q16 %>% group_by(X16) %>% count()

q16 <- 
  q16 %>% 
  left_join(to_filter, by = "X16")
  
q16 <- 
  q16 %>% 
  mutate(answer = ifelse(
    n == 1, "Other", X16)) %>% 
  select(-n, -X16)

#link TC3 to q16 IDs
q16.domain <- 
  q16 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() %>% 
  print()


Workflow.q16 <- q16.domain

nHR <- filter(Workflow.q16, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #75
nSE <- filter(Workflow.q16, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#116
nSSH <- filter(Workflow.q16, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #257



Workflow_Health <- filter(Workflow.q16, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q16, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q16, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "bottom", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Primary reason for using a commercial cloud") +
  xlab("") + 
  ylab("")

### Q17 - What components of the commercial cloud do you use?? ######
q17 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X17) %>% 
  unnest(X17) %>% 
  filter(!X17 == "Other") # n = 210

#add other unique answer (other) together
to_filter <- q17 %>% group_by(X17) %>% count()

q17.org <- 
  q17 %>% 
  mutate(answer = ifelse(
    X17 == "2i2c", "Other", ifelse(
      X17 == "Anonymous Functions, glue components, Serverless Databases", "Other", ifelse(
        X17 == "Databases (BigQuery)", "Object storage ", ifelse(
          X17 == "Docker", "Containers and container orchestration (Kubernetes) ", ifelse(
            X17 == "Dropbox", "Object storage ", ifelse(
              X17 == "FPGAs", "Other", ifelse(
                X17 == "Google Drive, one drive", "Object storage ", ifelse(
                  X17 == "I'm not sure what most of these terms refer to: I use Dropbox, Zotero and Google Drive", "Object storage ", ifelse(
                    X17 == "Je ne sais pas", NA, ifelse(
                      X17 == "Pour la recherche, aucun service sur nuage, informations trop sensibles", "Other", ifelse(
                        X17 == "These questions are too vague to gather any meaningful data.", NA, ifelse(
                          X17 == "aucune presentement", NA, ifelse(
                            X17 == "file storage and retrieval", "Object storage ", ifelse(
                              X17 == "gestion de versions, codes", "Shared filesystem ", ifelse(
                                X17 == "probably other aspects too, but I don't do this personally", "Other", ifelse(
                                  X17 == "simultaneous wireless syncing from desktop or phone to web", "Other", X17)
                                )))))))))))))))) %>% 
  drop_na() %>% 
  select(-X17)

#link TC3 to q17 IDs
q17.domain <- 
  q17.org %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  unique() %>% 
  drop_na() %>% 
  print() # n = 192


Workflow.q17 <- q17.domain

nHR <- filter(Workflow.q17, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #69
nSE <- filter(Workflow.q17, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#109
nSSH <- filter(Workflow.q17, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #59



Workflow_Health <- filter(Workflow.q17, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q17, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q17, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "bottom", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("What components of the commercial cloud do you use?") +
  xlab("") + 
  ylab("")


### Q18 & Q33 ######
q18.33 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X18, X33) %>% 
  unnest(c(X18, X33)) %>% 
  gather("Question", "answer", 2:3) # n = 239

#### Yes-No ####

#select only Yes and No
q18.33.y.n <- 
  q18.33 %>% 
  filter(answer == "Yes" | answer == "No") # n = 239

#Change replace X18 and X33 by the questions
q18.33.y.n$Question[q18.33.y.n$Question == "X18"] <- "Are there particular platforms or software services you currently deploy\nor wish to deploy in your commercial cloud environments which you feel\nwould be valuable to be offered as a national service for all researchers to access?"
q18.33.y.n$Question[q18.33.y.n$Question == "X33"] <- "Are there particular platforms or software services you currently deploy\nor wish to deploy in your own cloud environments which you feel would\nbe valuable to be offered as a national service for all researchers to access?"

#summarise data
q18.33.sum <- 
  q18.33.y.n %>% group_by(Question, answer) %>% count() %>% 
  drop_na()

#Add negative values to create the mirror barplot graph
q18.33.sum.flip <- q18.33.sum %>% 
  mutate(new_n = ifelse(Question == "Are there particular platforms or software services you currently deploy\nor wish to deploy in your commercial cloud environments which you feel\nwould be valuable to be offered as a national service for all researchers to access?",
                        -1*n, n))

#### Plot- Yes - No####

ggplot(q18.33.sum, aes(fill=Question, y=n, x=answer)) + 
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  xlab("Answer") + ylab("Proportion")+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Commercial cloud vs Alliance Community cloud")+
  scale_fill_manual(values =  cb_pie1)



#### Yes - specify ####
q18.33.specify <- 
  q18.33 %>% 
  filter(!answer == "Yes" & !answer == "No")

q18.33.specify <- read.csv("q18_33_clean.csv")

#list of "if yes please specify" for both questions
list <- q18.33.specify %>% group_by(Question,answer_clean) %>% count() %>% drop_na()


### Q19 & Q34 ######
q19 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X19) %>% 
  unnest(X19)
  
  q34 <- 
    survey_organized_spread %>% 
    select(Internal.ID, X34) %>% 
  unnest(X34)

  
q19.34 <- 
  full_join(q19, q34, by = "Internal.ID") %>% 
  gather("Question", "answer", 2:3) %>% 
  unique() # n = 241

#Change replace X19 and X34 by the questions
q19.34$Question[q19.34$Question == "X19"] <- "Which of the following functions have you used the commercial cloud for?"
q19.34$Question[q19.34$Question == "X34"] <- "How have you used the Alliance Cloud to share data (if at all)?"

#Organize data = gather "other please specify"

q19.34.org <- 
  q19.34 %>% 
  mutate(answer_n = ifelse(
    answer == "Sharing research data publicly", answer, ifelse(
      answer == "Storing research data, no sharing involved", answer, ifelse(
        answer == "Sharing research data with collaborators only", answer, ifelse(
          answer == "Sharing research data publicly", answer, ifelse(
            answer == "Storing research data, no sharing involved", answer, ifelse(
              answer == "Sharing research data with collaborators only", answer, ifelse(
                answer == "Other", "Other sharing activity", "Specified"
                )))))))) %>% 
  drop_na()

q19.34.org <- 
  q19.34.org %>% 
  filter(!answer_n == "Specified") # n = 341


#summarise data
q19.34.summary <- 
  q19.34.org %>% group_by(Question, answer_n) %>% count() %>% 
  drop_na()

#sum
q19.34.sum <- 
  q19.34.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q19.34.sum.merged <- 
  q19.34.summary %>% 
  left_join(q19.34.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q19.34.sum.flip <- q19.34.sum.merged %>% 
  mutate(new_n = ifelse(Question == "Which of the following functions have you used the commercial cloud for?",
                        -1*proportion, proportion))


#### Plot ####
ggplot(q19.34.sum.flip, aes(fill=Question, y=new_n, x=answer_n)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  xlab("Answer") + ylab("Number of responses")+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("Commercial cloud vs Alliance Community cloud")+
  scale_fill_manual(values =  cb_pie2)



### Q20 & Q35 ######
q20.35 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X20, X35) %>% 
  unnest(c(X20, X35)) %>% 
  gather("Question", "answer", 2:3) %>% 
  unique() # n = 241


#Change replace X20 and X35 by the questions
q20.35$Question[q20.35$Question == "X20"] <- "Is any of the content stored or shared on the commercial cloud controlled,\nhigh risk or sensitive data?"
q20.35$Question[q20.35$Question == "X35"] <- "Is any of the content stored or shared on the Alliance Cloud controlled,\nhigh risk or sensitive data?"

#Organize data = gather "other please specify"


#summarise data
q20.35.summary <- 
  q20.35 %>% group_by(Question, answer) %>% count() %>% 
  drop_na()

#sum
q20.35.sum <- 
  q20.35.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q20.35.sum.merged <- 
  q20.35.summary %>% 
  left_join(q20.35.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q20.35.sum.flip <- q20.35.sum.merged %>% 
  mutate(new_n = ifelse(Question == "Is any of the content stored or shared on the commercial cloud controlled,\nhigh risk or sensitive data?",
                        -1*proportion, proportion))


#### Plot ####
ggplot(q20.35.sum.flip, aes(fill=Question, y=new_n, x=answer)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("Commercial cloud vs Alliance Community cloud")+
  scale_fill_manual(values =  cb_pie2)


### Q21 & Q36 ######
q21.36 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X21, X36) %>% 
  unnest(c(X21, X36)) %>% 
  gather("Question", "answer", 2:3) %>% 
  unique() # n = 241


#Change replace X21 and X36 by the questions
q21.36$Question[q21.36$Question == "X21"] <- "Do you need to securely share data stored on the commercial cloud\nwith specific collaborators?"
q21.36$Question[q21.36$Question == "X36"] <- "Do you need to securely share data stored on the Alliance\ncloud-specific collaborators?"
q21.36$answer[q21.36$answer == "No "] <- "No"
q21.36$answer[q21.36$answer == "Yes "] <- "Yes"
q21.36$answer[q21.36$answer == "Not sure "] <- "Not sure"
q21.36$answer[q21.36$answer == "Non"] <- "No"

#Organize data = gather "other please specify"


#summarise data
q21.36.summary <- 
  q21.36 %>% group_by(Question, answer) %>% count() %>% 
  drop_na()

#sum
q21.36.sum <- 
  q21.36.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q21.36.sum.merged <- 
  q21.36.summary %>% 
  left_join(q21.36.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q21.36.sum.flip <- q21.36.sum.merged %>% 
  mutate(new_n = ifelse(Question == "Do you need to securely share data stored on the commercial cloud\nwith specific collaborators?",
                        -1*proportion, proportion))


#### Plot ####
ggplot(q21.36.sum.flip, aes(fill=Question, y=new_n, x=answer)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("The need to securely share data on clouds")+
  scale_fill_manual(values =  cb_pie2)



### Q22 & Q37 ######
q22 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X22) %>% 
  unnest(X22)

q37 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X37) %>% 
  unnest(X37)


q22.37 <- 
  full_join(q22, q37, by = "Internal.ID") %>% 
  gather("Question", "answer", 2:3) %>% 
  unique() # n = 256

#Change replace X22 and X37 by the questions
q22.37$Question[q22.37$Question == "X22"] <- "How do you transfer the data stored on the commercial cloud?"
q22.37$Question[q22.37$Question == "X37"] <- "How do you transfer the data stored on the Alliance Cloud?"
q22.37$answer[q22.37$answer == "Automatically using software (i.e., Globus)"] <- "Automatically using software (e.g., Globus)"
q22.37$answer[q22.37$answer == "Manually using software (i.e., Globus)"] <- "Manually using software (e.g., Globus)"


#summarise data
q22.37.summary <- 
  q22.37 %>% group_by(Question, answer) %>% count() %>% 
  drop_na()

#sum
q22.37.sum <- 
  q22.37.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q22.37.sum.merged <- 
  q22.37.summary %>% 
  left_join(q22.37.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q22.37.sum.flip <- q22.37.sum.merged %>% 
  mutate(new_n = ifelse(Question == "How do you transfer the data stored on the commercial cloud?",
                        -1*proportion, proportion))

#### Plot ####
ggplot(q22.37.sum.flip, aes(fill=Question, y=new_n, x=answer)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("Data transfer")+
  scale_fill_manual(values =  cb_pie2)


### Q23 & Q38 ######
q23.38 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X23, X38) %>% 
  unnest(c(X23, X38)) %>% 
  gather("Question", "answer", 2:3) # n = 239

#### Yes-No- Not sure ####

#select only Yes and No
q23.38.y.n <- 
  q23.38 %>% 
  filter(answer == "Yes" | answer == "No" | answer == "Not sure") # n = 237

#Change replace X23 and X38 by the questions
q23.38.y.n$Question[q23.38.y.n$Question == "X23"] <- "Do you have any concerns about storing data on the commercial cloud\n(e.g., safety/security, backups, costs of downloading or moving data)?"
q23.38.y.n$Question[q23.38.y.n$Question == "X38"] <- "Do you have any concerns about storing data on the Alliance Cloud\n(e.g., safety/security, backups)?"

#summarise data
q23.38.summary <- 
  q23.38.y.n %>% group_by(Question, answer) %>% count() %>% 
  drop_na()

#sum
q23.38.sum <- 
  q23.38.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q23.38.sum.merged <- 
  q23.38.summary %>% 
  left_join(q23.38.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q23.38.sum.flip <- q23.38.sum.merged %>% 
  mutate(new_n = ifelse(Question == "Do you have any concerns about storing data on the commercial cloud\n(e.g., safety/security, backups, costs of downloading or moving data)?",
                        -1*proportion, proportion))

#### Mirror Plot- Yes - No####

ggplot(q23.38.sum.flip, aes(fill=Question, y=new_n, x=answer)) + 
  geom_bar(position="stack", stat="identity")+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("Data storing")+
  scale_fill_manual(values =  cb_pie2)


#### Yes - specify ####
q23.38.specify <- 
  q23.38 %>% 
  filter(!answer == "Yes" & !answer == "No"  & !answer == "Not sure")

q23.38.specify <- read.csv("q23.38_clean.csv")

#list of "if yes please specify" for both questions
q23.38.specify <- 
  q23.38.specify %>% 
  mutate(answer_clean = ifelse(
    answer_clean == "Security and privacy", answer_clean, ifelse(
      answer_clean == "Costs", answer_clean, ifelse(
        answer_clean == "Backups", answer_clean, "Other"
      ))))


q23.38.specify.summary <- 
  q23.38.specify %>% group_by(Question,answer_clean) %>% count() %>% 
  drop_na() %>% arrange(-n)

##For Yes = please specify = Security and Privacy, costs, and backups


q23.38.specify.summary$Question[q23.38.specify.summary$Question == "X23"] <- "Do you have any concerns about storing data on the commercial cloud\n(e.g., safety/security, backups, costs of downloading or moving data)?"
q23.38.specify.summary$Question[q23.38.specify.summary$Question == "X38"] <- "Do you have any concerns about storing data on the Alliance Cloud\n(e.g., safety/security, backups)?"

#sum
q23.38.specify.sum <- 
  q23.38.specify.summary %>% group_by(Question) %>% summarise(sum = sum(n))

#merge sum table to summary table

q23.38.specify.merged <- 
  q23.38.specify.summary %>% 
  left_join(q23.38.specify.sum, by = "Question") %>% 
  mutate(proportion = (n/sum)*100)

#Add negative values to create the mirror barplot graph
q23.38.sum.flip <- q23.38.specify.merged %>% 
  mutate(new_n = ifelse(Question == "Do you have any concerns about storing data on the commercial cloud\n(e.g., safety/security, backups, costs of downloading or moving data)?",
                        -1*proportion, proportion))

#### Mirror Plot- Yes - describe concerns####

# cb_pie1 <- rep(c("#32322F", "#D6AB00"), 100)
ggplot(q23.38.sum.flip, aes(fill=Question, y=new_n, x=answer_clean)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  xlab("Answer") + ylab("Proportion")+
  ggtitle("Concerns about data storing")+
  scale_fill_manual(values =  cb_pie2)



### Q25 - Have you lost access to data stored on the commercial cloud due to lack of continuity of funding for your research? ######
q25 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X25) %>% 
  unnest(X25)

q25_summay <- 
  q25 %>% 
  group_by(X25) %>% 
  count()


#add domain
q25.domain <- 
  q25 %>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q25.domain.summary <- 
  q25.domain %>% 
  group_by(TC3, X25) %>% count() %>% drop_na()

#calculate sum and add it
q25.sum <- 
  q25.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q25.domain.summary <- 
  q25.domain.summary %>% 
  left_join(q25.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X25)

#### Pie chart ####
PieDonut(q25_summay, 
         aes(X25, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie)


#### plot - domain #### 

ggplot(q25.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie_3)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Have you lost access to data stored on the commercial cloud\ndue to lack of continuity of funding for your research?")

### Q26 - Do you get support when using the commercial cloud? ######
q26 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X26) %>% 
  unnest(X26) # n = 201

q26.y.n <- 
  q26 %>% 
  filter(X26 == "Yes" | X26 == "No") # n = 237


q26_summay <- 
  q26.y.n %>% 
  group_by(X26) %>% 
  count()


#add domain
q26.domain <- 
  q26.y.n %>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q26.domain.summary <- 
  q26.domain %>% 
  group_by(TC3, X26) %>% count() %>% drop_na()

#calculate sum and add it
q26.sum <- 
  q26.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q26.domain.summary <- 
  q26.domain.summary %>% 
  left_join(q26.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X26)
#### Pie chart ####
PieDonut(q26_summay, 
         aes(X26, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you get support when using the commercial cloud?", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie1)

#### plot - domain #### 

ggplot(q26.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie1)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Do you get support when using the commercial cloud?")

#### Yes - specify ####
q26.specify <- 
  q26 %>% 
  filter(!X26 == "Yes" & !X26 == "No")

q26.specify <- read.csv("q26_clean.csv")

#to be used to reorder the plot values
order <- as.data.frame(c(7:1))


q26.specify.summary <- 
  q26.specify %>% group_by(answer_clean) %>% count() %>% 
  drop_na() %>% arrange(-n)

#add total n (sum) = to be used to calculate proportions
sum <- sum(q26.specify.summary$n)

#add proportions
q26.specify.summary <- 
  cbind(q26.specify.summary, order) %>% 
  rename(order = 3) %>% 
  mutate(sum = sum, Percentage = (n/sum)*100)

#### Plot - yes - specify ####
ggplot(q26.specify.summary, aes(x= reorder(answer_clean, order))) + 
  geom_bar(aes(y=Percentage), stat= "identity") +
  scale_fill_manual(values =  "#D6AB00") + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=Percentage, label= round(Percentage, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Support source for using Commercial Cloud") +
  xlab("Source") + 
  ylab("Proportion (%)")

### Q27 - Would you like support from the Alliance when using the commercial cloud? ######
q27 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X27) %>% 
  unnest(X27)

q27_summay <- 
  q27 %>% 
  group_by(X27) %>% 
  count()

#add domain
q27.domain <- 
  q27%>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q27.domain.summary <- 
  q27.domain %>% 
  group_by(TC3, X27) %>% count() %>% drop_na()

#calculate sum and add it
q27.sum <- 
  q27.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q27.domain.summary <- 
  q27.domain.summary %>% 
  left_join(q27.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X27)
#### Pie chart ####
PieDonut(q27_summay, 
         aes(X27, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie)






#### plot - domain #### 

ggplot(q27.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie_3)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  geom_text(position = position_stack(vjust = .5), 
            aes(y=Proportion, label=round(Proportion, digits = 0))) +
  ggtitle("Would you like support from the Alliance when using the commercial cloud?")

### Q29 - What is/was your primary reason for using the Alliance Cloud?######
q29 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X29) %>% 
  unnest(X29) %>% 
  filter(!X29 == "Other") # n = 100

#Clean the data = delete column "Other" and change "additional comments" to "Other"
q29_clean <- 
  q29 %>% 
  mutate(answer_n = ifelse(
    X29 == "Additional computational resources", X29, ifelse(
      X29 == "Additional storage resources", X29, ifelse(
        X29 == "Faster-performing computational resources", X29, ifelse(
          X29 == "Web hosting or web portals", X29, ifelse(
            X29 == "Specialized hardware (e.g., TPUs, GPUs)", X29, ifelse(
              X29 == "Contributing to research work currently leveraging the Alliance cloud ", "Contributing to research work currently leveraging the Alliance cloud", ifelse(
                X29 == "More flexible storage resources", X29, ifelse(
                  X29 == "Grant or funding requirement ", "Grant or funding requirement", ifelse(
                    X29 == "Ease of use", X29, ifelse(
                      X29 == "Requiring cloud software not available on the commercial clouds ", "Requiring cloud software not available on the commercial clouds", ifelse(
                        X29 == "Replicating research study/analysis previously performed on the Alliance cloud", X29, ifelse(
                          X29 == "Matériel spécialisé (p. ex., TPU, GPU)", "Specialized hardware (e.g., TPUs, GPUs)", ifelse(
                            X29 == "Additional comments:", "Delete", ifelse(
                              X29 == "Autre (veuillez préciser) :", "Delete", "Other"
                              ))))))))))))))) %>% 
  filter(!answer_n == "Delete") # n = 100


#link TC3 to q29 IDs
q29.domain <- 
  q29_clean %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  drop_na() %>% 
  print()


Workflow.q29 <- q29.domain

nHR <- filter(Workflow.q29, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #28
nSE <- filter(Workflow.q29, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#71
nSSH <- filter(Workflow.q29, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #19



Workflow_Health <- filter(Workflow.q29, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q29, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q29, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer_n,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "bottom", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Primary reason for using the Alliance cloud") +
  xlab("") + 
  ylab("")


### Q30 - What components of the commercial cloud do you use?? ######
q30 <- 
  survey_organized_spread %>% # n = 507
  select(Internal.ID, X30) %>% 
  unnest(X30) %>% 
  filter(!X30 == "Other") # n = 92

#add other unique answer (other) together
to_filter <- q30 %>% group_by(X30) %>% count()

q30_n <- 
  q30 %>% 
  left_join(to_filter, by = "X30")

q30.org <- 
  q30_n %>% 
  mutate(answer = ifelse(
    X30 == "Heat orchestration", X30, ifelse(
      n == 1, "other", X30
    ))) %>% 
  drop_na() %>% 
  select(-X30, -n)

#link TC3 to q30 IDs
q30.domain <- 
  q30.org %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  unique() %>% 
  drop_na() %>% 
  print() # n = 91


Workflow.q30 <- q30.domain

nHR <- filter(Workflow.q30, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #23
nSE <- filter(Workflow.q30, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#66
nSSH <- filter(Workflow.q30, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #18



Workflow_Health <- filter(Workflow.q30, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q30, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q30, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Components of the Alliance Cloud") +
  xlab("") + 
  ylab("")


### Q32 - Are there cloud services which you would like to be able to access on the Alliance Cloud but currently cannot (e.g., Managed Kubernetes, Galaxy, Docker)? ######
q32 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X32) %>% 
  unnest(X32) # n = 96

q32.y.n <- 
  q32 %>% 
  filter(X32 == "Yes" | X32 == "No") # n = 237


q32_summay <- 
  q32.y.n %>% 
  group_by(X32) %>% 
  count()

#add domain
q32.domain <- 
  q32.y.n%>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q32.domain.summary <- 
  q32.domain %>% 
  group_by(TC3, X32) %>% count() %>% drop_na()

#calculate sum and add it
q32.sum <- 
  q32.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q32.domain.summary <- 
  q32.domain.summary %>% 
  left_join(q32.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X32)
#### Pie chart ####
PieDonut(q32_summay, 
         aes(X32, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie1)

#### plot - domain #### 

ggplot(q32.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie1)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Are there cloud services which you would like to be able to\naccess on the Alliance Cloud but currently cannot\n(e.g., Managed Kubernetes, Galaxy, Docker)?")

#### Yes - specify ####
q32.specify <- 
  q32 %>% 
  filter(!X32 == "Yes" & !X32 == "No")

q32.specify <- read.csv("q32_clean.csv")


q32.specify.summary <- 
  q32.specify %>% group_by(answer_clean) %>% count() %>% 
  drop_na() %>% arrange(-n)

#to be used to reorder the plot values
order <- as.data.frame(c(17:1))


#add total n (sum) = to be used to calculate proportions
sum <- sum(q32.specify.summary$n)

#add proportions
q32.specify.summary <- 
  cbind(q32.specify.summary, order) %>% 
  rename(order = 3) %>% 
  mutate(sum = sum, Percentage = (n/sum)*100)

#### Plot - yes - specify ####
ggplot(q32.specify.summary, aes(x= reorder(answer_clean, order))) + 
  geom_bar(aes(y=Percentage), stat= "identity") +
  scale_fill_manual(values =  "#D6AB00") + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=Percentage, label= round(Percentage, digits = 1))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "left", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Identifying Cloud-based Research Tools for a National Service") +
  xlab("Platforms or software services") + 
  ylab("Proportion (%)")

### Q39 - Would it be helpful to have tools that track your cloud data storage and remind you to back up or migrate content? ######
q39 <- 
  survey_organized_spread %>% 
  select(Internal.ID, X39) %>% 
  unnest(X39)

q39_summay <- 
  q39 %>% 
  group_by(X39) %>% 
  count()

#add domain
q39.domain <- 
  q39%>% 
  left_join(domain1, by = "Internal.ID")

#group by domain and answer
q39.domain.summary <- 
  q39.domain %>% 
  group_by(TC3, X39) %>% count() %>% drop_na()

#calculate sum and add it
q39.sum <- 
  q39.domain.summary %>% group_by(TC3) %>% summarise(sum = sum(n))

q39.domain.summary <- 
  q39.domain.summary %>% 
  left_join(q39.sum, by = "TC3") %>% 
  mutate(Proportion = (n/sum)*100) %>% 
  rename(Answer = X39)

#### Pie chart ####
PieDonut(q39_summay, 
         aes(X39, count= n), 
         ratioByGroup = FALSE, 
         showPieName=FALSE, 
         r0=0.0,r1=1,r2=1.4,start=pi/2,
         labelpositionThreshold=1, 
         showRatioThreshold = F, 
         title= "Do you currently, or have you in the past,\nuse(d) cloud resources to support your research?", 
         titlesize = 5, pieAlpha = 1, donutAlpha = 1, color = "black")+ scale_fill_manual(values =  cb_pie)




#### plot - domain #### 

ggplot(q39.domain.summary, aes(fill=Answer, y=Proportion, x=TC3)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_fill_manual(values =  cb_pie_3)+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))+
  ggtitle("Would it be helpful to have tools that track your cloud data\nstorage and remind you to back up or migrate content?")

### Q40 -  On top of the existing resources, which of the following technical support methods would you like to access? Check all that apply ######
#### data cleaning & preparation ####
#extract question 40
q40 <- survey_organized_spread %>% 
  select(Internal.ID, X40) %>% 
  unnest(X40) %>% 
  rename(answer = X40) # n = 92

#Create a table with domain for TC3 by ID
domain <- 
  domain_new_table1 %>% 
  select(-Domain, -n)

domain1 <- domain

#### domain1$TC3 <-
####  recode_factor(domain1$TC3, CIHR = "Health Research",
###            NSERC = "Sciences and Engineering", SSHRC = "Social Sciences and Humanities")


#Organize q40 by adding all "other" answers together
q40_orga <- 
  q40 %>% 
  filter(!answer == "Other") %>% #Other is an invalid answer, keeping it will add extra false answers as they are previsouly "Other (Please specify)"
  mutate(answer_n =
           ifelse(answer == "Accessible manuals (e.g., lay language; PDF)", "Accessible manuals", ifelse(
             answer == "Short video tutorials on basic cloud tasks", answer, ifelse(
               answer == "Online real-time chat with cloud support (i.e., Mattermost, Rocket Chat or Slack)", "Online real-time chat with cloud support", ifelse(
                 answer == "Topical cloud-related training", answer, ifelse(
                   answer == "Virtual office hours", answer, "Other"
                     )))))) # n = 92

#summarize the data
q40_summary <- 
  q40_orga %>% 
  group_by(answer_n) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#link TC3 to q40 IDs
q40.domain <- 
  q40_orga %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  select(-answer) %>% 
  print() ## n = 92


Workflow.q40 <- q40.domain

nHR <- filter(Workflow.q40, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #28
nSE <- filter(Workflow.q40, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#59
nSSH <- filter(Workflow.q40, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #22



Workflow_Health <- filter(Workflow.q40, TC3=="Health Research") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR)*100)

Workflow_SciEng <- filter(Workflow.q40, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE)*100)

Workflow_SSH <- filter(Workflow.q40, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer_n) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH)*100) 

Workflow_Tri1 <- rbind(Workflow_SSH, Workflow_SciEng, Workflow_Health)  

#### Plot ####
ggplot(Workflow_Tri1, aes(x=reorder(answer_n,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0))) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "bottom", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank())+
  ggtitle("Exploring Technical Support Options for Additional Resources") +
  xlab("") + 
  ylab("")
# fixed_plot_aspect()
