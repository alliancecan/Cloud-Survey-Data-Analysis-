# Prepare the directory and load the libraries ----------------------------


###Set working directory
setwd("...")

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

#survey data, english and french versions merged together
survey <- read.csv("FINAL_Alliance_Cloud_Survey_EN_FR_20220228.csv",
                   header = T,
                   encoding = "UTF-8",
                   na.strings=c("","NA")) %>% 
  rename(Internal.ID = Internal.ID)

#Delete no needed columns: Here columns 10, 12, 20, and 21 are for "Other please specify" which have been cleaned into new columns "Xn....New_Other". So no need to keep them in the table.

survey <- survey[c(-10,-12, -20, -21)] #delete columns "Other" and old "old please specify" have been reclassified into the "NEW_Other" column <<<<<<<< we need to name the columns, otherwise it is a bit confusing as to why or which

cbp1 <- rep(c("#B7B6B3", "#D6AB00","#00DBA7", "#56B4E9",
              "#32322F", "#FBFAFA", "#D55E00", "#CC79A7"), 100)

###
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

#Keep columns Internal.ID, Ques_num and Answer
survey_organized_clean <- 
  survey_organized %>% 
  select(Internal.ID, Ques_num, Answer)

#Spread the table: the output will have one column per question (in a list)
survey_organized_spread <- pivot_wider(survey_organized_clean, 
                                       names_from = Ques_num,
                                       values_from = Answer,
                                       values_fn = list)


### Q3 - Please choose your research domain ######

Domain_Breakdown <- survey_organized_spread %>% 
  select(Internal.ID, X3) %>% 
  unnest(X3) %>% 
  rename(Domain = X3)

domain_new_table <- Domain_Breakdown

#summarise the data = count domain's n
domain_summary <- 
  Domain_Breakdown %>% 
  group_by(Domain) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()

#### Group domains into TC3 ####
domain_summary1 <- 
  domain_summary %>% 
  mutate(TC3 = ifelse(Domain == "Natural Sciences ", "Sciences and Engineering", ifelse(
    Domain == "Humanities and the Arts ", "Social Sciences and Humanities", ifelse(
      Domain == "Social Sciences ", "Social Sciences and Humanities", ifelse(
        Domain == "Engineering and Technology ", "Sciences and Engineering", ifelse(
          Domain == "Medical, Health and Life Sciences ", "Health Research", "Sciences and Engineering"
        ))))))

#Link TC3 to Internal.ID = this will be used for the rest of the analysis as we will analyse data by TC3
domain <- 
  Domain_Breakdown %>% 
  left_join(domain_summary1, by = "Domain") %>% 
  select(-n) # n = 474

domain1 <- 
  domain %>% 
  select(-Domain)
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


#French version
#Organize q10 by adding all "other" answers together
q10_orga.fr <- 
  q10 %>%
  filter(!factor == "Other") %>% 
  mutate(answer =
           ifelse(factor == "Ease of use ", "Facilité d'utilisation", ifelse(
             factor == "Cost ", "Coût", ifelse(
               factor == "Scalability ", "Extensibilité", ifelse(
                 factor == "Vendor-agnostic features ", "Caractéristiques indépendantes du fournisseur" , ifelse(
                   factor == "security","Respect de la vie privée et sécurité" , ifelse(
                     factor == "privacy and security", "Respect de la vie privée et sécurité", ifelse(
                       factor == "Security", "Respect de la vie privée et sécurité", ifelse(
                         factor == "Privacy", "Respect de la vie privée et sécurité", ifelse(
                           factor == "safety-confidentiality", "Respect de la vie privée et sécurité", ifelse(
                             factor == "privacy", "Respect de la vie privée et sécurité", ifelse(
                               factor == "digital safety, privacy", "Respect de la vie privée et sécurité", ifelse(
                                 factor == "Security; Popularity (Which platform others in my discipline are using for ease of collaboration)", "Respect de la vie privée et sécurité", ifelse(
                                   factor == "Security, privacy, laws", "Respect de la vie privée et sécurité", ifelse(
                                     factor == "Security, accessibility in other countries (e.g., China)", "Respect de la vie privée et sécurité", ifelse(
                                       factor == "Security standards", "Respect de la vie privée et sécurité", ifelse(
                                         factor == "Security for sensitive health data", "Respect de la vie privée et sécurité", ifelse(
                                           factor == "Security and the ethics of the corporation producing it. If you're pushing the WEF agenda, take a hike.", "Respect de la vie privée et sécurité", "Autres"
                                         )))))))))))))))))) # the last "delete" is to delete "not applicable"


#summarize the data
q10_summary.fr <- 
  q10_orga.fr %>% 
  group_by(answer) %>% 
  count() %>% 
  arrange(-n) %>% 
  print()


#link TC3 to q7 IDs
q10.domain.fr <- 
  q10_orga.fr %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  select(-factor) %>% 
  print() ## n = 356


Workflow.q10.fr <- 
  q10.domain.fr %>% 
  unique()


nHR.fr <- filter(Workflow.q10.fr, TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #111
nSE.fr <- filter(Workflow.q10.fr, TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#191
nSSH.fr <- filter(Workflow.q10.fr, TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #114


Workflow_Health.fr <- filter(Workflow.q10.fr, TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR.fr)*100)

Workflow_SciEng.fr <- filter(Workflow.q10.fr, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE.fr)*100)

Workflow_SSH.fr <- filter(Workflow.q10.fr, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH.fr)*100) 


Workflow_Tri2.fr <- rbind(Workflow_SSH.fr, Workflow_SciEng.fr, Workflow_Health.fr)  

### Stacked Bar Graph on Cloud uses by TRC #### 
Workflow_Tri2.fr <- 
  Workflow_Tri2.fr %>% 
  mutate(TC3n = ifelse(
    TC3 == "Social Sciences and Humanities", "Sciences humaines et sociales", ifelse(
      TC3 == "Sciences and Engineering", "Sciences et génie", "Recherche en santé"
    )))

#French version
ggplot(Workflow_Tri2.fr, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3n), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0)), size = 5) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "bottom", legend.justification = "right", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank(),
        axis.text=element_text(size=20), legend.title = element_blank(),
        legend.text=element_text(size=20))+
  ggtitle("") +
  xlab("") + 
  ylab("")

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
q15$X15[q15$X15 == "vendor inkinds, philanthropy"] <- "Research grants "
q15$X15[q15$X15 == "Its bundled in with our overall storage for the University"] <- "Institutionally "

q15 <- q15 %>% drop_na()

#link TC3 to q15 IDs
q15.domain.eng <- 
  q15 %>% 
  left_join(domain1, by = "Internal.ID") %>% 
  print() ## n = 192

#French version
q15.domain.fr <- 
  q15.domain.eng %>% 
  mutate(FR = ifelse(
    X15 == "Research grants ", "Subventions de recherche", ifelse(
      X15 == "Industry grants or funding ", "Financement ou subventions de l’industrie", ifelse(
        X15 == "Institutionally ", "Établissement", ifelse(
          X15 == "Cloud research credits ", "Crédits de recherche en infonuagique", ifelse(
            X15 == "Personal funding ", "Financement personnel", ifelse(
              X15 == "service agreements", "Ententes de service", "Dons, frais d’utilisation"
            ))))))) %>% 
  select(-X15) %>% 
  rename(X15 = FR)


Workflow.q15.fr  <- q15.domain.fr 

Workflow.q15.fr  <- 
  q15.domain.fr %>% 
  mutate(TC3 = replace_na(Workflow.q15.fr$TC3, "Other")) %>% 
  rename(answer = X15) %>% 
  unique()

nHR.fr  <- filter(Workflow.q15.fr , TC3 == "Health Research") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #72
nSE.fr  <- filter(Workflow.q15.fr , TC3 == "Sciences and Engineering") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric()#102
nSSH.fr  <- filter(Workflow.q15.fr , TC3 == "Social Sciences and Humanities") %>% select(Internal.ID) %>% unique() %>% count() %>% as.numeric() #64



Workflow_Health.fr  <- filter(Workflow.q15.fr , TC3=="Health Research") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nHR.fr)*100)

Workflow_SciEng.fr  <- filter(Workflow.q15.fr, TC3=="Sciences and Engineering") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSE.fr)*100)

Workflow_SSH.fr <- filter(Workflow.q15.fr, TC3=="Social Sciences and Humanities") %>%
  group_by(TC3, answer) %>%
  summarize(n = n()) %>%
  arrange(desc(n),.by_group = T) %>%
  mutate('%' = (n / nSSH.fr)*100) 

Workflow_Tri1.fr <- rbind(Workflow_SSH.fr, Workflow_SciEng.fr, Workflow_Health.fr)  

#### Plot ####
Workflow_Tri1.fr <- 
  Workflow_Tri1.fr %>% 
  mutate(TC3n = ifelse(
    TC3 == "Social Sciences and Humanities", "Sciences humaines et sociales", ifelse(
      TC3 == "Sciences and Engineering", "Sciences et génie", "Recherche en santé"
    )))

#French version
ggplot(Workflow_Tri1.fr, aes(x=reorder(answer,`%`))) + 
  geom_bar(aes(y=`%`, fill = TC3n), stat= "identity") +
  scale_fill_manual(values =  cbp1) + 
  coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 0)), size = 5) +
  theme_linedraw(base_size = 18) +
  theme(legend.position = "bottom", legend.justification = "right", panel.grid.major.y = element_line(linetype = 2), panel.grid.minor.x = element_line(size = 0), panel.background = element_blank(),
        axis.text=element_text(size=20), legend.title = element_blank(),
        legend.text=element_text(size=20))+
  ggtitle("") +
  xlab("") + 
  ylab("")
