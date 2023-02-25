library(tidyverse)
library(maps)
library(mapdata)
library(ggmap)
library(ggplot2)
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


survey <- read.csv("merged_survey_CLEAN.csv", header=T, encoding = "UTF-8") # merged_survey_clean_20210520FPJ.csv
view(survey$Gender)

# General ####

survey$Institution_Type <- as.factor(survey$Institution_Type)
survey$Geography <- as.factor(survey$Geography)
survey$Affiliation <- as.factor(survey$Affiliation)
survey$Research_Domain <- as.factor(survey$Research_Domain)
survey$Affiliation <- as.factor(survey$Affiliation)
survey$AcademicPosition <- as.factor(survey$AcademicPosition)
survey$Affiliation <- recode(survey$Affiliation, "simon fraser university"="Simon Fraser University")
survey$Affiliation <- recode(survey$Affiliation, "university of Waterloo" ="University of Waterloo")
survey$Affiliation <- recode(survey$Affiliation, "university of waterloo" ="University of Waterloo")
survey$Affiliation <- recode(survey$Affiliation, "université de montréal" ="Université de Montréal")
survey$Affiliation <- recode(survey$Affiliation, "Concordia university" = "Concordia University")
summary(survey$AcademicPosition)

survey$TC3 <- recode_factor(survey$TC3, CIHR = "Health Research", 
                            NSERC = "Sciences and Engineering", SSHRC = "Social Sciences and Humanities")

## Setting up a color palette ####
color.palette <- c(brewer.pal(11, "RdBu"),brewer.pal(11, "RdBu"),brewer.pal(11, "RdBu")) #Not using this one for now

cbp1 <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 100)

# Demographics Analysis ###############################################################################################
# 
#
  ## Geographic Representation ########################################################################################
    
    select(survey, Geography) %>% 
      group_by(Geography)%>% 
      summarize(n = n()) %>% 
      arrange(desc(n),.by_group = T) %>% 
      mutate("%" = (n / sum(n))*100) %>%
      gt()
    
    Table_Representation_Geogrpahy <- select(survey, Geography, Affiliation) %>% 
      rename(Institution = Affiliation) %>% 
      group_by(Geography, Institution) %>% 
      summarize(n = n()) %>% 
      arrange(desc(n),.by_group = T) %>% 
      gt() %>% 
      tab_header(title="NDRIO Survey participation by region") 


      ### Ontario ######
      Ontario <-filter(survey, Geography %in% "Ontario") %>% 
        select( Geography, Affiliation) %>%
        group_by(Affiliation)  %>% 
        summarize(n = n()) %>% 
        arrange(n) %>%
        data.frame()
      
      Ontario$Affiliation <- factor(Ontario$Affiliation, levels=unique(Ontario$Affiliation))
      
      
      #### Bar graph for Affiliation - Ontario #####
        ggplot(Ontario, aes(x = Affiliation)) +
          geom_col(aes(y=n, fill=Affiliation)) + 
          scale_fill_manual(values =  cbp1) +
          geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
          coord_flip() + 
          theme_linedraw() +
          theme(legend.position = "none") +
          ggtitle("Ontario") +
          xlab("Institutional Affiliation") + 
          ylab("Number of respondents")
      
      
    ### Quebec ######
      Quebec <-filter(survey, Geography %in% "Québec") %>% 
        select( Geography, Affiliation) %>%
        group_by(Affiliation)  %>% 
        summarize(n = n()) %>% 
        arrange(n) %>%
        data.frame()
      
      
      Quebec$Affiliation <- factor(Quebec$Affiliation, levels=unique(Quebec$Affiliation))
      
      
      #### Bar graph for Affiliation - Quebec ######
        ggplot(Quebec, aes(x = Affiliation)) +
          geom_col(aes(y=n, fill=Affiliation)) + 
          scale_fill_manual(values =  cbp1) +
          geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
          coord_flip() + 
          theme_linedraw() +
          theme(legend.position = "none") +
          ggtitle("Québec") +
          xlab("Institutional Affiliation") + 
          ylab("Number of respondents")
      
      
    ### West ####
      West <-filter(survey, Geography %in% c("Alberta", "British Columbia", "Saskatchewan","Manitoba")) %>% 
        select( Geography, Affiliation) %>%
        group_by(Affiliation)  %>% 
        summarize(n = n()) %>% 
        arrange(n) %>%
        data.frame()
      
      
      West$Affiliation <- factor(West$Affiliation, levels=unique(West$Affiliation))
      
      
      #### Bar graph for Affiliation - Western Canada ####
          ggplot(West, aes(x = Affiliation)) +
            geom_col(aes(y=n, fill=Affiliation)) + 
            scale_fill_manual(values =  cbp1) +
            geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
            coord_flip() + 
            theme_linedraw() +
            theme(legend.position = "none") +
            ggtitle("Western Canada") +
            xlab("Institutional Affiliation") + 
            ylab("Number of respondents")
        

    ### East #####
      East <-filter(survey, Geography %in% c("New Brunswick", "Nova Scotia", "Newfoundland and Labrador", "Prince Edward Island")) %>% 
        select( Geography, Affiliation) %>%
        group_by(Affiliation)  %>% 
        summarize(n = n()) %>% 
        arrange(n) %>%
        data.frame()
      
      East$Affiliation <- factor(East$Affiliation, levels=unique(East$Affiliation))
      
      
      #### Bar graph for Affiliation - Eastern Canada ####
        ggplot(East, aes(x = Affiliation)) +
          geom_col(aes(y=n, fill=Affiliation)) + 
          scale_fill_manual(values =  cbp1) +
          geom_text(aes(y=n, label= n), hjust= -0.35, vjust=0) +
          coord_flip() + 
          theme_linedraw() +
          theme(legend.position = "none") +
          ggtitle("Eastern Canada") +
          xlab("Institutional Affiliation") + 
          ylab("Number of respondents")


    ### All Provinces ####
      Canada <- filter(survey, !Geography %in% c("National", "Regional", "International")) %>% 
        group_by(Geography) %>% 
        summarize(props = n()) %>%   
        arrange(props) 
  
      Canada$Geography <- factor(Canada$Geography, levels = unique(Canada$Geography))
  
      #### Pie chart - All Canada ####
      PieDonut(Canada, aes(Geography, count= props), ratioByGroup = FALSE, showPieName=FALSE, r0=0.25,r1=1,r2=1.4,start=pi/2,labelpositionThreshold=1, showRatioThreshold = F, title= "Respondents by Region", titlesize = 5) + 
        scale_fill_manual(values =  cbp1)
      
      
  ## Research Domain and Tri-Council ##################################################################################
Domain_Breakdown <- select(survey, TC3, D2) %>% filter(!TC3 == "") 

PieDonut(Domain_Breakdown, aes(TC3,D2),showPieName=FALSE, r0=0.0,r1=0.8,r2=1.4,start=, showRatioThreshold = F, title= "Respondents by Discipline") 

    
      
  ## Academic position ###################################################################################################
 
    Academic_Breakdown <- select(survey, D5, D6,D6O, D10)
          
        non_faculty <-filter(Academic_Breakdown, D5 == "No") %>% 
          mutate(status = "Non-faculty researchers", 
                 sub_status = ifelse(grepl("student", D10), "Graduate student", 
                                     ifelse(grepl("Postdoctoral Researcher", D10), "Postdoc",
                                            ifelse(grepl("Research Associate", D10), "Research Associate", 
                                                   ifelse(grepl("staff", D10), "Research support", "Other2"))))) %>% 
          select(status, sub_status)
        
        
        non_faculty_wrong <- filter(Academic_Breakdown, D5 == "Yes", D6 == "Other (please specify)", grepl("tud|doc|PhD|Research Associate|post", D6O)) %>% mutate(status= "Non-faculty researchers", 
                                                                                                                                                              sub_status = ifelse(grepl("tud|PhD", D6O), "Graduate student", 
                                                                                                                                                                                  ifelse(grepl("Post", D6O), "Postdoc",
                                                                                                                                                                                         ifelse(grepl("Associate", D6O), "Research Associate", "Other2" )))) %>% 
          select(status, sub_status)
          
        
        
          faculty <- filter(Academic_Breakdown,D5 == "Yes", !D6 == "Librarian" , !grepl("tud|doc|PhD|Associate|post", D6O)) %>% 
            mutate(status = "Faculty researchers", sub_status = ifelse(grepl("Assoc", D6), "Associate Professor", 
                                                                                                                                                                                           ifelse(grepl("Assis", D6), "Assistant Professor",
                                                                                                                                                                                                  ifelse(grepl("Adjunct", D6), "Adjunct or Limited term", 
                                                                                                                                                                                                         ifelse(grepl("Other", D6), "Other1", "Full Professor" ))))) %>% 
            select(status, sub_status)
          
           
          librarians <- filter(Academic_Breakdown,D5 == "Yes", D6 == "Librarian") %>% 
            mutate(status = "Non-faculty researchers", sub_status = "Librarian") %>% 
            select(status, sub_status)
            
        
          Academic_clean <- rbind(non_faculty, non_faculty_wrong , faculty, librarians) 
    
       #### Donut chart for Academic position - all ####  
        PieDonut(Academic_clean, aes(status,sub_status), color= "white",addPieLabel = TRUE, showPieName=F, r0=0.0,r1=0.8,r2=1.4,start=pi/2, showRatioThreshold = F, title= "Breakdown of Respondents by academic position", donutLabelSize = 4, titlesize =6) 
    
    #### ggplot Doughnut chart ####
Academic_clean_ggplot_sub <- Academic_clean %>% group_by(sub_status) %>% summarize(n = n()) %>% mutate(level = "sub", prop = (n/sum(n))*100) %>% rename("status" = "sub_status")
Academic_clean_ggplot_stat <- Academic_clean %>% group_by(status) %>% summarize(n= n()) %>% mutate(level = "status", prop = (n/sum(n))*100)

a <- rbind(Academic_clean_ggplot_sub, Academic_clean_ggplot_stat)
  
  a$status <- factor(a$status, levels = c("Adjunct or Limited term", "Other1","Assistant Professor","Associate Professor","Full Professor","Librarian","Other2","Postdoc", "Research Associate", "Research support", "Graduate student" , "Faculty researchers","Non-faculty researchers") )

a$name <- paste(a$status, "(", round(a$prop, digits = 2), "%)")
  
academic_palette <- c("#BBDFFA", "#97BAEC","#6987D5","#445BC1","#1c1c84","#ffbaba","#ff7b7b","#ff5252", "#ff0000", "#e3191c", "#b30024" , "#1c1c84","#b30024")
   
### Double doughnut chart by academic ####
ggplot(a, aes(x=level, y=prop, fill=status)) +
      geom_col() + 
      geom_text(aes(label=name), color = "white",position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y")+ scale_fill_manual(values =  academic_palette)+theme_linedraw(base_size = 18) +
      ggtitle("Breakdown of Respondents by academic position") +
     theme(legend.position = "none", axis.text.y = element_blank(),axis.ticks = element_blank()) +
      xlab("") + 
      ylab("") 
    
 
      
# General Survey Questions ############################################################################################

      ## G9 - How well are your current Digital Research Infrastructure needs being met? ##############################
       
    
     Needs <-  select(survey, CaseId, starts_with("G9"), -ends_with("_COMMENT")) %>% 
      rename("Storage" = G9A,  "Computing" = G9B,  "Support" = G9C, "Training" = G9D, "Funding" = G9E)
    
    Needs_Long <- gather(Needs, key,value, Storage:Funding) %>% arrange( by=CaseId)
        
    nComputing <- select(Needs_Long, key, value) %>% 
      filter(key == "Computing", !value == "") %>%
      count() %>%
      as.character()
      
    nStorage <- select(Needs_Long, key, value) %>% 
      filter(key == "Storage", !value == "") %>%
      count() %>%
      as.character()  
    
    nSupport <- select(Needs_Long, key, value) %>% 
      filter(key == "Support", !value == "") %>%
      count() %>%
      as.character() 
    
    nTraining <- select(Needs_Long, key, value) %>% 
      filter(key == "Training", !value == "") %>%
      count() %>%
      as.character() 
    
    nFunding <- select(Needs_Long, key, value) %>% 
      filter(key == "Funding", !value == "") %>%
      count() %>%
      as.character()
    
    All_Needs_likert <-  select(Needs_Long, key, value) %>% 
        group_by(key, value) %>%
        summarize(n=n()) %>%
        filter( !value == "") %>% 
        mutate( nDRI = as.numeric(ifelse(key == "Computing", nComputing, 
                            ifelse(key == "Funding", nFunding,
                                   ifelse(key == "Storage", nStorage,
                                          ifelse(key == "Support", nSupport, nTraining
                                                 ))))), 
                "%" = (n/nDRI)*100, 
                scale = factor(ifelse(value == "Strongly agree", 2, 
                                      ifelse(value == "Agree", 1,
                                             ifelse(value == "Neutral", 0,
                                                    ifelse(value == "Disagree", -1, -2)
                                                    )))),
                    "neg%" = ifelse(value == "Strongly agree","" , 
                        ifelse(value == "Agree", "",
                               ifelse(value == "Neutral", `%`/-2,
                                      ifelse(value == "Disagree", `%`*-1, `%`*-1)))),
                    "pos%" = ifelse(value == "Strongly agree",`%`, 
                                ifelse(value == "Agree", `%`,
                                       ifelse(value == "Neutral", `%`/2,
                                              ifelse(value == "Disagree", "", "")))))
    
             negs <- select(All_Needs_likert, key, value, n, scale,`neg%` ) %>% rename("%" = `neg%`)
             pos <- select(All_Needs_likert, key, value, n, scale,`pos%`  ) %>% rename("%" = `pos%`)
             
             a <- rbind(negs, pos)
             
             All_Needs_likert$key <- factor(All_Needs_likert$key)
             All_Needs_likert$value <- factor(All_Needs_likert$value, levels = unique(All_Needs_likert$value), ordered = T)
             
             All_Needs_likert$value <- factor(All_Needs_likert$value , levels =c("Strongly disagree","Disagree", "Neutral", "Agree", "Strongly agree")) 
             display.brewer.pal(n = 8, name = 'RdBu')
             brewer.pal(n = 8, name = "RdBu")# "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC"
             
             likert_color <- c("#B2182B","#F4A582", "#d3d3d3","#92C5DE","#2166AC" )
             
             
    ### Likert Graph on DRI satisfatction - General ####
       ggplot(All_Needs_likert,aes( x=key, y=`%`, fill=value))+geom_col()+scale_fill_manual(values =  likert_color) + 
         geom_text(aes(label = round(`%`, digits = 2)), position = position_stack(vjust = .5)) +
         coord_flip()+
         theme_linedraw(base_size = 18) +
         ggtitle("Satisfaction with current DRI ") +
         xlab("Needs") + 
         ylab("")  
      
    
       ## G9 by TRC - How well are your current Digital Research Infrastructure needs being met? #################################################
       TC3_Needs <-  select(survey, CaseId, TC3, starts_with("G9"), -ends_with("_COMMENT")) %>% 
         rename("Storage" = G9A,  "Computing" = G9B,  "Support" = G9C, "Training" = G9D, "Funding" = G9E) 
       
       TC3_Needs$TC3 <- as.character(TC3_Needs$TC3)
       
       TC3_Needs_Long <- gather(TC3_Needs, key,value, Storage:Funding) %>% 
         arrange( by=CaseId) %>% group_by(TC3, key) %>% 
         filter(!value =="") 
       
       
       
       TC3_Needs_sub <- select(TC3_Needs_Long, key, value) %>% 
         group_by(TC3, key,value) %>% 
         summarize(n= n()) %>%  filter(!TC3 == "") %>% 
         mutate(scale = factor(ifelse(value == "Strongly agree", 2, 
                                      ifelse(value == "Agree", 1,
                                             ifelse(value == "Neutral", 0,
                                                    ifelse(value == "Disagree", -1, -2)
                                                    )))))
      # calculate DRI/TC n
       n_TC3_Needs <- summarize(TC3_Needs_sub, sum(n))
       
       # add DRI/TC n to Needs
       TC3_Needs_sub$group_n <- rep(n_TC3_Needs$`sum(n)`, each=5)
      
       #add %
       TC3_Needs_sub <- mutate(TC3_Needs_sub, "%" = (n/group_n)*100)
       
       TC3_Needs_sub$value <- factor(TC3_Needs_sub$value, levels =c("Strongly disagree","Disagree", "Neutral", "Agree", "Strongly agree"), order=T) 
     
       TC3_Needs_sub <- arrange(TC3_Needs_sub, TC3, key, value)
         
       ### Likert Graph on DRI satisfaction by TRC ####
       ggplot(TC3_Needs_sub, aes(x=key, y=`%`, fill=value))+geom_col()+facet_grid(rows=vars(TC3)) + 
         scale_fill_manual(values =  likert_color) + 
         geom_hline(yintercept = 50, linetype="dotted", color = "black", size=.75) +
         coord_flip() +
         geom_text(aes(label = round(`%`, digits = 2)), position = position_stack(vjust = .5)) +
         theme_linedraw(base_size = 18) +
         ggtitle("Satisfaction with current DRI") +
         xlab("Needs") + 
         ylab("") 
       
       
      ### Trying to center around Neutral ####
      TC3_Needs_subNeutralPos <- filter(TC3_Needs_sub, value == "Neutral") %>% mutate(link = `%`/2, value2 = "Pos")
      TC3_Needs_subNeutralNeg <- filter(TC3_Needs_sub, value == "Neutral") %>% mutate(link = `%`/-2, value2 = "Neg")
      
      TC3_Needs_subPos <- filter(TC3_Needs_sub, value == c("Strongly agree", "Agree")) %>% mutate(link = `%`, value2 = "Pos")
            TC3_Needs_subNeg <- filter(TC3_Needs_sub, value == c("Strongly disagree", "Disagree")) %>% mutate(link = `%`/-1, value2 = "Neg")
      
    TC3_Needs_all <- rbind(TC3_Needs_subNeutralPos,TC3_Needs_subNeutralNeg, TC3_Needs_subPos, TC3_Needs_subNeg)
    
    TC3_Needs_all$value <- factor(TC3_Needs_all$value, levels =c("Strongly agree", "Agree","Disagree", "Neutral","Strongly disagree"), ordered=T)
    
    likert_color2 <- c("#92C5DE", "#F4A582","#d3d3d3","#2166AC","#B2182B" )
      
  
    # REALLY NOT SURE WHY THE TEXT ON THE LEFT HAND SIDE IS NOT ALIGNED...
    # 
    ### Centered Likert Graph attempt on DRI Satisfaction by TRC ####
    ggplot(data =TC3_Needs_all, aes(x=key, y=link))+
      geom_col(data= filter(TC3_Needs_all, value2 == "Pos"), aes(x=key, y=link, fill=factor(value, levels = c("Strongly agree","Agree", "Neutral")))) + 
      geom_col(data= filter(TC3_Needs_all, value2 == "Neg"), aes(x=key, y=link, fill=factor(value, levels = c( "Neutral", "Disagree", "Strongly disagree"))), position= position_stack(reverse = T)) +
     # geom_text(data= filter(TC3_Needs_all, value2 == "Pos"), aes(x= key, y=link, label = round(link, digits = 2)), position = position_stack(vjust = .5)) +
      #geom_text(data= filter(TC3_Needs_all, value2 == "Neg"), aes(x=key, y= link, label = round(link, digits = 2))) +
      scale_fill_manual(values =  likert_color2) + 
      facet_grid(rows=vars(TC3)) +
      coord_flip() +
      theme_linedraw(base_size = 18) + 
      theme(legend.position = "none")
      ggtitle("Satisfaction with current DRI") +
      xlab("Needs") + 
      ylab("") +
      xlim(-100,100)
      

       
                      
                      ### THIS ENDED UP BEING UNNECESARY
       
       TC3_nStorage <- select(TC3_Needs_Long, key, value) %>% 
        ilter(key == "Storage", !value == "") %>%
        count() %>%
        as.character()  
       
       TC3_nSupport <- select(TC3_Needs_Long, key, value) %>% 
        ilter(key == "Support") %>%
         count() %>%
         as.character() 
       
       TC3_nTraining <- select(TC3_Needs_Long, key, value) %>% 
         filter(key == "Training", !value == "") %>%
         count() %>%
         as.character() 
       
       TC3_nFunding <- select(TC3_Needs_Long, key, value) %>% 
        filter(key == "Funding", !value == "") %>%
       count() %>%
       as.character()
       
       TC3_All_Needs_likert <-  select(TC3_Needs_Long, key, value) %>% 
        group_by(key, value) %>%
       summarize(n=n()) %>%
       filter( !value == "") %>% 
       mutate( nDRI = as.numeric(ifelse(key == "Computing", nComputing, 
                                       ifelse(key == "Funding", nFunding,
                                             ifelse(key == "Storage", nStorage,
                                                   ifelse(key == "Support", nSupport, nTraining
                                                  ))))), 
          "%" = (n/nDRI)*100, 
         scale = factor(ifelse(value == "Strongly agree", 2, 
                                ifelse(value == "Agree", 1,
                                      ifelse(value == "Neutral", 0,
                                            ifelse(value == "Disagree", -1, -2)
                                    )))),
        "neg%" = ifelse(value == "Strongly agree","" , 
                       ifelse(value == "Agree", "",
                             ifelse(value == "Neutral", `%`/-2,
                                   ifelse(value == "Disagree", `%`*-1, `%`*-1)))),
       "pos%" = ifelse(value == "Strongly agree",`%`, 
                      ifelse(value == "Agree", `%`,
                            ifelse(value == "Neutral", `%`/2,
                                  ifelse(value == "Disagree", "", "")))))
       
      
      ## G1 - Which of these activities are part of your research workflows? ###########################################

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
     
      ### Stacked Bar Graph - Research Workflows by Council ####
    ggplot(Workflow_Tri, aes(x=reorder(Source,`%`))) + 
      geom_bar(aes(y=`%`, fill = TC3), stat= "identity") +
      scale_fill_manual(values =  cbp1) + 
      coord_flip() +geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
      theme_linedraw(base_size = 18) +
      theme(legend.position = "none")
      ggtitle("Activities in Research Workflows") +
      xlab("") + 
      ylab("")+
     fixed_plot_aspect()



    ## G13 - What should NDRIO Prioritize to meet your DRI Needs ######################################################
    
    Priorities <- select(survey, CaseId, TC3, starts_with("G13M"), -ends_with("O")) %>%
    gather(Option, Source, 3:7) %>% filter(!Source == "")
    
    nHR_Priorities <- filter(Priorities, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #248
    nSE_Priorities <- filter(Priorities, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #667
    nSSH_Priorities <- filter(Priorities, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #279
    
    
    
    Priorities_Health <- filter(Priorities, TC3=="Health Research") %>%
      group_by(TC3, Source) %>%
      summarize(n = n()) %>%
      arrange(desc(n),.by_group = T) %>%
      mutate('%' = (n / nHR_Priorities)*100)
    
    Priorities_SciEng <- filter(Priorities, TC3=="Sciences and Engineering") %>%
      group_by(TC3, Source) %>%
      summarize(n = n()) %>%
      arrange(desc(n),.by_group = T) %>%
      mutate('%' = (n / nSE_Priorities)*100)
    
    Priorities_SSH <- filter(Priorities, TC3=="Social Sciences and Humanities") %>%
      group_by(TC3, Source) %>%
      summarize(n = n()) %>%
      arrange(desc(n),.by_group = T) %>%
      mutate('%' = (n / nSSH_Priorities)*100)
    
    
    Priorities_Tri <- rbind(Priorities_SSH, Priorities_SciEng, Priorities_Health)  
    
        ### Stacked Bar Graph - Priorities by Council ####
    ggplot(Priorities_Tri, aes(x=reorder(Source, `%`), y=`%`)) + 
      geom_col(aes(fill= TC3)) +
      coord_flip() +
      scale_fill_manual(values =  cbp1) + 
      geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
      theme_linedraw(base_size = 18) +
      theme(legend.position = "none")+
      ggtitle("DRI Priorities") +
      xlab("") + 
      ylab("") 
    
    
  ## G2 - 3 items that influence the most your managing and sharing practices #############################################################
      
    ### data ####
  Influences_data <- select(survey, CaseId, TC3, starts_with("G2A"), -ends_with("_OTH")) %>%
    gather(Option, Source, 3:5) %>% filter(!Source == "")
  
  nHR_Influences_data <- filter(Influences_data, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #225
  nSE_Influences_data <- filter(Influences_data, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #574
  nSSH_Influences_data <- filter(Influences_data, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #245
  
  Influences_data_Health <- filter(Influences_data, TC3=="Health Research") %>%
      group_by(TC3, Source) %>%
      summarize(n = n()) %>%
      arrange(desc(n),.by_group = T) %>%
      mutate('%' = (n / nHR_Influences_data)*100)
    
  Influences_data_SciEng <- filter(Influences_data, TC3=="Sciences and Engineering") %>%
      group_by(TC3, Source) %>%
      summarize(n = n()) %>%
      arrange(desc(n),.by_group = T) %>%
      mutate('%' = (n / nSE_Influences_data)*100)
    
  Influences_data_SSH <- filter(Influences_data, TC3=="Social Sciences and Humanities") %>%
      group_by(TC3, Source) %>%
      summarize(n = n()) %>%
      arrange(desc(n),.by_group = T) %>%
      mutate('%' = (n / nSSH_Influences_data)*100)
    
  Influences_data_all <- rbind(Influences_data_Health, Influences_data_SciEng, Influences_data_SSH) 
  Influences_data_all$pillar <- rep("data", length(Influences_data_all$TC3))
    
    ### code ####
  Influences_code <- select(survey, CaseId, TC3, starts_with("G2B"), -ends_with("_OTH")) %>%
    gather(Option, Source, 3:5) %>% filter(!Source == "")
  
  nHR_Influences_code <- filter(Influences_code, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #154
  nSE_Influences_code <- filter(Influences_code, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #532
  nSSH_Influences_code <- filter(Influences_code, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #140
  
  Influences_code_Health <- filter(Influences_code, TC3=="Health Research") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR_Influences_code)*100)
  
  Influences_code_SciEng <- filter(Influences_code, TC3=="Sciences and Engineering") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE_Influences_code)*100)
  
  Influences_code_SSH <- filter(Influences_code, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH_Influences_code)*100)
  
  Influences_code_all <- rbind(Influences_code_Health, Influences_code_SciEng, Influences_code_SSH) 
  Influences_code_all$pillar <- rep("code", length(Influences_code_all$TC3))
  
  ### code & data ###
  
  Influences_all <- rbind(Influences_code_all, Influences_data_all)
  
  ### Bar graph for code and data sharing practices ####
  ggplot(Influences_all, aes(x=Source, y=`%`)) + 
    geom_col(aes(fill= pillar)) +
    facet_grid(rows = vars(TC3)) +
    scale_fill_manual(values =  cbp1) + 
    geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    ggtitle("Influences on management and sharing") +
    xlab("") + 
    ylab("") +
    theme( axis.text.x=element_text(size=11, angle=30, vjust=1, hjust=1))
  
  
  ## G3 - Funding needs ######################################################################################
  
  Funding_Needs <- select(survey, CaseId, TC3, starts_with("G3M"), -ends_with("O")) %>%
     gather( Option, Source, 3:24) %>% filter( !Source == "", !Source == "NA")
  
  tail(Funding_Needs)
  
  nHR_Funding_Needs <- filter(Funding_Needs, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #220
  nSE_Funding_Needs <- filter(Funding_Needs, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #601
  nSSH_Funding_Needs <- filter(Funding_Needs, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #254
  
  Funding_Needs_Health <- filter(Funding_Needs, TC3=="Health Research") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR_Funding_Needs)*100)
  
  Funding_Needs_SciEng <- filter(Funding_Needs, TC3=="Sciences and Engineering") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE_Funding_Needs)*100)
  
  Funding_Needs_SSH <- filter(Funding_Needs, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH_Funding_Needs)*100)
  
  
  Funding_Needs_Tri <- rbind(Funding_Needs_SSH, Funding_Needs_SciEng, Funding_Needs_Health) 
  
  ### Stacked Bar Graph - Priorities by Council ####
  ggplot(Funding_Needs_Tri, aes(x=reorder(Source, `%`), y=`%`)) + 
    geom_col(aes(fill= TC3)) +
    coord_flip() +
    scale_fill_manual(values =  cbp1) + 
    geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    theme(legend.position = "none") +
    ggtitle("DRI Funding Needs") +
    xlab("") + 
    ylab("")

  
  ## G4 - Access to Professional IT #################################################################
  IT_color <- c("#2166AC" , "#B2182B","#d3d3d3")
  
  TC3_IT <-  select(survey, CaseId, TC3, starts_with("G4")) %>% 
    rename( "Within team" = G4A, "Within Department/Faculty" = G4B,  "Within Institution" = G4C, "Network of collaborators" = G4D,"Provincial Service" = G4E,  "National Service" = G4F) 
  
  TC3_IT$TC3 <- as.character(TC3_IT$TC3)
  
  TC3_IT_Long <- gather(TC3_IT, key,value, 3:8) %>% 
    arrange( by=CaseId) %>% group_by(TC3, key) %>% 
    filter(!value =="") 
  
  
  
  TC3_IT_sub <- select(TC3_IT_Long, key, value) %>% 
    group_by(TC3, key,value) %>% 
    summarize(n= n()) %>%  filter(!TC3 == "") 
  
  # calculate IT/TC n
  n_TC3_IT <- summarize( TC3_IT_sub, sum(n))
  
  # add IT/TC n to Needs
  TC3_IT_sub$group_n <- rep(n_TC3_IT$`sum(n)`, each=3)
  
  #add %
  TC3_IT_sub <- mutate(TC3_IT_sub, "%" = (n/group_n)*100)
  
  TC3_Needs_sub$value <- factor(TC3_Needs_sub$value, levels =c("Strongly disagree","Disagree", "Neutral", "Agree", "Strongly agree"), order=T) 
  # a %>% TC3_Needs_sub %>% mutate(x = ifelse(TC3 == "Health Research" & key == "Computing",))
  TC3_IT_sub <- arrange(TC3_IT_sub, TC3, key, value)
  
  TC3_IT_sub$value <- factor(TC3_IT_sub$value, levels = c("Yes", "No", "Not sure"))
  TC3_IT_sub$key <- factor(TC3_IT_sub$key, levels = c( "Within team", "Within Department/Faculty",  "Within Institution", "Network of collaborators","Provincial Service", "National Service" ))
  
  ### Stacked Bar Graph - IT Support by scale and Council 
  ggplot(TC3_IT_sub, aes(x=key, y=`%`, fill= value)) + 
    geom_col(position=position_dodge()) + 
    scale_fill_manual(values =  IT_color) + 
    facet_grid(rows = vars(TC3)) +
    geom_text(position = position_dodge(width=1), vjust = -1, aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    ggtitle("Existing IT Support") +
    theme(legend.position = "none") +
    xlab("") + 
    ylab("") +
    theme( axis.text.x=element_text(size=14)) + 
    ylim(0, 90)
  
    
  
  ## G5 - Discovery of new tools #################################################################
  
  Discovery <- select(survey, CaseId, TC3, starts_with("G5M"), -ends_with("O")) %>%
    gather( Option, Source, 3:13) %>% filter( !Source == "" , !Source ==  "NA")
  
  nHR_Discovery <- filter(Discovery, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #227
  nSE_Discovery <- filter(Discovery, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #615
  nSSH_Discovery <- filter(Discovery, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #255
  

  Discovery_Health <- filter(Discovery, TC3=="Health Research") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR_Discovery)*100)
  
  Discovery_SciEng <- filter(Discovery, TC3=="Sciences and Engineering") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE_Discovery)*100)
  
  Discovery_SSH <- filter(Discovery, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH_Discovery)*100)
  
  
  Discovery_Tri <- rbind(Discovery_SSH, Discovery_SciEng, Discovery_Health) 
  
  ### Stacked Bar Graph - Discovery channels by Council ####
  ggplot(Discovery_Tri, aes(x=reorder(Source, `%`), y=`%`)) + 
    geom_col(aes(fill= TC3)) +
    coord_flip() +
    scale_fill_manual(values =  cbp1) + 
    geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    theme(legend.position = "none") +
    ggtitle("Channels for the discovery of DRI tools and resources") +
    xlab("") + 
    ylab("") 
  
  
  ## G6 - Location of infrastructure ###########################################################
  
  DRI_Location <- select(survey, CaseId, TC3, starts_with("G6M"), -ends_with("O")) %>%
    gather( Option, Source, 3:13) %>% filter( !Source == "" , !Source ==  "NA") 
  
  nHR_DRI_Location <- filter(DRI_Location, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #231
  nSE_DRI_Location <- filter(DRI_Location, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #620
  nSSH_DRI_Location <- filter(DRI_Location, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #255
  
  DRI_Location_Health <- filter(DRI_Location, TC3=="Health Research") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR_DRI_Location)*100)
  
  DRI_Location_SciEng <- filter(DRI_Location, TC3=="Sciences and Engineering") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE_DRI_Location)*100)
  
  DRI_Location_SSH <- filter(DRI_Location, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH_DRI_Location)*100)
  
  
  DRI_Location_Tri <- rbind(DRI_Location_SSH, DRI_Location_SciEng, DRI_Location_Health) 
  
  ### Stacked Bar Graph - Location of computing infrastructure by Council ####
  ggplot(DRI_Location_Tri, aes(x=reorder(Source, `%`), y=`%`)) + 
    geom_col(aes(fill= TC3)) +
    coord_flip() +
    scale_fill_manual(values =  cbp1) + 
    geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    ggtitle("Location of computing infrastructure") +
    theme(legend.position = "none") +
    xlab("") + 
    ylab("")
  
  ## G7 - Training requirements ##################################################################
  Training <- select(survey, CaseId, TC3, starts_with("G7M"), -ends_with("O")) %>%
    gather( Option, Source, 3:7) %>% filter( !Source == "" , !Source ==  "NA", !TC3 == "") 
  
  tail(Training)
  
  nHR_Training <- filter(Training, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #224
  nSE_Training <- filter(Training, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #588
  nSSH_Training <- filter(Training, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #236
  
  Training_Health <- filter(Training, TC3=="Health Research") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR_Training)*100)
  
  Training_SciEng <- filter(Training, TC3=="Sciences and Engineering") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE_Training)*100)
  
  Training_SSH <- filter(Training, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH_Training)*100)
  
  
  Training_Tri <- rbind(Training_SSH, Training_SciEng, Training_Health) 
  
  ### Stacked Bar Graph - Training needs by Council ####
  ggplot(Training_Tri, aes(x=reorder(Source, `%`), y=`%`)) + 
    geom_col(aes(fill= TC3)) +
    coord_flip() +
    scale_fill_manual(values =  cbp1) + 
    geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    ggtitle("Training needs") +
    theme(legend.position = "none") +
    xlab("") + 
    ylab("") 
  
  ## G8 - Training providers ##################################################################
  #
  Training_Provider <- select(survey, CaseId, TC3, starts_with("G8M"), -ends_with("O")) %>%
    gather( Option, Source, 3:5) %>% filter( !Source == "" , !Source ==  "NA", !TC3 == "") 
  
  tail(Training)
  
  nHR_Training_Provider <- filter(Training_Provider, TC3 == "Health Research") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #218
  nSE_Training_Provider <- filter(Training_Provider, TC3 == "Sciences and Engineering") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #587
  nSSH_Training_Provider <- filter(Training_Provider, TC3 == "Social Sciences and Humanities") %>% select( CaseId) %>% unique() %>% count() %>% as.numeric() #237
  
  Training_Provider_Health <- filter(Training_Provider, TC3=="Health Research") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nHR_Training_Provider)*100)
  
  Training_Provider_SciEng <- filter(Training_Provider, TC3=="Sciences and Engineering") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSE_Training_Provider)*100)
  
  Training_Provider_SSH <- filter(Training_Provider, TC3=="Social Sciences and Humanities") %>%
    group_by(TC3, Source) %>%
    summarize(n = n()) %>%
    arrange(desc(n),.by_group = T) %>%
    mutate('%' = (n / nSSH_Training_Provider)*100)
  
  
  Training_Provider_Tri <- rbind(Training_Provider_SSH, Training_Provider_SciEng, Training_Provider_Health) 
  
  ### Stacked Bar Graph - Training resources by Council ####
  ggplot(Training_Provider_Tri, aes(x=reorder(Source, `%`), y=`%`)) + 
    geom_col(aes(fill= TC3)) +
    coord_flip() +
    scale_fill_manual(values =  cbp1) + 
    geom_text(position = position_stack(vjust = .5), aes(y=`%`, label=round(`%`, digits = 2))) +
    theme_linedraw(base_size = 18) +
    ggtitle("Training resources") +
    theme(legend.position = "none") +
    xlab("") + 
    ylab("") 
 