#-----------------------------------------------------------------------------#
# Paper: The People's Captain: Understanding Police Officers as an            #
#        Electoral Brand                                                      #
# Author: Jacob Turner                                                        #
# Last Update: April 20, 2024                                                 #
#-----------------------------------------------------------------------------#

# This code replicates the analysis in the paper and duplicates Figures 6-12  #
# from the main paper and the figures from Appendix A                         #

setwd("YOUR DIRECTORY")

library(cregg)
library(ggplot2)
library(dotwhisker)

####################### Setup #########################

#Read in cleaned conjoint data
cjframe<-read.csv('conjoint_dataframe.csv',
                  encoding='UTF-8')

#Set reference levels
cjframe$prof<-factor(cjframe$prof,
                levels=c('office','captain','doctor',
                         'police','pastor','professor'))
cjframe$race<-factor(cjframe$race,
                levels=c('black','white','pardo'))
cjframe$sex<-factor(cjframe$sex,
               levels=c('male','female'))
cjframe$politician<-factor(cjframe$politician,
                      levels=c('novice','politician'))
cjframe$econ<-factor(cjframe$econ,
                levels=c('right','left'))
cjframe$crime<-factor(cjframe$crime,
                 levels=c('video','prevention','ironFist'))
cjframe$slogan<-factor(cjframe$slogan)
cjframe$resp3<-factor(cjframe$resp3)

#create ggplot theme
basictheme<-theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15),
        legend.position="none")



####################### Model Estimation #######################

#unconditional binary outcomes, AMCE

#Security Competency
m1a<-cj(data=cjframe[!is.na(cjframe$seguranca),],
       seguranca~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Corruption Competency
m2a<-cj(data=cjframe[!is.na(cjframe$corruption),],
       corruption~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Employment Competency
m3a<-cj(data=cjframe[!is.na(cjframe$employ),],
       employ~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Healthcare Competency
m4a<-cj(data=cjframe[!is.na(cjframe$saude),],
       saude~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Education Competency
m5a<-cj(data=cjframe[!is.na(cjframe$education),],
       education~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Average non-security competency
mcompsa<-cj(data=cjframe[!is.na(cjframe$law),],
           other~prof+crime+race+sex+politician+econ+crime+slogan,
           id=~id)

#Strong Leadership
m6a<-cj(data=cjframe[!is.na(cjframe$leader),],
       leader~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Vote Choice
m7a<-cj(data=cjframe[!is.na(cjframe$binchoice),],
       binchoice~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Right-wing Ideology
m8a<-cj(data=cjframe[!is.na(cjframe$binideology),],
       binideology~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Respectful of Democracy
m9a<-cj(data=cjframe[!is.na(cjframe$bindem),],
       bindem~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Promotes Rule of Law
m10a<-cj(data=cjframe[!is.na(cjframe$law),],
        law~prof+crime+race+sex+politician+econ+crime+slogan,
        id=~id)

#unconditional binary outcomes, MM

#Security Competency
m1b<-mm(data=cjframe[!is.na(cjframe$seguranca),],
       seguranca~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id,
       h0=.5)

#Corruption competency
m2b<-mm(data=cjframe[!is.na(cjframe$corruption),],
       corruption~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Employment competency
m3b<-mm(data=cjframe[!is.na(cjframe$employ),],
       employ~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Healthcare competency
m4b<-mm(data=cjframe[!is.na(cjframe$saude),],
       saude~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Education competency
m5b<-mm(data=cjframe[!is.na(cjframe$education),],
       education~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Average non-security competency
mcompsb<-mm(data=cjframe[!is.na(cjframe$law),],
           other~prof+crime+race+sex+politician+econ+crime+slogan,
           id=~id)

#Strong leadership
m6b<-mm(data=cjframe[!is.na(cjframe$leader),],
       leader~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Vote choice
m7b<-mm(data=cjframe[!is.na(cjframe$binchoice),],
       binchoice~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Right Wing
m8b<-mm(data=cjframe[!is.na(cjframe$binideology),],
       binideology~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Respectful of democracy
m9b<-mm(data=cjframe[!is.na(cjframe$bindem),],
       bindem~prof+crime+race+sex+politician+econ+crime+slogan,
       id=~id)

#Promotes rule of law
m10b<-mm(data=cjframe[!is.na(cjframe$law),],
        law~prof+crime+race+sex+politician+econ+crime+slogan,
        id=~id)

#Conditional on Respondent Ideology, MM

#Rule of Law
lawhet<-cj(data=cjframe[!is.na(cjframe$law),],
           law~prof+crime+race+sex+politician+econ+crime+slogan,
           id=~id,
           estimate = "mm",
           by=~resp3)

#Corruption
corhet<-cj(data=cjframe[!is.na(cjframe$law),],
           corruption~prof+crime+race+sex+politician+econ+crime+slogan,
           id=~id,
           estimate = "mm",
           by=~resp3)






####################### Plots #######################

#AMCE plots

#Replicate Figure 6
overall<-rbind(m1a,mcompsa,m6a,m8a,m9a,m10a,m2a)
overall<-overall[overall$level=="police",]
names(overall)[names(overall)=="outcome"]<-"term"

png("plots/amce_overall_police.png")
dwplot(overall,
       dot_args = list(color="black"),
       whisker_args = list(color="black"),ci = .95)+
  ggtitle("Estimated AMCE for Police\non Selected Outcomes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15))+
  scale_y_discrete(name="Outcome",
                   labels=c("Corruption (H5b)","Rule of Law (H5a)",
                            "Respects Democracy (H4)",
                            "Right-Wing (H3)","Strong Leader (H2)",
                            "Other Policy Areas (H1b)",
                            "Public Security (H1a)"))+
  scale_x_continuous(name="AMCE")+
  geom_vline(xintercept = 0,lty="dashed")
dev.off()

#Replicate Figure 7
competency<-rbind(m1a,m3a,m4a,m5a)
competency<-competency[competency$level=="police",]
names(competency)[names(competency)=='outcome']<-'term'

png("plots/amce_competencies_police.png")
dwplot(competency,
       dot_args = list(color="black"),
       whisker_args = list(color="black"),ci = .95)+
  ggtitle("Estimated AMCE for Police\nin Different Policy Areas")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15))+
  scale_y_discrete(name="Policy Area",
                   labels=c("Education","Healthcare","Employment","Security"))+
  scale_x_continuous(name="AMCE")+
  geom_vline(xintercept = 0,lty="dashed")
dev.off()

#Replicate Figure 8
png("plots/amce_security_prof_policy.png")
plot(m1a[grepl("police|ironFist|prevention|video",m1a$level),],feature_headers = FALSE)+
  ggtitle("AMCE for Police and Security Policies\non Security Effectiveness")+
  basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Police","Video Monitoring (Reference)","Prevention","Iron Fist"))+
  scale_color_manual(values=rep("black",3))+xlab("AMCE")
dev.off()

#Replicate Figure 9
png("plots/amce_ideology_plot_profs.png")
plot(m8a[m8a$feature=='prof',],feature_headers = FALSE)+
  ggtitle("AMCE for Professions\non Ideology")+
  basictheme+
  scale_color_manual(values="black")+
  scale_y_discrete(name="levels",
                   labels=c("Businessperson (Reference)","Soldier","Doctor","Police",
                            "Pastor","Professor"))+xlab("AMCE")
dev.off()

#Replicate Figure 10
rolplot<-rbind(m2a,m10a)
demlead<-rbind(m9a,m6a)
demleadrolcorup<-rbind(demlead,rolplot)
demleadrolcorup<-demleadrolcorup[demleadrolcorup$level=="police",]
names(demleadrolcorup)[names(demleadrolcorup)=="outcome"]<-"term"

png("plots/amce_demleadrolcorup_police.png")
dwplot(demleadrolcorup,
       dot_args = list(color="black"),
       whisker_args = list(color="black"),ci = .95)+
  ggtitle("Estimated AMCE for Police\non Rule of Law, Corruption \nLeadership, and Respect for Democracy")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15))+
  scale_y_discrete(name="Outcome",
                   labels=c("Corruption","Rule of Law",
                            "Strong Leader","Respects Democracy"))+
  scale_x_continuous(name="AMCE",limits=c(-.1,.18))+
  geom_vline(xintercept = .0,lty="dashed")
dev.off()

#Replicate Figure 11
png("plots/amce_democracy_plot_profs_policy.png")
plot(m9a[(m9a$level=='police'|m9a$feature=="crime"),])+
  ggtitle("AMCE for Police and Security Policy\non Respect for Democracy")+
  basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Video Monitoring (Reference)","Prevention","Iron Fist",
                            "(Security Proposal)","Police","(Profession"))+
  scale_color_manual(values=rep("black",5))+xlab("AMCE")
dev.off()

#Replicate Figure 12
lplot<-corhet[corhet$level=="police",]
names(lplot)[names(lplot)=="resp3"]<-"term"

png("plots/mm_het_ideology_corruption.png")
dwplot(lplot,
       dot_args = list(color="black"),
       whisker_args = list(color="black"),ci = .95)+
  ggtitle("Estimated MM for Police\non Corruption\nConditional on Respondent Ideology")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15))+
  scale_x_continuous(name="MM")+
  geom_vline(xintercept = 0.5,lty="dashed")
dev.off()

lplot<-lawhet[lawhet$level=="police",]
names(lplot)[names(lplot)=="resp3"]<-"term"

png("plots/mm_het_ideology_law.png")
dwplot(lplot,
       dot_args = list(color="black"),
       whisker_args = list(color="black"),ci = .95)+
  ggtitle("Estimated MM for Police\non Rule of Law\nConditional on Respondent Ideology")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=12),
        axis.title = element_text(size=15))+
  scale_x_continuous(name="MM")+
  geom_vline(xintercept = 0.5,lty="dashed")
dev.off()


############################## Appendix A #############################

#Vote Choice AMCE
png("plots/amce_choice_plot.png")
plot(m7a,size=2)+
  ggtitle("AMCE for All Features\non Vote Choice")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Vote Choice MM
png("plots/mm_choice_plot.png")
plot(m7b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Vote Choice")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Security Competency AMCE
png("plots/amce_security_plot.png")
plot(m1a,size=2)+
  ggtitle("AMCE for All Features\non Security Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Security Competency MM
png("plots/mm_security_plot.png")
plot(m1b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Security Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Corruption Competency AMCE
png("plots/amce_corrupt_plot.png")
plot(m2a,size=2)+
  ggtitle("AMCE for All Features\non Corruption Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Corruption Competency MM
png("plots/mm_corrupt_plot.png")
plot(m2b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Corruption Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Education Competency AMCE
png("plots/amce_educ_plot.png")
plot(m5a,size=2)+
  ggtitle("AMCE for All Features\non Education Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Education Competency MM
png("plots/mm_educ_plot.png")
plot(m5b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Education Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Healthcare Competency AMCE
png("plots/amce_health_plot.png")
plot(m4a,size=2)+
  ggtitle("AMCE for All Features\non Healthcare Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Healthcare Competency MM
png("plots/mm_health_plot.png")
plot(m4b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Healthcare Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Employment Competency AMCE
png("plots/amce_employ_plot.png")
plot(m3a,size=2)+
  ggtitle("AMCE for All Features\non Employment Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Employment Competency MM
png("plots/mm_employ_plot.png")
plot(m3b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Employment Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Strong Leader AMCE
png("plots/amce_leader_plot.png")
plot(m6a,size=2)+
  ggtitle("AMCE for All Features\non Strong Leader")+
  basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Strong Leader MM
png("plots/mm_leader_plot.png")
plot(m6b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Strong Leader")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Right Wing AMCE
png("plots/amce_ideology_plot.png")
plot(m8a,size=2)+
  ggtitle("AMCE for All Features\non Ideology")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Right Wing MM
png("plots/mm_ideology_plot.png")
plot(m8b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Ideology")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Respects Democracy AMCE
png("plots/amce_democracy_plot.png")
plot(m9a,size=2)+
  ggtitle("AMCE for All Features\non Respect for Democracy")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Respects Democracy MM
png("plots/mm_democracy_plot.png")
plot(m9b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Respect for Democracy")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()

#Promotes Rule of Law AMCE
png("plots/amce_rol_plot.png")
plot(m10a,size=2)+
  ggtitle("AMCE for All Features\non Respect for Rule of Law")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))+xlab("AMCE")
dev.off()

#Promotes Rule of Law MM
png("plots/mm_rol_plot.png")
plot(m10b,vline=0.5,size=2)+
  ggtitle("MM for All Features\non Respect for Rule of Law")+
  theme(plot.title = element_text(hjust = 0.5))+basictheme+
  scale_y_discrete(name="Feature",
                   labels=c("Generic","corruption","Crime","Employment",
                            "(Slogan)","Right-wing","Left-wing","(Ideology)",
                            "Political Novice","Experienced Politician",
                            "(Political Experience)","Male","Female","(Sex)",
                            "Black","White","Pardo","(Race)","Video Monitoring",
                            "Prevention","Iron Fist","(Security Proposal)",
                            "Business Owner","Soldier","Doctor","Police",
                            "Pastor","Professor","(Profession)"))+
  scale_color_manual(values=rep("black",7))
dev.off()


























