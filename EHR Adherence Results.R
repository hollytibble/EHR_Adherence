library(tidyverse)
library(corrplot)
library(lubridate)
rm(list=ls())
load("Adherence.RData")

CMA_Measures<-result
rm(result)

freqs$freqs_cat<-ifelse(freqs$prescs==1,"1",
                        ifelse(freqs$prescs<=5,"2-5",
                               ifelse(freqs$prescs<=10,"6-10",
                                      ifelse(freqs$prescs<=15,"11-15",
                                             ifelse(freqs$prescs<=20,"16-20",
                                                    ifelse(freqs$prescs<=25,"21-25",
                                                           ">25"))))))
freqs_cat<-freqs %>% group_by(freqs_cat) %>% summarise(sumcat=sum(freq))
freqs_cat$freqs_cat<-factor(freqs_cat$freqs_cat,levels=c("1","2-5","6-10","11-15","16-20","21-25",">25"))
ggplot(freqs_cat)+
  geom_col(aes(x=freqs_cat,y=sumcat), fill="steelblue") +
  xlab("Number of Prescriptions") +
  ylab("Number of People") + theme_bw() +
  scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
  theme(text = element_text(size=20))
rm(freqs_cat)

###-----------------------------------------------------------------------------------------###

CS<-CSA_measures %>%  gather(measure,value,-ID, -PrescDate) %>% ungroup

tapply(CS$value,CS$measure,summary)

summary(CS %>% filter(measure=="CSG" & !is.na(value)) %>% select(value))
summary(CS %>% filter(measure=="CSA" & !is.na(value)) %>% select(value))
summary(CS %>% filter(measure=="CSA_3" & !is.na(value)) %>% select(value))
summary(CS %>% filter(measure=="CSA_5" & !is.na(value)) %>% select(value))
summary(CS %>% filter(measure=="CSA_10" & !is.na(value)) %>% select(value))

ggplot(CS %>% filter(measure=="CSG")) +
  geom_density(aes(x=value,fill="measure")) +
  theme_bw() + xlab("Value") + ylab("Density") +
  scale_fill_manual(values="orange") +
  theme(text = element_text(size=20)) +
  facet_grid(rows =vars(measure), scales="free")

ggplot(CS %>% filter(measure!="CSG" & value<=4)) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20))

CS %>% select(-PrescDate) %>% 
  filter(measure %in% c("CSA","CSG")) %>%
  group_by(ID,measure) %>% 
  summarize(median=median(value)) %>%
  group_by(measure) %>%
  summarise(med=median(median))

CS$measure<-factor(CS$measure,levels=c("CSA","CSA_3","CSA_5","CSA_10","CSG"))
CS_out<-CS %>% select(-ID,-PrescDate)
write.csv(CS_out,"CS_out.csv")
ggplot(CS %>% filter(measure!="CSG")) +
  geom_boxplot(aes(x=measure,y=value, fill=measure)) +
  xlab("Adherence Measure") +
  ylab("Value") + theme_bw() + 
  coord_cartesian(ylim=c(0,4)) +
  theme(legend.position = "none",
        text = element_text(size=20))

CS_out$measure<-factor(CS_out$measure,levels=c("CSG","CSA",
                                               "CSA_3","CSA_5",'CSA_10'))
ggplot(CS_out) +
  geom_boxplot(aes(x=measure,y=value, fill=measure),outlier.shape = NA) +
  xlab("Adherence Measure") +
  ylab("Value") + theme_bw() + 
  coord_cartesian(ylim=c(0,4)) +
  theme(legend.position = "none",
        text = element_text(size=20))

sum(freqs %>% filter(prescs>=10) %>% select(freq))*100/length(unique(CMA_Measures$ID))
rm(freqs)

# variation from one year to the next
CSA_cor<-CS %>%
  filter(!is.na(value)) %>%
  group_by(ID, measure) %>%
  arrange(ID,measure, PrescDate) %>%
  mutate(value_next = lead(value),
         value_next2 = lead(value,2)) %>%
  filter(!is.na(value_next)) %>%
  ungroup
summarize(CSA_cor %>% group_by(measure), c(cor(value_next,value,method="spearman"),
                                           cor.test(value_next,value)$p.value,
                                           sum(!is.na(value_next))))
summarize(CSA_cor %>% group_by(measure), c(cor(value_next2,value,use = "complete",method="spearman"),
                                           cor.test(value_next2,value,use = "complete")$p.value,
                                           sum(!is.na(value_next2))))

###-----------------------------------------------------------------------------------------###

# How many year periods?
nrow(CMA_Measures %>% filter(nchar(period)==4))
# how many with no prescriptions
nrow(CMA_Measures %>% filter(nchar(period)==4 & CMA8_1==0))
nrow(CMA_Measures %>% filter(nchar(period)==4 & CMA8_1==0))*100/nrow(CMA_Measures %>% filter(nchar(period)==4))
# this should be zero
nrow(CMA_Measures %>% filter(nchar(period)==4 & CMA8_1==0 & !is.na(CMA5_3)))==0
# how had one prescription
nrow(CMA_Measures %>% filter(nchar(period)==4 & is.na(CMA5_3) & CMA8_1!=0))
nrow(CMA_Measures %>% filter(nchar(period)==4 & is.na(CMA5_3)  & CMA8_1!=0))*100/nrow(CMA_Measures %>% filter(nchar(period)==4))
# thus percentage with missing CMA1 and CMA5s
nrow(CMA_Measures %>% filter(nchar(period)==4 & is.na(CMA5_3)))*100/nrow(CMA_Measures %>% filter(nchar(period)==4))

# How many year quarters?
nrow(CMA_Measures %>% filter(nchar(period)==6))
# how many with no prescriptions
nrow(CMA_Measures %>% filter(nchar(period)==6 & CMA8_1==0))
nrow(CMA_Measures %>% filter(nchar(period)==6 & CMA8_1==0))*100/nrow(CMA_Measures %>% filter(nchar(period)==6))
# this should be zero
nrow(CMA_Measures %>% filter(nchar(period)==6 & CMA8_1==0 & !is.na(CMA5_3)))==0
# how had one prescription
nrow(CMA_Measures %>% filter(nchar(period)==6 & is.na(CMA5_3) & CMA8_1!=0))
nrow(CMA_Measures %>% filter(nchar(period)==6 & is.na(CMA5_3)  & CMA8_1!=0))*100/nrow(CMA_Measures %>% filter(nchar(period)==6))
# thus percentage with missing CMA1 and CMA5s
nrow(CMA_Measures %>% filter(nchar(period)==6 & is.na(CMA5_3)))*100/nrow(CMA_Measures %>% filter(nchar(period)==6))

CMA_all<-CMA_Measures %>% filter(period=="all") %>% select(-period) %>% gather(measure,value,-ID)
tapply(CMA_all$value,CMA_all$measure,summary)

ggplot(CMA_all %>% filter(measure=="CMA1" & value<4)) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), cols=vars("All Followup"),scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values="orange")

ggplot(CMA_all %>% filter(measure!="CMA1")) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), cols=vars("All Followup"),scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20))

CMA_y<-CMA_Measures %>% filter(nchar(period)==4) %>% gather(measure,value,-ID,-period)
tapply(CMA_y$value,CMA_y$measure,summary)

ggplot(CMA_y %>% filter(measure=="CMA1" & value<4)) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), cols=vars("Years"),scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values="orange")

ggplot(CMA_y %>% filter(measure!="CMA1")) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), cols=vars("Years"),scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20))

CMA_q<-CMA_Measures %>% filter(nchar(period)==6) %>% gather(measure,value,-ID,-period)
tapply(CMA_q$value,CMA_q$measure,summary)

ggplot(CMA_q %>% filter(measure=="CMA1" & value<4)) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), cols=vars("Quarters"),scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20)) +
  scale_fill_manual(values="orange")

ggplot(CMA_q %>% filter(measure!="CMA1")) +
  geom_density(aes(x=value,fill=measure)) +
  facet_grid(rows =vars(measure), cols=vars("Quarters"),scales="free") +
  theme_bw()  + xlab("Value") + ylab("Density")  +
  theme(text = element_text(size=20))

CMA_all_X<-bind_rows(CMA_Measures %>% filter(period=="all" & !is.na(CMA1)) %>% 
                       select(-period,-CMA1) %>% gather(measure,value,-ID) %>% 
                       mutate(group="2+"),
                     CMA_Measures %>% filter(period=="all" & is.na(CMA1)) %>% 
                       select(-period,-CMA1) %>% gather(measure,value,-ID) %>% 
                       mutate(group="1")) %>%
  filter(!is.na(value))
ggplot(CMA_all_X) +
  geom_boxplot(aes(x=measure,y=value, fill=as.factor(group))) +
  xlab("Adherence Measure") +
  ylab("Value") + theme_bw() +
  labs(fill="Total \nNumber of \nPrescriptions")+
  theme(text = element_text(size=20))

CMA_y_X<-bind_rows(CMA_Measures %>% filter(nchar(period)==4 & !is.na(CMA1)) %>% 
                       select(-period,-CMA1) %>% gather(measure,value,-ID) %>% 
                       mutate(group="2+"),
                     CMA_Measures %>% filter(nchar(period)==4 & is.na(CMA1)) %>% 
                       select(-period,-CMA1) %>% gather(measure,value,-ID) %>% 
                       mutate(group="1")) %>%
  filter(!is.na(value))

ggplot(CMA_y_X) +
  geom_boxplot(aes(x=measure,y=value, fill=as.factor(group))) +
  xlab("Adherence Measure") +
  ylab("Value") + theme_bw() +
  labs(fill="Annual \nNumber of \nPrescriptions") +
  theme(text = element_text(size=20))

CMA_q_X<-bind_rows(CMA_Measures %>% filter(nchar(period)==6 & !is.na(CMA1)) %>% 
                     select(-period,-CMA1) %>% gather(measure,value,-ID) %>% 
                     mutate(group="2+"),
                   CMA_Measures %>% filter(nchar(period)==6 & is.na(CMA1)) %>% 
                     select(-period,-CMA1) %>% gather(measure,value,-ID) %>% 
                     mutate(group="1")) %>%
  filter(!is.na(value))

ggplot(CMA_q_X) +
  geom_boxplot(aes(x=measure,y=value, fill=as.factor(group))) +
  xlab("Adherence Measure") +
  ylab("Value") + theme_bw() +
  labs(fill="Quarterly \nNumber of \nPrescriptions") +
  theme(text = element_text(size=20))

rm(CMA_q_X,CMA_y_X,CMA_all_X)

###-----------------------------------------------------------------------------------------###

# variation from one year to the next
CMA_y_cor<-CMA_y %>%
  filter(!is.na(value)) %>%
  group_by(ID, measure) %>%
  arrange(ID,measure,period) %>%
  mutate(value_next = lead(value),
         value_next2 = lead(value,2)) %>%
  filter(!is.na(value_next)) %>%
  ungroup

# variation from one quarter to the next
CMA_q_cor<-CMA_q %>%
  filter(!is.na(value)) %>%
  group_by(ID, measure) %>%
  arrange(ID,measure,period) %>%
  mutate(value_next = lead(value),
         value_next2 = lead(value,2)) %>%
  filter(!is.na(value_next)) %>%
  ungroup


summarize(CMA_y_cor %>% group_by(measure), c(cor(value_next,value,use = "complete",method="spearman")))
summarize(CMA_y_cor %>% group_by(measure), c(sum(!is.na(value_next))))

summarize(CMA_y_cor %>% group_by(measure), c(cor(value_next2,value,use = "complete",method="spearman")))
summarize(CMA_y_cor %>% group_by(measure), c(sum(!is.na(value_next2))))


summarize(CMA_q_cor %>% group_by(measure), c(cor(value_next,value,use = "complete",method="spearman")))
summarize(CMA_q_cor %>% group_by(measure), c(sum(!is.na(value_next))))

summarize(CMA_q_cor %>% group_by(measure), c(cor(value_next2,value,use = "complete",method="spearman")))
summarize(CMA_q_cor %>% group_by(measure), c(sum(!is.na(value_next2))))


###-----------------------------------------------------------------------------------------###

# ALL: for people with 2+ prescs (if 1, then CMA8s all the same, and corr==1)
plot1<-cor(CSA_measures %>% ungroup %>% select(-ID,-PrescDate),
           use="complete",method="spearman")
write.csv(plot1,file="corrplot1.csv")
plot1<-as.matrix(plot1)
rownames(plot1)<-c("CSA","CSG","CSA_3",'CSA_5',"CSA_10")
corrplot(plot1,
         addCoef.col = "white",type="lower",
         diag=F,method="color",tl.col="black", 
         number.cex=1.5, tl.cex=1.5, cl.cex=1) 


###-----------------------------------------------------------------------------------------###

# ALL: for people with 2+ prescs (if 1, then CMA8s all the same, and corr==1)
plot2<-cor(CMA_Measures %>% filter(period=="all") %>%
      select(-ID,-period),use="complete",method = "spearman")
write.csv(plot2,file="corrplot2.csv")
plot2<-as.matrix(plot2)
rownames(plot2)<-c("CMA1","CMA5_1","CMA5_2","CMA5_3","CMA8_1","CMA8_2","CMA8_3")
corrplot(plot2,
         addCoef.col = "white",type="lower",
         diag=F,method="color",tl.col="black",
         number.cex=1.1, tl.cex=1.1, cl.cex=1,
         mar=c(1,1,1,1))

plot3<-cor(CMA_Measures %>% filter(nchar(period)==4) %>%
             select(-ID,-period),use="complete",method = "spearman")
write.csv(plot3,file="corrplot3.csv")
plot3<-as.matrix(plot3)
rownames(plot3)<-c("CMA1","CMA5_1","CMA5_2","CMA5_3","CMA8_1","CMA8_2","CMA8_3")
corrplot(plot3,
         addCoef.col = "white",type="lower",
         diag=F,method="color",tl.col="black",
         number.cex=1.1, tl.cex=1.1, cl.cex=1,
         mar=c(1,1,1,1))

plot4<-cor(CMA_Measures %>% filter(nchar(period)==6) %>%
             select(-ID,-period),use="complete",method = "spearman")
write.csv(plot4,file="corrplot4.csv")
plot4<-as.matrix(plot4)
rownames(plot4)<-c("CMA1","CMA5_1","CMA5_2","CMA5_3","CMA8_1","CMA8_2","CMA8_3")
corrplot(plot4,
         addCoef.col = "white",type="lower",
         diag=F,method="color",tl.col="black",
         number.cex=1.1, tl.cex=1.1, cl.cex=1,
         mar=c(1,1,1,1))



###-----------------------------------------------------------------------------------------###

# ALL: for people with 2+ prescs
x<-cor(left_join(CMA_Measures %>% filter(period=="all" & !is.na(CMA1)),CSA_measures) %>%
         select(-ID,-period,-PrescDate),use="pairwise", method="spearman")
plot5<-x[1:7,8:12]
write.csv(plot5,file="corrplot5.csv")
plot5<-as.matrix(plot5)
rownames(plot5)<-c("CMA1","CMA5_1","CMA5_2","CMA5_3","CMA8_1","CMA8_2","CMA8_3")
corrplot(plot5,addCoef.col = "white",method="color",tl.col="black",mar=c(1,1,1,1),
         number.cex=1.1, tl.cex=1.1, cl.cex=1)

# YEAR: for people with 2+ prescs (if 1, then CMA8s all the same, and corr==1)
x2<-cor(left_join(CMA_Measures %>% filter(nchar(period)==4 & !is.na(CMA1)),
                  CSA_measures %>% mutate(period=as.character(year(PrescDate)))) %>%
         select(-ID,-period,-PrescDate),use="pairwise", method="spearman")
plot6<-x2[1:7,8:12]
write.csv(plot6,file="corrplot6.csv")
plot6<-as.matrix(plot6)
rownames(plot6)<-c("CMA1","CMA5_1","CMA5_2","CMA5_3","CMA8_1","CMA8_2","CMA8_3")
corrplot(plot6,addCoef.col = "white",method="color",tl.col="black",mar=c(1,1,1,1),
         number.cex=1.1, tl.cex=1.1, cl.cex=1)

# QUARTER: for people with 2+ prescs (if 1, then CMA8s all the same, and corr==1)
x3<-cor(left_join(CMA_Measures %>% filter(nchar(period)==6 & !is.na(CMA1)),
                  CSA_measures %>% 
                    mutate(period=paste(year(PrescDate),1+floor(month(PrescDate)/4),sep="_"))) %>%
          select(-ID,-period,-PrescDate),use="pairwise", method="spearman")
plot7<-x3[1:7,8:12]
write.csv(plot7,file="corrplot7.csv")
plot7<-as.matrix(plot7)
rownames(plot7)<-c("CMA1","CMA5_1","CMA5_2","CMA5_3","CMA8_1","CMA8_2","CMA8_3")
corrplot(plot7,addCoef.col = "white",method="color",tl.col="black",mar=c(1,1,1,1),
         number.cex=1.1, tl.cex=1.1, cl.cex=1)


                 
