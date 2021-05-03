### pre-amble
rm(list=ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(pracma)
set.seed(12345)

##########################################################################
###  Record Cleaning 
##########################################################################

prescribing <- read.csv("20190919 amended_pis.csv", stringsAsFactors=FALSE)
names(prescribing)[names(prescribing)=="Index10"]<-"ID"

### data cleaning
prescribing$PrescDate<-as.Date(prescribing$PrescDate)
prescribing$DispDate<-as.Date(prescribing$DispDate)
prescribing<-prescribing[which((format(prescribing$PrescDate,"%Y")!="2017" |
                                 format(prescribing$PrescDate,"%m")!="04") &
                                 as.numeric(format(prescribing$PrescDate,"%Y"))>=2009),]

### Load in the dataset which contains all dose information
prescribing<-left_join(prescribing,read.csv("Final_dose_instructions_and_variables2.csv", 
                                            stringsAsFactors=FALSE)) %>%
  mutate(dose = toupper(ePRNativeDoseInstructions),
         drugname = ifelse(PIApprovedName==PIPrescribableItemName,
                           PIPrescribableItemName,
                           paste(PIPrescribableItemName,PIApprovedName))) %>%
  select(-ePRNativeDoseInstructions,-PIApprovedName,-PIPrescribableItemName) %>%
  # Some have been deleted
  filter(substr(prescribing$dose,1,7)!="DELETED")

##########################################################################
###  Prescribing Keywords Dictionary
##########################################################################

### Generic Drugs
med_ICS<-c("BECLOMETASONE", "BUDESONIDE", "FLUTICASONE", 
           "MOMETASONE", "CICLESONIDE")
med_LABA<-c("BAMBUTEROL", "FORMOTEROL", "SALMETEROL")
med_list<-c("med_LABA","med_ICS")

brand_BAMBUTEROL<-c("BAMBEC")
brand_FORMOTEROL<-c("ATIMOS", "FORADIL", "OXIS", "FOSTAIR",
                    "SYMBICORT", "DUORESP SPIROMAX", "FOBUMIX", "FLUTIFORM")
brand_SALMETEROL<-c("NEOVENT", "SEREVENT", "SOLTEL","SERETIDE", "AIRFLUSAL", 
                    "SIRDUPLA", "SEREFLO", "ALOFLUTE", "COMBISAL", 
                    "FUSACOMB", "STALPEX")
brand_BECLOMETASONE<-c("BECLOMETHASONE","CLENIL", "QVAR", "KELHALE", "SOPROBEC", 
                       "BECODISKS", "PULVINAL BECLOMETASONE", "ASMABEC","FOSTAIR")
brand_BUDESONIDE<-c("BUDELIN", "PULMICORT","SYMBICORT", "DUORESP SPIROMAX", "FOBUMIX")
brand_MOMETASONE<-c("ASMANEX", "TWISTHALER")
brand_FLUTICASONE<-c("FLIXOTIDE","FLUTIFORM", "SERETIDE", "AIRFLUSAL", 
                     "SIRDUPLA", "SEREFLO", "ALOFLUTE", "COMBISAL", 
                     "FUSACOMB", "STALPEX","RELVAR ELLIPTA")
brand_CICLESONIDE<-c("ALVESCO")
brand_list<-substr(ls()[which(substr(ls(),1,5)=="brand")],7,30)

exclusion_brands<-c("NASONEX","FLIXONASE","ANORO ELLIPTA","SUMATRIPTAN","AVAMYS",
                    "RHINOCORT","NASOBEC","NASOFAN","RYNACROM","PIRINASE","SPIOLTO",
                    "DYMISTA","POLLENASE","VIVIDRIN","DUAKLIR","SEEBRI", "ULTIBRO", 
                    "PRED FORTE", "TRELEGY", "TRIMBOW", "BRALTUS", "RINATEC", 
                    "ENTOCORT", "BENACORT", "AIRCORT", "BUDEFLAM", "BUDENOFALK",
                    "CORTIMENT", "JORVEZA", "AZELASTINE", "CUTIVATE", "ELOCON", 
                    "NALCROM", "CATACROM", "ASPIRE", "OPTICROM", "OPTREX", "BECONASE",
                    "MURINE", "ACLIDINIUM", "GENUAIR", "OLADATEROL", "YANIMO")

exclusion_keywords<-c("NASAL","NOSE","NOSTRIL","NASULE", "HAYFEVER",
                      "EYE","EAR","DROP","TONGUE", "FOAM","ENEMA", 
                      "RECTAL","SUPPOSITOR", "CREAM",
                      "OINTMENT", "ULCER","SKIN","PATCH","APPLY")

### formulation keywords
formulation_keywords_sol<-c("SACHET", "RESPULE", "NEB", "VIAL", "AMPOULE")

### Doses
doses_mcg<-c("10000","5000","4000","2000","1000","500","400",
             "320","250","200","184","160","125","100","92","80","65","50")
doses_mg<-c("0.5","20","10","5","4","2","1")

### Pack Size Doses
pack_size_doses<-c(200,120,112,100,60,56,50,40,30,28,24,20,14,5)

### Dose Frequency Keywords
dose_freq_one<-c("ONCE","O-D","O\\.D")
dose_freq_two<-c("TWICE","TWO TIMES","2 TIMES","TD","TID","BID","BD",
                 "B-D","B\\.D")
dose_freq_four<-c("QID","FOUR TIMES","4 TIMES")
dose_freq_daily<-c("DAILY","EVERY DAY","EACH DAY",
                   "MANE","NOCTE","MORN","NIGHT","EVE","BEDTIME",
                   "A\\.M","P\\.M","AM","PM")

### Dose Quantity Keywords
dose_quant<-c("PUF","DOSE","CLICK","BLISTER", "TAB", "SACHET", "NEB", "RESP",
              "VIAL","CAP","INHALATION","AMPOULE","DOSE","ACTUATION","TWIST")

##########################################################################
###   Drug Classification
##########################################################################

presc_unique<-prescribing %>% 
  select(drugname,dose,ePRNDName,PIDrugFormulation) %>% 
  add_count(drugname,dose,ePRNDName,PIDrugFormulation) %>%
  distinct() %>%
  arrange(-n)

# Identifying and assigning drug inclusion keywords
presc_unique$key<-""
for (med in med_list) {
  filter<-vapply(get(paste0("med_",med)),
                 function(x) str_detect(presc_unique$drugname, x), 
                 logical(nrow(presc_unique)))
  presc_unique[,paste0("flag_",med)]<-apply(filter,1,sum)
  for (medx in get(paste0("med_",med))) {
    filter<-vapply(get(paste0("brand_",medx)),
                   function(x) str_detect(presc_unique$drugname, x), 
                   logical(nrow(presc_unique)))
    presc_unique[,paste0("flag_",med)]<-(presc_unique[,paste0("flag_",med)]+apply(filter,1,sum))>0
    temp<-str_detect(presc_unique$drugname,medx) | as.logical(apply(filter,1,sum))
    presc_unique[temp,"key"]<-paste(presc_unique$key[temp],medx,sep="_")
  }
}
presc_unique$key<-ifelse(str_detect(presc_unique$drugname,"RELVAR ELLIPTA") |
                           str_detect(presc_unique$drugname,"VILANTEROL"),
                         paste(presc_unique$key,"VILANTEROL",sep="_"),
                         presc_unique$key)
presc_unique$key<-ifelse(str_detect(presc_unique$drugname,"MOXISLYTE"),
                         "_",
                         presc_unique$key)
presc_unique$key<-str_replace(substr(presc_unique$key,2,30)," ","_")

### Drop if they aren't in any of these categories
presc_unique<-presc_unique %>%
  filter(flag_ICS==1) %>%
  mutate(drug_class = ifelse(flag_LABA==1, "ICS+LABA","ICS")

# drop sprays
presc_unique<-presc_unique %>% filter(PIDrugFormulation!="SPRAY")

# drop drops
presc_unique<-presc_unique %>% filter(PIDrugFormulation!="DROPS")

# Removing records matching exclusion brand names 
for (brand in exclusion_brands) {
  presc_unique<-presc_unique[which(str_detect(presc_unique$drugname,brand)==F),]
}

## Flagging brand names
presc_unique$brandname<-""
for (med in brand_list) {
  for (brand in get(paste0("brand_",med))) {
    presc_unique$brandname <- ifelse(str_detect(presc_unique$drugname, brand)==T,
                                     brand,
                                     presc_unique$brandname)
  }
}

presc_unique$brandname <-ifelse(presc_unique$brandname %in% c("","BECLOMETHASONE"),
                                "",
                                presc_unique$brandname)

## Getting rid of records matching ICS exclusion keywords
for (keyword in exclusion_keywords) {
  presc_unique<-presc_unique %>%
    filter(str_detect(dose,keyword)==F & 
             str_detect(drugname,keyword)==F &
             str_detect(ePRNDName,keyword)==F)
}

### Changing the drug class of solutions
filters_solution<-vapply(formulation_keywords_sol,
                         function(x) str_detect(paste0(presc_unique$dose,
                                                       presc_unique$drugname,
                                                       presc_unique$ePRNDName),x), 
                         logical(nrow(presc_unique)))
sum(presc_unique %>% filter(drug_class=="ICS") %>% select(n))
presc_unique<-presc_unique %>%
  mutate(solution=apply(filters_solution,1,sum),
         drug_class=ifelse(drug_class=="ICS" & (is.na(solution) | solution>0 | PIDrugFormulation %in% c("CAPS","SOL")),
                           "ICS_SOL",
                           drug_class)) %>%
  filter(drug_class!='ICS_SOL') %>%
  select(-solution)

prescribing_asthma_ICS<-inner_join(prescribing,presc_unique) %>%
  dplyr::select(ID,key,PrescDate,drugname,ePRNDName,PIItemStrength.UOM,prescribed_quantity,
                dispensed_quantity,drug_class,ndx,brandname,disp_sub,dose,PIDrugFormulation) %>%
  ungroup
rm(presc_unique)

##########################################################################
###  ICS daily medicine amount used calculation
##########################################################################

# Dose frequency - as required/needed coded as minimum
filters_freq_one<-vapply(dose_freq_one,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                         logical(nrow(prescribing_asthma_ICS)))
filters_freq_two<-vapply(dose_freq_two,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                         logical(nrow(prescribing_asthma_ICS)))
filters_freq_four<-vapply(dose_freq_four,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                          logical(nrow(prescribing_asthma_ICS)))
filters_freq_daily<-vapply(dose_freq_daily,function(x) str_detect(prescribing_asthma_ICS$dose,x),
                           logical(nrow(prescribing_asthma_ICS)))
prescribing_asthma_ICS$freq_dose<-ifelse(apply(filters_freq_one,1,sum)>=1,1,NA)
prescribing_asthma_ICS$freq_dose<-ifelse(is.na(prescribing_asthma_ICS$freq_dose) &
                                           (apply(filters_freq_two,1,sum)>=1 |
                                              ((str_detect(prescribing_asthma_ICS$dose, "MORN") |
                                                  str_detect(prescribing_asthma_ICS$dose, "AM") |
                                                  str_detect(prescribing_asthma_ICS$dose, "A.M") |
                                                  str_detect(prescribing_asthma_ICS$dose, "MANE")) &
                                                 (str_detect(prescribing_asthma_ICS$dose, "EVE") |
                                                    str_detect(prescribing_asthma_ICS$dose, "NIGHT") |
                                                    str_detect(prescribing_asthma_ICS$dose, "BEDTIME") |
                                                    str_detect(prescribing_asthma_ICS$dose, "PM") |
                                                    str_detect(prescribing_asthma_ICS$dose, "P.M") |
                                                    str_detect(prescribing_asthma_ICS$dose, "NOCTE")))),
                                         2,prescribing_asthma_ICS$freq_dose)
prescribing_asthma_ICS$freq_dose<-ifelse(apply(filters_freq_four,1,sum)>=1,4,prescribing_asthma_ICS$freq_dose)
prescribing_asthma_ICS$freq_dose<-ifelse(is.na(prescribing_asthma_ICS$freq_dose),
                                         ifelse(apply(filters_freq_daily,1,sum)>=1,
                                                1,
                                                prescribing_asthma_ICS$freq_dose),
                                         prescribing_asthma_ICS$freq_dose)
table(prescribing_asthma_ICS$freq_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)
# table(prescribing_asthma_ICS$freq_dose, prescribing_asthma_ICS$key, useNA = "ifany")
sum(is.na(prescribing_asthma_ICS$freq_dose) & prescribing_asthma_ICS$key %in% c("CICLESONIDE","FLUTICASONE_VILANTEROL"))
sum(is.na(prescribing_asthma_ICS$freq_dose) & !prescribing_asthma_ICS$key %in% c("CICLESONIDE","FLUTICASONE_VILANTEROL"))
prescribing_asthma_ICS$freq_dose<-ifelse(is.na(prescribing_asthma_ICS$freq_dose),
                                         ifelse(prescribing_asthma_ICS$key %in% c("CICLESONIDE","FLUTICASONE_VILANTEROL"),
                                                1,2),
                                         prescribing_asthma_ICS$freq_dose)
table(prescribing_asthma_ICS$freq_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)


## Dose Quantity
prescribing_asthma_ICS$daily_dose<-NA
dose_quantities<-c("1","2","3","4","ONE","TWO","THREE","FOUR")
dose_quantities_num<-rep(c(1,2,3,4),2)
for (k in 1:8) {
  prescribing_asthma_ICS$daily_dose<-ifelse(str_detect(prescribing_asthma_ICS$dose,
                                                       paste0("TAKE ",dose_quantities[k]))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0("INHALE ",dose_quantities[k]))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," AT "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," TO BE TAKEN "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k],"PUF"))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," PUF"))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," P "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k],"P "))==TRUE |
                                              str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," DAILY"))==TRUE,
                                            dose_quantities_num[k],
                                            prescribing_asthma_ICS$daily_dose)
  for (keyword in dose_quant) {
    prescribing_asthma_ICS$daily_dose<-ifelse(str_detect(prescribing_asthma_ICS$dose,
                                                         paste0(dose_quantities[k]," ",keyword))==TRUE,
                                              dose_quantities_num[k],
                                              prescribing_asthma_ICS$daily_dose)
  }
}
table(prescribing_asthma_ICS$daily_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)
# table(prescribing_asthma_ICS$daily_dose, prescribing_asthma_ICS$key, useNA = "ifany")
list<-c("BUDESONIDE","CICLESONIDE","FLUTICASONE_SALMETEROL","FLUTICASONE_VILANTEROL","MOMETASONE")
sum(is.na(prescribing_asthma_ICS$daily_dose) & prescribing_asthma_ICS$key %in% list)
sum(is.na(prescribing_asthma_ICS$daily_dose) & !prescribing_asthma_ICS$key %in% list)
prescribing_asthma_ICS$daily_dose<-ifelse(is.na(prescribing_asthma_ICS$daily_dose),
                                          ifelse(prescribing_asthma_ICS$key %in% list,
                                                 1,
                                                 2),
                                          prescribing_asthma_ICS$daily_dose)
table(prescribing_asthma_ICS$daily_dose, useNA = "ifany")*100/nrow(prescribing_asthma_ICS)

## How much do they take per day
prescribing_asthma_ICS$daily_dose_units<-prescribing_asthma_ICS$freq_dose*prescribing_asthma_ICS$daily_dose

##########################################################################
###  Adherence: Part 1
##########################################################################

prescribing_asthma_ICS$qty<-ifelse(!is.na(prescribing_asthma_ICS$dispensed_quantity),
                                   ifelse(prescribing_asthma_ICS$dispensed_quantity<1,
                                          1,
                                          prescribing_asthma_ICS$dispensed_quantity),
                                   ifelse(prescribing_asthma_ICS$prescribed_quantity<1,
                                          1,
                                          prescribing_asthma_ICS$prescribed_quantity))

round(table(prescribing_asthma_ICS$qty)*100/nrow(prescribing_asthma_ICS),1)
#table(prescribing_asthma_ICS$qty,prescribing_asthma_ICS$drug_class)

# doses per unit extraction
prescribing_asthma_ICS$doses_per_pack<-ifelse(prescribing_asthma_ICS$qty>=14,prescribing_asthma_ICS$qty,NA)
for (dose in pack_size_doses) {
  prescribing_asthma_ICS$doses_per_pack<-ifelse(is.na(prescribing_asthma_ICS$doses_per_pack) &
                                                  (str_detect(prescribing_asthma_ICS$ePRNDName, paste0(dose," DOSE"))==T |
                                                     str_detect(prescribing_asthma_ICS$ePRNDName, paste0(dose,"DOSE"))==T |
                                                     str_detect(prescribing_asthma_ICS$ePRNDName, paste0(dose,"-DOSE"))==T |
                                                     str_detect(prescribing_asthma_ICS$ePRNDName, paste0(dose," X "))==T) , 
                                                dose, 
                                                prescribing_asthma_ICS$doses_per_pack)
}

sum(!is.na(prescribing_asthma_ICS$doses_per_pack))*100/nrow(prescribing_asthma_ICS) # 15.2

##########################################################################
###  Medication Strength
##########################################################################

sum(prescribing_asthma_ICS$PIItemStrength.UOM=="")*100/nrow(prescribing_asthma_ICS)

## Look for the values followed by keywords
prescribing_asthma_ICS$strength<-NA
for (k in doses_mcg) {
  prescribing_asthma_ICS$strength<-ifelse(is.na(prescribing_asthma_ICS$strength) &
                                            ((str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "/"))==T &
                                                prescribing_asthma_ICS$drug_class=="ICS+LABA") |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MCG"))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MCG", sep=" "))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MICROGRAM", sep=" "))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MICROGRAM"))==T),
                                          as.numeric(k),
                                          prescribing_asthma_ICS$strength)
}
for (k in doses_mg) {
  prescribing_asthma_ICS$strength<-ifelse(is.na(prescribing_asthma_ICS$strength) &
                                            (str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MG"))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MG", sep=" "))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k, "MILLIGRAM"))==T |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste(k, "MILLIGRAM", sep=" "))==T),
                                          as.numeric(k)*1000,
                                          prescribing_asthma_ICS$strength)
}
sum(is.na(prescribing_asthma_ICS$strength))*100/nrow(prescribing_asthma_ICS)
#View(prescribing_asthma_ICS %>% filter(is.na(strength)) %>% count(ePRNDName) %>% arrange(-n))

# mostly accuhalers and evohalers etc
for (k in doses_mcg) {
  prescribing_asthma_ICS$strength<-ifelse(is.na(prescribing_asthma_ICS$strength) &
                                            (str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," ACCUHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," CLICKHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," EVOHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0(k," TURBOHALER")) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("QVAR ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("SERETIDE ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("INHAL ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("EVOHALER ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("SERETIDE MDI ",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("SERETIDE",k)) |
                                               str_detect(prescribing_asthma_ICS$ePRNDName, paste0("ALVESCO ",k))),
                                          as.numeric(k),
                                          prescribing_asthma_ICS$strength)
}
sum(is.na(prescribing_asthma_ICS$strength))*100/nrow(prescribing_asthma_ICS)

# from manual comparison against our appendix dose strength table
# a lot more need removing than in SIVEII because of the dose matching problem
nrow(prescribing_asthma_ICS)
prescribing_asthma_ICS<-prescribing_asthma_ICS %>%
  filter(!(key=="BECLOMETASONE" & !is.na(strength) & !strength %in% c(50,100,200,250,400))) %>%
  filter(!(key=="BECLOMETASONE_FORMOTEROL" & !is.na(strength) & !strength %in% c(100,200))) %>%
  filter(!(key=="BUDESONIDE" & !is.na(strength) & !strength %in% c(100,200,250,400))) %>%
  filter(!(key=="BUDESONIDE_FORMOTEROL" & !is.na(strength) & !strength %in% c(50,100,160,200,320,400))) %>%
  filter(!(key=="FLUTICASONE" & !is.na(strength) & !strength %in% c(50,100,125,250,500))) %>%
  filter(!(key=="FLUTICASONE_FORMOTEROL" & !is.na(strength) & !strength %in% c(50,125,250))) %>%
  filter(!(key=="FLUTICASONE_SALMETEROL" & !is.na(strength) & !strength %in% c(50,100,125,250,500))) %>%
  filter(!(key=="FLUTICASONE_VILANTEROL" & !is.na(strength) & !strength %in% c(92,184))) %>%
  filter(!(key=="MOMETASONE" & !is.na(strength) & !strength %in% c(200,400))) 
nrow(prescribing_asthma_ICS)
length(unique(prescribing_asthma_ICS$ID))

mode_strength_key<-prescribing_asthma_ICS %>%
  filter(!is.na(strength)) %>%
  dplyr::count(key,strength) %>%
  arrange(key,n) %>%
  group_by(key) %>%
  dplyr::slice(n()) %>%
  dplyr::rename(mode_strength = strength) %>%
  select(-n)
prescribing_asthma_ICS<-left_join(prescribing_asthma_ICS,mode_strength_key) %>%
  mutate(strength = ifelse(is.na(strength),mode_strength,strength)) %>%
  select(-mode_strength)
sum(is.na(prescribing_asthma_ICS$strength))
rm(mode_strength_key)


##########################################################################
###  Adherence: Part 2
##########################################################################

# doses per unit: modal values by brandname, key, and strength
temp_mode_doses_per_pack<-prescribing_asthma_ICS %>%
  filter(!is.na(doses_per_pack)) %>%
  add_count(key, strength, brandname, name="total") %>%
  add_count(key, strength, brandname, doses_per_pack, name = "numer") %>%
  group_by(key, strength, brandname) %>%
  mutate(mode_doses_per_pack = Mode(doses_per_pack),
         percentage = round(100*numer/total,3)) %>%
  select(key, mode_doses_per_pack, strength,brandname,percentage) %>%
  arrange(-percentage) %>%
  dplyr::slice(1) 

prescribing_asthma_ICS<-left_join(prescribing_asthma_ICS,temp_mode_doses_per_pack) %>%
  mutate(flag = ifelse(is.na(doses_per_pack) & !is.na(mode_doses_per_pack),1,0),
         doses_per_pack=ifelse(is.na(doses_per_pack),mode_doses_per_pack,doses_per_pack)) %>%
  select(-mode_doses_per_pack)

imputed<-prescribing_asthma_ICS %>% filter(flag==1)
summary(imputed$percentage)
sum(imputed$percentage<60)
sum(imputed$percentage<60)*100/nrow(imputed)
imputed %>% filter(percentage<60) %>% count(key,brandname,strength)
rm(imputed)


sum(!is.na(prescribing_asthma_ICS$doses_per_pack))*100/nrow(prescribing_asthma_ICS) # 98.8

# impute missing values from medicines.org.uk
prescribing_asthma_ICS$doses_per_pack<-ifelse(is.na(prescribing_asthma_ICS$doses_per_pack) & 
                                                prescribing_asthma_ICS$key=="FLUTICASONE_FORMOTEROL",
                                              120,prescribing_asthma_ICS$doses_per_pack)
prescribing_asthma_ICS$doses_per_pack<-ifelse(is.na(prescribing_asthma_ICS$doses_per_pack) & 
                                                prescribing_asthma_ICS$key=="FLUTICASONE_SALMETEROL",
                                              ifelse(prescribing_asthma_ICS$strength==500,
                                                     60,120),
                                              prescribing_asthma_ICS$doses_per_pack)
prescribing_asthma_ICS$doses_per_pack<-ifelse(is.na(prescribing_asthma_ICS$doses_per_pack) & 
                                                prescribing_asthma_ICS$key=="BECLOMETASONE_FORMOTEROL",
                                              120,prescribing_asthma_ICS$doses_per_pack)
prescribing_asthma_ICS$doses_per_pack<-ifelse(is.na(prescribing_asthma_ICS$doses_per_pack) & 
                                                prescribing_asthma_ICS$key=="BECLOMETASONE",
                                              100,prescribing_asthma_ICS$doses_per_pack)
prescribing_asthma_ICS$doses_per_pack<-ifelse(is.na(prescribing_asthma_ICS$doses_per_pack) & 
                                                prescribing_asthma_ICS$key=="BUDESONIDE_FORMOTEROL",
                                              ifelse(prescribing_asthma_ICS$strength==400,
                                                     60,120),
                                              prescribing_asthma_ICS$doses_per_pack)
sum(is.na(prescribing_asthma_ICS$doses_per_pack))==0

prescribing_asthma_ICS$quantity<-ifelse(prescribing_asthma_ICS$qty<=15,
                                        prescribing_asthma_ICS$qty*prescribing_asthma_ICS$doses_per_pack,
                                        prescribing_asthma_ICS$doses_per_pack)

## Save File to Use for Quantity coding and EHR prescriptions analyses
save(prescribing_asthma_ICS,file="prescriptions_adherence_analysis.RData")