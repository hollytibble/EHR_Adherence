### pre-amble
rm(list=ls())
library(stringr)
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(zoo)
library(data.table)
library(pracma)
library(corrplot)
library(svMisc)
set.seed(12345)

### Load File
load("prescriptions_adherence_analysis.RData")

# This data (prescribing_asthma_ICS) has the following columns:
# ID: Personal identifier
# PrescDate: date of prescription
# quantity: the number of doses contained in the inhaler, such as 120 puffs
# daily_dose_units: the number of doses prescribed to be taken daily, such as 2 puffs twice a day = 4
# drug_class: asthma medication category (ICS, ICS+LABA, and LTRA)

df<-prescribing_asthma_ICS %>%
  mutate(supply_duration = quantity/daily_dose_units) %>%
  group_by(ID) %>%
  arrange(ID, PrescDate) %>%
  add_count(ID) %>%
  mutate(flag=sum(drug_class=="LTRA")/n) %>%
  filter(drug_class!="LTRA" | flag>=0.9) %>%
  mutate(interval_duration = as.numeric(lead(PrescDate)-PrescDate)) %>%
  select(ID,PrescDate,supply_duration,interval_duration) 
summary(df$supply_duration)
summary(df$interval_duration)

df<-df %>%
  group_by(ID) %>%
  # here we are just combining prescriptions on the same day into one - assuming they should not be taken at the same time. 
  mutate(supply_duration=ifelse(row_number()!=1 & !is.na(lag(interval_duration)) & lag(interval_duration==0),
                                supply_duration+lag(supply_duration),
                                supply_duration)) %>%
  filter(is.na(interval_duration) | interval_duration!=0) %>%
  mutate(refill=row_number(),
         total_refills=max(refill))

freqs<-df %>% count(ID, name="prescs") %>% 
  ungroup %>% count(prescs, name='freq') %>%
  mutate(perc = freq*100/length(unique(df$ID)))

CSA_measures<-df %>%
  group_by(ID) %>%
  arrange(ID,PrescDate) %>%
  mutate(CSA=supply_duration/interval_duration,
         CSG=pmax(0,((interval_duration-supply_duration)/interval_duration)),
         CSA_cum=cumsum(CSA),
         CSG_cum=cumsum(CSG),
         CSA_3 = (CSA_cum-lag(CSA_cum,3))/3,
         CSA_5 = (CSA_cum-lag(CSA_cum,5))/5,
         CSA_10=(CSA_cum-lag(CSA_cum,10))/10) %>%
  filter(!is.na(interval_duration)) %>%
  select(ID,PrescDate,CSA,CSG,CSA_3,CSA_5,CSA_10)

df<-df %>%
  group_by(ID) %>%
  mutate(excess = pmax(0,supply_duration-interval_duration),
         gap=pmax(0,interval_duration-supply_duration),
         cum_excess=cumsum(excess),
         cum_gap=cumsum(gap*(cum_excess>0 & gap>0)), # only want the cumulative of the gap where there is usable excess
         leftovers = ifelse(cum_excess>0 & 
                              gap>0 &
                              cum_excess>=cum_gap,
                            -1*gap,
                            excess),
         last=PrescDate) %>%
  select(ID,refill,PrescDate,supply_duration,leftovers,interval_duration,last)

frame<-df %>% 
  group_by(ID) %>%
  arrange(ID,PrescDate) %>%
  slice(1) %>% 
  mutate(startdate=PrescDate,
         start=1,
         end=as.numeric(as.Date("2017-03-31")-PrescDate)) %>%
  select(ID,start,end,startdate)
rm(prescribing_asthma_ICS)

# At this point, before we expand to the daily level, we are going to calculate adherence by person to save memory
CMA_Measures<-NULL
CMA8<-NULL
names<-unique(df$ID)
for (i in 1:length(names)) {
  name<-names[i]
  framex<-frame %>% filter(ID==name)
  framex<-setDT(framex)[,list(ID=ID,startdate=startdate,PrescDate=seq(start,end+1)), by=1:nrow(framex)]
  framex$PrescDate<-framex$startdate+framex$PrescDate-1
  framex$startdate<-framex$nrow<-NULL
  framex$ID<-as.character(framex$ID)
  
  dfx<-left_join(framex,df %>% filter(ID==name), 
                 by=c("ID","PrescDate")) %>%
    arrange(PrescDate) %>%
    mutate(refill=na.locf(refill),
           last=na.locf(last),
           days_since_last = PrescDate-last,
           leftovers = ifelse(is.na(leftovers),0,leftovers))
  rm(framex)
  
  # Split into periods of years and quarters, and get rid of full periods which pass before first prescription
  dfx_y<-dfx %>%
    mutate(period=as.character(year(PrescDate))) %>%
    group_by(period) %>%
    mutate(temp = sum(refill)) %>%
    filter(temp!=0) %>%
    ungroup %>%
    select(-temp)
  dfx_q<-dfx_y %>%
    mutate(quarter=1+floor(month(PrescDate)/4),
           period = paste(period,quarter,sep="_")) %>%
    group_by(period) %>%
    mutate(temp = sum(refill)) %>%
    filter(temp!=0) %>%
    ungroup %>%
    select(-temp,-quarter)
  
  dfz<-bind_rows(dfx %>% mutate(period="all"),
                 dfx_y,dfx_q)
  
  # for CMA8, I will need information about the last supply to carryover when we are looking at subperiods
  # So I will work this out before i create the periods. 
  temp_CMA8<-dfx %>%
    mutate(duration_locf=na.locf(supply_duration)) 
  temp_CMA8<-bind_rows(temp_CMA8 %>% mutate(period="all"),
                       temp_CMA8 %>% mutate(period=as.character(year(PrescDate))),
                       temp_CMA8 %>% mutate(quarter = 1+floor(month(PrescDate)/4),
                                            period=paste(year(PrescDate),quarter,sep="_")) %>%
                         select(-quarter))
  rm(dfx,dfx_q,dfx_y)
  
  # CMA Measures 1 and 5
  periods<-unique(dfz$period)
  for (per in periods) {
    tempx<-dfz %>% filter(period==per) 
    if(sum(!is.na(tempx$supply_duration))<2) {  # these measures need there to be two prescriptions in each period
      tempx<-tempx %>%
        select(ID,period) %>%
        mutate(CMA1=NA,CMA5_1=NA,CMA5_2=NA,CMA5_3=NA) %>%
        slice(1)
    } else {
      tempx<-tempx %>%
        # they don't use the bit before the first prescription, so we cut that off, as well as the time after the last one
        group_by(refill) %>%
        mutate(temp=sum(!is.na(supply_duration))) %>%
        ungroup %>% 
        filter(temp>0 & refill!=max(refill)) %>%
        mutate(supply_all_over = ifelse(refill==min(refill),
                                        supply_duration,
                                        supply_duration+lag(cumsum(leftovers))),
               duration_locf = na.locf(supply_duration),
               all_locf=na.locf(supply_all_over),
               supply1=ifelse(!is.na(supply_duration),1,pmax(0,(duration_locf-days_since_last)>=1)), # no carryover
               supply2=ifelse(!is.na(supply_duration),1,pmax(0,(pmin(duration_locf*2,all_locf)-days_since_last)>=1)), # capped carryover
               supply3=ifelse(!is.na(supply_duration),1,pmax(0,(all_locf-days_since_last)>=1)),    #all carryover
               window_length = as.numeric(max(PrescDate)-min(PrescDate)+1),
               CMA1 = sum(supply_duration, na.rm = T)/window_length,
               CMA5_1=sum(supply1)/window_length,
               CMA5_2=sum(supply2)/window_length,
               CMA5_3=sum(supply3)/window_length) %>%
        select(ID,period,CMA1,CMA5_1, CMA5_2,CMA5_3) %>%
        slice(1)
    }
    CMA_Measures<-bind_rows(CMA_Measures,tempx)
  }
  
  # CMA Measure 8 has a different window and needs to be calculated alone
  periods<-unique(temp_CMA8$period)
  for (per in periods) {
    tempx<-temp_CMA8 %>% filter(period==per) %>%
      mutate(duration_locf = ifelse(PrescDate==min(PrescDate),duration_locf,NA), # keep the value only for the first date in the period
             # the date when the leftover supply at the start of a period is exhausted (lagged start)
             start = as.Date(ifelse(is.na(first(supply_duration)),
                                    first(PrescDate)+first(duration_locf)-first(days_since_last),
                                    first(PrescDate))),
             # identify any medication accrued in the lagged start window
             exclusion_supply_t = ifelse(PrescDate<start & !is.na(supply_duration),supply_duration,0),
             # sum the banked supply to add on to the amount available at the lagged start
             exclusion_supply = sum(exclusion_supply_t)) 
    # if they had enough medication at the start of the period to cover the whole thing, skip it and say 100% adherence
    if (first(tempx$start)>last(tempx$PrescDate)) {
      tempx<-tempx %>%
        select(ID,period) %>%
        mutate(CMA8_1=1,CMA8_2=1,CMA8_3=1) %>%
        slice(1)
    } else {
      tempx<-tempx %>%
        # exclude time before lagged window
        filter(PrescDate>=start) %>% 
        # add quantity of excluded supply to the start
        mutate(supply_duration = ifelse(is.na(supply_duration) & PrescDate==min(PrescDate),exclusion_supply,supply_duration), # else na.locf will fail
               supply_all_over = ifelse(refill==min(refill),
                                        supply_duration,
                                        supply_duration+lag(cumsum(leftovers))),
               duration_locf=na.locf(supply_duration),
               all_locf=na.locf(supply_all_over),
               supply1=ifelse(!is.na(supply_duration) & supply_duration>0,1,pmax(0,(duration_locf-days_since_last)>=1)), # no carryover
               supply2=ifelse(!is.na(supply_duration) & supply_duration>0,1,pmax(0,(pmin(duration_locf*2,all_locf)-days_since_last)>=1)), # capped
               supply3 = ifelse(!is.na(supply_duration) & supply_duration>0,1,pmax(0,(all_locf-days_since_last)>=1)),
               window_length=as.numeric(max(PrescDate)-min(PrescDate)+1),
               CMA8_1 = sum(supply1)/window_length,
               CMA8_2 = sum(supply2)/window_length,
               CMA8_3 = sum(supply3)/window_length) %>%
        select(ID,period,CMA8_1,CMA8_2,CMA8_3) %>%
        slice(1)
    }
    CMA8<-bind_rows(CMA8,tempx)
  }
  progress(i,max.value = length(names))
}

CMA_Measures<-left_join(CMA8,CMA_Measures)

save.image("Adherence.RData")
