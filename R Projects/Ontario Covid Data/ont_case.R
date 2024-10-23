#library#
library(lubridate) 
library(slider)
library(tidyverse)
library(zoo)

##### Load in Data #####


setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))          #this should be the folder where your datafile and script are stored

d <- read.csv('covidtesting.csv')     #open up this file to make sure it came in okay

#d.pop <- read.csv('pop_age_prov.csv')


#head(d)                                 #a quick glance at the first few rows

#summary(d1)



##### Mutate Data #####

changer <- function(x, na.rm = FALSE) (x - lag(x, na.rm = na.rm))

d1 <- d %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Date = ymd(Reported.Date), Hospitalized = Number.of.patients.hospitalized.with.COVID.19, ICU=Number.of.patients.in.ICU.due.to.COVID.19) %>%
  arrange(Date) %>%
  select(Date,Total.Cases, Confirmed.Positive, Resolved,Deaths,Hospitalized,ICU) %>%
  mutate_if(is.numeric, list(change= changer))


d.curr <- d1 %>%
  select(Date | !ends_with("change")) %>%
  pivot_longer(-Date, names_to='type', values_to='val') %>%
  drop_na() 

d.curr1 <- filter(d.curr, Date >= ymd('2020-11-01'))
p.change <- ggplot(d.curr1, aes(x=Date, y=val, colour=type)) +
  theme_bw()+
  theme(legend.position = "none") +
  facet_wrap(~type, scales = "free_y") +
  geom_line(size=1) +
  scale_color_brewer(palette = 'Dark2')
p.change



sev_avg <- function(x, na.rm=FALSE) rollmean(x, k=7, fill=NA)

d.change <- d1 %>%
  select(Date | ends_with("change")) %>%
  mutate_if(is.numeric, list(sev.d = sev_avg)) %>%
  pivot_longer(-Date, names_to='type', values_to='val') %>%
  drop_na() %>%
  separate(type, c('faceter'), sep='_', extra = 'drop', remove=F)

d.change1<- filter(d.change, Date >= ymd('2021-06-18'))
p.change <- ggplot(d.change1, aes(x=Date, y=val, colour=type)) +
  theme_bw()+
  theme(legend.position = "none") +
  facet_wrap(~faceter,scales = "free_y") +
  geom_line(size=1) +
  scale_color_brewer(palette = 'Paired')
p.change


d.delta <- d %>%
  mutate(Date = ymd(Reported.Date)) %>%
  select(Date | starts_with("Total_Lineage")) %>%
  mutate_if(is.numeric, list(change= changer))

