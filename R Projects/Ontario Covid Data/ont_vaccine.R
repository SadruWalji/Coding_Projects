#library#
library(lubridate) 
library(slider)
library(tidyverse)
library(zoo)


##### Load in Data #####


setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))          #this should be the folder where your datafile and script are stored

d <- read.csv('vaccine_doses.csv')     #open up this file to make sure it came in okay

d.pop <- read.csv('pop_age_prov.csv')


#head(d)                                 #a quick glance at the first few rows

#summary(d1)



##### Mutate Data #####


d.pop1 <- d.pop %>%
  subset(GEO=='Ontario') %>%
  select(GEO,VALUE,Age.group) %>%
  pivot_wider(names_from = Age.group, values_from= VALUE) %>%
  mutate(prename = GEO, pop= (`All ages` - `0 to 4 years` - `5 to 9 years` - (`10 to 14 years`) - (`15 to 19 years`)/2)) %>% #
  select(pop)


d1 <- d %>%
  mutate_if(is.factor, as.character) %>%
  mutate(Date = ymd(report_date)) %>%
  arrange(Date) %>%
  mutate(second=total_individuals_fully_vaccinated- lag(total_individuals_fully_vaccinated), 
         first=previous_day_total_doses_administered - second,
         sev_day_avg=round(slide_index_dbl(previous_day_total_doses_administered, Date, sum, .before = days(6))/7),
         first_7_avg = round(slide_index_dbl(first, Date, sum, .before = days(6))/7),
         second_7_avg = round(slide_index_dbl(second, Date, sum, .before = days(6))/7),
         sev_daily_tot = total_doses_administered - lag(total_doses_administered)) %>%
  select(Date, total_doses_administered, previous_day_total_doses_administered,  first, second, sev_day_avg, first_7_avg, second_7_avg, sev_daily_tot)


d.rates <- d1 %>%
  select(-c(total_doses_administered,previous_day_total_doses_administered)) %>%
  drop_na() %>%
  pivot_longer(-Date, names_to = 'type', values_to='val' )
  

p.rates <- ggplot(d.rates, aes(x=Date, y=val, colour=type)) +
  theme_bw()+
  theme(legend.justification = c(0,1), 
        legend.position = c(0.001,0.998),
        legend.box.background = element_rect(color="black", size=1)) +
  geom_line(size=1) +
  scale_color_brewer(palette = 'Paired', 
                     labels = c("First Dose", "First Dose (7d avg)", "Second Dose", "Second Dose (7d avg)", "Total Administered", "Total Administered (7d avg)")) +
  coord_cartesian(xlim=c(ymd('2021-01-13'), max(d.rates$Date)))  +
  labs(colour = 'Dose', y='Daily Stabby Stabs')
p.rates

ggsave('Dose Rate Ont.png', p.rates, dpi=600)






d2 <- merge(d1,d.pop1)


d.randanalysis <- d2 %>%
  drop_na() %>%
  mutate(Days_till_deadline = as.numeric(difftime(ymd('2021-10-01'),Date, units='days')),
         first_rate = 100*cumsum(first)/pop,
         second_rate = 100*cumsum(second)/pop 
         ) 




d.randy <- d.randanalysis %>%
  select(Date, first_rate, second_rate) %>%
  pivot_longer(-Date, names_to='type', values_to='val') %>%
  drop_na()
  
  

p.randy <- ggplot(d.randy, aes(x=Date, y=val, colour=type)) +
  theme_bw()+
  geom_line(size=1) +
  scale_color_brewer(palette = 'Dark2') +
  coord_cartesian(xlim=c(ymd('2021-01-13'), max(d.rates$Date))) +
  labs(colour="# of times\nstabbed", y='Adults Stabbed (%)')
p.randy
  
  
d.latest0 <- d.randanalysis %>%
  mutate(f_r_change = round(first_rate - lag(first_rate), digits=2), 
         s_r_change= round(second_rate - lag(second_rate), digits=2),
         d_f_t_80 = round((pop*(80-first_rate))/(100*first_7_avg)),
         d_s_t_70 = round((pop*(70-second_rate))/(100*second_7_avg)),
         d_s_t_75 = round((pop*(75-second_rate))/(100*second_7_avg))
                        )

d.latest <- d.latest0 %>%
    slice_max(Date) %>%
    select(-c(2,3,8:9))

d.projected <- d.latest0 %>%
  select(Date, d_f_t_80, d_s_t_70, d_s_t_75) %>%
  pivot_longer(-Date, names_to='type', values_to='val') %>%
  filter(Date > ymd('2021-06-07')) %>%
  drop_na()

p.projected <- ggplot(d.projected, aes(x=Date, y=val, colour=type)) +
  theme_bw()+
  geom_line(size=1) +
  scale_color_brewer(palette = 'Dark2', 
                     labels = c('1st 80%', '2nd 70%', "2nd 75%")) +
  labs(colour="Population \nSecond Dose\nTarget", y='Days Until Target')
p.projected


  