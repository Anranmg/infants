# load data
data=read.csv('time.csv')

# select non-missing value  1491
time=data%>%
  filter(sl3bedtime1 %in% seq(1,12) & sl3morning1 %in% seq(1,12) &
         sl3bedtime2 %in% seq(0,60) & sl3morning2 %in% seq(0,60) &
         sl3bedtime3 %in% c("1:00 AM", "2:00 PM") & sl3morning3 %in% c("1:00 AM", "2:00 PM"))%>%
  mutate(sl3bedtime1=as.numeric(as.character(sl3bedtime1)), 
         sl3morning1=as.numeric(as.character(sl3morning1)))%>% # convert time to integer
  mutate(bedhr=ifelse(sl3bedtime3=='1:00 AM', sl3bedtime1, sl3bedtime1+12), 
         bedhr=ifelse(sl3bedtime3=='1:00 AM' & sl3bedtime1==12, 0, bedhr),   # bed at 12.00 am to 0.00 a.m.
         morhr=ifelse(sl3morning3=='1:00 AM', sl3morning1, sl3morning1+12),
         morhr=ifelse(sl3morning3=='2:00 PM' & sl3morning1==12, morhr-12, morhr), # wake at 12.00 p.m. to 12.00
         morhr=ifelse(sl3morning3=='1:00 AM' & sl3bedtime3=='2:00 PM', morhr+24, morhr),
         morhr=ifelse(sl3morning3=='2:00 PM' & sl3bedtime3=='2:00 PM' & morhr==12, morhr+24, morhr))

# combine hour, min and am/pm to military time
library(lubridate)
hm_hr=function(x){
  x%>%as.difftime()%>%as.numeric('hours')%>%round(digits = 2)
}

temp=
  time%>%
  mutate(spid, bed=hm(paste(bedhr, sl3bedtime2)), mor=hm(paste(morhr, sl3morning2)), 
         duration=mor-bed, midpoint=mor+bed)%>%
  mutate(bed=hm_hr(bed), mor=ifelse(hm_hr(mor)>24,hm_hr(mor)-24, hm_hr(mor)), 
         duration=hm_hr(duration), midpoint=hm_hr(midpoint)/2)%>%
  mutate(midpoint=ifelse(midpoint>24, midpoint-24, midpoint))%>%
  select(-bedhr,-morhr)%>%
  arrange(duration)

write.csv(temp, 'hr.csv')

# distribution of midpoint and time
temp_usual=temp%>%filter(duration>0)

hist(temp_usual$midpoint, main='histogram of midpoint (25% - 75%)', breaks=60)
abline( v=quantile(temp_usual$midpoint, c(.25,.75)), col='red', lty=5)
text(10, 150, '25% - 75% is [2.0, 3.5]')

hist(temp_usual$duration, main='histogram of duration',breaks = 100)
abline( v=quantile(temp_usual$duration, c(.25,.75)), col='red', lty=5)
text(4, 150, '25% - 75% is [7.5, 9.5]')














































