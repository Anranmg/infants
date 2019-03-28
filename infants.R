library('dplyr')
library('ggplot2')

data=readxl::read_excel('infants.xlsx')

# check any duplicate 
data=data%>%
  filter(ID!=' ')

data.n=data
names(data.n)[48]='BWSDS'
names(data.n)[50]='blsds'
names(data.n)[4]='b_mother_height'
names(data.n)[5]='b_mother_weight_pre'
names(data.n)[6]='b_mother_weight_curr'

# !!!!!!!!!!!!!!!!!!! translate usually not to no !!!!!!!!!!!!!!!!!!!
data.n[data.n$ID=='38745683',74]=0
data.n[data.n$ID=='38745683',110]=1

# table for missing values
table.miss=function(x, y){
  # select relevant columns
  baby=data.n%>%
    select(ID,matches(x))
  
  # count # of missings
  baby$miss.n=apply(baby, 1, function(x) sum(is.na(x)))
  
  # return dataframe
  print(paste('table for',y))
  baby%>%
    filter(miss.n>0)%>%
    arrange(-miss.n)%>%
    knitr::kable()
}

# baby's columns
## weight sds
table.miss('WSDS', 'weight sds')

## length sds
table.miss('[^//]lSDS$', 'length sds')

## hcsds
table.miss('hcSDS', 'hc sds')

## adiposity_subscapular
table.miss('adiposity_subscapular', 'adiposity_subscapular')

## adiposity_triceps
table.miss('adiposity_triceps', 'adiposity_triceps')

## feeding 
table.miss('_breastfeeding$', 'breast feeding')

## full breastfeeding 
table.miss('full breastfeeding', 'full breastfeeding')

## formula
table.miss('formula$', 'formula')

# parents' information
## mother height
table.miss('mother_height', 'mother height')

## mother weight
table.miss('mother_weight', 'mother weight')

## pAHI
table.miss('pAHI', 'mother weight')

## preg num
table.miss('preg_number', 'preg num.')

## father height
table.miss('father_height', 'father height')

## father weight
table.miss('father_weight', 'father weight')

## mat medical
table.miss('matGlu', 'preg num.')
table.miss('matinsulin', 'mat insulin')
table.miss('matCRP', 'mat CRP')
table.miss('matCholesterol', 'mat Cholesterol')
table.miss('matHDL', 'mat HDL')
table.miss('matLDL', 'mat LDL')
table.miss('matHbA1C', 'mat HbA1C')


