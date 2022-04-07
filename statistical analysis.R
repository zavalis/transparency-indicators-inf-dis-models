
library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))
library(readr)
#install.packages('gmodels')
library(gmodels)

# BELOW WE HAVE THE FISHER EXACT TESTS THAT WERE USED IN THE PAPER FOR STATISTICAL ANALYSIS
transp=read_csv('./results/transparency_indicators.csv')


# ALL TO FISHER
# 2019 v 2021 Non Covid-19
transp2019v2021noncovid=transp[transp$Publication_Year %in% c('2019','2021 Non Covid-19'),]
fisher.test(table(transp2019v2021noncovid$is_open_code, transp2019v2021noncovid$Publication_Year))
fisher.test(table(transp2019v2021noncovid$is_open_data, transp2019v2021noncovid$Publication_Year))
fisher.test(table(transp2019v2021noncovid$is_coi_pred, transp2019v2021noncovid$Publication_Year))
fisher.test(table(transp2019v2021noncovid$is_fund_pred, transp2019v2021noncovid$Publication_Year))
fisher.test(table(transp2019v2021noncovid$is_register_pred, transp2019v2021noncovid$Publication_Year))

# 2021 Covid vs Non-Covid
transp2021covidv2021noncovid=transp[transp$Publication_Year %in% c('2021 Covid-19','2021 Non Covid-19'),]
fisher.test(table(transp2021covidv2021noncovid$is_open_code, transp2021covidv2021noncovid$Publication_Year))
fisher.test(table(transp2021covidv2021noncovid$is_open_data, transp2021covidv2021noncovid$Publication_Year))
fisher.test(table(transp2021covidv2021noncovid$is_coi_pred, transp2021covidv2021noncovid$Publication_Year))
fisher.test(table(transp2021covidv2021noncovid$is_fund_pred, transp2021covidv2021noncovid$Publication_Year))
fisher.test(table(transp2021covidv2021noncovid$is_register_pred, transp2021covidv2021noncovid$Publication_Year))

# top 5 diseases
transp['Disease'][!transp$Disease %in% c("Covid-19","Malaria", 'Influenza illnesses','Dengue','General (theoretical models)'),]='Others'
fisher.test(table(transp$Disease, transp$is_coi_pred))
fisher.test(table(transp$Disease, transp$is_register_pred))
fisher.test(table(transp$Disease, transp$is_fund_pred))
fisher.test(table(transp$Disease, transp$is_open_code))
fisher.test(table(transp$Disease, transp$is_open_data))
table(transp$Journal, transp$is_coi_pred)

# top 3 journals
transp['Journal'][!transp$Journal %in% c("PLoS One","Sci Rep", 'Int J Environ Res Public Health'),]='Other'
transptop3journ <- transp[transp$Journal %in% c("PLoS One","Sci Rep", 'Int J Environ Res Public Health'),]
fisher.test(table(transp$Journal, transp$is_coi_pred)) 
fisher.test(table(transp$Journal, transp$is_register_pred))
fisher.test(table(transp$Journal, transp$is_fund_pred))
fisher.test(table(transp$Journal, transp$is_open_code))
fisher.test(table(transp$Journal, transp$is_open_data))

# the models
fisher.test(table(transp$Model, transp$is_coi_pred),simulate.p.value=TRUE,B=1e7) 
fisher.test(table(transp$Model, transp$is_register_pred))
fisher.test(table(transp$Model, transp$is_fund_pred))
fisher.test(table(transp$Model, transp$is_open_code),simulate.p.value=TRUE,B=1e7)
fisher.test(table(transp$Model, transp$is_open_data))

table(transp$Journal)

# 2019 vs 2021
transp['Publication_Year'][transp$Publication_Year %in% c('2021 Covid-19','2021 Non Covid-19'),]='2021'
transp2019v2021=transp
fisher.test(table(transp2019v2021$is_open_code, transp2019v2021$Publication_Year))
fisher.test(table(transp2019v2021$is_open_data, transp2019v2021$Publication_Year))
fisher.test(table(transp2019v2021$is_coi_pred, transp2019v2021$Publication_Year))
fisher.test(table(transp2019v2021$is_fund_pred, transp2019v2021$Publication_Year))
fisher.test(table(transp2019v2021$is_register_pred, transp2019v2021$Publication_Year))


# MULTIVARIATE LOGISTIC REGRESSION
library(tidyverse)
transp=read_csv(file='./results/transparency_indicators.csv')
# Due to there being too many diseases...
#transp['Journal'][transp$Journal %in% c("PLoS One","Sci Rep", 'Int J Environ Res Public Health'),]='Top3'
transp['Journal'][!transp$Journal %in% c("PLoS One","Sci Rep", 'Int J Environ Res Public Health'),]='Other'


#relevel all columns so that we have the references that we want
table(transp$Model)
transp$Model=factor(transp$Model,c('Compartmental','Time Series', 'Spatiotemporal','Agent-based', 'Multiple'))
transp$Publication_Year=factor(transp$Publication_Year, c('2019', '2021 Covid-19', '2021 Non Covid-19'))
transp$Journal=factor(transp$Journal, c('Other','PLoS One', 'Sci Rep', 'Int J Environ Res Public Health'))

#coi pred
glm.basic <- glm(is_coi_pred ~ Model+Publication_Year+Journal, data = transp, family = 'binomial')
summary(glm.basic)
i=round(exp(coef(glm.basic)),digits=2)

df = as.data.frame(round(exp(confint(glm.basic)),digits=2)) %>% 
  unite(x, c('2.5 %', '97.5 %'), sep = ", ", remove = FALSE)

count=1
listA=list()
for (x in i){listA=c(listA,paste0(as.character(x),' (',as.character(df$x[count]),') '))
count=count+1}

#fund pred
glm.basic <- glm(is_fund_pred ~ Model+Publication_Year+Journal, data = transp, family = 'binomial')
summary(glm.basic)
i=round(exp(coef(glm.basic)),digits=2)

df = as.data.frame(round(exp(confint(glm.basic)),digits=2)) %>% 
  unite(x, c('2.5 %', '97.5 %'), sep = ", ", remove = FALSE)

count=1
listB=list()
for (x in i){listB=c(listB,paste0(as.character(x),' (',as.character(df$x[count]),') '))
count=count+1}

# registration log reg is not be able to be conducted considering there is too few models to actually compare

glm.basic <- glm(is_open_code ~ Model+Publication_Year+Journal, data = transp, family = 'binomial')
summary(glm.basic)
as.data.frame(round(exp(coef(glm.basic)),digits=2))
i= round(exp(coef(glm.basic)),digits=2)

df = as.data.frame(round(exp(confint(glm.basic)),digits=2)) %>% 
  unite(x, c('2.5 %', '97.5 %'), sep = ", ", remove = FALSE)

count=1
listC=list()
for (x in i){listC=c(listC,paste0(as.character(x),' (',as.character(df$x[count]),') '))
count=count+1}

glm.basic <- glm(is_open_data ~ Model+Publication_Year+Journal, data = transp, family = 'binomial')
glm.basic
summary(glm.basic)
i= round(exp(coef(glm.basic)),digits=2)
df = as.data.frame(round(exp(confint(glm.basic)),digits=2)) %>% 
  unite(x, c('2.5 %', '97.5 %'), sep = ", ", remove = FALSE)

count=1
listD=list()
for (x in i){listD=c(listD,paste0(as.character(x),' (',as.character(df$x[count]),') '))
count=count+1}

multivartable=do.call(rbind, Map(data.frame, is_open_code=listC, is_open_data=listD, is_coi_pred=listA, is_fund_pred=listB))
write.csv(multivartable, './results/multivarlogregtablez.csv')
df[,]

