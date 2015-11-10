library(dplyr)
library(tidyr)

# Challenge 1
ds <- swiss %>%
  add_rownames() %>%
  tbl_df()

ds %>%
  filter(Catholic<60) %>%
  mutate(Fertile=ifelse(Fertility>50,'Fertile','Infertile')) %>%
  select(-rowname,-Catholic,-Fertility) %>%
  gather(Variable,Value,-Fertile) %>% 
  group_by(Fertile,Variable) %>%
  summarise(Mean=mean(Value) %>% round(2)) %>%
  spread(Fertile,Mean) %>%
  tbl_df()
  
# Challenge 2
ds %>%
  filter(Catholic<60) %>%
  mutate(Fertile=ifelse(Fertility>50,'Fertile','Infertile')) %>%
  select(-rowname,-Catholic,-Fertility) %>%
  gather(Variable,Value,-Fertile) %>% 
  group_by(Fertile,Variable) %>%
  summarise(meanSD=paste0(mean(Value) %>% round(2),'(',sd(Value) %>% round(2),' SD)')) %>%
  spread(Fertile,meanSD) %>%
  tbl_df()

# Challenge 3


# Challenge 4

ds %>%
  select(-rowname) %>%
  gather(Variable,Value) %>%
  group_by(Variable) %>%
  summarise(Min=min(Value),
            Mean=mean(Value),
            Max=max(Value))

# Challenge 5
 
ds %>%
  filter(Education>=8,Infant.Mortality<18,(Fertility>=50 & Fertility<=60)) %>%
  select(rowname)

# Challenge 6

ds %>%
  mutate(Educated=as.factor(ifelse(Education>8,'yes','no'))) %>%
  group_by(Educated) %>%
  select(-rowname,-Education) %>%
  do(cor(.[-6]) %>% broom::tidy()) %>%
  gather(Var2,Correlation,-.rownames,-Educated) %>%
  filter(Var2 != .rownames) %>%
  select(Educated,Var1=.rownames,Var2,Correlation) %>%
  knitr::kable()



# Challenge 7

install.packages('broom')

swiss %>%
  gather(Indep, Xvalue, Fertility, Agriculture) %>%
  gather(Dep, Yvalue, Education, Catholic) %>%
  group_by(Dep, Indep) %>%
  do(lm(Yvalue~Xvalue, data= .) %>% 
       broom::tidy()) %>%
  knitr::kable()

#Add covariate
swiss %>%
  gather(Indep, Xvalue, Fertility, Agriculture) %>%
  gather(Dep, Yvalue, Education, Catholic) %>%
  group_by(Dep, Indep) %>%
  do(lm(Yvalue~Xvalue+Infant.Mortality+Examination, data=.) %>% 
       broom::tidy()) %>%
  filter(term=='Xvalue') %>% #only show the adjusted slope of the main slope
  select(Dep,Indep,estimate,std.error) %>%
  knitr::kable()
