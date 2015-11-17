
#Date: 9th Nov 2015

ds <- swiss %>% 
  add_rownames() %>% 
  tbl_df()

#question 1
ds %>% 
  mutate(Fertility = ifelse(
        Fertility>50, 'Fertile','Infertile')) %>% 
  filter(Catholic < 60) %>% 
  select(matches('^E'),
         matches('^A'),
         matches('^I'),
         matches('^F')) %>% 
  gather(Variable, Value,-Fertility) %>% 
  group_by(Fertility, Variable) %>% 
  summarise(mean = mean(Value) %>%  round(2)) %>%
  spread(Fertility, mean) %>% 
  tbl_df()


#question 2
ds %>% 
  mutate(Fertility = ifelse(
    Fertility>50, 'Fertile','Infertile')) %>% 
  filter(Catholic < 60) %>% 
  select(matches('^E'),
         matches('^A'),
         matches('^I'),
         matches('^F')) %>% 
  gather(Variable, Value,-Fertility) %>% 
  group_by(Fertility, Variable) %>% 
  summarise(meanSD = paste0(mean(Value) %>%  round(2),
                            ' (',
                         sd(Value) %>%  round(2),
                            ' SD',
                         ')')) %>% 
  spread(Fertility, meanSD) %>% 
  tbl_df()

#question 3
#skip- see their answer

#question 4
ds %>% 
  select(-rowname) %>% 
  gather(Variable, Value) %>% 
  group_by(Variable) %>% 
  summarise(Min = min(Value),
            Mean = mean(Value),
            Max = max(Value))

  

#question 5
ds %>% 
  filter((Education >= 8), Infant.Mortality < 18 , (Fertility < 60 & Fertility > 50)) %>% 
  select(rowname)


#question 6
ds %>%
  select(-rowname) %>% 
  mutate(Educated = ifelse(
    Education>8, 'Yes','No')) %>% 
  group_by(Educated)  %>% 
  do(cor(.[-7]) %>%
       broom::tidy()) %>% 
  gather(Variable, value,-Educated,-.rownames) %>% 
  filter(value != 1)




#cor(ds[-1])
  
#gather(Variable, value,-Educated) %>% 


#question 7
install.packages('broom')
swiss %>%
  tbl_df() %>%
  gather(Indep, Xvalue, Fertility, Agriculture) %>%
  gather(Dep, Yvalue, Education, Catholic) %>%
  group_by(Dep, Indep) %>%
  do(lm(Yvalue ~ Xvalue + Infant.Mortality + Examination, data = .) %>%
       broom::tidy()) %>%
  filter(term == 'Xvalue') %>%
  select(Dep, Indep, estimate, std.error) %>%
  knitr::kable()

     
  
  
