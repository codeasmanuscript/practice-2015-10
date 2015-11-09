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

