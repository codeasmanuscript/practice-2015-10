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
