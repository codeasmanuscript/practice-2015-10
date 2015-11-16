install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(tidyr)

head(swiss)
tail(swiss)
summary(swiss)
str(swiss)

# these all show the same thing:
head(swiss)
swiss %>% head
swiss %>% head()
swiss %>% head(.) # the dot specifies where you want to put the data; if you don't put a dot, R puts the data in the first specifier

# e.g.:
swiss %>%lm(Education~Infant.Mortality, data=.) %>%
  summary %>%
  coef
#same as:
coef(summary(lm(Education~Infant.Mortality, data=swiss)))

# Using dplyr
## Create a new dataframe
ds <- swiss %>%
  add_rownames() %>%
  tbl_df()


##SELECT
ds %>%
  select(Education, Catholic)
ds %>%
  select(contains("Edu"),
         matches("Cath"),
         starts_with("F"),
         matches("^F"),#the hat means starts-with
         matches("n$"))#the $ means ends-with
#a dot would mean, given anything (1 character); * means everything (unlim characters)

##FILTER
ds %>%
  filter(Catholic<50|Fertility>80) %>%
  str()

#if you want to self-contain, do:
ds %>%
  dplyr::filter(Catholic<50|Fertility>80)

ds %>%
  filter(Education==10 & 
           Infant.Mortality>5)
ds %>%
  filter(rowname != "Rolle") #anything but this row

ds %>%
  
##MUTATE
ds %>%
  mutate(testing="yes",
         Educated=ifelse(
           Education>20,"Yes","No")) %>%
  select(testing,Educated)

##TIDYR commands
ds %>%
  select(-rowname) %>% #you can use only a couple variables if  you want by adding a filter
  mutate(Fertile=ifelse(
    Fertility>70,"Yes","No")) %>%
  gather(Variable,Value,-Fertile) %>%  #converted into long format
  group_by(Fertile,Variable) %>%
  summarise(mean=mean(Value),
            sd=sd(Value),
            median=median(Value))

ds %>%
  select(-rowname) %>%
  mutate(Fertile=ifelse(
    Fertility>70,"Yes","No")) %>%
  gather(Variable,Value,-Fertile) %>%
  group_by(Fertile,Variable) %>%
  summarise(meanSD=paste0(mean(Value) %>% round(2),'(',sd(Value) %>% round(2),')')) %>%
  spread(Fertile,meanSD)# putting from long into wide, so that "No" and "Yes" are to the right versus on top of each other
#paste is returning a character

ds %>%
  arrange(Education,desc(Agriculture)) %>%
  select(Education,Agriculture)

# Review: select, filter, arrange, summarise, group_by, gather, spread

ds %>%
  select(County=rowname) %>%
  mutate(County=gsub('e$','',County) %>% 
           gsub('^C','HAHAHA',.))#dot here, b/c this is where that data needs to go
#gsub=global substitute
#same thing:
ds %>%
  select(County=rowname) %>%
  mutate(County=County %>% 
           gsub('e$','',.) %>% 
           gsub('^C','HAHAHA',.))
