
install.packages('dplyr')
install.packages('tidyr')

library(dplyr)
library(tidyr)

head(swiss)
tail(swiss)

summary(swiss)
str(swiss)
names(swiss)

#command+shift+m gives %>%. pipe build on a previous function
head(swiss)
swiss %>% head
swiss %>% head() # it will use the first avaiable place (dont want this for regression)
swiss %>% head(.) # the . specifies where the data has to go to

swiss %>% lm(Education ~ Infant.Mortality, data=.) %>% 
  summary %>% 
  coef %>% 
  
  # this avoids the need for putting things in bracketts 
  # this will place the swiss data int he right palce
  
  
#using dplyr
  
  
ds <- swiss %>% 
  add_rownames() %>% 
  tbl_df()

#rowname was only an attribute 
#addg table dataframe property to it- the printing is prettier
ds
swiss


ds %>%  
  select(Education, Catholic) %>% 

  #if you end up naeanting to select by the same name or partial name

ds %>% 
  select(contains('Edu'),
         matches('Cath'),
         starts_with('F')
         )

#matches('^F'),
#matches('n$'),
#matches('.'),
#matches('*'),
#matches('C.*l')

#in reg exp, . means one (anything) but * means everything. . means a wildcard for one character. but * wild card for any lenght. . is one character
#contains and starts with can be a subset of matches. Matches is very powerful. matches uses regular expression.
# learn regular expression
  #regular expression: ^ means starts with, $ means ends with so you can substitute the #starts with. reg exp is usefulwhen you want to do find and replace.

#filter
dim(ds)
 ds %>% 
   filter(Catholic < 50 | Fertility > 40) %>% 
   str()

 dplyr::add_rownames()
 #THis is way of calling a library.
 
 ds %>% 
   filter(Education == 10 & 
            Infant.Mortality >5)
 
 ds %>% 
   filter(rowname != 'Rolle') %>% 
   str()
  
 #mutate
 ds %>% 
   mutate(testing = 'yes',
          Educated =ifelse(
            Education >20, 'Yes', 'No'
          )) %>% 
   select(testing, Educated)
 
 #it is better to do this instead of assigning into objects each time so that youdont overpopulate and it is 
 #easy wen you want to change something. and then you assign the final one if you needed
 
 
 #tidyr
 ds %>% 
   select(-rowname) %>% 
   gather(Variable, Value) %>% #variable is the factor level/variable and the value is the actual value. you coudl kee this line the same- call it the same
   group_by(Variable) %>% 
   summarise(mean = mean(Value),
             sd = sd(Value),
             median = median(Value))
 
   #if you want only a few variables, you filter first
 #the group_by adds an attribute, so anything that occurs after group_by will be done by the the grouping
 
 ds %>%
   select(-rowname) %>%
   mutate(Fertile = ifelse(Fertility > 70, 'Yes', 'No')) %>%
   gather(Variable, Value, -Fertile) %>% #variable is the factor level/variable and the value is the actual value. you coudl kee this line the same- call it the same
   group_by(Fertile, Variable) %>%
   summarise(meanSD = paste0(mean(Value) %>%  round(2),
                           ' (',
                           sd(Value) %>%  round(2),
                           ')')) %>%
   spread(Fertile, meanSD)
 
  #paste retruns a character 
 #spread changes the output table to a wide format. otherwise Fertile will be in long format
 
 #arrange
 #sorting by
 ds %>% 
   arrange(Education, Agriculture) %>% 
   select(Education, Agriculture)
  
  
 ds %>% 
   arrange(Education, desc(Agriculture)) %>% 
   select(Education, Agriculture)
 
 
 ds %>% 
   select(County = rowname) %>% 
   mutate(County =gsub('e$', "", County) %>% 
                  gsub('^C','HAHAHA', .))
  #gsub means global substite. the . says thats where the output needs to go. so R has to know where to put it first
 # thats why it is mentioned in the first row. An alternate in as below. You are already telling  R which dataset
 #you are working with.
 
 ds %>% 
   select(County = rowname) %>% 
   mutate(County = County %>% 
            gsub('e$', "", .) %>% 
            gsub('^C','HAHAHA', .))
 
 
 
 
 
 
 
 
 
 