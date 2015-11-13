
install.packages('dplyr')
install.packages('tidyr')


library(dplyr)
library(tidyr)
library(magrittr)

head(swiss)
tail(swiss)
summary(swiss)
str(swiss)
names(swiss)

head(swiss)
swiss %>% head
swiss %>% head()
swiss %>% head(.)
swiss %>% lm(Education ~ Infant.Mortality,
             data = .) %>%
    summary %>%
    coef

coef(summary(lm(Education~Infant.Mortality,
                data = swiss)))

swiss %>% lm(Education ~ Infant.Mortality)
swiss %>% lm(., Education ~ Infant.Mortality)

ds <- swiss %>%
    dplyr::add_rownames() %>%
    dplyr::tbl_df()
ds
swiss

names(ds)
ds %>%
    select(Education, Catholic)

ds %>%
    select(contains('Edu'),
           matches('Cath'),
           starts_with('F'),
           matches('^F'),
           matches('n$'),
           matches('.'),
           matches('*'),
           matches('C.*l'))

dim(ds)
ds %>%
    filter(Catholic < 50 | Fertility > 80) %>%
    str()

ds %>%
    filter(Education == 10 &
               Infant.Mortality > 5)

ds %>%
    filter(rowname != 'Rolle')

ds %>%
    mutate(testing = 'yes',
           Educated = ifelse(
               Education > 20, 'Yes', 'No'
           )) %>%
    select(testing, Educated)

summary(ds)

ds %>%
    select(-rowname) %>%
    mutate(Fertile = ifelse(
        Fertility > 70, 'Yes', 'No'
    )) %>%
    gather(Variable, Value, -Fertile) %>%
    group_by(Fertile, Variable) %>%
    summarise(mean = mean(Value),
              sd = sd(Value),
              median = median(Value))

ds %>%
    select(-rowname) %>%
    mutate(Fertile = ifelse(
        Fertility > 70, 'Yes', 'No'
    )) %>%
    gather(Variable, Value, -Fertile) %>%
    group_by(Fertile, Variable) %>%
    summarise(meanSD = paste0(
        mean(Value) %>% round(2),
        ' (',
        sd(Value) %>% round(2),
        ')'
    )) %>%
    spread(Fertile, meanSD)
ds %>%
    arrange(Education, desc(Agriculture)) %>%
    select(Education, Agriculture)
ds %>%
    select(County = rowname) %>%
    mutate(County = County %>%
               gsub('Mnt$', 'MOUNT', .) %>%
               gsub('^C', 'HAHAHA', .))

install.packages('broom')
swiss %>%
    tbl_df() %>%
    gather(Indep, Xvalue, Fertility, Agriculture) %>%
    gather(Dep, Yvalue, Education, Catholic) %>%
    group_by(Dep, Indep) %>%
    do(lm(Yvalue ~ Xvalue + Infant.Mortality + Examination, data = .) %>%
           broom::tidy()) %>%
    filter(term == 'Xvalue') %>%
    select(Dep, Indep, term, estimate, std.error) %>%
    knitr::kable()










