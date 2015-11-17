library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)

dm <- diamonds

qplot(x=carat, y=price, data=dm)
qplot(x=carat, y=price, data=dm, color=cut)
qplot(x=carat, data=dm, fill=cut)
qplot(x=carat, data=dm, fill=cut, binwidth=1)

b <- ggplot(dm, aes(x=color,y=depth))
b + geom_boxplot()
b + geom_boxplot() + labs(title="Depth and colour of round cut diamonds", x="Diamond colour, from J(worse) to D(best)", y="Depth (mm)")
b + geom_boxplot() +
  labs(title="Depth and colour of round cut diamonds by clarity", x="Diamond colour, from J(worse) to D(best)", y="Depth (mm)") +
  facet_wrap(~clarity,ncol=4)

str(dm)
dm %>%
  select(1,5:10) %>%
  cor() %>%
  round(3) %>%
  melt() %>%
  tbl_df() %>%
  ggplot(aes(x=Var1,y=Var2, fill=value))+
  geom_tile()+
  scale_fill_gradient2(low="blue",high="red", mid="white", limit=c(-1,1))

dm %>%
  dplyr::select(1,5:10) %>%
  gather(Variable, Value) %>%
  group_by(Variable) %>%
  dplyr::summarise(mean = mean(Value)) %>%
  tbl_df() %>%
  ggplot(aes(x=mean,y=Variable))+
  geom_point()+
  scale_x_log10()+
  labs(x="Mean", y="Variable")

dm %>%
  mutate(Price=ifelse(price>=2000,'expensive','inexpensive')) %>%
  dplyr::select(-cut,-color,-clarity,-price) %>%
  gather(Variable, Value,-Price) %>%
  group_by(Price,Variable) %>%
  dplyr::summarise(mean = mean(Value)) %>%
#   spread(Price,mean) %>%
  tbl_df() %>%
  ggplot(aes(x=mean,y=Variable,color=Price))+
  geom_point(size=4)+
  labs(x="Mean", y="Diamond Attribute")

dm %>%
  mutate(Price=ifelse(price>=2000,'expensive','inexpensive')) %>%
  dplyr::select(-cut,-color,-clarity,-price) %>%
  gather(Variable, Value,-Price) %>%
  group_by(Price,Variable) %>%
  dplyr::summarise(mean = mean(Value), sd=sd(Value)) %>%
#   spread(mean,sd) %>%
  tbl_df() %>%
  ggplot(aes(x=mean,y=Variable,color=Price))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd),height=.5)+
  labs(x="Mean", y="Diamond Attribute")
