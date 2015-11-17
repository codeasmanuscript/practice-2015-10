

#plotting R 

str(diamonds)
qplot(carat, price, data=diamonds)

qplot(carat, price, data=diamonds, color=cut)

qplot(carat, data=diamonds, fill=cut)

qplot(carat, data=diamonds, fill=cut, binwidth=1)

plot1 <- ggplot(diamonds, aes(x=color, y=depth))+
  geom_point()
plot1

plot2 <-ggplot(diamonds, aes(x=color, y=depth))+
  geom_boxplot()
plot2


plot3 <-ggplot(diamonds, aes(x=color, y=depth))+
  geom_boxplot()+
  labs(title="Depth and color of round cut diamonds", x="Diamond color", y="Depth (mm)")
  #scale_x_discrete(limits=rev(diamonds$color))
plot3


plot4 <-ggplot(diamonds, aes(x=color, y=depth))+
  geom_boxplot()+
  labs(title=" Depth and color of round cut diamonds by clarity", x="Diamond color", y="Depth (mm)")+
  facet_wrap(~clarity, ncol=4)
plot4

plot5 <- diamonds %>% 
  select(1,5:10) %>% 
  cor() %>% 
  round(3) %>% 
  melt() %>% 
  tbl_df() %>% 
 ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_tile()
plot5

diamonds %>% 
  select(1,5:10) %>% 
  gather(Variable, Value) %>% 
  group_by(Variable) %>% 
  summarise(mean =mean(Value)) %>% 
  tbl_df() %>% 
  ggplot(aes(x=Variable, y=mean))+
  geom_point()+
  labs(plot.title="Mean values", x="Characteristic of diamonds", y="mean value")
  


diamonds %>% 
  select(1,5:10) %>% 
  mutate(price = ifelse(
    price>2000, 'Expensive','Inexpensive')) %>%
    gather(Variable, Value, -price) %>% 
    group_by(Variable,price) %>% 
    summarise(mean =mean(Value)) %>% 
    tbl_df() %>% 
    ggplot(aes(y=Variable, x=mean))+
    geom_point(aes(color=price))+
    labs(plot.title="mean characteristics divided by price", x="diamond characteristic", y="mean")


diamonds %>% 
  select(1,5:10) %>% 
  mutate(price = ifelse(
    price>2000, 'Expensive','Inexpensive')) %>%
  gather(Variable, Value, -price) %>% 
  group_by(Variable,price) %>% 
  summarise(mean =mean(Value),
            sd = sd(Value)) %>% 
  tbl_df() %>% 
  ggplot(aes(y=Variable, x=mean))+
  geom_errorbarh(aes(xmin=mean-sd,xmax=mean+sd ), height=0.2)+
  geom_point(aes(color=price))+
  labs(plot.title="mean characteristics divided by price", x="diamond characteristic", y="mean")




  