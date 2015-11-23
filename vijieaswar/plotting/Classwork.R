
Geoms are geometric objects

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)

#qplot- good for a quick view; use ggplot for publication quality graphs


mpg
str(mpg)

qplot(displ, hwy, data=mpg)

qplot(displ, hwy, data=mpg, color=drv)

qplot(displ, hwy, data=mpg, geom = c("point","smooth"))
#have to specify that you want pointsand smooth, else it will only give smooth
#default for smooth is 95% CI. Can alter this as something else

qplot(hwy, data=mpg, fill=drv)
#in hisotgram you can only use one variable.
# default binwidth is range/30.

# ~ placement depends on how you want to present the facets.
qplot(displ,hwy,data=mpg, facets=.~drv)
qplot(displ,hwy,data=mpg, facets=drv~.)

qplot(hwy,data=mpg,facets=drv~., binwidth=2)

qplot(log(displ), data=mpg, geom= "density", color=drv)

#could do linear regression in qplot
qplot(log(displ), log(hwy),data=mpg, color=drv,geom=c("point","smooth"),method="lm")

#ggplot will only work on data frames, if you have a matrix- convert to dataframes
g <- ggplot(mpg, aes(displ, hwy))

summary(g)

g+ geom_point()
g+ geom_point(aes(color=drv))+ geom_smooth(method="lm") #you use aes if you want it to vary by data
g+ geom_point(color="steelblue", size=4, alpha=1/2) +#alpha: transparency is related to the density.
  geom_smooth(method="lm")+ 
  facet_grid(.~drv)+
  labs(title="practice session",x="engine displ", y="highway miles")+
  theme_bw(base_family="Times")

#get the code from here
#http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#ddply- apply a funciton on a dataframe and bring it back as a dataframe

mpgsum <- summarySE(mpg, measurevar = "displ", groupvars = c("class","drv"))
mpgsum

ggplot(mpgsum, aes(x=drv,y=displ, color=class))+
  geom_errorbar(aes(ymin=displ-se,ymax=displ+se), width=0.1)+
  geom_point()


library(reshape2)

#cor returns a matric, not a df
#creating a heatmap
mtcars %>% 
  select(c(1,3,4,5,6,7)) %>% 
  cor() %>% 
  round(2) %>% 
  melt() %>% 
  ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low="red",high="blue",mid="white", limit=c(-1,1))



  
