
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)

# PLOTTING- GGPLOT2 - grammar of graphics

# need to have a dataframe (use as.dataframe() code)

#######
#qplot#
#######

# a quick plotting tool - use ggplot for publication figures
?mpg
str(mpg)

# color the different data points
qplot(displ,hwy, data=mpg, color=drv)
#add a line
qplot(displ,hwy, data=mpg, geom=c("point","smooth")) 
  # default 95%CI for smooth, which follows the trend

# create a histogram
qplot(hwy, data=mpg, fill=drv)

# add a facet (row~column)
qplot(displ, hwy, data=mpg, facets=.~drv)

# facet histogram
qplot(hwy, data=mpg, facets=drv~., binwidth=2)

# density plots
qplot(log(displ), data=mpg, geom="density",color=drv)

# linear regressions in qplot
qplot(log(displ), log(hwy), data=mpg, color=drv, geom=c("point","smooth"), method="lm")


##########
# ggplot #
##########

# need to add layers to base
g <- ggplot(mpg, aes(displ,hwy))
summary(g)
# each portion of the graph is separated - easier to modify each point

# add points to the graph
g + geom_point()

# add a line
g + geom_point(aes(color=drv)) + geom_smooth(method="lm")

# just specify a color for the points, not based on a factor.
# alpha = transparency based on density
g + geom_point(color="steelblue", size=4, alpha=1/2) +
  geom_smooth(method="lm") +
  facet_grid(.~drv) +
  labs(title="Fuel economy of 38 popular car models", x="Engine Displacement (l)", y="Highway miles per gallon") +
  theme_bw(base_family="serif")

# Create a summary function

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
#   library(plyr)
  
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


mpgsum <- summarySE(mpg, measurevar="displ", groupvars=c("class","drv"))

ggplot(mpgsum, aes(x=drv, y=displ, color=class))+
  geom_errorbar(aes(ymin=displ-se, ymax=displ+se),width=.1)+
  geom_point()

# source() 


# Create a heat map
library(reshape2)

mtcars %>%
  select(c(1,3:7)) %>%
  cor() %>%
  round(2) %>%
  melt() %>%
  ggplot(aes(x=Var1,y=Var2, fill=value))+
  geom_tile()+ # gives a heat map of correlation
  scale_fill_gradient2(low="blue",high="red", mid="white", limit=c(-1,1))
