library(tidyverse)
library(ggplot2)


chlamydia = read.csv("https://query.data.world/s/cu0r246l5ljt4q3411q66hoq",header=T)
aids = read.csv("https://query.data.world/s/b75zwogm4rndt5lwhkmrfnhwc",header=T)

#chlamydia.backup = chlamydia
#aids.backup = aids

chlamydia$Cases = as.numeric(chlamydia$Cases)
chlamydia$Population = as.numeric(chlamydia$Population)

aids$Cases = as.numeric(aids$Cases)
aids$Population = as.numeric(aids$Population)


summary(chlamydia)
summary(aids)

chlamydia$Disease = 'Chlamydia'
aids$Disease = 'AIDS'

df.all = rbind(chlamydia,aids)
df.all$Disease = as.factor(df.all$Disease)
df.all$Year = as.factor(df.all$Year)

df = df.all %>%
  group_by(Disease,Year) %>%
  summarise(Population=sum(Population),Cases=sum(Cases),Rate=(100*sum(Cases))/sum(Population))

p = ggplot(df, aes(x=Year,y=Population,col=Disease))
p + geom_line(aes(group=Disease))
#Discrepancy between Populations in data sets... AIDS is almost exactly linear...

df = df.all %>%
  filter(as.numeric(Year) <= 2014) %>%
  group_by(Disease,Geography) %>%
  summarise(Population = sum(Population))

p = ggplot(df,aes(x=Disease,y=Population))
p + geom_bar(stat='identity') + facet_wrap(~Geography)



library(trelliscopejs)
qplot(Year, Rate, data = df.all) +
  theme_bw() +
  facet_trelliscope(~ Geography, nrow = 4, ncol = 6)






df = df.all %>%
  group_by(Disease,Year,Geography) %>%
  summarise(Rate = 100*sum(Cases)/sum(Population))

p = ggplot(df, aes(x=Year,y=Rate,col=Disease))
p + geom_line(aes(group=Disease))




df = df.all %>% 
  filter(Disease == 'AIDS') %>%
  group_by(Year) %>%
  summarise(Population=sum(Population),Cases=sum(Cases))
p = ggplot(df, aes(x=Year,y=Cases))
p + geom_line(group=1)

df = df.all %>% 
  filter(Disease == 'Chlamydia') %>%
  group_by(Year) %>%
  summarise(Population=sum(Population),Cases=sum(Cases))
p = ggplot(df, aes(x=Year,y=Cases))
p + geom_line(group=1)
