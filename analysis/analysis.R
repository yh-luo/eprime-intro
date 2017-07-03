# import data
dat = read.table("subject-1.txt", skip=1, sep="\t", header=T)
dat = dat[,c("Subject", "condition", "Running", "stim.ACC", "stim.RT")]

# exclude practice data
dat = subset(dat, Running!="Practice")

#
# ACC
#
## conditional means
ACC.table = aggregate(stim.ACC~condition, data=dat, FUN=mean)
## conditional SE
SE.table = aggregate(stim.ACC~condition, data=dat, FUN=function(i) sd(i)/sqrt(160))
## merge them
ACC.table = merge(ACC.table, SE.table, by="condition")
colnames(ACC.table) = c("condition", "mean_ACC", "se_ACC")
## prune data a little bit
ACC.table[,c(2:3)] = round(ACC.table[,c(2:3)], 3)

# graphical exploration
library(ggplot2)
ggplot(ACC.table, aes(condition, mean_ACC))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=mean_ACC-se_ACC, ymax=mean_ACC+se_ACC), width=.3)+
  ylim(0.8,1)+
  labs(x="Condition", y="ACC")+
  coord_flip()+
  theme_classic()

#
# RT
#
## select hit trial
dat2 = subset(dat, stim.ACC==1)
## conditional mean RT
RT.table = aggregate(stim.RT~condition, data=dat2, FUN=mean)
## conditional SE of RT
SE.table = aggregate(stim.RT~condition, data=dat2, FUN=function(i) sd(i)/sqrt(160))
## merge them
RT.table = merge(RT.table, SE.table, by="condition")
colnames(RT.table) = c("condition", "mean_RT", "se_RT")
## prune data a little bit
RT.table[,c(2:3)] = round(RT.table[,c(2:3)], 3)

## graphical exploration
ggplot(RT.table, aes(condition, mean_RT))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=mean_RT-se_RT, ymax=mean_RT+se_RT), width=.3)+
  labs(x="Condition", y="Reaction Time (ms)")+
  ylim(460, 520)+
  coord_flip()+
  theme_classic()

### done ###

#
# With tidyverse and pipes(magrittr)
#

# import data
pacman::p_load(tidyverse, magrittr)
dat = read.table("subject-1.txt", skip=1, sep="\t", h=T)
dat = dat[,c("Subject", "condition", "Running", "stim.ACC", "stim.RT")]

# ACC
ACC.table <- dat %>% filter(., Running!="Practice") %>% group_by(condition) %>% summarise(m_ACC = mean(stim.ACC), 
                                                                                          se_ACC = sd(stim.ACC)/sqrt(n()))
ggplot(ACC.table, aes(x=condition, y=m_ACC))+
  geom_errorbar(aes(ymin=m_ACC-se_ACC, ymax=m_ACC+se_ACC), width=.3)+
  geom_point(size=2)+
  ylim(0.8,1)+
  labs(x="Condition", y="ACC")+
  coord_flip()+
  theme_classic()

# RT
RT.table <- dat %>% filter(., Running!="Practice" & stim.ACC==1) %>% group_by(condition) %>% summarise(m_RT=mean(stim.RT), 
                                                                                                       se_RT = sd(stim.RT)/sqrt(n()))
ggplot(RT.table, aes(x=condition, y=m_RT))+
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), width=.3)+
  geom_point(size=2)+
  coord_flip()+
  labs(x="Condition", y="Reaction Time (ms)")+
  ylim(460, 520)+
  theme_classic()

### done ###
