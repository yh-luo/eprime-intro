pacman::p_load(tidyverse, magrittr)
dat = read.table("subject-1.txt", skip=1, sep="\t", h=T)
dat = dat[,c("Subject", "condition", "Running", "stim.ACC", "stim.RT")]

sum.table <- dat %>% filter(., Running!="Practice") %>% group_by(condition) %>% summarise(m_ACC = mean(stim.ACC), 
                                                                             se_ACC = sd(stim.ACC)/sqrt(n()))
ggplot(sum.table, aes(x=condition, y=m_ACC))+
  geom_errorbar(aes(ymin=m_ACC-se_ACC, ymax=m_ACC+se_ACC), width=.3)+
  geom_point(size=2)+
  ylim(0.7,1)+
  labs(x="Condition", y="ACC")+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=12))

RT.table <- dat %>% filter(., Running!="Practice" & stim.ACC==1) %>% group_by(condition) %>% summarise(m_RT=mean(stim.RT), 
                                                                                                       se_RT = sd(stim.RT)/sqrt(n()))
ggplot(RT.table, aes(x=condition, y=m_RT))+
  geom_errorbar(aes(ymin=m_RT-se_RT, ymax=m_RT+se_RT), width=.3)+
  geom_point(size=2)+
  coord_flip()+
  labs(x="Condition", y="Reaction Time (ms)")+
  theme_classic()+
  theme(text=element_text(size=12))
