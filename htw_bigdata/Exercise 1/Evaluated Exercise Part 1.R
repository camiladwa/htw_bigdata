library(ggplot2)
library(dplyr)
library(tidyr)

os_count <- read.csv('os_count', header = FALSE, col.names = c('os', 'count'))
pck_count <- read.csv('package_count', header = FALSE, col.names = c('pckg', 'count'))

summary(os_count)
summary(pck_count)
sum(pck_count$count)
pck_count[is.na(pck_count$pckg),]
os_count[is.na(os_count$os),]
length(unique(pck_count$pckg))
length(unique(os_count$os))

os_25 <- os_count %>%
  drop_na()%>%
  arrange(desc(count))%>%
  head(25)


pckg_25 <- pck_count %>%
  drop_na()%>%
  arrange(desc(count))%>%
  head(25)

top_25_log = sum(pckg_25$count)  
top_25_log/total_log

ggplot(os_25, aes(reorder(os, count), count)) +
  geom_bar(stat="identity", fill="steelblue")+
  coord_flip()+
  labs(y= "Occurrence", x = "Operating System")+
  geom_text(aes(label = count, hjust = 0))+
  ylim(0, 1800000)
  theme(axis.text.x = element_text(angle = 90, vjust =0, hjust= 1))+
  scale_y_continuous(labels = scales::comma)


ggplot(pckg_25, aes(x = reorder(pckg, count), count)) +
  geom_bar(stat="identity", fill="steelblue")+
  coord_flip()+
  labs(y= "Occurrence", x = "Package")+
  theme(axis.text.x = element_text( vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)

---------------
  
  
ml <- read.csv('ml', header = FALSE, col.names = c('pckg', 'count'))
topic <- read.csv('topics', header = FALSE, col.names = c('topic','pckg', 'count'))

summary(ml)
summary(topic)
ml[is.na(ml$pckg),]
topic[is.na(topic$pckg),]
length(unique(ml$pckg))
length(unique(topic$pckg))
sum(ml$count)
sum(topic$count)



ml_10 <- ml %>%
  mutate(count= as.numeric(count))%>%
  arrange(desc(count))%>%
  head(10)

topic_summary <- topic %>%
  mutate(count= as.numeric(count))%>%
  group_by(topic)%>%
  summarise(count = sum(count))


topics_10 <- topic %>%
  mutate(count= as.numeric(count))%>%
  arrange(desc(count))%>%
  head(10) 


ggplot(ml_10, aes(reorder(pckg, -count), count)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(y= "Occurrence", x = "Package")+
  geom_label(aes(label = count))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)



ggplot(topic_summary, aes(reorder(topic, -count), count)) +
  geom_bar(stat="identity", fill=c("#999999", "#E69F00", "#56B4E9"))+
  coord_flip()+
  labs(y= "Occurrence", x = "")+
  geom_label(aes(label = count))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)




ggplot(topics_10, aes(x = reorder(pckg, -count), count, fill = topic)) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_bar(stat="identity")+
  labs(y= "Occurrence", x = "Package")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)
