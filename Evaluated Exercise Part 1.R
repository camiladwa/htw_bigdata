library(ggplot2)
library(dplyr)
library(tidyr)

os_count <- read.csv('os_count', header = FALSE, col.names = c('os', 'count'))
pck_count <- read.csv('package_count', header = FALSE, col.names = c('pckg', 'count'))

summary(os_count)
summary(pck_count)

os_25 <- os_count %>%
  drop_na()%>%
  arrange(desc(count))%>%
  head(25)


pckg_25 <- pck_count %>%
  drop_na()%>%
  arrange(desc(count))%>%
  head(25) 
  

ggplot(os_25, aes(reorder(os, -count), count)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(y= "Occurrence", x = "Operating System")+
  geom_label(aes(label = count))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)


ggplot(pckg_25, aes(x = reorder(pckg, -count), count)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(y= "Occurrence", x = "Package")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = scales::comma)



