# Packages

library(tidyverse)
library(ggthemes)
library(ggthemr)
library(ggrepel)
library(ggpubr)
library(reshape2)

# Get the Data

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Understanding the data

str(passwords)

summary(passwords)

# Check if there are any duplicated passwords
# There are none:)

passwords$password[duplicated(passwords$password)]

# Filter data by time_unit

passwords_years1 <- passwords %>%
  filter(time_unit == "years")%>%
  filter(strength == 9 | strength == 10)

glimpse(passwords_years1)

# Graph

plot <- ggplot(passwords_years1, aes(rank, as.character(strength))) + 
  geom_point(aes(color = category), size = 4) + 
    xlab("Rank") +
  ylab("Strength") +
  facet_wrap( ~ category)+
  geom_label_repel(aes(label = password),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 14, hjust = 0.5, face = "bold", colour = "black",
      vjust = -1
    ),
    plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="black"),
    plot.background = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic", color = "blue"),
    axis.title.x = element_text(face = "bold", color = "black", size = 12),
    axis.title.y = element_text(
      face = "bold", color = "black",
      size = 12
    )
  ) +
  guides(color=FALSE)+
  labs(caption = "Data Source: Information is Beautiful | code by: @magwanjiru") +
  labs(title = "Passwords",
       subtitle = "These passwords with a strength of 9 or 10 take years to match with the value")

plot + theme_stata()
