library(tidyverse)
library(here)

files <- list.files(path=here("sample","data"), full.names = TRUE)

df <- read_csv(files)

df.origin <- df %>% 
  mutate(congruency = case_when(word == ink ~ "Congruent",
                                word != ink ~ "Incongruent"),
         resp_color = case_when(response == "f" ~ "red",
                                response == "j" ~ "green"),
         ans = case_when(ink == resp_color ~ 1,
                         ink != resp_color ~ 0)) %>% 
  filter(practice == "main") %>% 
  select(ID, congruency, reactiontime, ans) %>% 
  group_by(ID, congruency) %>% 
  mutate(M = mean(reactiontime),
         SD = sd(reactiontime)) %>% 
  ungroup()

  

## Mean+-3SD is regarded as outliers

df.main <- df.origin %>% 
  mutate(outlier = case_when(reactiontime < M-3*SD ~ 0,
                             reactiontime > M+3*SD ~ 0,
                             TRUE ~ 1)) %>% 
  filter(ans == 1,
         outlier == 1) %>% 
  select(ID, congruency, reactiontime)

for_ttest <- df.main %>% 
  group_by(ID, congruency) %>% 
  summarise(reactiontime = mean(reactiontime)) %>% 
  ungroup()

t.test(reactiontime ~ congruency, data=for_ttest)

for_plot <- for_ttest %>% 
  group_by(congruency) %>% 
  summarise(ReactionTime = mean(reactiontime), 
            n = n(),
            SE = sd(reactiontime)/sqrt(n))

library(papaja)
library(scales)

ggplot(data=for_plot,
       aes(x=congruency,
           y=ReactionTime,
           fill=congruency))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = ReactionTime-SE,
                    ymax = ReactionTime+SE),
                width = 0.1)+
  scale_y_continuous(limits = c(300, 410),
                     breaks = seq(300,410,20),
                     oob = squish)+
  papaja::theme_apa()

