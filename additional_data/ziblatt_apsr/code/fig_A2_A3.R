library(tidyverse)
rm(list  = ls())
## Load the data

df <- read_rds('data/data_main.rds') %>% 
  mutate(hannover_dist_hist = max(hannover_sim, na.rm = T) - 
           hannover_sim) 

#### Figure A.2: Historical and contemporary dialectal distance ####

p1 <- ggplot(df, aes(x = hannover_dist_hist, y = hannover_dist)) +
  geom_point(alpha = 0.75, size = 2, color = 'black',
             shape = 21, fill = 'white') +
  geom_smooth(method = 'lm', se = F, color = 'black') +
  theme_bw() + xlab('Distance from standard German\n(19th century)') +
  ylab('Distance from standard German\n(contemporary)')
p1



#### Figure A.3: Dialectal distance and AfD vote shares in 2017 ####

p2 <- ggplot(df, aes(x = hannover_dist, y = afd_party_17)) +
  geom_point(alpha = 0.75, size = 2, color = 'black',
             shape = 21, fill = 'white') +
  geom_smooth(method = 'lm', se = F, color = 'black') +
  theme_bw() + xlab('Distance from standard German') +
  ylab('AfD vote, 2017')
p2

