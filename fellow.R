library(tidyverse)
library(ggrepel)


ayt20_sindex <- read_csv("../../../Music/sel_ayt45.csv") %>%
  janitor::clean_names() %>% 
  select(accession_name:sindex)


skimr::skim(ayt20_sindex)

colnames(ayt20_sindex %>% select_if(is.numeric))


# 
# ayt20s_long <- ayt20_sindex %>% 
#   select(-c(l,a,b)) %>% 
#   select(accession_name,fyld,dyld,dm,shtwt,everything()) %>% 
#   pivot_longer(-c(accession_name,sindex), names_to = "traits", values_to = "values")
# 
# View(ayt20s_long)


checks <- c("IITA-TMS-IBA000070","TMS18F1139P0088", "TMS19F12190091", "TMS14F1036P0007", "TMEB419", "IITA-TMS-IBA30572")


ayt20_sindex <- ayt20_sindex %>% mutate(categor_y = if_else(accession_name %in% checks, "check","selection"))
colnames(ayt20_sindex)

View(ayt20_sindex)

ayt20_sindex$accession_name
ayt20_sindex %>% ggplot(aes(x = dm, y = fyld, label = accession_name)) +
  geom_point(aes(colour = sindex, size = dm)) +
  geom_text(aes(colour = factor(categor_y))) +
  scale_color_gradient(low = "red", high = "blue") +
  geom_text(nudge_x = 0.05, hjust = 0, size = 4) +
  geom_smooth(se = FALSE, method = lm, fullrange = FALSE) +
  theme_light()


ayt20_sindex %>% ggplot(aes(x = dm, y = fyld, label = accession_name)) +
  geom_point(aes(colour = categor_y, size = sindex)) +
  geom_smooth(se = FALSE, method = lm, fullrange = FALSE) +
  geom_text(data = subset(ayt20_sindex, sindex > 80),vjust = 0, nudge_y = .2)+
  theme_minimal()
