# More fun with the statistical atlas! To recreate: https://www.loc.gov/resource/g3701gm.gct00010/?sp=10

# data from the nhgis site that Dr. Hofmann cited in Graphics Group

library(tidyverse)
library(stringr)
# make the state symbols the state flag just because that's a thing someone wrote a package to do! 
devtools::install_github("baptiste/ggflags")
library(ggflags)

dat1 <- read_csv("dat/nhgis0001_ts_nominal_state.csv")
head(dat)

dat1 %>% gather(Census, Population, A00AA1790:A00AA2010) %>% 
  mutate(Census = as.integer(str_replace(Census,"A00AA", "")),
         STATE = str_trim(str_replace(STATE, "Territory", ""))) %>% 
  group_by(Census, STATE) %>% 
  summarize(TotPop = sum(na.omit(Population))) %>% 
  filter(TotPop != 0) %>%
  arrange(Census, desc(TotPop)) %>% 
  mutate(rank = row_number()) -> dat1.2

territories <- data.frame(name1 = unique(dat1.2$STATE)[is.na(match(unique(dat1.2$STATE), state.name))], 
                          stringsAsFactors = F)
territories$name2 <- paste(territories$name1, "Territory")
territories$name2[3] <- territories$name1[3]

for (i in 1:6){
  dat1.2$STATE[dat1.2$STATE == territories$name1[i]] <- territories$name2[i]
}

states <- data.frame(name = state.name, abb = state.abb, stringsAsFactors = F)
dat1.2$state.abbr <- NA

for (i in 1:nrow(states)){
  dat1.2$state.abbr[dat1.2$STATE == states$name[i]] <- tolower(states$abb[i])
}
dat1.2$state.abbr[dat1.2$STATE == "District Of Columbia"] <- "dc"

dat1.2$state <- paste0("us-", dat1.2$state.abbr)

figdat <-  dat1.2 %>% filter(Census < 1891 & state %in% names(.flaglist))

ggplot() + 
  geom_line(data = figdat, aes(x = -Census, y = -rank, group = STATE)) + 
  geom_flag(data = figdat, aes(x = -Census, y = -rank, country = state), size = 6) + 
  geom_text(aes(x = -seq(1790, 1890, 10), y = 0, label = seq(1790, 1890, 10))) + 
  geom_text(data = figdat %>% filter(Census ==1890), aes(x = -1900, y = -rank, label = STATE), hjust = 0, size = 3) + 
  geom_text(data = figdat %>% filter(Census ==1790), aes(x = -1780, y = -rank, label = STATE), hjust = 1, size = 3) + 
  scale_country() + 
  theme_void() + 
  theme(legend.position = 'none')

library(RColorBrewer)
brewer.pal()
devtools::install_github("karthik/wesanderson")
library(wesanderson)
cols <-  colorRampPalette(wes_palette("GrandBudapest1", n=4))(49)
wes_palettes

ggplot() + 
  geom_line(data = figdat, aes(x = -Census, y = -rank, group = STATE)) + 
  geom_label(data = figdat, aes(x = -Census, y = -rank, fill = STATE, label = STATE), color = 'black', size = 2) + 
  scale_fill_manual(values = cols) + 
  geom_text(aes(x = -seq(1790, 1890, 10), y = 0, label = seq(1790, 1890, 10))) + 
  geom_text(data = figdat %>% filter(Census ==1890), aes(x = -1900, y = -rank, label = STATE), hjust = 0, size = 3) + 
  geom_text(data = figdat %>% filter(Census ==1790), aes(x = -1780, y = -rank, label = STATE), hjust = 1, size = 3) + 
  theme_void() + 
  theme(legend.position = 'none')
