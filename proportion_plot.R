# http://www.loc.gov/pictures/resource/ppmsca.33913/?co=anedub
library(tidyverse)
t1 <- read_csv("dat/nhgis0003_csv/nhgis0003_ds1_1790_nation.csv")
#AAQ001:      Non-White: Free
#AAQ002:      Non-White: Slave

t11 <- c("year" = 1790, "free" = t1$AAQ001, "slave" = t1$AAQ002)
  
glimpse(t1)
t2 <- read_csv("dat/nhgis0003_csv/nhgis0003_ds2_1800_nation.csv")
#AAY001:      Free
#AAY002:      Slave
t21 <- c("year" = 1800, "free" = t2$AAY001, "slave" = t2$AAY002)

t3 <- read_csv("dat/nhgis0005_csv/nhgis0005_ds3_1810_nation.csv")
# AA7001:      Free
# AA7002:      Slave
glimpse(t3)
t31 <- c("year" = 1810, "free" = t3$AA7001, "slave" = t3$AA7002)

t4 <- read_csv("dat/nhgis0005_csv/nhgis0005_ds4_1820_nation.csv")
glimpse(t4)
# ABB003:      Nonwhite: Slave >> Male
# ABB004:      Nonwhite: Slave >> Female
# ABB005:      Nonwhite: Free >> Male
# ABB006:      Nonwhite: Free >> Female
t41 <- c("year" = 1820, "free" = t4$ABB005 + t4$ABB006, "slave" = t4$ABB003 + t4$ABB004)

t5 <- read_csv("dat/nhgis0005_csv/nhgis0005_ds5_1830_nation.csv")
glimpse(t5)
# ABQ001:      Slave
# ABQ002:      Free
t51 <- c("year" = 1830, "free" = t5$ABQ002, "slave" = t5$ABQ001)

t6 <- read_csv("dat/nhgis0005_csv/nhgis0005_ds7_1840_nation.csv")
# ACS001:      White
# ACS002:      Nonwhite: Free
# ACS003:      Nonwhite: Slave
t61 <- c("year" = 1840, "free" = t6$ACS002, "slave" = t6$ACS003)

t7 <- read_csv("dat/nhgis0005_csv/nhgis0005_ds10_1850_nation.csv")
head(t7)
# AE6001:      White
# AE6002:      Nonwhite: Free
# AE6003:      Nonwhite: Slave
t71 <- c("year" = 1850, "free" = t7$AE6002, "slave" = t7$AE6003)

t8 <- read_csv("dat/nhgis0005_csv/nhgis0005_ds14_1860_nation.csv")
head(t8)
# AH3001:      White
# AH3002:      Free colored
# AH3003:      Slave
# AH3004:      Indian
# AH3005:      Half breed
# AH3006:      Asiatic
t81 <- c("year" = 1860, "free" = t8$AH3002, "slave" = t8$AH3003)

t9 <- read_csv("dat/nhgis0004_csv/nhgis0004_ds17_1870_county.csv")
# AK3002:      Colored

t91 <- c("year" = 1870, "free" = sum(t9$AK3002, na.rm = T), "slave" = 0)

dat <- data.frame(rbind(t11, t21, t31, t41, t51, t61, t71, t81, t91))
dat

figdat <- dat %>% gather(status, pop, free:slave) %>% 
  group_by(year) %>% mutate(total_pop = sum(pop), perc = pop/total_pop) %>% ungroup
ggplot() + 
  geom_area(data = figdat, aes(x = year, y = pop, fill = status), position = 'fill') + 
  geom_text(data = figdat %>% filter(status == "free"), hjust = -.1,vjust = -1,size = 3,
            aes(x = year, y = 1-perc, label = paste0(round(100*perc,1), "%"))) + 
  scale_fill_manual(values = c("forestgreen", "black")) + 
  scale_x_continuous(breaks = seq(1790,1870, 10), position = "top") + 
  ggtitle("Proportion of Freemen and Slaves among Black Americans")
