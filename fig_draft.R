library(ggplot2)
library(cowplot)

#Plaquetas
Plt <- data_base("Plt")
levels(Plt$ind) <- c("0", "6", "11", "16", "21")
Plt2 <- Plt %>%
  group_by(ind) %>%
  summarise(me = median(values))

a2 <- ggplot(data = Plt)+
  geom_boxplot(aes(x= ind, y = values), width = 0.2, fill = "gray")+
  geom_line(data = Plt2, aes(x = ind, y = me, group = 1), inherit.aes = FALSE)+
  scale_y_continuous(breaks = seq(0,300000,by = 50000), limits = c(0,300000))+
  theme_cowplot()
  


a2

#Fibrinogeno
Fibri <- data_base("Fibri")

levels(Fibri$ind) <- c("0", "6", "11", "16", "21")
Fibri2 <- Fibri %>%
  group_by(ind) %>%
  summarise(me = median(values))

a3 <-ggplot(data = Fibri)+
  geom_boxplot(aes(x= ind, y = values), width = 0.2, fill = "gray")+
  geom_line(data = Fibri2, aes(x = ind, y = me, group = 1), inherit.aes = FALSE)+
  ylim(c(0,400))+
  theme_cowplot()
gridExtra::grid.arrange(a1,a2,a3, nrow = 1)



library(gridExtra)
aa <- grid.arrange(a1,b1,c1,d1,bottom = "Days")
grid.arrange(e1,aa, nrow = 1)
##Factor II
FII <- data_base("FII")

levels(FII$ind) <- c("0", "6", "11", "16", "21")
FII_i <- FII %>%
  group_by(ind) %>%
  summarise(me = median(values))


FV <- data_base("\\FV\\b")
levels(FV$ind) <- c("0", "6", "11", "16", "21")
FV_i <- FV %>%
  group_by(ind) %>%
  summarise(me = median(values))

FVII <- data_base("\\FVII\\b")
levels(FVII$ind) <- c("0", "6", "11", "16", "21")
FVII_i <- FVII %>%
  group_by(ind) %>%
  summarise(me = median(values))



ggplot()+
  geom_boxplot(data = FII, aes(x = ind, y = values), width = 0.2)+
  geom_point(data = FII_i, aes(x = ind, y = me))+
  xlab("Factor II")+
  ylim(c(0,100))
 
ggplot()+
  geom_point(data = FII_i, aes(x = ind, y = me), 
             alpha = 0.3, width = 0.1)+
  geom_boxplot(data = FII, aes(x = ind, y = values), width = 0.2)+
  xlab("Factor II")+
  ylim(c(0,100))
