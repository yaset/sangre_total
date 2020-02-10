library(ggplot2)
library(cowplot)


hb2$ind <- as.factor(hb2$ind)
levels(hb2$ind) <- c("0", "6", "11", "16", "21")
hl <- hb2 %>%
  group_by(ind) %>%
  summarise(me = median(values))

a1 <- ggplot(data = hb2)+
  geom_boxplot(aes(x = ind, y = values), width = 0.2, fill = "gray")+
  geom_point(data = hl, aes(x = ind, y = me), inherit.aes = FALSE)+
  geom_line(data = hl, aes(x = ind, y = me, group = 1), inherit.aes = FALSE)+
  ylim(c(0,18))+
  theme_cowplot()



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
  geom_point(data = FII_i, aes(x = ind, y = me), 
               alpha = 0.3, width = 0.1)+
  geom_line(data = FII_i, aes(x = ind, y = me, group = 1), inherit.aes = FALSE, color = "Red", size = 1,
            linetype = "dashed")+
  geom_point(data = FV_i, aes(x = ind, y = me), 
               alpha = 0.3,width = 0.1)+
  geom_line(data = FV_i, aes(x = ind, y = me, group = 1), inherit.aes = FALSE, color = "Blue", size = 1,
            linetype = "dashed")+
  geom_point(data = FVII_i, aes(x = ind, y = me), 
               alpha = 0.3,width = 0.1)+
    geom_line(data = FVII_i, aes(x = ind, y = me, group = 1), inherit.aes = FALSE, color = "Green", size = 1,
            linetype = "dashed")+
  
  ylim(c(0,100))

