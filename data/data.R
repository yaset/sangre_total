
library(tidyverse)
library(readxl)
library(plotrix)
blood <- read_excel("data/Base de datos.xlsx")



data1 <- blood %>%
  select(`Hb 0`, `Hb 6`,
         `Hb 11`, `Hb 16`,
         `Hb 21`)
data1 <- stack(data1)

data1$time[data1$ind == "Hb 0"] <- 0
data1$time[data1$ind == "Hb 6"] <- 6
data1$time[data1$ind == "Hb 11"] <- 11
data1$time[data1$ind == "Hb 16"] <- 16
data1$time[data1$ind == "Hb 21"] <- 21


data2 <- data1 %>%
  group_by(time) %>%
  summarise(mean = mean(values), sd = sd(values), es = std.error(values),
            min = min(values), max = max(values), norm = shapiro.test(values)[[2]])


ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es),
                width = 0.1)+
  geom_point()+
  geom_line()+
  ylim(c(10,16))+
  xlab("Days")+
  ylab("Hemoglobin")+
  theme_bw()
 
a <- glm(V1 ~ V2, data = data1)





###Los grados de libertad no son aca deberia ser K = 1-n
aov(V1 ~ V2, data = data1)
summary(aov(V1 ~ V2, data = data1))

###correction de factores
summary(aov(V1 ~ factor(V2), data = data1))
anova(lm(V1 ~ factor(V2), data = data1))

##-------------------------------

blood <- as.data.frame()


anova(lm(values ~ ind, data = Hb))













######Hemoglobina

blood %>%
  summarise( h0 = mean(`Hb 0`), h0sd = sd(`Hb 0`),
             h6 = mean(`Hb 6`), h6sd = sd(`Hb 6`),
             h11 = mean(`Hb 11`), h11sd = sd(`Hb 11`),
             h16 = mean(`Hb 16`), h16sd = sd(`Hb 16`),
             h21 = mean(`Hb 21`), h21sd = sd(`Hb 21`),)

a <- blood$`Plt 0`
ks.test(a, "pnorm", mean = mean(a, na.rm = TRUE), sd = sd(a, na.rm = TRUE))

a1 <- ggplot(data = blood, aes(y = `Hb 0` ))+
  geom_boxplot()+
  ylim(c(10,20))
a2 <- ggplot(data = blood, aes(y = `Hb 6` ))+
  geom_boxplot()+
  ylim(c(10,20)) 
a3 <- ggplot(data = blood, aes(y = `Hb 11` ))+
  geom_boxplot()+
  ylim(c(10,20))
a4 <- ggplot(data = blood, aes(y = `Hb 16` ))+
  geom_boxplot()+
  ylim(c(10,20))
a5 <- ggplot(data = blood, aes(y = `Hb 21` ))+
  geom_boxplot()+
  ylim(c(10,20))



png(filename = "figures/hb.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()


#plaquetas

blood %>%
  summarise( h0 = mean(`Plt 0`), h0sd = sd(`Plt 0`),
             h6 = mean(`Plt 6`), h6sd = sd(`Plt 6`),
             h11 = mean(`Plt 11`), h11sd = sd(`Plt 11`),
             h16 = mean(`Plt 16`), h16sd = sd(`Plt 16`),
             h21 = mean(`Plt 21`), h21sd = sd(`Plt 21`),)



a <- blood$`Plt 0`
ks.test(a, "pnorm", mean = mean(a, na.rm = TRUE), sd = sd(a, na.rm = TRUE))


min(c(blood$`Plt 0`, blood$`Plt 6`, blood$`Plt 11`,
      blood$`Plt 16`, blood$`Plt 21`))
max(c(blood$`Plt 0`, blood$`Plt 6`, blood$`Plt 11`,
      blood$`Plt 16`, blood$`Plt 21`))


a1 <- ggplot(data = blood, aes(y = `Plt 0` ))+
  geom_boxplot()+
  ylim(c(20000,300000))
a2 <- ggplot(data = blood, aes(y = `Plt 6` ))+
  geom_boxplot()+
  ylim(c(20000,300000))
a3 <- ggplot(data = blood, aes(y = `Plt 11` ))+
  geom_boxplot()+
  ylim(c(20000,300000))
a4 <- ggplot(data = blood, aes(y = `Plt 16` ))+
  geom_boxplot()+
  ylim(c(20000,300000))
a5 <- ggplot(data = blood, aes(y = `Plt 21` ))+
  geom_boxplot()+
  ylim(c(20000,300000))



png(filename = "figures/plt.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()


# Fibrinogeno
blood$`Fibri 0`

blood %>%
  summarise( h0 = mean(`Fibri 0`), h0sd = sd(`Fibri 0`),
             h6 = mean(`Fibri 6`), h6sd = sd(`Fibri 6`),
             h11 = mean(`Fibri 11`), h11sd = sd(`Fibri 11`),
             h16 = mean(`Fibri 16`), h16sd = sd(`Fibri 16`),
             h21 = mean(`Fibri 21`), h21sd = sd(`Fibri 21`),)



a <- blood$`Fibri 0`
ks.test(a, "pnorm", mean = mean(a, na.rm = TRUE), sd = sd(a, na.rm = TRUE))


min(c(blood$`Fibri 0`, blood$`Fibri 6`, blood$`Fibri 11`,
      blood$`Fibri 16`, blood$`Fibri 21`))
max(c(blood$`Fibri 0`, blood$`Fibri 6`, blood$`Fibri 11`,
      blood$`Fibri 16`, blood$`Fibri 21`))


a1 <- ggplot(data = blood, aes(y = `Fibri 0` ))+
  geom_boxplot()+
  ylim(c(150,400))
a2 <- ggplot(data = blood, aes(y = `Fibri 6` ))+
  geom_boxplot()+
  ylim(c(150,400))
a3 <- ggplot(data = blood, aes(y = `Fibri 11` ))+
  geom_boxplot()+
  ylim(c(150,400))
a4 <- ggplot(data = blood, aes(y = `Fibri 16` ))+
  geom_boxplot()+
  ylim(c(150,400))
a5 <- ggplot(data = blood, aes(y = `Fibri 21` ))+
  geom_boxplot()+
  ylim(c(150,400))



png(filename = "figures/Fibri.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()

######Factores
##Factor II
blood$`FII 0`


blood %>%
  summarise( h0 = mean(`FII 0`), h0sd = sd(`FII 0`),
             h6 = mean(`FII 6`), h6sd = sd(`FII 6`),
             h11 = mean(`FII 11`), h11sd = sd(`FII 11`),
             h16 = mean(`FII 16`), h16sd = sd(`FII 16`),
             h21 = mean(`FII 21`), h21sd = sd(`FII 21`),)



a <- blood$`FII 0`
shapiro.test(a)


min(c(blood$`FII 0`, blood$`FII 6`, blood$`FII 11`,
      blood$`FII 16`, blood$`FII 21`))
max(c(blood$`FII 0`, blood$`FII 6`, blood$`FII 11`,
      blood$`FII 16`, blood$`FII 21`))


a1 <- ggplot(data = blood, aes(y = `FII 0` ))+
  geom_boxplot()+
  ylim(c(0,180))
a2 <- ggplot(data = blood, aes(y = `FII 6` ))+
  geom_boxplot()+
  ylim(c(0,180))
a3 <- ggplot(data = blood, aes(y = `FII 11` ))+
  geom_boxplot()+
  ylim(c(0,180))
a4 <- ggplot(data = blood, aes(y = `FII 16` ))+
  geom_boxplot()+
  ylim(c(0,180))
a5 <- ggplot(data = blood, aes(y = `FII 21` ))+
  geom_boxplot()+
  ylim(c(0,180))



png(filename = "figures/FII.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()

##Factor II
blood$`FV 0`


blood %>%
  summarise( h0 = mean(`FV 0`), h0sd = sd(`FV 0`),
             h6 = mean(`FV 6`), h6sd = sd(`FV 6`),
             h11 = mean(`FV 11`), h11sd = sd(`FV 11`),
             h16 = mean(`FV 16`), h16sd = sd(`FV 16`),
             h21 = mean(`FV 21`), h21sd = sd(`FV 21`),)



a <- blood$`FV 0`
shapiro.test(a)


min(c(blood$`FV 0`, blood$`FV 6`, blood$`FV 11`,
      blood$`FV 16`, blood$`FV 21`))
max(c(blood$`FV 0`, blood$`FV 6`, blood$`FV 11`,
      blood$`FV 16`, blood$`FV 21`))


a1 <- ggplot(data = blood, aes(y = `FV 0` ))+
  geom_boxplot()+
  ylim(c(0,180))
a2 <- ggplot(data = blood, aes(y = `FV 6` ))+
  geom_boxplot()+
  ylim(c(0,180))
a3 <- ggplot(data = blood, aes(y = `FV 11` ))+
  geom_boxplot()+
  ylim(c(0,180))
a4 <- ggplot(data = blood, aes(y = `FV 16` ))+
  geom_boxplot()+
  ylim(c(0,180))
a5 <- ggplot(data = blood, aes(y = `FV 21` ))+
  geom_boxplot()+
  ylim(c(0,180))



png(filename = "figures/FV.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()

##Factor VII
blood$`FVII 0`


blood %>%
  summarise( h0 = mean(`FVII 0`), h0sd = sd(`FVII 0`),
             h6 = mean(`FVII 6`), h6sd = sd(`FVII 6`),
             h11 = mean(`FVII 11`), h11sd = sd(`FVII 11`),
             h16 = mean(`FVII 16`), h16sd = sd(`FVII 16`),
             h21 = mean(`FVII 21`), h21sd = sd(`FVII 21`))



a <- blood$`FVII 0`
shapiro.test(a)


min(c(blood$`FVII 0`, blood$`FVII 6`, blood$`FVII 11`,
      blood$`FVII 16`, blood$`FVII 21`))
max(c(blood$`FVII 0`, blood$`FVII 6`, blood$`FVII 11`,
      blood$`FVII 16`, blood$`FVII 21`))


a1 <- ggplot(data = blood, aes(y = `FVII 0` ))+
  geom_boxplot()+
  ylim(c(0,350))
a2 <- ggplot(data = blood, aes(y = `FVII 6` ))+
  geom_boxplot()+
  ylim(c(0,350))
a3 <- ggplot(data = blood, aes(y = `FVII 11` ))+
  geom_boxplot()+
  ylim(c(0,350))
a4 <- ggplot(data = blood, aes(y = `FVII 16` ))+
  geom_boxplot()+
  ylim(c(0,350))
a5 <- ggplot(data = blood, aes(y = `FVII 21` ))+
  geom_boxplot()+
  ylim(c(0,350))



png(filename = "figures/FVII.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()


##Factor VIII
blood$`FVII 0`


blood %>%
  summarise( h0 = mean(`FVIII 0`), h0sd = sd(`FVIII 0`),
             h6 = mean(`FVIII 6`), h6sd = sd(`FVIII 6`),
             h11 = mean(`FVIII 11`), h11sd = sd(`FVIII 11`),
             h16 = mean(`FVIII 16`), h16sd = sd(`FVIII 16`),
             h21 = mean(`FVIII 21`), h21sd = sd(`FVIII 21`))



a <- blood$`FVIII 0`
shapiro.test(a)


min(c(blood$`FVIII 0`, blood$`FVIII 6`, blood$`FVIII 11`,
      blood$`FVIII 16`, blood$`FVIII 21`))
max(c(blood$`FVIII 0`, blood$`FVIII 6`, blood$`FVIII 11`,
      blood$`FVIII 16`, blood$`FVIII 21`))


a1 <- ggplot(data = blood, aes(y = `FVIII 0` ))+
  geom_boxplot()+
  ylim(c(0,200))
a2 <- ggplot(data = blood, aes(y = `FVIII 6` ))+
  geom_boxplot()+
  ylim(c(0,200))
a3 <- ggplot(data = blood, aes(y = `FVIII 11` ))+
  geom_boxplot()+
  ylim(c(0,200))
a4 <- ggplot(data = blood, aes(y = `FVIII 16` ))+
  geom_boxplot()+
  ylim(c(0,200))
a5 <- ggplot(data = blood, aes(y = `FVIII 21` ))+
  geom_boxplot()+
  ylim(c(0,200))



png(filename = "figures/FVIII.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()

##Factor IX
blood$`FIX 6`


blood %>%
  summarise( h0 = mean(`FIX 0`), h0sd = sd(`FIX 0`),
             h6 = mean(`FIX 6`), h6sd = sd(`FIX 6`),
             h11 = mean(`FIX 11`), h11sd = sd(`FIX 11`),
             h16 = mean(`FIX 16`), h16sd = sd(`FIX 16`),
             h21 = mean(`FIX 21`), h21sd = sd(`FIX 21`))



a <- blood$`FIX 0`
shapiro.test(a)


min(c(blood$`FIX 0`, blood$`FIX 6`, blood$`FIX 11`,
      blood$`FIX 16`, blood$`FIX 21`))
max(c(blood$`FIX 0`, blood$`FIX 6`, blood$`FIX 11`,
      blood$`FIX 16`, blood$`FIX 21`))


a1 <- ggplot(data = blood, aes(y = `FIX 0` ))+
  geom_boxplot()+
  ylim(c(0,200))
a2 <- ggplot(data = blood, aes(y = `FIX 6` ))+
  geom_boxplot()+
  ylim(c(0,200))
a3 <- ggplot(data = blood, aes(y = `FIX 11` ))+
  geom_boxplot()+
  ylim(c(0,200))
a4 <- ggplot(data = blood, aes(y = `FIX 16` ))+
  geom_boxplot()+
  ylim(c(0,200))
a5 <- ggplot(data = blood, aes(y = `FIX 21` ))+
  geom_boxplot()+
  ylim(c(0,200))



png(filename = "figures/FIX.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()

##Factor X
blood$`FIX 6`


blood %>%
  summarise( h0 = mean(`FX 0`), h0sd = sd(`FX 0`),
             h6 = mean(`FX 6`), h6sd = sd(`FX 6`),
             h11 = mean(`FX 11`), h11sd = sd(`FX 11`),
             h16 = mean(`FX 16`), h16sd = sd(`FX 16`),
             h21 = mean(`FX 21`), h21sd = sd(`FX 21`))



a <- blood$`FX 0`
shapiro.test(a)


min(c(blood$`FX 0`, blood$`FX 6`, blood$`FX 11`,
      blood$`FX 16`, blood$`FX 21`))
max(c(blood$`FX 0`, blood$`FX 6`, blood$`FX 11`,
      blood$`FX 16`, blood$`FX 21`))


a1 <- ggplot(data = blood, aes(y = `FX 0` ))+
  geom_boxplot()+
  ylim(c(0,200))
a2 <- ggplot(data = blood, aes(y = `FX 6` ))+
  geom_boxplot()+
  ylim(c(0,200))
a3 <- ggplot(data = blood, aes(y = `FX 11` ))+
  geom_boxplot()+
  ylim(c(0,200))
a4 <- ggplot(data = blood, aes(y = `FX 16` ))+
  geom_boxplot()+
  ylim(c(0,200))
a5 <- ggplot(data = blood, aes(y = `FX 21` ))+
  geom_boxplot()+
  ylim(c(0,200))



png(filename = "figures/FX.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()



##Factor XI
blood$`FIX 6`


blood %>%
  summarise( h0 = mean(`FXI 0`), h0sd = sd(`FXI 0`),
             h6 = mean(`FXI 6`), h6sd = sd(`FXI 6`),
             h11 = mean(`FXI 11`), h11sd = sd(`FXI 11`),
             h16 = mean(`FXI 16`), h16sd = sd(`FXI 16`),
             h21 = mean(`FXI 21`), h21sd = sd(`FXI 21`))



a <- blood$`FXI 0`
shapiro.test(a)


min(c(blood$`FXI 0`, blood$`FXI 6`, blood$`FXI 11`,
      blood$`FXI 16`, blood$`FXI 21`))
max(c(blood$`FXI 0`, blood$`FXI 6`, blood$`FXI 11`,
      blood$`FXI 16`, blood$`FXI 21`))


a1 <- ggplot(data = blood, aes(y = `FXI 0` ))+
  geom_boxplot()+
  ylim(c(0,200))
a2 <- ggplot(data = blood, aes(y = `FXI 6` ))+
  geom_boxplot()+
  ylim(c(0,200))
a3 <- ggplot(data = blood, aes(y = `FXI 11` ))+
  geom_boxplot()+
  ylim(c(0,200))
a4 <- ggplot(data = blood, aes(y = `FXI 16` ))+
  geom_boxplot()+
  ylim(c(0,200))
a5 <- ggplot(data = blood, aes(y = `FXI 21` ))+
  geom_boxplot()+
  ylim(c(0,200))



png(filename = "figures/FXI.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()


##Factor PCR
blood$`FIX 6`


blood %>%
  summarise( h0 = mean(`FXI 0`), h0sd = sd(`FXI 0`),
             h6 = mean(`FXI 6`), h6sd = sd(`FXI 6`),
             h11 = mean(`FXI 11`), h11sd = sd(`FXI 11`),
             h16 = mean(`FXI 16`), h16sd = sd(`FXI 16`),
             h21 = mean(`FXI 21`), h21sd = sd(`FXI 21`))



a <- blood$`FXI 0`
shapiro.test(a)


min(c(blood$`FXI 0`, blood$`FXI 6`, blood$`FXI 11`,
      blood$`FXI 16`, blood$`FXI 21`))
max(c(blood$`FXI 0`, blood$`FXI 6`, blood$`FXI 11`,
      blood$`FXI 16`, blood$`FXI 21`))


a1 <- ggplot(data = blood, aes(y = `FXI 0` ))+
  geom_boxplot()+
  ylim(c(0,200))
a2 <- ggplot(data = blood, aes(y = `FXI 6` ))+
  geom_boxplot()+
  ylim(c(0,200))
a3 <- ggplot(data = blood, aes(y = `FXI 11` ))+
  geom_boxplot()+
  ylim(c(0,200))
a4 <- ggplot(data = blood, aes(y = `FXI 16` ))+
  geom_boxplot()+
  ylim(c(0,200))
a5 <- ggplot(data = blood, aes(y = `FXI 21` ))+
  geom_boxplot()+
  ylim(c(0,200))



png(filename = "figures/FXI.png",
    width = 750, height = 500)
gridExtra::grid.arrange(a1,a2,a3,a4,a5, nrow= 1)
dev.off()