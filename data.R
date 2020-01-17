rm(list = ls())
library(tidyverse)
library(readxl)
library(plotrix)
library(rstatix)
library(multcompView)

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
            var = var(values),
            min = min(values), max = max(values), norm = shapiro.test(values)[[2]])

data2
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 1.2)+
  geom_line()+
  ylim(c(0,18))+
  xlab("Days")+
  ylab("Hemoglobin")

png(filename = "figures/hemoglobin.png",
    width = 750, height = 500)
p1
dev.off()

model <- lm(values ~ factor(ind), data = data1) 
anova(model)


#Plaquetas

data1 <- blood %>%
  select(`Plt 0`, `Plt 6`,
         `Plt 11`, `Plt 16`,
         `Plt 21`)

data1 <- stack(data1)

data1$time[data1$ind == "Plt 0"] <- 0
data1$time[data1$ind == "Plt 6"] <- 6
data1$time[data1$ind == "Plt 11"] <- 11
data1$time[data1$ind == "Plt 16"] <- 16
data1$time[data1$ind == "Plt 21"] <- 21


data2 <- data1 %>%
  group_by(time) %>%
  summarise(mean = mean(values), sd = sd(values), es = std.error(values),
            var = var(values),
            min = min(values), max = max(values), norm = shapiro.test(values)[[2]])

data2
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 1.2)+
  geom_line()+
  ylim(c(0,250000))+
  xlab("Days")+
  ylab("Plaquetas")

png(filename = "figures/plaquetas.png",
    width = 750, height = 500)
p1
dev.off()

model <- lm(values ~ ind, data = data1) 
anov <- aov(model)

TukeyHSD(anov)
dunn_test(formula = values ~ ind, data = data1, p.adjust.method = "bonferroni")

# Fibrinogeno

data1 <- blood %>%
  select(`Fibri 0`, `Fibri 6`,
         `Fibri 11`, `Fibri 16`,
         `Fibri 21`)

data1 <- stack(data1)

data1$time[data1$ind == "Fibri 0"] <- 0
data1$time[data1$ind == "Fibri 6"] <- 6
data1$time[data1$ind == "Fibri 11"] <- 11
data1$time[data1$ind == "Fibri 16"] <- 16
data1$time[data1$ind == "Fibri 21"] <- 21


data2 <- data1 %>%
  group_by(time) %>%
  summarise(mean = mean(values), sd = sd(values), es = std.error(values),
            var = var(values),
            min = min(values), max = max(values), norm = shapiro.test(values)[[2]])

data2
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 1.2)+
  geom_line()+
  ylim(c(0,300))+
  xlab("Days")+
  ylab("Plaquetas")

png(filename = "figures/fibrinogeno.png",
    width = 750, height = 500)
p1
dev.off()

model <- lm(values ~ ind, data = data1) 
anov <- aov(model)
summary(anov)

TukeyHSD(anov)
dunn_test(formula = values ~ ind, data = data1, p.adjust.method = "bonferroni")


######Factores
##Factor II

data1 <- blood %>%
  select(`FII 0`, `FII 6`,
         `FII 11`, `FII 16`,
         `FII 21`)

data1 <- stack(data1)

data1$time[data1$ind == "FII 0"] <- 0
data1$time[data1$ind == "FII 6"] <- 6
data1$time[data1$ind == "FII 11"] <- 11
data1$time[data1$ind == "FII 16"] <- 16
data1$time[data1$ind == "FII 21"] <- 21


data2 <- data1 %>%
  group_by(time) %>%
  summarise(mean = mean(values), sd = sd(values), es = std.error(values),
            var = var(values),
            min = min(values), max = max(values), norm = shapiro.test(values)[[2]])

data2
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = log(mean)))+
  geom_point(aes(y = log(mean)))+
  geom_errorbar(aes(ymin = log(mean) - log(es), ymax = log(mean) + log(es)), width = 1.2)+
  geom_line()+
  xlab("Days")+
  ylab("Log Factor II")+
  ylim(c(0,7))

png(filename = "figures/FII.png",
    width = 750, height = 500)
p1
dev.off()

data1$logvalues <- log(data1$values)

model <- lm(logvalues ~ ind, data = data1) 
anov <- aov(model)
summary(anov)

data1$sujeto <- factor(rep(1:length(data1$values)))


kruskal.test(log(values) ~ ind, data = data1)
TukeyHSD(data = data1, formula = logvalues ~ ind)
dunn_test(formula = logvalues ~ ind, data = data1, p.adjust.method = "bonferroni")





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