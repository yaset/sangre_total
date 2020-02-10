rm(list = ls())
source("functions.R")

###Data
blood <- read_excel("data/Base de datos.xlsx")
blood <- read_excel("C:/Users/eyase/Downloads/Base de datos.xlsx")
summary(blood$`Leucos 0`)

###Hemoglobin
#data

Hb <- data_base("Hb")
resume(Hb)
anova_te(Hb)

##### 4 bolsas vs 1, almacenamiento, facilidad de colocar al paciente, el tiempo de transfusion, el numero de cateteres
##### mas riesgo de rx transfusionales menos donadores, facilidad de acceso, proyeccion de costos, ventajas y desventajas, almacenamiento 
##### 
blood$`TEG angle 1`
angle <- data_base("TEG angle ") 
angle <- angle %>%
  filter(ind != "TEG angle 21")
  

blood$`TEG MA 1`
M <- data_base("TEG MA")
M <- M %>%
  filter(ind != "TEG MA 21")

blood$`TEG R 1`
R <- data_base("TEG R")
R <- R %>%
  filter(ind != "TEG R 21")



resume(R)
anova_te(R)
anova_te(M)




resume(angle)
anova_te(angle)

resume(M)
anova_te(R)



data2 <- resume(angle)
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  xlim(c(0,16))+
  ylim(c(0,100))

p1
data2 <- resume(M)
p2 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  xlim(c(0,16))+
  ylim(c(0,100))


p1

data2 <- resume(R)
p3 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  xlim(c(0,16))+
  ylim(c(0,15))
  p1

resume(R)
gridExtra::grid.arrange(p1,p2,p3,nrow = 1)

p1  
xlab("Days")+
  ylab("Hemoglobin g/dL")+
  


###Graphic
data2 <- resume(Hb)
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,18))+
  xlab("Days")+
  ylab("Hemoglobin g/dL")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/hemoglobin.png",
    width = 750, height = 500)
p1
dev.off()

Hb <- data_base("Hb")
resume(Hb)
anova_te(Hb)


###Graphic

Hto <- data_base("Hcto")
data2 <- resume(Hto)
p1 <-ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,50))+
  xlab("Days")+
  ylab("Hematocrito")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/hcto.png",
    width = 750, height = 500)
p1
dev.off()


#Plaquetas
Plt <- data_base("Plt")
data2 <- resume(Plt)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,300000))+
  xlab("Days")+
  ylab("Plaquetas")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/plaquetas.png",
    width = 750, height = 500)
p1
dev.off()

#Fibrinogeno
Fibri <- data_base("Fibri")
data2 <- resume(Fibri)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,400))+
  xlab("Days")+
  ylab("Fibrinogeno")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/fibri.png",
    width = 750, height = 500)
p1
dev.off()

######Factores
##Factor II
FII <- data_base("FII")
data2 <- resume(FII)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,100))+
  xlab("Days")+
  ylab("Factor II")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FII.png",
    width = 750, height = 500)
p1
dev.off()

## Factor V
FV <- data_base("\\FV\\b")
data2 <- resume(FV)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,100))+
  xlab("Days")+
  ylab("Factor V")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FV.png",
    width = 750, height = 500)
p1
dev.off()

##Factor VII
FVII <- data_base("\\FVII\\b")
data2 <- resume(FVII)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,100))+
  xlab("Days")+
  ylab("Factor VII")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FVII.png",
    width = 750, height = 500)
p1
dev.off()

##Factor VIII
FVIII <- data_base("\\FVIII\\b")
data2 <- resume(FVIII)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,150))+
  xlab("Days")+
  ylab("Factor VIII")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FVIII.png",
    width = 750, height = 500)
p1
dev.off()

##Factor IX
FIX <- data_base("\\FIX\\b")
data2 <- resume(FIX)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,150))+
  xlab("Days")+
  ylab("Factor IX")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FIX.png",
    width = 750, height = 500)
p1
dev.off()

##Factor X
FX <- data_base("\\FX\\b")
data2 <- resume(FX)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,150))+
  xlab("Days")+
  ylab("Factor X")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FX.png",
    width = 750, height = 500)
p1
dev.off()



##Factor XI
FXI <- data_base("\\FXI\\b")
data2 <- resume(FXI)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,150))+
  xlab("Days")+
  ylab("Factor XI")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/FXI.png",
    width = 750, height = 500)
p1
dev.off()

##PCC
PCC <- data_base("\\PCC\\b")
data2 <- resume(PCC)
p1 <- ggplot( data = data2,mapping =  aes(x= time, y = mean))+
  geom_point(aes(y = mean))+
  geom_errorbar(aes(ymin = mean - es, ymax = mean + es), width = 2)+
  geom_line()+
  ylim(c(0,150))+
  xlab("Days")+
  ylab("PCC")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14))

png(filename = "figures/PCC.png",
    width = 750, height = 500)
p1
dev.off()
