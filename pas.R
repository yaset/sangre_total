rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(cutpointr)


data <- read_excel("C:/Users/eyase/Downloads/REBOA_REGISTRO_2020-01-27_registros_1-419.xlsx")



table(data$`CLAMPEO AORTA`)


str(data)

data <- data %>%
  filter(`CLAMPEO AORTA` == "2" | `CLAMPEO AORTA` == "1")

reboa <- data


reboa1 <- reboa %>%
  select(`PAS INICIAL`, `MORT INTQX`,`MECANISMO TX`)

reboa1 <- reboa1 %>%
  rename(pas1 = `PAS INICIAL`, morta_qx = `MORT INTQX`,
         meca_tx = `MECANISMO TX`)

reboa1$morta_qx[reboa1$morta_qx == "NA"] <- "0"

summary(reboa1$pas1)
table(reboa1$morta_qx)
table(reboa1$meca_tx)


reboa1$morta_qx <- as.factor(reboa1$morta_qx)
levels(reboa1$morta_qx) <- c("no", "yes")

reboa1$pas1 <- as.numeric(reboa1$pas1)

reboa1 <- reboa1 %>%
  mutate(p10 = ifelse(pas1 <= 10,1,0),
         p20 = ifelse(pas1 <= 20,1,0),
         p30 = ifelse(pas1 <= 30,1,0),
         p40 = ifelse(pas1 <= 40,1,0),
         p50 = ifelse(pas1 <= 50,1,0),
         p60 = ifelse(pas1 <= 60,1,0),
         p70 = ifelse(pas1 <= 70,1,0),
         p80 = ifelse(pas1 <= 80,1,0),
         p90 = ifelse(pas1 <= 90,1,0),
         p100 = ifelse(pas1 <= 100,1,0),
         p110 = ifelse(pas1 <= 110,1,0),
         p120 = ifelse(pas1 <= 120,1,0),
         p130 = ifelse(pas1 <= 130,1,0),
         p140 = ifelse(pas1 <= 140,1,0),
         p150 = ifelse(pas1 <= 150,1,0),
         )

a10 <- table(reboa1$p10, reboa1$morta_qx)
a20 <- table(reboa1$p20, reboa1$morta_qx)
a30 <- table(reboa1$p30, reboa1$morta_qx)
a40 <- table(reboa1$p40, reboa1$morta_qx)
a50 <- table(reboa1$p50, reboa1$morta_qx)
a60 <- table(reboa1$p60, reboa1$morta_qx)
a70 <- table(reboa1$p70, reboa1$morta_qx)
a80 <- table(reboa1$p80, reboa1$morta_qx)
a90 <- table(reboa1$p90, reboa1$morta_qx)
a100 <- table(reboa1$p100, reboa1$morta_qx)
a110 <- table(reboa1$p110, reboa1$morta_qx)
a120 <- table(reboa1$p120, reboa1$morta_qx)
a130 <- table(reboa1$p130, reboa1$morta_qx)
a140 <- table(reboa1$p140, reboa1$morta_qx)
a150 <- table(reboa1$p150, reboa1$morta_qx)

id <- seq(10,140, by = 10)
tab <- as.data.frame(id)
tab$tp[1] <- a10[4] 
tab$tp[2] <- a20[4] 
tab$tp[3] <- a30[4] 
tab$tp[4] <- a40[4] 
tab$tp[5] <- a50[4] 
tab$tp[6] <- a60[4] 
tab$tp[7] <- a70[4] 
tab$tp[8] <- a80[4] 
tab$tp[9] <- a90[4] 
tab$tp[10] <- a100[4] 
tab$tp[11] <- a110[4] 
tab$tp[12] <- a120[4] 
tab$tp[13] <- a130[4] 
tab$tp[14] <- a140[4] 



tab$tpp[1] <- (a10[4] + a10[2])
tab$tpp[2] <- (a20[4] + a20[2])
tab$tpp[3] <- (a30[4] + a30[2])
tab$tpp[4] <- (a40[4] + a40[2])
tab$tpp[5] <- (a50[4] + a50[2])
tab$tpp[6] <- (a60[4] + a60[2])
tab$tpp[7] <- (a70[4] + a70[2])
tab$tpp[8] <- (a80[4] + a80[2])
tab$tpp[9] <- (a90[4] + a90[2])
tab$tpp[10] <- (a100[4] + a100[2])
tab$tpp[11] <- (a110[4] + a110[2])
tab$tpp[12] <- (a120[4] + a120[2])
tab$tpp[13] <- (a130[4] + a130[2])
tab$tpp[14] <- (a140[4] + a140[2])

tab <- tab %>%
  filter(id != 150)

conf <- Hmisc::binconf(tab$tp,tab$tpp)
tab <- cbind(tab,conf)


ggplot(data = tab, aes(x = id, y = PointEst))+
  geom_line()+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, fill = "Red")+
  scale_x_continuous(breaks = seq(10,140,by = 10))+
  scale_y_continuous(breaks = seq(0,1,by = 0.05), limits = c(0,1))
  geom_rect(aes(xmin = 60, xmax = 70, ymin = 0, ymax = 1 ), alpha = 0.1)

ggplot(data = tab, aes(x = id, y = exp(PointEst)))+
    geom_line()+
    ylim(c(1,2))
  
    
  
  
cp1 <- cutpointr(data = reboa1, x = pas1, class = morta_qx,
                direction = "<=", pos_class = "yes", neg_class = "no",
                method = maximize_metric )


plot(cp1)
reboa1
tab

View(cp1$roc_curve)
summary(cp1)
