########FUNCTIONS
library(tidyverse)
library(readxl)
library(plotrix)
library(rstatix)
library(multcompView)
library(xlsx)
library(tinytex)


####Funcion para data
data_base <- function(chara) {
  string <- grep(chara, names(blood), value = TRUE)
  data1 <- blood %>%
    select(string)
  data1 <- stack(data1)
  data1$time[data1$ind == string[1]] <- 0
  data1$time[data1$ind == string[2]] <- 6
  data1$time[data1$ind == string[3]] <- 11
  data1$time[data1$ind == string[4]] <- 16
  data1$time[data1$ind == string[5]] <- 21
  
  return(data1)
}

#####Funcion de resumen
resume <- function(data_n){
  data2 <- data_n %>%
    group_by(time) %>%
    summarise(mean = mean(values),
              median = median(values),
              IQR = IQR(values),
              sd = sd(values), 
              es = std.error(values),
              var = var(values),
              min = min(values), max = max(values), 
              shapiro = shapiro.test(values)[[2]])
  
  return(data2)
  
}

###Funcion para anova
anova_te <- function(dat1) {
  model <- lm(values ~ factor(ind), data = dat1) 
  anov2 <- anova(model)
  if(anov2$`Pr(>F)`[1] < 0.01){
    anov = aov(model)
    tunkey = TukeyHSD(anov)
    dunn =  dunn_test(formula = values ~ ind, data = dat1, 
                      p.adjust.method = "bonferroni")
  }
  else {
    anov = NA
    tunkey = NA
    dunn = NA
  }
  
  return(list(anova = anov2,
              tunkey = tunkey,
              Kruskal_bonfe = dunn))
  
}
