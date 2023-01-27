install.packages("ggbiplot")
install.packages("psych")
install.packages("tidyverse")
install.packages("devtools")
library(psych)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggbiplot)

options(scipen = 999)
"---------------------------------------------------"

data <- read.csv("./Binance.csv", sep = ",")
glimpse(data)

data_transform <- data %>% 
  mutate(
    symbol = NULL
  )
glimpse(data_transform)

pca_data <- prcomp(data_transform, center = TRUE , scale = TRUE)

summary(pca_data)

plot(pca_data$x[,1], pca_data$x[,2], xlab = "PCA 1", ylab = "PCA 2")

autovectores <- pca_data$rotation 
autovalores <- pca_data$sdev * pca_data$sdev

pca_var_pct <- round(autovalores / sum(autovalores)*100, digits = 2)
barplot(pca_var_pct, main = "Scree Plot", xlab = "Componente Principal", ylab = "Variacion Porcentual")

round(cor(data, pca_data$x), digits = 3)

