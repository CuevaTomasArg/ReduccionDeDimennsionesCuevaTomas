install.packages("ggbiplot")
install.packages("psych")
install.packages("tidyverse")
install.packages("devtools")
remotes::install_github("vqv/ggbiplot")
library(psych)
library(ggplot2)
library(dplyr)
library(devtools)
library(plyr)
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

round(cor(data_transform, pca_data$x), digits = 3)
screeplot(pca_data, type = "l", main = "Screeplot binance")

pca_binance <- data.frame(
  Modelo = rownames(data_transform$x),
  x = data_transform$x[, 1],
  y = data_transform$x[, 2]
)

ggplot(data = pca_binance, aes(x, y, label = Modelo)) +
  geom_text() +
  xlab(paste0("CP 1: ", pca_var_pct[1],"%")) +
  ylab(paste0("CP 2: ", pca_var_pct[2],"%")) +
  theme_bw() +
  ggtitle("Gráfico PCA")

