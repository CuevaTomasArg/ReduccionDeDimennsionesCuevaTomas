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

data <- read.csv("./Binance.csv", sep = ",")
glimpse(data)
row__names <- data[,2]
rownames(data) <- row__names

data <- data %>% 
  mutate(
    symbol = NULL,
    X = NULL,
    count = as.double(count)
  ) 




View(data)

pca_data <- prcomp(data, center = TRUE , scale = TRUE)

names(pca_data)
summary(pca_data)

plot(pca_data$x[,1], pca_data$x[,2], xlab = "PCA 1", ylab = "PCA 2")
autovectores <- pca_data$rotation 
autovalores <- pca_data$sdev * pca_data$sdev

pca_var_pct <- round(autovalores / sum(autovalores)*100, digits = 2)
barplot(pca_var_pct, main = "Scree Plot", xlab = "Componente Principal", ylab = "Variacion Porcentual")

round(cor(data, pca_data$x), digits = 3)
screeplot(pca_data, type = "l", main = "Screeplot binance")

glimpse(pca_data)

pca_binance <- data.frame(
  Modelo = rownames(pca_data$x),
  x = pca_data$x[, 1],
  y = pca_data$x[, 2]
)

View(mtcars)
View(data)

View(pca_binance)

ggplot(data = pca_binance, aes(x, y, label = Modelo)) +
  geom_text() +
  xlab(paste0("CP 1: ", pca_var_pct[1],"%")) +
  ylab(paste0("CP 2: ", pca_var_pct[2],"%")) +
  theme_bw() +
  ggtitle("Gráfico PCA")

pca_final <- principal(r = data,nfactors = 4,rotate = "none")

pca_final