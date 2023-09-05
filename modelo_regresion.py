---
title: "Modelo de Regresión"
author: "Katherine Jimenez: 1106918 y Jose De Leon: 1108002"
date: "04/09/2023"
output:
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```


```{r libraries}
#Library
library(datasets)
library(tidyverse)
library(magrittr)
library(dplyr)
library(caret)
library(ggplot2)
library(scales)
require(plyr)
require(caret) 
require(ROCR) 
require(dplyr) 
require(ROSE)
library(readxl)
library(skimr)
library(GGally)

#Data Loading

df <- read_excel("D:\\MASTER EN ESTADISTICA APLICADA\\8_Octavo Trimestre\\Analisis Predictivo\\Clase II\\Consumo-Combustible-Mensual-2023 (1).xlsx")
View(df)
```

``` {r Descriptive Statistics}
##df %>% glimpse()
##df %>% summary()
skim(df[,2:6])

```
## Matriz de Correlación
```{r}
ggpairs(df[,2:6], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


```
Las variables con una mayor relación lineal con los costos son el gas (r = 0.669) y el carbón (r = 0.484)

##Modelo Predictivo
```{r Modelo 1, echo=FALSE, include=FALSE, warning=FALSE}
mod1 <- lm(Costo ~ Gas + Oil2 + Oil6 + Carbon, data = df)
summary(mod1)
```
```{r}
mod2 <- lm(Costo ~ Gas + Carbon, data = df)
summary(mod2)
```




```{r}
##Grafico de residuales
par(mod1)
hist(mod1[['residuals']], col = "white", breaks = 50)
boxplot(mod1[['residuals']], horizontal=TRUE, col = "blue",  outline=TRUE, add = TRUE) #Se puede observar que la mediana es casi cercana a 0. Y  que los percentiles 25 y 75 estan aproximadamente a la misma distancia de 0, y el min y el max tambien parecen estar a la misma distancia desde 0.

```


##Para este modelo predictivo sin separacion de data de entrenamiento, se observa lo siguiente:
1. El intercepto es de 1.044  cuando todo el consumo del Gas es cero. 
2. El intercepto es -8.17 cuando el consumo del Fuel 2 es cero
3. El intercepto es 7.17 cuando el consumo del Fuel 6 es cero
4. El carbon es 1.12 cuando el consumo de carbon es 0.
5. El R2 indica que un 47% de la variacion puede ser explicado por el modelo lo que no es un buen indicador.
6. Para un nivel de significancia de 5% y estando el p-value por debajo se infiere que las variables de Gas y Carbon son significativas y se acepta la hipotesis nula. 

```{r  plot, echo = FALSE, warning= FALSE}
# hist(df$Gas, main = "Histograma del consumo del combustible tipo Gas")
# hist(df$Oil2, main = "Histograma del consumo del combustible tipo FUel 2")
# hist(df$Oil6, main = "Histograma del consumo del combustible tipo FUel 6")
# hist(df$Carbon, main = "Histograma del consumo del combustible tipo Carbon")

```
