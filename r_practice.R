install.packages("readxl")
install.packages("readcsv")
install.packages("tidyverse")
install.packages("mapdata")
install.packages("patchwork")
install.packages("dplyr")

library(ggplot2)
library(mapdata)
library(tidyverse)
library(ggplot2)

forest_fires <- read.csv("C:/Users/User/Downloads/forest+fires/forestfires.csv")
View(forest_fire


ggplot(data = forest_fires,mapping = aes(x = 'x', y = 'y') + geom_point('FFMC
                                                                        '))

