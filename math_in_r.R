#area of a triangle
area_triangle <- function(height, length){
  1/2 * length * height
}
area_triangle(12, 10)


library(datasets)
data(mtcars)
head(mtcars, 15)
library(ggplot2)
ggplot(aes(x=disp, y=mpg,), data=mtcars)+geom_point()+ggtitle("displacement vs miles per gallon")
mtcars$vs <- as.factor(mtcars$vs)

ggplot(aes(x=vs, y=mpg), data = mtcars) + geom_boxplot()
ggplot(aes(x = vs, y = mpg, fill = vs), data = mtcars) + geom_boxplot(alpha=0.3) +
  theme(legend.position="none")
ggplot(aes(x=wt), data=mtcars) + geom_histogram(bindwidth=0.5)

install.packages("GGally")
library(GGally)
ggpairs(iris, mapping=ggplot2::aes(colour=Species))

View(mtcars)
plot_1 <- ggplot(data = mtcars, aes(x = hp, y = mpg,)) + geom_point() + geom_smooth(method = "lm")+ ggtitle(label = "Relationship between Horsepower(hp) and Fuel Consumption(mpg)") + xlab("Horsepower(hp)") + ylab("Miles Per Gallon")
plot_1
plot_2 <- ggplot(data = mtcars, aes(x = qsec, y = hp)) +geom_point() + geom_smooth(method = "lm") 
plot_2

