#install.packages('ggplot2')
library(ggplot2)

#ggplot practices

head(mpg)
# Set default
g = ggplot(data = mpg, aes(x = displ, y = hwy))

g + geom_point()
g + geom_smooth(method = "lm")

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() + geom_smooth(method = "lm")

g <- ggplot(data = mpg, aes(x = displ, y = hwy))
g + geom_point(aes(color = class))
g + geom_point(aes(size = class))
g + geom_point(aes(shape = class))
g + geom_point(aes(alpha = class))


g + geom_point() + facet_grid(. ~ cyl)
g + geom_point() + facet_grid(drv ~ .)
g + geom_point() + facet_grid(drv ~ cyl)
g + geom_point() + facet_wrap( ~ class)


ggplot(data = diamonds, aes(x = cut)) +
  geom_bar(aes(fill = cut))
ggplot(data = diamonds, aes(x = color)) +
  geom_bar(aes(fill = cut))

g <- ggplot(data = diamonds, aes(x = depth))
zoom <- coord_cartesian(xlim = c(55, 70))
g + geom_histogram(binwidth = 0.2) + zoom


g + geom_histogram(aes(fill=cut), binwidth = 0.2,
                   position = position_stack(reverse=TRUE)) + zoom


g + geom_histogram(binwidth = 0.2) + facet_wrap( ~ cut) + zoom


g + geom_histogram(binwidth = 0.2) + geom_freqpoly(binwidth=0.2)
+ facet_wrap( ~ cut) + zoom

g + geom_freqpoly(aes(color = cut), binwidth = 0.2) +
  facet_wrap( ~ cut) + zoom

g + geom_freqpoly(aes(color = cut), binwidth = 0.2) + zoom

library(ggplot2)
g <- ggplot(data = diamonds, aes(x = carat, y = price))
g + geom_point(aes(color = cut))

g + geom_point() + geom_density2d()
g + geom_point() + geom_smooth(aes(color = cut), method = "loess", se = FALSE)
g + geom_point(size = 0.5, alpha=0.05)

# Iris
ggplot(data = iris, aes(x = Sepal.Width, y=Sepal.Length, color=Species)) + theme_bw() +
  stat_density2d(geom='polygon', aes(fill=Species, alpha=..level..), size = 0) +
  geom_rug(position = 'jitter')
