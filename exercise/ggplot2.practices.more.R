library(ggplot2)
texas = read.csv("n:/dataset/texas.csv")
head(texas)
g <- ggplot(data = texas, aes(x = long, y = lat))
g + geom_point() # What's going on here?

g + geom_polygon(aes(group = group))

texas2 = texas[sample(nrow(texas)), ] #Row order matters!
  ggplot(data = texas2, aes(x = long, y = lat)) +


# install.packages("maps")
# help(package = "maps")
library(maps)
counties = map_data("county") # Using the built-in USA county
# map dataset.
ggplot(data = counties, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = group)) +
  guides(fill=F)

tx <- g + geom_polygon(aes(group = group, fill = bin))
tx2 = tx + ggtitle("Population of Texas Counties")
#Creates a ggplot2 title, and adds it to the tx graph.

g <- ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point()
str(g)

# g$ to see available options
g$coordinates
g2 <- g + coord_polar()
g2$coordinates #ciirdinate systems
g + coord_polar()
g + coord_flip()
g + coord_fixed(ratio = 1/10000)
g + coord_trans(y = "log10")
g + coord_trans(y = "log10", x = "log10")
g + coord_cartesian()
g + coord_cartesian(ylim = c(0,5000), xlim = c(0, 1))

tx + coord_map()

d2 <- subset(diamonds, color == "D")
cc <- ggplot(data = d2, aes(x = color)) + geom_bar(aes(fill = cut), position = "fill")
cc
cc + coord_polar(theta = "y")

#SCALE
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = cty)) +
  scale_color_gradient(low = "blue", high = "green")

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(size = cyl))

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(size = cyl)) + scale_size_area()
#Size of points proportional to value.

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(shape = fl)) +
  scale_shape_manual(values = c(0, 15, 1, 16, 3))


r <- ggplot(data = mpg, aes(x = displ, y = cty)) +
  geom_point(aes(color = drv, shape = fl))
#Specify the shapes manually
r + scale_shape_manual(values = c(0, 15, 1, 16, 3))

#COLOR
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

tx + scale_fill_brewer(palette = "OrRd")
tx + scale_fill_brewer(palette = "Spectral")
tx + scale_fill_brewer(palette = "Blues") +
  ggtitle('Texas Population') +
  ylab('Lotsa Lattitude') + xlab('Long') +
  theme_bw() + coord_map() +
  theme(plot.title = element_text(hjust=0.5))
tx + scale_fill_manual(values = c("red", "orange", "yellow",  "green", "blue"))

#THEME
tx + theme_grey()
tx + theme_bw()
tx + theme(panel.border = element_rect(colour = "black", fill = NA))

library(ggthemes) # install.packages("ggthemes")
p <- ggplot(data = diamonds, aes(x = color)) +
  geom_bar(aes(fill = cut))

p + theme_excel() + scale_fill_excel()
p + theme_economist() + scale_fill_economist()
p + theme_few() + scale_fill_few()
p + theme_solarized() + scale_fill_solarized()
p + theme_stata() + scale_fill_stata()
p + theme_wsj() + scale_fill_wsj(palette = "black_green")
p + theme_gdocs() + scale_fill_gdocs()
p + theme_tufte() + scale_fill_tableau()
p + ylab("Number of Diamonds")

q <- ggplot(data = mpg, aes(x =
                              displ, y = hwy)) +
  geom_point(aes(color = cty))
q
q + theme(legend.position = "bottom")

83
#Continuous.
q + guides(color = "colorbar")
#Discrete.
q + guides(color = "legend")
#Neither.
q + guides(color = "none")

q + guides(color = "legend")
tx + scale_fill_grey()
tx + scale_fill_grey(name = "Legend")
tx + scale_fill_grey(name = "Legend",
  labels = c("label 1", "label 2", "label 3","label 4", "label 5"))
tx + scale_fill_discrete(name = "Legend name")

#Code from the earlier section; modify to add legend aspects.
tx + scale_fill_brewer(palette = "Blues") +
  xlab("") +
  ylab("") +
  theme_bw() +
  coord_map() +
  ggtitle("Population of Texas Counties")

tx + scale_fill_brewer(
  palette = "Blues",
  name = "Population",
  labels = c("0 - 999", "1,000 - 9,999",
             "10,000 - 99,999", "100,000 - 999,999",
             "1,000,000+")) +
  xlab("") +
  ylab("") +
  theme_bw() +
  coord_map() +
  ggtitle("Population of Texas Counties")


#VIOLIN PLOTS
p <- ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
  geom_violin(aes(fill = Species)); p

p <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_violin(fill = 'gray', alpha = 0.5) +
  geom_dotplot(aes(fill = Species), binaxis = "y", stackdir =
                 "center"); p

p <- ggplot(mpg, aes(x = 1)) +
  geom_bar(aes(fill = class)) +
  coord_polar(theta = "y"); p

set.seed(1)
dir <- cut_interval(runif(100,0,360), n=16)
mag <- cut_interval(rgamma(100,15), n=4)
sample = data.frame(dir=dir, mag=mag)
p <- ggplot(sample, aes(x=dir, fill=mag) ) +
  geom_bar() +
  coord_polar(); p

#install.packages("vcd")
library(vcd)
mosaic(Survived ~ Class + Sex, data=Titanic, shade=T,
       highlighting_fill=c('red4',"skyblue"),
       highlighting_direction="left")

install.packages("treemap")
library(treemap)
data = read.csv('n:/dataset/data/apple.csv', TRUE)
treemap(data,
        index=c("item", "subitem"),
        vSize="time1206",
        vColor="time1106",
        type="comp",
        title='Apple Corp. Financial Statements',
        palette='RdBu')


data <- read.csv('n:/dataset/data/soft_impact.csv', TRUE)
library(reshape2)
library(ggplot2)
data.melt <- melt(data, id='Year')
p <- ggplot(data.melt, aes(x=Year, y=value,
                           group=variable, fill=variable) ) +
  geom_area(color='black', size=0.3,
            position=position_fill() ) +
  scale_fill_brewer()
p

install.packages("car")
library(car)
scatterplotMatrix(mpg[ ,c(3,8,9)],
                  diagonal='histogram',
                  ellipse=TRUE)

install.packages("corrplot")
library(corrplot)
corrplot(cor(mtcars), order="hclust")

fillcolor <- ifelse(economics[440:470,'psavert'] >
                      mean(economics[440:470,'psavert'])
                    , 'steelblue', 'red4')
p <- ggplot(economics[440:470,], aes(x=date, y=psavert) ) +
  geom_bar(stat='identity',
           fill=fillcolor)
p

p <- ggplot(economics[300:470,], aes(x=date, ymax=psavert, ymin=0) ) +
  geom_linerange(color='grey20', size=0.5) +
  geom_point(aes(y=psavert), color='red4') +
  theme_bw()
p

fill.color <- ifelse(economics$date > '1980-01-01' &
                       economics$date < '1990-01-01',
                     'steelblue', 'red4')
p <- ggplot(economics, aes(x=date, ymax=psavert, ymin=0) ) +
  geom_linerange(color=fill.color, size=0.9) +
  geom_text(aes(x=as.Date("1985-01-01",'%Y-%m-%d'), y=13),
            label="1980s") +
  theme_bw()
p

#install.packages('openair')
library(openair)
data(mydata)
calendarPlot(mydata, pollutant = "o3", year = 2003)


#MAPS
library(maps)
library(ggplot2)
world <- map_data("world")
naples <- ggplot(world, aes(x=long, y=lat, group=group) ) +
  geom_path(color='gray10', size=0.3) +
  geom_point(x=-81, y=26, size=10, shape='*') +
  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  coord_map("ortho", orientation=c(26, -80, 0))+
  theme(panel.grid.major = element_line(colour = "gray50"),
        panel.background = element_rect(fill = "white"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank() ) +
  ggtitle("Naples, Florida")
naples


map <- map_data('state')
arrests <- USArrests; names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
usmap <- ggplot(data=arrests) +
  geom_map(map =map, aes(map_id=region, fill=murder), color='gray40') +
  expand_limits(x=map$long, y=map$lat) +
  scale_fill_continuous(high='red2', low='white') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        axis.title=element_blank(), legend.position = c(0.95,0.28),
        legend.background=element_rect(fill="white", colour="white") ) +
  coord_map('mercator') +
  labs(title='Number of Murders Country-wide in 1973')
usmap


