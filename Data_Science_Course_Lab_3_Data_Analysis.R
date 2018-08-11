# require will show if a package is already installed
# if you have any missing packages use install.packages to install
require(ggplot2)

# for datasaurus data set
#install.packages("datasauRus")

# for animations
#install.packages("gganimate")

# if you get the following error, try installing from github using the command below
#> install.packages("gganimate") 
#  Warning in install.packages :
#    package ‘gganimate’ is not available (for R version 3.2.3)

#install.packages("devtools")
# devtools::install_github("dgrtwo/gganimate")

library(gganimate)

# for the pipe operator %>% 
library(magrittr)


library(datasauRus)
library(ggplot2)

# for group_by and summarize
library(dplyr)

# First work on the Anscombe's Quartet
data("anscombe")
summary(anscombe)

sapply(anscombe, mean)
sapply(anscombe, sd)
plot(y1 ~ x1, data = anscombe)

# Store the initial- global- plotting parameters
op <- par(no.readonly = TRUE)

# Decide on the lower/upper limits for the x- and y-axis, so each chart has the same range.
xls <- c(2, 19)
yls <- c(2, 14)

# Set various graphical parameters:
par(oma = c(2, 2, 0, 2), # Expands the outside margins to allow a common X/Y label.
    mar = c(3, 3, 1, 0), # Tweaks the margins of each plot, so the bottom 
                         #   layers don't draw over the x-axis label.
    mfrow = c(2, 2),     # 2x2 plot.
    pch = 16             # Filled-in circles.
   )

# We plot each group in the quartet separately
plot(y1 ~ x1, data = anscombe, xlim = xls, ylim = yls, xlab = "", ylab = "", col = "red")
plot(y2 ~ x2, data = anscombe, xlim = xls, ylim = yls, xlab = "", ylab = "", col = "blue")
plot(y3 ~ x3, data = anscombe, xlim = xls, ylim = yls, xlab = "", ylab = "", col = "green")
plot(y4 ~ x4, data = anscombe, xlim = xls, ylim = yls, xlab = "", ylab = "", col = "black")

# Adds in a shared x- and a shared y-axis label
mtext(text = "X axis", side = 1, line = 0, outer = TRUE)
mtext(text = "Y axis", side = 2, line = 0, outer = TRUE)

# Set the parameters back
par(op)


# let's try plotting using ggplot

# nothing's plotted out below, adding geometry will plot the graph
ggplot(data = anscombe, mapping = aes(x = x1, y = y1))

# add points
ggplot(data = anscombe, mapping = aes(x = x1, y = y1)) + geom_point()

# add lines
ggplot(data = anscombe, mapping = aes(x = x1, y = y1)) + geom_line()

# add points and lines at the same time
ggplot(data = anscombe, mapping = aes(x = x1, y = y1)) +
    geom_line() +
    geom_point()

# draw using a red line
ggplot(anscombe) +
    geom_line(mapping = aes(x = x1, y = y1), col = "red") +
    labs(title = "Line should be red", x = "X", y = "Y")

# Try plotting (x1,y1) and (x2,y2) pairs at the same time
ggplot(anscombe) +
    geom_line(mapping = aes(x = x1, y = y1), col = "red") +
    geom_line(mapping = aes(x = x2, y = y2), col = "blue") +
    labs(title = "Superposition of two anscombe pairs", x = "X", y = "Y")


# Start working on the DatasauRus Dozen

dim(datasaurus_dozen)
ncol(datasaurus_dozen)
nrow(datasaurus_dozen)

unique(datasaurus_dozen$dataset)

length(unique(datasaurus_dozen$dataset))
devtools::install_github('thomasp85/gganimate')

# equivalent to above
unique(datasaurus_dozen$dataset) %>% length()

datasaurus_dozen %>%
    group_by(dataset)

datasaurus_dozen %>%
    group_by(dataset) %>%
    summarise(mean_x = mean(x), mean_y = mean(y))

datasaurus_dozen %>%
    group_by(dataset) %>%
    summarise(sd_x = sd(x), sd_y = sd(y))

# altogether
datasaurus_dozen %>%
    group_by(dataset) %>%
    summarise(x_mean = mean(x), y_mean = mean(y), x_sd = sd(x), y_sd = sd(y))

# same as above
datasaurus_dozen %>%
    group_by(dataset) %>%
    summarise_if(is.double, funs(mean = mean, sd = sd))


#filter on "dino"
datasaurus_dozen[datasaurus_dozen$dataset=="dino",]

# same as above
datasaurus_dozen %>%
    filter(dataset == "dino")

#filter on "dino" with x < 30
datasaurus_dozen[datasaurus_dozen$dataset=="dino" & datasaurus_dozen$x<30,]

# same as above
datasaurus_dozen %>%
    filter(dataset == "dino") %>%
    filter(x < 30)

# only display "x" and "y" columns for dino
datasaurus_dozen[datasaurus_dozen$dataset=="dino", c("x", "y")]


ggplot(datasaurus_dozen, aes(x = x, y = y)) +
    geom_point()

# add color
ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
    geom_point()

# only plot one dataset, say "dino"
datasaurus_dozen %>%
    filter(dataset == "dino") %>%
    ggplot(aes(x = x, y = y, colour = "red")) +
    geom_point()

# plot two of the datasets, say "h_lines" and "v_lines"
datasaurus_dozen %>%
    filter(dataset %in% c("h_lines", "v_lines")) %>%
    ggplot(aes(x = x, y = y, colour = dataset)) +
    geom_point()

# plot one dataset per facet
datasaurus_dozen %>%
    filter(dataset %in% c("slant_down", "slant_up")) %>%
    ggplot(aes(x = x, y = y, colour = dataset)) +
    geom_point() +
    facet_wrap(~ dataset)

ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
       geom_point()+
       theme_void()+
       theme(legend.position = "none")+
       facet_wrap(~dataset, ncol=3)


# add back legend
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
       geom_point()+
       theme_void()+
#       theme(legend.position = "none")+
       facet_wrap(~dataset, ncol=3)


# add back legend and theme
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
       geom_point()+
#       theme_void()+
#       theme(legend.position = "none")+
       facet_wrap(~dataset, ncol=3)


# you need to install ImageMagick for the following animation
# brew install ImageMagick

p <- ggplot(datasaurus_dozen, aes(x = x, y = y, frame = dataset)) +
    geom_point() +
    theme_gray(20) +
    theme(legend.position = "none")

gganimate(p, title_frame = TRUE, "./DataSaurus_Dozen_from_R.gif")

# save the dataframes for later use
write.csv(anscombe, file = "Anscombes_Quartet.csv", row.names=FALSE)
write.csv(datasaurus_dozen, file = "Datasaurus_Dozen.csv", row.names=FALSE)

