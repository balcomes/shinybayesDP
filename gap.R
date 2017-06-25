library(readr)
library(ggplot2)
library(gganimate)
library(scales)
library(tweenr)
library(dplyr)
library(gapminder)

data(gapminder)

gapminder_edit <- gapminder %>%
  arrange(country, year) %>%
  select(gdpPercap,lifeExp,year,country, continent, pop) %>%
  rename(x=gdpPercap,y=lifeExp,time=year,id=country) %>%
  mutate(ease="linear")

#gapminder_tween <- tween_elements(gapminder_edit,
                                  #"time", "id", "ease", nframes = 300) #%>%
  #mutate(year = round(time), country = .group) %>%
  #left_join(gapminder, by=c("country","year","continent")) %>%
  #rename(population = pop.x)

p2 <- ggplot(gapminder_edit,
             aes(x=x, y=y, frame = 300)) +
  geom_point(aes(size=10, color=continent),alpha=0.8) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  scale_x_log10(labels=comma)

gganimate(p2, filename="gapminder-tween.gif", title_frame = FALSE, interval = 0.05)