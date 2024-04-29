library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(forcats)

places <- read.csv("places_tried.csv", na.strings="n/a")

places$coffee.rating <- substr(places$coffee.rating, 1, nchar(places$coffee.rating) - 3)
places$study.rating <- substr(places$study.rating, 1, nchar(places$study.rating) - 3)

write.csv(places, "places.csv")

places <- read.csv("places.csv", na.strings=c("n/a", "", "n/a "))

sum_per_city_coffee <-
  places %>%
  group_by(city) %>%
  summarize(
    average = round(mean(coffee.rating, na.rm=T), digits = 1)
  )  %>%
  drop_na()

sum_per_city_coffee <-
  sum_per_city_coffee %>%
  arrange(-average)

coffee_summary <- ggplotly(
  ggplot(data=sum_per_city_coffee) +
    geom_col(
      mapping = aes(
        x = fct_reorder(city, average),
        y = average,
        fill = city,
        text = paste0(city, " has an average rating of ", average,"/10 (❁´◡`❁)")
    ),
    width = 1
  ) 
  + xlab("City")
  + ylab("Average Coffee Rating out of 10")
  + coord_flip(),
  tooltip = "text"
) 

print(coffee_summary)

Sys.setenv("plotly_username"="joelllaaanovelllaaa")
Sys.setenv("plotly_api_key"="7bYpOvA0twgWFiVoQ5sf")

api_create(coffee_summary, filename = "sexydata.R")
