library(rtweet)

#
# search for 10,000 tweets sent from the US
rt <- search_tweets(
  "#adulting", geocode = lookup_coords("usa"), n = 10000
)

## create lat/lng variables using all available tweet and profile geo-location data
rt <- lat_lng(rt)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)


## plot lat and lng points onto state map
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


## for other ways to tweak the maps from the code check out the map library
#https://cran.r-project.org/web/packages/maps/maps.pdf
## the ggmap library is good too
library(ggmap)
qmplot(lng, lat, data = rt, maptype = "toner-lite", color = I("blue"))




## plot time series of tweets
ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #adulting Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )