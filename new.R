# library(raster)
# library(leaflet)
# library(plotly)
# 
# adm1 <- getData('GADM', country='NGA', level=0)
# adm2 <- getData('GADM', country='NGA', level=1)
# adm3 <- getData('GADM', country='NGA', level=2)
# 
# 
# View(adm1@polygons)
# 
# ggplot2::fortify(adm1@polygons)
# 
# fadm1 = ggplot2::fortify(adm1)
# fadm2 = ggplot2::fortify(adm2)
# fadm3 = ggplot2::fortify(adm3)
# 
# View(fadm1)
# library(tidyverse)
# 
# ?fortify
# # leaflet(fadm1) %>% addTiles() %>% addMarkers(lng = ~long, lat = ~lat,  popup = ~paste(lat, long, sep = '<br/>'))
# 
# 
# 
# ?leaflet
# m <- leaflet() %>% addTiles()
# m
# 
# library(leaflet)
# m <- leaflet() %>% addTiles()
# m  # a map with the default OSM tile layer
# 
# 
# # set bounds
# m %>% fitBounds(0, 10, 100, 20)
# 
# ?fitBounds
# # move the center to Snedecor Hall
# m <- m %>% setView(-93.65, 42.0285, zoom = 17)
# ?setView
# 
# 
# m
# 
# # popup
# m %>% addPopups(-93.65, 42.0285, "Here is the <b>Department of Statistics</b>, ISU")
# rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
# rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)
# 
# # use automatic bounds derived from lng/lat data
# m <- m %>% clearBounds()
# 
# # popup
# m %>% addPopups(rand_lng(), rand_lat(), "Random popups")
# 
# # marker
# m %>% addMarkers(rand_lng(), rand_lat())
# m %>% addMarkers(
#   rand_lng(), rand_lat(), popup = paste("A random letter", sample(LETTERS, 10))
# )
# 
# Rlogo <- file.path(R.home("doc"), "html", "logo.jpg")
# m %>% addMarkers(
#   174.7690922, -36.8523071, icon = list(
#     iconUrl = Rlogo, iconSize = c(100, 76)
#   ), popup = "R was born here!"
# )
# 
# m %>% addMarkers(rnorm(30, 175), rnorm(30, -37), icon = list(
#   iconUrl = Rlogo, iconSize = c(25, 19)
# ))
# 
# # circle (units in metres)
# m %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 50, 150))
# 
# # circle marker (units in pixels)
# m %>% addCircleMarkers(rand_lng(50), rand_lat(50), color = "green")
# m %>% addCircleMarkers(rand_lng(100), rand_lat(100), radius = runif(100, 5, 15))
# 
# # rectangle
# m %>% addRectangles(
#   rand_lng(), rand_lat(), rand_lng(), rand_lat(),
#   color = "red", fill = FALSE, dashArray = "5,5", weight = 3
# )
# 
# # polyline
# m %>% addPolylines(rand_lng(50), rand_lat(50))
# 
# # polygon
# m %>% addPolygons(rand_lng, rand_lat(), layerId = "foo")
# 
# # geoJSON
# seattle_geojson <- list(
#   type = "Feature",
#   geometry = list(
#     type = "MultiPolygon",
#     coordinates = list(list(list(
#       c(-122.36075812146,  47.6759920119894),
#       c(-122.360781646764, 47.6668890126755),
#       c(-122.360782108665,  47.6614990696722),
#       c(-122.366199035722, 47.6614990696722),
#       c(-122.366199035722,  47.6592874248973),
#       c(-122.364582509469, 47.6576254522105),
#       c(-122.363887331445,  47.6569107302038),
#       c(-122.360865528129, 47.6538418253251),
#       c(-122.360866157644,  47.6535254473167),
#       c(-122.360866581103, 47.6533126275176),
#       c(-122.362526540691,  47.6541872926348),
#       c(-122.364442114483, 47.6551892850798),
#       c(-122.366077719797,  47.6560733960606),
#       c(-122.368818463838, 47.6579742346694),
#       c(-122.370115159943,  47.6588730808334),
#       c(-122.372295967029, 47.6604350102328),
#       c(-122.37381369088,  47.660582362063),
#       c(-122.375522972109, 47.6606413027949),
#       c(-122.376079703095,  47.6608793094619),
#       c(-122.376206315662, 47.6609242364243),
#       c(-122.377610811371,  47.6606160735197),
#       c(-122.379857378879, 47.6610306942278),
#       c(-122.382454873022,  47.6627496239169),
#       c(-122.385357955057, 47.6638573778241),
#       c(-122.386007328104,  47.6640865692306),
#       c(-122.387186331506, 47.6654326177161),
#       c(-122.387802656231,  47.6661492860294),
#       c(-122.388108244121, 47.6664548739202),
#       c(-122.389177800763,  47.6663784774359),
#       c(-122.390582858689, 47.6665072251861),
#       c(-122.390793942299,  47.6659699214511),
#       c(-122.391507906234, 47.6659200946229),
#       c(-122.392883050767,  47.6664166747017),
#       c(-122.392847210144, 47.6678696739431),
#       c(-122.392904778401,  47.6709016021624),
#       c(-122.39296705153, 47.6732047491624),
#       c(-122.393000803496,  47.6759322346303),
#       c(-122.37666945305, 47.6759896300663),
#       c(-122.376486363943,  47.6759891899754),
#       c(-122.366078869215, 47.6759641734893),
#       c(-122.36075812146,  47.6759920119894)
#     )))
#   ),
#   properties = list(
# categories <- LETTERS[1:10]
# df <- data.frame(
#   lat = rand_lat(100), lng = rand_lng(100), size = runi   name = "Ballard",
#     population = 48000,
#     # You can inline styles if you want
#     style = list(
#       fillColor = "yellow",
#       weight = 2,
#       color = "#000000"
#     )
#   ),
#   id = "ballard"
# )
# m %>% setView(-122.36075812146, 47.6759920119894, zoom = 13) %>% addGeoJSON(seattle_geojson)
# 
# 
# # use the Dark Matter layer from CartoDB
# leaflet() %>% addTiles("https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
#                        attribution = paste(
#                          "&copy; <a href=\"https://openstreetmap.org\">OpenStreetMap</a> contributors",
#                          "&copy; <a href=\"https://cartodb.com/attributions\">CartoDB</a>"
#                        )
# ) %>% setView(-122.36, 47.67, zoom = 10)
# 
# # provide a data frame to leafletf(100, 5, 20),
#   category = factor(sample(categories, 100, replace = TRUE), levels = categories),
#   value = rnorm(100)
# )
# m <- leaflet(fadm1) %>% addTiles()
# m %>% addCircleMarkers(~long, ~lat)
# m %>% addCircleMarkers(~long, ~lat, radius = runif(100, 4, 10), color = c("red"))
# 
# # Discrete colors using the "RdYlBu" colorbrewer palette, mapped to categories
# RdYlBu <- colorFactor("RdYlBu", domain = categories)
# m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
#                        color = ~RdYlBu(category), fillOpacity = 0.5)
# 
# # Continuous colors using the "Greens" colorbrewer palette, mapped to value
# greens <- colorNumeric("Greens", domain = NULL)
# m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
#                        color = ~greens(value), fillOpacity = 0.5)
# 
# dat <- head(fadm1)
# 
# dat
# 
# 
# 
# 
# 
# leaflet(dat, options = leafletOptions(minZoom = 0, maxZoom = 30)) %>% 
#   addTiles() %>%
#   addCircleMarkers(lng = ~long, lat = ~lat, opacity = 1, weight = 10, color = "green") %>% 
#   addPolylines(lng = ~long, lat = ~lat, color = "red")
# 
# 
# 
# library(htmltools)
# 
# df <- read.csv(textConnection(
#   "Name,Lat,Long
# Samurai Noodle,47.597131,-122.327298
# Kukai Ramen,47.6154,-122.327157
# Tsukushinbo,47.59987,-122.326726"
# ))
# 
# leaflet(df) %>% addTiles() %>%
#   addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name))
# 
# 
# library(htmltools)
# 
# df <- read.csv(textConnection(
#   "Name,Lat,Long
# Samurai Noodle,47.597131,-122.327298
# Kukai Ramen,47.6154,-122.327157
# Tsukushinbo,47.59987,-122.326726"))
# 
# leaflet(df) %>% addTiles() %>%
#   addMarkers(~Long, ~Lat, label = ~htmlEscape(Name))
# 
# 
# # Change Text Size and text Only and also a custom CSS
# leaflet() %>% addTiles() %>% setView(-118.456554, 34.09, 13) %>%
#   addMarkers(
#     lng = -118.456554, lat = 34.105,
#     label = "Default Label",
#     labelOptions = labelOptions(noHide = T)) %>%
#   addMarkers(
#     lng = -118.456554, lat = 34.095,
#     label = "Label w/o surrounding box",
#     labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>%
#   addMarkers(
#     lng = -118.456554, lat = 34.085,
#     label = "label w/ textsize 15px",
#     labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
#   addMarkers(
#     lng = -118.456554, lat = 34.075,
#     label = "Label w/ custom CSS style",
#     labelOptions = labelOptions(noHide = T, direction = "bottom",
#                                 style = list(
#                                   "color" = "red",
#                                   "font-family" = "serif",
#                                   "font-style" = "italic",
#                                   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
#                                   "font-size" = "12px",
#                                   "border-color" = "rgba(0,0,0,0.5)"
#                                 )))
# 
# 
# 
# 
# library(rgdal)
# library(remotes)
# library(tidyverse)
# 
# # remotes::install_github('wpgp/wopr', force = TRUE)
# library(wopr)
# 
# # catalogue <- getCatalogue(spatial_query = T)
# catalogue <- getCatalogue()
# 
# 
# selection <- subset(
#   catalogue,
#   country == "NGA" &
#     category == "Popualation" &
#     version == "v1.2"
# )
# downloadData(selection)
# 
# 
# library(sf)
# 
# View(shape) <- st_read(dsn = "NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp")
# shape$geometry
# 
# 
# 
# 
# dat <- read.csv("GPS&CSV/22.GS.C5.AYT_csv.csv")
# shape %>%
#   ggplot(aes(fill = mean)) +
#   geom_sf() +
#   scale_fill_continuous(name = "Population")
# 
# 
# 
# 
dat1 <- read.csv("GPS&CSV/22.GS.C5.AYT_csv.csv")
dat2 <- read.csv("GPS&CSV/22WRT_csv.csv")
dat3 <- read.csv("GPS&CSV/22YRT_csv.csv")

files <- list.files(path = ".", pattern = ".csv")
x <- files[1]
x
Y <- strsplit(x, ".", fixed = TRUE)
as.list(Y)

ayt20_sindex <- read_csv("AYT20_SINDEX.csv")
dat1 <- dat1 %>% select(c(Longitude, Latitude))
dat1 <- dat1[1:21,]
ayt20_sindex <- cbind(ayt20_sindex, dat1)

ayt20_sindex <- ayt20_sindex %>% janitor::clean_names()
View(ayt20_sindex)
ayt20_sindex <- ayt20_sindex %>% select(-c(sn, x15, x17, l, a, b))
View(ayt20_sindex)


# library(leaflet)
# color_palette <- colorBin("Blues", domain = shape$mean)
# 
# total_dat %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(
#     lng = ~Longitude,
#     lat = ~Latitude,
#     weight = 1) %>%
#   addLegend(
#     pal = color_palette,
#     values = ~mean,
#     opacity = 0.7,
#     title = "Population",
#     position = "bottomright"
#   )



library(tidyverse)
library(sf)



lev1 <- st_read("Selections_Visualization/NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp")
lev1
ayt20_sindex <- read_csv("AYT20_SINDEX.csv")


checks <- c("IITA-TMS-IBA000070","TMEB419","TMS13F1160P0004","TMS14F1036P0007","IITA-TMS-IBA30572","IITA-TMS-IBA980581")

# calculate for checks average 
checks_mean <- ayt20_sindex %>%
  filter(accession_name %in% checks) %>%
  add_row(accession_name = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
  filter(accession_name == "check_mean")

# insert check mean into dataset
ayt20_sindex <- bind_rows(ayt20_sindex,checks_mean)
ayt20_sindex <- ayt20_sindex %>% janitor::clean_names()
# dataset for percentage difference against checks average
ayt20_sindex_checkdiff <- ayt20_sindex %>% 
  select(-sindex) %>% 
  mutate(across(where(is.numeric), .fns = ~((./.[accession_name == "check_mean"]-1)*100))) %>% 
  mutate(sindex=ayt20_sindex$sindex) %>% 
  mutate(rank = factor(row_number()))

x <- ayt20_sindex %>% filter(accession_name == "check_mean")
x
x <- as.numeric(x$sindex[1])
x
ayt20_sindex <- ayt20_sindex %>% mutate(compare = if_else(sindex > x,"greater","lesser"))


View(ayt20_sindex)

tf <- ggplot() + 
  geom_sf(data = lev1, show.legend = TRUE) + 
  geom_sf(data = lev1, colour = "white", fill = "grey") + 
  # geom_text(data = lev1) +
  geom_point(data = ayt20_sindex, mapping = aes(x = longitude, y = latitude, color = sindex, size = dyld,
                                                text = paste0("<b>Sindex: </b>", sindex,"<br>",
                                                              "<b>FYLD: </b>", fyld,"<br>",
                                                               "<b>DYLD: </b>", dyld,"<br>"))) + 
  scale_color_gradient2(low = "red", midpoint = -20.4443, mid = "yellow", high = "green") +
  coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
  geom_sf_text(data = lev1, aes(label = statename)) +
  theme_dark()

tf

plotly::ggplotly(tf, tooltip = "text")


library(mapview)
library(sf)
stations_sf <-st_read('NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp')
mapview(stations_sf)









###################################################################################################


library("tidyverse")
library("leaflet")
dat <- read.csv("~/Desktop/GPS/GPS&CSV/22.GS.C5.AYT_csv.csv")

lev1 %>% leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>% 
  addPolygons(color = "#cecece", weight = .8, smoothFactor = 0.5, 
              fillOpacity = .1,
              layerId = ~statename,
              highlightOptions = highlightOptions(color = "red", weight = 1, bringToFront = TRUE)) %>% 
  # addMarkers(label = ayt20_sindex$accession_name, 
  #            clusterOptions = markerClusterOptions(),
  #            popup = ifelse(ayt20_sindex$accession_name == "IITA-TMS-IBA000070",
  #                           "IITA-TMS-IBA000070 is the one", # Value if True
  #                           "Not the one")) %>% 
  addCircles(data = ayt20_sindex, lng = ~longitude, lat = ~latitude, color = "red",
                 popup = ~accession_name) %>% 
  setView(lng = 9.0820, lat = 8.6753, zoom = 6) %>% 
  addMiniMap(
    toggleDisplay = TRUE,
    tiles = providers$Stamen.TonerLite
)



##################################################################################################

library(tidyverse)
library(plotly)
library(readxl)


uyt40_dyld <- read_csv("~/Desktop/git_workspace/Visualizations/uyt40_dyld.csv")
head(uyt40_dyld)

locs <- data.frame(location = c("Ibadan", "Mokwa", "Ago-owu", "Onne", "Otobi", "Ubiaja"), lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493),
                   long = c(3.9470,5.0547,4.3258,7.1516,8.0897,6.3918))
locs

colnames(uyt40_dyld)[2] <- "Accession"





# uyt40_dyld %>% janitor::clean_names()
piv <- uyt40_dyld %>% pivot_longer(cols = -c(Trait, Accession, Combined), names_to = "location", values_to = "values")
piv
piv2 <- piv %>%  mutate(
  location = case_when(
    stringr::str_detect(location, pattern = regex("AG"))        ~ "Ago-owu",
    stringr::str_detect(location, pattern = regex("IB"))        ~ "Ibadan",
    stringr::str_detect(location, pattern = regex("MK"))        ~ "Mokwa",
    stringr::str_detect(location, pattern = regex("ON"))        ~ "Onne",
    stringr::str_detect(location, pattern = regex("OT"))        ~ "Otobi",
    stringr::str_detect(location, pattern = regex("UB"))        ~ "Ubiaja",
    stringr::str_detect(location, pattern = regex("IK"))        ~ "Ikenne"
  )
) %>% 
  mutate(
  long = case_when(
    location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% select(long) %>% as.numeric(),
    location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% select(long) %>% as.numeric(),
    location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% select(long) %>% as.numeric(),
    location == "Onne"      ~ locs %>% filter(location=="Onne") %>% select(long) %>% as.numeric(),
    location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% select(long) %>% as.numeric(),
    location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% select(long) %>% as.numeric()
  )
) %>% 
  mutate(
    lat = case_when(
      location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% select(lat) %>% as.numeric(),
      location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% select(lat) %>% as.numeric(),
      location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% select(lat) %>% as.numeric(),
      location == "Onne"      ~ locs %>% filter(location=="Onne") %>% select(lat) %>% as.numeric(),
      location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% select(lat) %>% as.numeric(),
      location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% select(lat) %>% as.numeric()
    )
  ) 
piv2 %>% View()

filtered_piv2 <- piv2 %>% arrange(desc(Combined)) %>%  top_frac(.1, Combined)
filtered_piv2


Accession : ",accession, "<br>",
                                         "DYLD : ",values, "<br>",
                                         "location :",location, "<br>
  
  
  
  x <- 1
  paste(if_else(x < 1,paste0(x, "lesser"),paste0(x, "greater")))


# 
# 
# checks <- uyt40_dyld$accession[1]
# 
# uyt40_dyld <- uyt40_dyld %>% janitor::clean_names()
# 
# 
# 
# ?geom_sf
# checks_mean <- uyt40_dyld %>% 
#   filter(accession %in% checks) %>%
#   add_row(accession = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
#   filter(accession == "check_mean") %>% 
#   mutate(trait = replace_na("DYLD"))
# checks_mean
# 
# dat1 <- bind_rows(uyt40_dyld,checks_mean)
# tail(dat1)
# 
# piv2_difference <- dat1 %>% 
#   mutate(across(where(is.numeric), .fns = ~((./.[accession == "check_mean"]-1)*100)))

tf <- ggplot() + 
  # geom_sf(data = lev1, show.legend = TRUE) + 
  geom_sf(data = lev1, colour = "white", fill = "black", size = 0.1) + 
  geom_text(data = filtered_piv2, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
  geom_point(data = filtered_piv2, mapping = aes(x = long, y = lat, size = values, color = values, alpha=values,
                                                 text = paste0("<b> Trait: ",Trait,"</b> \n",
                                                               "<b> Accession: ", Accession, "</b> \n",
                                                               "<b> DYLD: ",values,"</b>")))+
  # scale_color_viridis_c() +
  # scale_color_gradient2(low = "red", midpoint = 6.357, mid = "yellow", high = "green") +
  # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
  # geom_sf_text(data = lev1, aes(label = statename)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
  facet_wrap(~fct_inorder(Accession), ncol = 2)

tf

plotly::ggplotly(tf, tooltip = "text")

#######################################################################################

tf <- ggplot() + 
  # geom_sf(data = lev1, show.legend = TRUE) + 
  geom_sf(data = lev1, aes(fill = capacity))+ 
  geom_text(data = filtered_piv2, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
  geom_point(data = filtered_piv2, mapping = aes(x = long, y = lat, size = values, color = values, alpha=values,
                                                 text = paste0("<b> Trait: ",Trait,"</b> \n",
                                                               "<b> Accession: ", Accession, "</b> \n",
                                                               "<b> DYLD: ",values,"</b>")))+
  # scale_color_viridis_c() +
  # scale_color_gradient2(low = "red", midpoint = 6.357, mid = "yellow", high = "green") +
  # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
  # geom_sf_text(data = lev1, aes(label = statename)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
  #facet_wrap(~fct_inorder(Accession), ncol = 2)

tf

plotly::ggplotly(tf, tooltip = "text" )

###########################################################################################

x <- c("1","2")
x[length(x)]


data.frame(
  Trait = c("accession", "dyld", "category"),
  Values = c(1, 2, 2 )
)
