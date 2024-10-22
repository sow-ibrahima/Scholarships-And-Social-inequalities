---
title: "mainRmd"
author: "Ibrahima SOW"
date: "23/11/2020"
output: html_document
---
 
```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(readr)
library(leaflet)
library(sf)
library(data.table)
library(viridis)
library(xml2)
library(rvest)
library(ggthemes)
library(ggiraph)
library(rgdal) 
```

```{r message=FALSE, warning=FALSE, include=FALSE}
mydata <-  read_delim("data/fr-esr-parcoursup.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
mydata_sf <- subset(mydata, select = c(contrat_etab, dep, dep_lib, pct_bours, g_olocalisation_des_formations))
#head(mydata_sf)
```


```{r}
# Récup deps infos 
deps_info <- distinct(mydata_sf, dep, dep_lib) %>% na.omit()
deps_info$dep <- as.character(deps_info$dep)
# Récup lat et long
lat_long <- as.data.frame(str_split_fixed(mydata_sf$g_olocalisation_des_formations, ",", 2))
mydata_sf <- cbind(mydata_sf, lat_long) %>% na.omit() 
names(mydata_sf)[6] <- "lat" 
names(mydata_sf)[7] <- "long" 
mydata_sf$g_olocalisation_des_formations=NULL
mydata_sf$pct_bours <- as.numeric(mydata_sf$pct_bours)
mydata_sf$lat <- as.numeric(mydata_sf$lat)
mydata_sf$long <- as.numeric(mydata_sf$long)
lat_long <-as.data.frame(apply(mydata_sf[,5:6], 2, function(x) tapply(x, mydata_sf$dep, mean)))

# Moyenne taux de boursiers par dept. 
df <- as.data.frame(tapply(mydata_sf$pct_bours, mydata_sf$dep, mean)) %>%
       tibble::rownames_to_column("dep") 

# Bindind
df <- cbind(df,lat_long)
df <- deps_info %>% inner_join(df, by= "dep")

names(df)[1] <- "dep"
names(df)[3] <- "pct_bours"
df$dep <- as.numeric(df$dep)
df
```



```{r} 
content <- read_html("http://www.journaldunet.com/business/salaire/classement/departements/revenus")
content2 <- read_html("http://www.journaldunet.com/business/salaire/classement/departements/revenus?page=2")

tables <- content %>% html_table(fill = TRUE)
tables2 <- content2 %>% html_table(fill = TRUE)

 
first_table <- tables[[1]] 
second_one <- tables2[[1]]             
dept_revenus <- rbind(first_table, second_one)  %>%
                    select(-c(Rang)) 
names(dept_revenus)[2]<-paste("revenus") 

dept_num <- as.data.frame(str_split_fixed(dept_revenus$Département, "[(]", 2))
colnames(dept_num) <- c("dept_name", "dep")
revenus_df <- cbind(dept_num, dept_revenus) %>%
                    select(-c(Département)) %>%
                       select(everything(), revenus)
revenus_df$dep <- gsub("[)]","" , revenus_df$dep) %>% as.numeric()
revenus_df$revenus <- gsub("[[:blank:]]", "", revenus_df$revenus)
revenus_df$revenus <- substr(revenus_df$revenus, 1,4) %>% as.numeric() *2 *12 # 2 pour représenter un ménage et 12 pour l'année 

revenus_df
``` 

```{r include=FALSE}
f_depts <- readOGR("depts")
f_depts@data
```

```{r}
names(f_depts)[1] <- "dep"
f_depts$dep <- as.numeric(f_depts$dep)
new_df <- sp::merge(f_depts, df, by="dep", duplicateGeoms = TRUE)
newdf <- sp::merge(new_df, revenus_df, by="dep", duplicateGeoms = TRUE)
```

```{r message=FALSE, warning=FALSE}
num_pal <- colorNumeric(viridis(5, option = "inferno", direction = -1), newdf@data$pct_bours, na.color = "#808080")
revenus_pal <- colorNumeric(viridis(5, option = "inferno", direction = -1), newdf@data$revenus, na.color = "#808080")

# get domain of numeric data
domain <- range(na.omit(newdf@data$pct_bours))
pal <- colorNumeric(
  palette = c("white", "grey", "black"),
  domain = domain
)
#
metropole <- leaflet(newdf, height = "800px") %>%
       addProviderTiles(providers$CartoDB.DarkMatter,
       options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)") %>%
       setView(2, 40, 5) %>%
    addPolygons(data=newdf,
                fillColor = "black",
                color = "yellow",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                popup = newdf@data$pct_bours, group = "France Polygons") %>%
      addPolygons(data=newdf,
                fillColor = ~revenus_pal(newdf@data$revenus),
                color = "black", 
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label = paste(newdf@data$dep_lib, round(newdf@data$revenus),as.character("€")),
                popup = newdf@data$pct_bours, group = "Revenus depts Poly") %>%
    addCircleMarkers(~long, ~lat, ~(pct_bours-15), stroke = T, weight = 1, opacity = 1,
                     fillColor = ~pal(pct_bours), color = ~pal(pct_bours), 
                     label = paste(newdf@data$dep_lib, round(newdf@data$pct_bours),as.character("%")),
                     group = "Scholarships depts markers") %>%
    leaflet::addLegend(pal=revenus_pal, values=newdf@data$revenus, opacity=1, group = "Revenus depts Poly" ) %>%
    leaflet::addLegend(pal=pal, values=newdf@data$pct_bours, opacity=1,group ="Scholarships depts markers") %>%
addLayersControl(
    baseGroups = c("Basic Layer (default)"),
    overlayGroups = c("France Polygons","Revenus depts Poly", "Scholarships depts markers"), position = "topleft",
    options = layersControlOptions(collapsed = T)
  )
metropole
```

```{r}
# La réunion
reunion_data <- newdf[newdf$nom=="La Reunion",]
reunion <- leaflet(reunion_data, height = "200px") %>% 
 addProviderTiles(providers$CartoDB.DarkMatter,
 options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)")  %>%
  setView(lng = 55.53251, lat = -21.133165, zoom = 8) %>% 
  addControl("La Réunion", position = "bottomleft") %>%
  addPolygons(data=reunion_data,
                fillColor = "orange",
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= reunion_data@data$dep_lib,
                popup = reunion_data@data$pct_bours, group = "France colored Polygons") %>%
  addPolygons(data=reunion_data,
                fillColor = ~revenus_pal(reunion_data@data$revenus),
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= reunion_data@data$dep_lib,
                popup = reunion_data@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), weight = 5, stroke = TRUE, 
                     color = "blue",fillColor = "bleu", group = "Scholarships depts markers")
# La martinique 
Martinique <- newdf[newdf$nom=="Martinique",]
martinique <- leaflet(Martinique, height = "200px") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
  options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)")  %>%
  setView(lng = -61.01893, lat = 14.654532, zoom = 8) %>% 
  addControl("Martinique", position = "bottomleft") %>%
  addPolygons(data=Martinique,
                fillColor = "orange",
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= Martinique@data$dep_lib,
                popup = Martinique@data$pct_bours, group = "France colored Polygons") %>%
  addPolygons(data=Martinique,
                fillColor = ~revenus_pal(Martinique@data$revenus),
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= Martinique@data$dep_lib,
                popup = Martinique@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), weight = 5, stroke = TRUE, 
                     color = "blue",fillColor = "bleu", group = "Scholarships depts markers")
# La guadeloupe 
Guadeloupe <- newdf[newdf$nom=="Guadeloupe",]
guadeloupe <- leaflet(Guadeloupe, height = "200px") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
  options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)")  %>%
  setView(lng = -61.53982, lat = 16.197587, zoom = 8) %>% 
  addControl("Guadeloupe", position = "bottomleft") %>%
  addPolygons(data=Guadeloupe,
                fillColor = "orange",
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= Guadeloupe@data$dep_lib,
                popup = Guadeloupe@data$pct_bours, group = "France colored Polygons") %>%
  addPolygons(data=Guadeloupe,
                fillColor = ~revenus_pal(Guadeloupe@data$revenus),
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= Guadeloupe@data$dep_lib,
                popup = Guadeloupe@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), weight = 5, stroke = TRUE, 
                     color = "blue",fillColor = "bleu", group = "Scholarships depts markers")
# La guyane 
Guyane <- newdf[newdf$nom=="Guyane",]
guyane <- leaflet(Guyane, height = "200px") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
  options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)")  %>%
  setView(lng = -53.23917, lat = 3.922325, zoom = 6) %>% 
  addControl("Guyane", position = "bottomleft") %>%
  addPolygons(data=Guyane,
                fillColor = "orange",
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= Guyane@data$dep_lib,
                popup = Guyane@data$pct_bours, group = "France colored Polygons") %>%
  addPolygons(data=Guyane,
                fillColor = ~revenus_pal(Guyane@data$revenus),
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= Guyane@data$dep_lib,
                popup = Guyane@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), weight = 5, stroke = TRUE, 
                     color = "blue",fillColor = "bleu", group = "Scholarships depts markers")
```

```{r}
library(htmltools)
leaflet_grid <- 
  tagList(tags$table(width = "100%", border = "1px",
                     tags$tr(
                       tags$td(reunion, width = "30%"), # reduce first column width
                       tags$td(metropole, rowspan = 4)  # span across the four other maps
                     ),
                     tags$tr(
                       tags$td(martinique)
                       ),
                     tags$tr(
                       tags$td(guadeloupe)
                     ),
                     tags$tr(
                       tags$td(guyane)
                     ) 
  )
          )

browsable(leaflet_grid)
``` 


```{r} 
reunion_data <- newdf[newdf$nom=="La Reunion",]
reunion <- leaflet(reunion_data, height = "200px") %>% 
 addProviderTiles(providers$CartoDB.DarkMatter,
 options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)")  %>%
  setView(lng = 55.53251, lat = -21.133165, zoom = 8) %>% 
  addControl("La Réunion", position = "bottomleft") %>%
  addPolygons(data=reunion_data,
                fillColor = "orange",
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= reunion_data@data$dep_lib,
                popup = reunion_data@data$pct_bours, group = "France colored Polygons") %>%
  addPolygons(data=reunion_data,
                fillColor = ~revenus_pal(reunion_data@data$revenus),
                color = "black",
                weight = 0.5, smoothFactor = 0.5,
                opacity = .5, fillOpacity = 1,
                label= reunion_data@data$dep_lib,
                popup = reunion_data@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), weight = 5, stroke = TRUE, 
                     color = "blue",fillColor = "bleu", group = "Scholarships depts markers")
reunion
```





----
```{r}
   addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){

            make_shapes <- function(colors, sizes, borders, shapes) {
                shapes <- gsub("circle", "50%", shapes)
                shapes <- gsub("square", "0%", shapes)
                paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
            }
            make_labels <- function(sizes, labels) {
                paste0("<div style='display: inline-block;height: ", 
                       sizes, "px;margin-top: 4px;line-height: ", 
                       sizes, "px;'>", labels, "</div>")
            }

            legend_colors <- make_shapes(colors, sizes, borders, shapes)
            legend_labels <- make_labels(sizes, labels)

            return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
        }
```








-----
```{r message=FALSE, warning=FALSE, include=FALSE} 
dpt_shape <-  st_read(dsn = 'depts/') 
names(dpt_shape)[1] <- "dep"
dpt_shape$dep <- as.numeric(dpt_shape$dep)
dpt_shape <- dpt_shape[dpt_shape$dep<100,]
```

```{r}
#mydata_sf <- dpt_shape %>% inner_join( y = mydata_sf, by = "dep") 
mydata_sf_out <- dpt_shape %>% inner_join(y = df, by = "dep") %>% 
                    subset(select=-c(nuts3, wikipedia, nom)) 
table(is.na(df$dep))
# tapply(mydata_sf$pct_bours, mydata_sf$dep, mean) 
# tapply(mydata_sf_out$pct_bours, mydata_sf_out$dep, mean)
```

```{r} 
p <- mydata_sf_out %>% ggplot2::ggplot() + 
  geom_sf(aes(fill = pct_bours)) + 
  scale_fill_viridis(limits = c(10, 30), direction=-1, option = "inferno") + 
  theme_bw() + theme_map() +
  theme(legend.position="top") + 
  ggtitle("Pourcentage de boursiers par départements")  

ggiraph(code = {print(p)},hover_css = "{fill:orange;}")
```
 
```{r} 
p1 <- mydata_sf_out[1:10732,] %>% ggplot2::ggplot() + 
  geom_sf(aes(fill = pct_bours)) + 
  scale_fill_viridis(direction=-1, option = "magma") + theme_bw() + theme_map() +
  theme(legend.position="top")
p1
```

```{r}
revenus_df$dep <- as.numeric(revenus_df$dep )
revenus_sf_out <- dpt_shape %>% left_join( y = revenus_df, by = "dep")
#out <- dpt_shape %>% inner_join( y = revenus_df, by = "dep")
```

```{r}
library(ggthemes)
revenus_sf_out %>% ggplot2::ggplot() + 
  geom_sf(aes(fill = revenus)) + 
  scale_fill_viridis(direction=-1, option = "inferno") + theme_bw() + theme_map() + 
  theme(legend.position="left")
```

