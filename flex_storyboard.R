---
title: "Data viz"
runtime: shiny 
output:
   flexdashboard::flex_dashboard:
    source_code: embed
    storyboard: true
    theme: darkly
    css: viz_project.css
    social: "menu" 
    vertical_layout: fill
---


```{r message=FALSE, warning=FALSE}
# Packages loading
library(flexdashboard)
library(shiny)
library(shinybusy)
library(vov)
```

# Landing Page 

```{r}
add_busy_bar(timeout = 400, color = "#c7636a", centered = FALSE, height = "6px")

# tags$img(
#      src = "img_lp.png",
#      style = 'position: fixed; opacity: 0.8;filter: brightness(30%);'
#    )
```

```{r}
my_jumbotron <- function(header , content, button = TRUE, button_link, ...){
  
  button_label = c(...)
  if (button){
    div(class = "jumbotron",
        h1(header, align="center"), p(content, align="center"), p(a(href = button_link ,target="_blank",
        class = "btn btn-primary btn-lg button", role= 'button', button_label), align="center"))
} else {
    div(class = "jumbotron", h1(header), p(content))
}
  
}

fixedPage(
use_vov(),
    fade_in_up(
my_jumbotron(header="Welcome", content="Un peu de contexte t'vois !!! Lorem ipsum dolor sit amet, et bla et bla", button =  "True", button_link="https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-parcoursup/information/", button_Label = "Check the dataset"))

)
```

Story Board {.storyboard}
=========================================

### **Formations prédominantes par région:** Un texte qui explique in shortened words ce qu'on veut montrer avec cette carte. Lorem ipsum dolor sit amet, consectetur adipiscing elit.

```{r include=FALSE}
library(tidyverse)
library(readr)
library(leaflet)
library(sf)
library(data.table)
library(viridis)
library(xml2)
library(rvest)
library(rgdal) 
# Data loading and tidying
mydata <-  read_delim("data/fr-esr-parcoursup.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
mydata_sf <- subset(mydata, select = c(contrat_etab, dep, dep_lib, pct_bours, g_olocalisation_des_formations))
f_depts <- readOGR("depts")
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
```

```{r}
# Revenu par département - Web Scrapping
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
# Spatial data tidying
names(f_depts)[1] <- "dep"
f_depts$dep <- as.numeric(f_depts$dep)
new_df <- sp::merge(f_depts, df, by="dep", duplicateGeoms = TRUE)
newdf <- sp::merge(new_df, revenus_df, by="dep", duplicateGeoms = TRUE)
# Color palette for maps
num_pal <- colorNumeric(viridis(5, option = "inferno", direction = -1), newdf@data$pct_bours, na.color = "#808080")
revenus_pal <- colorNumeric(viridis(5, option = "inferno", direction = -1), newdf@data$revenus, na.color = "#808080")

# get domain of numeric data
domain <- range(na.omit(newdf@data$pct_bours))
pal <- colorNumeric(
  palette = c("white", "grey", "black"),
  domain = domain
)
```

```{r}
metropole <- leaflet(newdf, height = "800px") %>%
       addProviderTiles(providers$CartoDB.DarkMatter,
       options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)") %>%
       setView(2, 47, 6) %>%
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

### **Quid des Dom-Tom:** Un texte qui explique in shortened words ce qu'on veut montrer avec cette carte. Lorem ipsum dolor sit amet, consectetur adipiscing elit.


```{r}
# La réunion
reunion_data <- newdf[newdf$nom=="La Reunion",]
reunion <- leaflet(reunion_data, height = "200px") %>% 
         addProviderTiles(providers$CartoDB.DarkMatter,
         options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)") %>%
         setView(lng = 55.53251, lat = -21.133165, zoom = 8) %>% 
      addPolygons(data=reunion_data,
                  fillColor = "black",
                  color = "yellow",
                  weight = 0.5, smoothFactor = 0.5,
                  opacity = .5, fillOpacity = 1,
                  popup = reunion_data@data$pct_bours, group = "France Polygons") %>%
      addPolygons(data=reunion_data,
                  fillColor = ~revenus_pal(reunion_data@data$revenus),
                  color = "black", 
                  weight = 0.5, smoothFactor = 0.5,
                  opacity = .5, fillOpacity = 1,
                  label = paste(reunion_data@data$dep_lib, round(reunion_data@data$revenus),as.character("€")),
                  popup = reunion_data@data$pct_bours, group = "Revenus depts Poly") %>%
      addCircleMarkers(~long, ~lat, ~(pct_bours-15), stroke = T, weight = 1, opacity = 1,
                       fillColor = ~pal(pct_bours), color = ~pal(pct_bours), 
                       label = paste(reunion_data@data$dep_lib, round(reunion_data@data$pct_bours),as.character("%")),
                       group = "Scholarships depts markers")
# La martinique 
Martinique_data <- newdf[newdf$nom=="Martinique",]
martinique <- leaflet(Martinique_data, height = "200px") %>% 
         addProviderTiles(providers$CartoDB.DarkMatter,
         options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)") %>%
         setView(lng = -61.01893, lat = 14.654532, zoom = 8) %>%
      addPolygons(data=Martinique_data,
                  fillColor = "black",
                  color = "yellow",
                  weight = 0.5, smoothFactor = 0.5,
                  opacity = .5, fillOpacity = 1,
                  popup = Martinique_data@data$pct_bours, group = "France Polygons") %>%
      addPolygons(data=Martinique_data,
                  fillColor = ~revenus_pal(Martinique_data@data$revenus),
                  color = "black", 
                  weight = 0.5, smoothFactor = 0.5,
                  opacity = .5, fillOpacity = 1,
                  label = paste(Martinique_data@data$dep_lib, round(Martinique_data@data$revenus),as.character("€")),
                  popup = Martinique_data@data$pct_bours, group = "Revenus depts Poly") %>%
      addCircleMarkers(~long, ~lat, ~(pct_bours-15), stroke = T, weight = 1, opacity = 1,
                       fillColor = ~pal(pct_bours), color = ~pal(pct_bours), 
                       label = paste(Martinique_data@data$dep_lib, round(Martinique_data@data$pct_bours),as.character("%")),
                       group = "Scholarships depts markers")
# La guadeloupe 
Guadeloupe_data <- newdf[newdf$nom=="Guadeloupe",]
guadeloupe <- leaflet(Guadeloupe_data, height = "200px") %>% 
     addProviderTiles(providers$CartoDB.DarkMatter,
     options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)") %>%
     setView(lng = -61.53982, lat = 16.197587, zoom = 8) %>%
  addPolygons(data=Guadeloupe_data,
              fillColor = "black",
              color = "yellow",
              weight = 0.5, smoothFactor = 0.5,
              opacity = .5, fillOpacity = 1,
              popup = Guadeloupe_data@data$pct_bours, group = "France Polygons") %>%
  addPolygons(data=Guadeloupe_data,
              fillColor = ~revenus_pal(Guadeloupe_data@data$revenus),
              color = "black", 
              weight = 0.5, smoothFactor = 0.5,
              opacity = .5, fillOpacity = 1,
              label = paste(Guadeloupe_data@data$dep_lib, round(Guadeloupe_data@data$revenus),as.character("€")),
              popup = Guadeloupe_data@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), stroke = T, weight = 1, opacity = 1,
                   fillColor = ~pal(pct_bours), color = ~pal(pct_bours), 
                   label = paste(Guadeloupe_data@data$dep_lib, round(Guadeloupe_data@data$pct_bours),as.character("%")),
                   group = "Scholarships depts markers")
# La guyane 
Guyane_data <- newdf[newdf$nom=="Guyane",]
guyane <- leaflet(Guyane_data, height = "200px") %>% 
     addProviderTiles(providers$CartoDB.DarkMatter,
     options = providerTileOptions(noWrap = TRUE), group = "Basic Layer(default)") %>%
     setView(lng = -53.23917, lat = 3.922325, zoom = 5) %>% 
  addPolygons(data=Guyane_data,
              fillColor = "black",
              color = "yellow",
              weight = 0.5, smoothFactor = 0.5,
              opacity = .5, fillOpacity = 1,
              popup = Guyane_data@data$pct_bours, group = "France Polygons") %>%
  addPolygons(data=Guyane_data,
              fillColor = ~revenus_pal(Guyane_data@data$revenus),
              color = "black", 
              weight = 0.5, smoothFactor = 0.5,
              opacity = .5, fillOpacity = 1,
              label = paste(Guyane_data@data$dep_lib, round(Guyane_data@data$revenus),as.character("€")),
              popup = Guyane_data@data$pct_bours, group = "Revenus depts Poly") %>%
  addCircleMarkers(~long, ~lat, ~(pct_bours-15), stroke = T, weight = 1, opacity = 1,
                   fillColor = ~pal(pct_bours), color = ~pal(pct_bours), 
                   label = paste(Guyane_data@data$dep_lib, round(Guyane_data@data$pct_bours),as.character("%")),
                   group = "Scholarships depts markers")
```

```{r} 
library(htmltools)
leaflet_grid <- tagList(tags$table(width = "1400px", height = "607px",border = "1px",
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

### **Domaine de formations à Rennes: ** Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

```{r}
library(plotly)
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
            geom_bar(position = "dodge")
ggplotly(p)
```


### **Boursiers/Femmes dans le scientifique: ** Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

```{r}
library(metricsgraphics)
mjs_plot(mtcars, x=wt, y=mpg) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")
```
