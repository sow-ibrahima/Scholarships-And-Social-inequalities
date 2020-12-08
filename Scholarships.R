---
title: "Data Viz Scholarships in France"
author: "Robinet - Fischer - Sow"
runtime: shiny
output: 
    flexdashboard::flex_dashboard:
      orientation: rows
      storyboard: TRUE
      source_code: embed
      theme: darkly
      css: viz_project.css
      social: "menu" 
---

### Description : 
Short student project about how scholarships can help students from low income families go further in their studies. 
```{r setup, include=FALSE}
library(flexdashboard)
library(shiny)
library(shinybusy)
library(vov)
```

#  Home Page {data-orientation=rows}     

Row 
-------------------------------------

Row 
-------------------------------------
```{r}
add_busy_bar(timeout = 400, color = "#c7636a", centered = FALSE, height = "6px")
```

```{r}
my_jumbotron <- function(header , content, button = TRUE, button_link, ...){ 
  
  button_label = c(...)
  if (button){
    div(class = "jumbotron",
        h1(header, align="center"), p(content, align="center"), p(a(href = button_link ,target="_blank",class = "btn btn-primary btn-lg button", role= 'button', button_label), align="center"))
} else {
    div(class = "jumbotron", h1(header), p(content))
}
  
}
```

```{r}
fixedPage(
use_vov(),
    fade_in_up(
my_jumbotron(header=div(style="text-align:center","Question"), content=h4(div(style="text-align:center;font-size: 200%;color:#ab3e3c;","La bourse permet-elle de gommer les inégalités dans la poursuite d'études ?")), button =  "FALSE")),

use_vov(),
    fade_in_up(
my_jumbotron(header=h2("Hypothèses :"), content=h4(div(style="text-align:left",(HTML(paste0("
            - Les boursiers sont désavantagés de base", "<br>", 
           "- Les boursiers s'orientent vers des formations spécifiques <br>",
           "- Les boursiers poursuivent des études moins longues
             "))))), button =  "True", button_link="https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-parcoursup/information/", button_Label = "Le jeu de données"),
delay = 10000
))
```


Story Board {.storyboard}
=========================================

### **Locatlisation des boursiers:**<br/> Ici, nous représentons le taux de boursiers et mettons ces chiffres en perspective par rapport au niveau du département considéré.

```{r}
library(tidyverse) # tidying 
library(readr) # data importing 
library(leaflet) # fancy mappin'
library(sf) # spatial data handlin'
library(data.table) # better programming / less loops 
library(viridis) # colors 
library(xml2) # web scrappin'
library(rvest) # web scrappin'
library(rgdal) # spatial data importin'
```

```{r}
# Data loading and tidying
mydata <-  read_delim("fr-esr-parcoursup.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
mydata_sf <- subset(mydata, select = c(contrat_etab, dep, dep_lib, pct_bours, g_olocalisation_des_formations))
f_depts <- readOGR("new_shp")
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
# Main map -> Metropole / Fancy Leaflet ! 
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

### **Quid des DROM:**<br/>  Les départements et territoires d'Outre-mer accentuent ce constat.

```{r}
# Adding DROMs / souligner différences 
## La réunion
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
## La martinique 
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
## La guadeloupe 
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
## La guyane 
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
# Faceting leaflet maps using hmtl tools - Fancier
library(htmltools)
leaflet_grid <- tagList(tags$table(width = "1400px", height = "607px",border = "1px",
                     tags$tr(
                       tags$td(reunion, width = "20%"), # reduce first column width
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

### **Des formations privilégiées ?** <br/> Les boursiers s'orienteraient vers études plus professionnantes.  
```{r}
library(grid)
library(gtable)
```

```{r}
# Regroupement des formations en domaines d'étude
mydata$domaine <- fct_recode(mydata$form_lib_voe_acc, 
                "Autre" = "Année préparatoire",
                "Autre" = "BPJEPS",
                "Autre" = "Classe préparatoire aux études supérieures",
                "Eco/droit" = "Classe préparatoire économique et commerciale",
                "Littér/art" = "Classe préparatoire littéraire",
                "Science/tech" = "Classe préparatoire scientifique",
                "Science/tech" = "CUPGE - Sciences, technologie, santé",
                "Autre" = "DU",
                "Littér/art" = "Licence - Arts-lettres-langues",
                "Eco/droit" = "Licence - Droit-économie-gestion",
                "Science/tech" = "Licence - Sciences - technologies - santé",
                "Social" = "Licence - Sciences humaines et sociales",
                "Autre" = "Licence - Sciences humaines et sociales / Sciences - technologies - santé",
                "Autre" = "Mention complémentaire",
                "Autre" = "Mise à niveau",
                "Service" = "BTS - Services",
                "Social" = "D.E secteur social",
                "Littér/art" = "Ecole d'architecture",
                "Science/tech" = "Formations d'ingénieurs",
                "Autre" = "Bachelor",
                "Production" = "BTSA",
                "Production" = "DUT - Production",
                "Eco/droit" = "Ecoles de commerce et de management",
                "Production" = "BTS - Production",
                "Service" = "D.E secteur sanitaire",
                "Science/tech" = "DEUST",
                "Service" = "DUT - Service",
                "Science/tech" = "Formation en ingénierie",
                "Service" = "DCG",
                "Littér/art" = "DN MADE",
                "Autre" = "CPES",
                "Eco/droit" = "CUPGE - Droit-économie-gestion",
                "Autre" = "Diplôme accrédité par un Etat étranger",
                "Littér/art" = "Diplôme des métiers d'Arts",
                "Littér/art" = "Licence - Arts-lettres-langues / Sciences humaines et sociales",
                "Eco/droit" = "Licence - Droit-économie-gestion / Sciences - technologies - santé",
                "Autre" = "Licence - STAPS",
                "Autre" = "Titre professionnel",
                "Autre" = "Cadre Technique",
                "Service" = "Classes préparatoires aux écoles paramédicales",
                "Littér/art" = "CUPGE - Arts Lettres Langues",
                "Social" = "CUPGE - Sciences humaines et sociales",
                "Autre" = "DEJEPS",
                "Autre" = "Diplôme d'établissement",
                "Littér/art" = "Ecole supérieure d'art",
                "Autre" = "Licence - Droit-économie-gestion / Arts-lettres-langues",
                "Autre" = "Licence - Droit-économie-gestion / Sciences humaines et sociales",
                "Autre" = "Technicien supérieur")
```


```{r}
#table(mydata$domaine) # Domaines et nombre d'ecole s'y rattachant

taux_boursier <- rep(0, length(table(mydata$domaine)))
labels_domaine <- names(table(mydata$domaine))

for ( i in 1:(length(taux_boursier))) {
  Fili <- mydata[mydata$domaine == labels_domaine[i],]
  taux_boursier[i] <- paste(floor((sum(Fili$acc_brs)/sum(Fili$acc_tot))*100),"%")
}
```

```{r}
# Choix des couleurs
couleur_boursier <-"#00aebc"
couleur_etudiant <-"#e7c21b" 
```

```{r}
## Graphique
plot1 <- ggplot(mydata) +
  aes(x=domaine) +
  geom_bar(aes(y=acc_tot), stat="identity", fill=couleur_boursier) +
  geom_bar(aes(y=acc_brs), stat="identity", fill=couleur_etudiant, width = 0.6)+
  scale_y_continuous(name = "Nombre d'étudiants", limits = c(0,160000))+
  labs(title = "Orientation des nouveaux étudiants",
       subtitle = "Parcoursup 2019",
       caption = "Source : data.gouv.fr")+
  theme( axis.line = element_line(colour = "black", 
                                  size = 0.5, linetype = "solid"))+
  annotate(geom="text", x=1, y=5000, label=taux_boursier[1],
                 color="black",fontface = "bold")+
  annotate(geom="text", x=2, y=5000, label=taux_boursier[2],
           color="black")+
  annotate(geom="text", x=3, y=5000, label=taux_boursier[3],
           color="black")+
  annotate(geom="text", x=4, y=5000, label=taux_boursier[4],
           color="black")+
  annotate(geom="text", x=5, y=5000, label=taux_boursier[5],
           color="black")+
  annotate(geom="text", x=6, y=5000, label=taux_boursier[6],
           color="black",fontface = "bold")+
  annotate(geom="text", x=7, y=5000, label=taux_boursier[7],
           color="black",fontface = "bold")
```


```{r}

## Légende et positionnement
# Construction de 4 grobs - 2 symboles et 2 labels
carre_boursier = rectGrob(height = .5, width = .5, gp = gpar(fill = couleur_boursier, col = NA))
carre_etudiant = rectGrob(height = .5, width = .5, gp = gpar(fill = couleur_etudiant, col = NA))
texte_boursier = textGrob("Boursiers", x = 0, just = "left")
texte_etudiant = textGrob("Étudiants", x = 0, just = "left")

# Construction de la gtable - 2 colonnes X 4 lignes
leg = gtable(width = unit(c(1,2), "cm"), height = unit(c(1,1,1,1), "cm"))
leg = gtable_add_grob(leg, rectGrob(gp = gpar(fill = NA, col = "black")), t=2,l=1,b=3,r=2)

# Placement des 4 grobs et du titre de la legende dans la table
leg = gtable_add_grob(leg, carre_boursier, t=2, l=1)
leg = gtable_add_grob(leg, carre_etudiant, t=3, l=1)
leg = gtable_add_grob(leg, texte_boursier, t=3, l=2)
leg = gtable_add_grob(leg, texte_etudiant, t=2, l=2)

leg = gtable_add_grob(leg, textGrob("Légende"), t=1, l=1, r=2)

# Recuperer le grob ggplot de la figure
g = ggplotGrob(plot1)


#Positionnement
pos = g$layout[grepl("panel", g$layout$name), c('t', 'l')] # Position du panneau
g = gtable_add_cols(g, sum(leg$widths), pos$l) # Ajout d'une colonne de droite'une colonne à la droite du panneau
g = gtable_add_grob(g, leg, t = pos$t, l = pos$l + 1) # Ajout de la legende dans cette nouvelle colonne
g = gtable_add_cols(g, unit(6, "pt"), pos$l) # Ajout d'une colonne d'espace

# Dessin
grid.newpage()
grid.draw(g)
```

### **Des études moins longues ? **<br/> Les  boursiers poursuivraient des études plus courtes comparées aux non boursiers. 

```{r}
mydata$longueur <- fct_recode(mydata$form_lib_voe_acc,
                          "Longues" = "Année préparatoire",
                          "Courtes" = "BPJEPS",
                          "Longues" = "Classe préparatoire aux études supérieures",
                          "Longues" = "Classe préparatoire économique et commerciale",
                          "Longues" = "Classe préparatoire littéraire",
                          "Longues" = "Classe préparatoire scientifique",
                          "Longues" = "CUPGE - Sciences, technologie, santé",
                          "Courtes" = "DU",
                          "Longues" = "Licence - Arts-lettres-langues",
                          "Longues" = "Licence - Droit-économie-gestion",
                          "Longues" = "Licence - Sciences - technologies - santé",
                          "Longues" = "Licence - Sciences humaines et sociales",
                          "Longues" = "Licence - Sciences humaines et sociales / Sciences - technologies - santé",
                          "Courtes" = "Mention complémentaire",
                          "Longues" = "Mise à niveau",
                          "Courtes" = "BTS - Services",
                          "Courtes" = "D.E secteur social",
                          "Longues" = "Ecole d'architecture",
                          "Longues" = "Formations d'ingénieurs",
                          "Longues" = "Bachelor",
                          "Courtes" = "BTSA",
                          "Courtes" = "DUT - Production",
                          "Longues" = "Ecoles de commerce et de management",
                          "Courtes" = "BTS - Production",
                          "Courtes" = "D.E secteur sanitaire",
                          "Courtes" = "DEUST",
                          "Courtes" = "DUT - Service",
                          "Longues" = "Formation en ingénierie",
                          "Courtes" = "DCG",
                          "Courtes" = "DN MADE",
                          "Longues" = "CPES",
                          "Longues" = "CUPGE - Droit-économie-gestion",
                          "Courtes" = "Diplôme accrédité par un Etat étranger",
                          "Longues" = "Diplôme des métiers d'Arts",
                          "Longues" = "Licence - Arts-lettres-langues / Sciences humaines et sociales",
                          "Longues" = "Licence - Droit-économie-gestion / Sciences - technologies - santé",
                          "Courtes" = "Licence - STAPS",
                          "Courtes" = "Titre professionnel",
                          "Longues" = "Cadre Technique",
                          "Courtes" = "Classes préparatoires aux écoles paramédicales",
                          "Longues" = "CUPGE - Arts Lettres Langues",
                          "Longues" = "CUPGE - Sciences humaines et sociales",
                          "Courtes" = "DEJEPS",
                          "Courtes" = "Diplôme d'établissement",
                          "Longues" = "Ecole supérieure d'art",
                          "Longues" = "Licence - Droit-économie-gestion / Arts-lettres-langues",
                          "Longues" = "Licence - Droit-économie-gestion / Sciences humaines et sociales",
                          "Courtes" = "Technicien supérieur")

#table(mydata$longueur) # Longueur d'études et nombre d'ecole s'y rattachant

taux_boursier_longueur <- rep(0, length(table(mydata$longueur)))
taux_boursier_long_text <- rep(0, length(table(mydata$longueur)))
labels_longueur <- names(table(mydata$longueur))

for ( k in 1:(length(taux_boursier_longueur))) {
  Filiere <- mydata[mydata$longueur == labels_longueur[k],]
  taux_boursier_longueur[k] <- round((sum(Filiere$acc_brs)/sum(Filiere$acc_tot))*100,0)
  taux_boursier_long_text[k] <- paste(round((sum(Filiere$acc_brs)/sum(Filiere$acc_tot))*100,0),"%")
}

df <- data.frame(longueur = names(table(mydata$longueur)),
                 Taux = c(taux_boursier_longueur,100 - taux_boursier_longueur),
                 Statut = c(rep("Boursiers", 2), rep("Étudiants", 2)))


ggplot(df) +
  aes(x=longueur, y=Taux, fill=Statut) +
  scale_fill_manual(values=c(couleur_etudiant,couleur_boursier))+
  geom_bar(stat='identity')+
  labs(title = "Comparaison de la longueur des études selon le statut boursier",
       subtitle = "Parcoursup 2019",
       caption = "Source : data.gouv.fr",
       x = "Durée des études",
       y = "Proportion")+
  annotate(geom="text", x=1, y=92, label=taux_boursier_long_text[2],
           color="black",fontface = "bold")+
  annotate(geom="text", x=2, y=93, label=taux_boursier_long_text[1],
           color="black",fontface = "bold")
```

### **Ce qu'il faut retenir: **<br/> Des résultats qui contrastent avec les conclusions du ministère de l'enseignement. Quelques limites à l'approche adoptée ici.

<font size="6"><center><font color="black">**En définitive**</font></center></font>
<br />
<br />
<br />
- <font size="5"> Les boursiers sont à priori issus de milieux moins favorisés. </font>
<br />
<br />
<br />
<br />
- <font size="5"> Contrairement aux chiffres du ministère ils ne semblent ici pas se diriger vers une domaine de formation particulière. </font>
<br />
<br />
<br />
<br />
- <font size="5"> Ils poursuivent à priori des études moins longues (2ans). </font>
<br />
<br />
<br />
<br />
- <font size="5"> Pour aller plus loin. </font>

