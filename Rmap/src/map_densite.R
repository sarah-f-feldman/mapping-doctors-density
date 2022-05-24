#####################
# mapping densities #
#####################

# References

# https://statnmap.com/2018-07-14-introduction-to-mapping-with-sf-and-co/
# https://geocompr.robinlovelace.net/adv-map.html
# https://github.com/statnmap/prez/blob/master/2018-07-06_RR2018_Statnmap.pdf

library(sf)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
#library(patchwork)


extraWD = "data"
# if (!file.exists(file.path(extraWD, "departement.zip"))) {
#   githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data/departement.zip"
#   download.file(githubURL, file.path(extraWD, "departement.zip"))
#   unzip(file.path(extraWD, "departement.zip"), exdir = extraWD) #Il a fallut le lancer seul
# }
unzip(file.path(extraWD, "departement.zip"), exdir = extraWD) 


#---------------------------------
# MAPPING DEPARTMENTS OF FRANCE

departements_L93 <- sf::st_read(dsn = extraWD, layer = "DEPARTEMENT",
                                quiet = TRUE) %>% 
  st_transform(2154) %>% 
  mutate_at(.vars = c("CODE_DEPT", "NOM_DEPT"), as.character)

#departements_L93 <-departements_L93 %>% mutate(CODE_REG = ifelse(CODE_DEPT == 77, "90", CODE_REG))
ggplot(departements_L93) +
  aes(fill = CODE_REG) +
  scale_fill_viridis_d() +
  geom_sf() +
  coord_sf(crs = 4326) + #Pour que les lignes de lattitude longitudes soient droites
  guides(fill = FALSE) +
  ggtitle("Coord. geographiques") +
  theme(title = element_text(size = 16),
        plot.margin = unit(c(0,0.1,0,0.25), "inches"))


#----------------------------------
# DENSITY DATA:  data cleaning

ds <- read.csv2("data/rpps-medecins_2015_toanalyze.csv", stringsAsFactors = FALSE)

dpt <- ds %>% filter(str_detect(dpt, "[0-9]"))
#Je separe les numero des noms
dpt <- dpt %>% separate(dpt, c("dpt_num","dpt_nom"), " - ")
#Je retire les accents
#En fait seuls les noms avec des accents ont ete encodes en UTF-8 =, les autres sont unknown
# Encoding(dpt$dpt_nom)
# [1] "UTF-8"
dpt$dpt_nom <- iconv(dpt$dpt_nom,from="UTF-8",to="ASCII//TRANSLIT")
dpt$dpt_nom <- toupper(dpt$dpt_nom)

new_d <- left_join(departements_L93, dpt, by = c("CODE_DEPT" = "dpt_num") )
new_d <- new_d %>% mutate_at(c("cardio_malvasc","geriatrie","generalistes", "med_ge"), as.numeric)

#---------------------------
# PREPARING LEGENDS
quintmedg <- quantile(new_d$med_ge, probs = seq(0,1,0.20))
quintcardio <- quantile(new_d$cardio_malvasc, probs = seq(0,1,0.20))
quintgeriatrie <- quantile(new_d$geriatrie, probs = seq(0,1,0.20))
new_d$quint_medg <- findInterval(new_d$med_ge, quintmedg, all.inside = TRUE)
new_d$quint_cardio <- findInterval(new_d$cardio_malvasc, quintcardio, all.inside = TRUE)
new_d$quint_geria <- findInterval(new_d$geriatrie, quintgeriatrie, all.inside = TRUE)


legend_medge <- new_d %>% group_by(quint_medg) %>% summarise (min = min(med_ge), max = max(med_ge), min_max = paste0(min, " ; ", max)) %>% ungroup %>% data.frame()
legend_cardio <- new_d %>% group_by(quint_cardio) %>% summarise (min = min(cardio_malvasc), max = max(cardio_malvasc), min_max = paste0(min, " ; ", max)) %>% ungroup %>% data.frame()
legend_geria <- new_d %>% group_by(quint_geria) %>% summarise (min = min(geriatrie), max = max(geriatrie), min_max = paste0(min, " ; ", max)) %>% ungroup %>% data.frame()

#-------------------------
# PLOT


#med ge
ggplot(new_d) +
  geom_sf() +
  aes(fill = factor(quint_medg)) +
  scale_fill_manual(values = c("1" = "#FFCFB8", "2" = "#FFA994", 
                               "3" = "#FF7657", "4" = "#FF4F42", 
                               "5" = "#F52100"), 
                    labels = legend_medge$min_max) +
 
  coord_sf(crs = 4326) + #Pour que les lignes de lattitude longitudes soient droites
  ggtitle("Densité de généralistes pour 100 000 habitants") +
  theme_minimal()+ 
  theme(axis.text = element_blank(), 
        panel.grid.major = element_line(colour = "white"))+
  theme(title = element_text(size = 14),
        plot.margin = unit(c(0,0.1,0,0.25), "inches")) +
  theme(legend.title = element_blank())

#cardio
ggplot(new_d) +
  aes(fill = factor(quint_cardio)) +
  scale_fill_manual(values = c("1" = "#FFCFB8", "2" = "#FFA994", 
                               "3" = "#FF7657", "4" = "#FF4F42", 
                               "5" = "#F52100"), 
                    labels = legend_cardio$min_max) +
  geom_sf() +
  coord_sf(crs = 4326) + #Pour que les lignes de lattitude longitudes soient droites
  ggtitle("densité de cardiologues pour 100 000 habitants") +
  theme_minimal()+ 
  theme(axis.text = element_blank(), 
        panel.grid.major = element_line(colour = "white"))+
  theme(title = element_text(size = 16),
        plot.margin = unit(c(0,0.1,0,0.25), "inches")) +
  theme(legend.title = element_blank())

