#### REDÉCOUPAGE AUTOMATIQUE DES CIRCONSCRIPTIONS LÉGISLATIVES FRANÇAISES ####
####           @ Adrien Fabre, adrien.fabre@psemail.eu 2017               ####
####             Librement réutilisable avec attribution                  ####

linux <- TRUE
if (linux) { options(download.file.method = "wget") # For Ubuntu 14.04
} else options(download.file.method = "auto")

package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package('foreign')
package("readxl")
package("mfx")
package("Hmisc")
package("plyr")

# setwd("/var/www/We give the 99 percents/Redécoupage") # /!\ Changer le chemin d'accès
setwd("/var/www/RedecoupageEquitable/Annexes") # /!\ Changer le chemin d'accès
load("env.RData")


##### Préparation: cantons #####
# Cantons 2015 données par OSM. Source: https://www.data.gouv.fr/fr/datasets/contours-osm-des-cantons-electoraux-departementaux-2015/
cantons_osm <- read.dbf("cantons/cantons_2015.dbf", as.is = TRUE) # cantons métropolitains
# Les données géographiques sont ensuite simplifiées (jusqu'à 10%) à l'aide de mapshaper.org pour optimiser l'utilisation de la mémoire par autoredistrict

# # Jonction de ce jeu de données avec les résultats électoraux (non utilisée in fine) :
# # Résultats 2è tour Présidentielle 2017. Source: https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/
# election2017 <- read_excel("Résultats présidentielle/2017.xls")
# cantons_osm$canton <- as.numeric(cantons_osm$canton)
# cantons_osm$dep[cantons_osm$dep=="2A"] <- 120
# cantons_osm$dep[cantons_osm$dep=="2B"] <- 121
# cantons_osm$dep <- as.numeric(cantons_osm$dep)
# cantons_osm$dep[cantons_osm$dep==120] <- "2A"
# cantons_osm$dep[cantons_osm$dep==121] <- "2B"
# cantons_osm$canton <- paste(cantons_osm$dep, cantons_osm$canton)
# election2017$canton <- paste(election2017[[1]], election2017$X__2)
# election2017$Macron <- as.numeric(election2017$X__22)
# election2017$LePen <- as.numeric(election2017$X__29)
# election2017 <- election2017[-c(1:3),-c(1:32)]
# cantons_osm <- merge(cantons_osm, election2017, sort=FALSE)
# 
# # Nettoyage, rajout d'une colonne pour les nouvelles circos et exportation
# cantons_osm$circos_nouvelles <- c(1:length(cantons_osm[[1]]))
# cantons_osm <- cantons_osm[,-c(3:8)]
# cantons_osm$population <- as.numeric(cantons_osm$population)
write.dbf(cantons_osm, "cantons/cantons simple.dbf")


##### Préparation: Français de l'étranger #####
# Le nombre d'inscrits sur les données électorales de 2017 ont été utilisées pour déterminer la population française dans chaque pays
# Source: https://www.data.gouv.fr/s/resources/elections-presidentielles-2017-2nd-tour-vote-des-francais-etablis-hors-de-france/20170509-121225/Presidentielle2017-FAE-2nd-tour-pays.csv
# Source des données géographiques "étranger": http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip
# Afin de combiner les données démographiques/électorales avec les données géographiques, j'ai inséré le Kosovo dans l'Albanie et Jérusalem dans Israël (directement dans le .csv)
#  et j'ai recoder les noms de pays par le code FIPS présent dans les données géographiques
etranger2017 <- read.csv("Résultats présidentielle/2017 étranger.csv", stringsAsFactors=FALSE, sep=";", fileEncoding="latin1") # données démographiques/électorales
etranger2017$inscrits <- etranger2017[[2]]
etranger2017$Macron <- etranger2017[[9]]
etranger2017$exprimes <- etranger2017[[7]]
etranger2017 <- etranger2017[-c(2:12)]
monde <- read.dbf("Monde/étranger.dbf") # données géographiques originales
monde$Pays <- as.vector(monde$FIPS)
monde$Pays[is.na(monde$Pays)] <- as.vector(monde$ISO3[is.na(monde$Pays)])
monde2 <- merge(monde, etranger2017, by="Pays", all.x=TRUE, sort=FALSE)
monde2 <- merge(monde, monde2, by="Pays", all.x=TRUE, sort=FALSE)
monde2 <- monde2[,c(1,6,24:26)]
write.dbf(monde2, "Monde/2017 étranger.dbf")
# Afin d'avoir des circonscriptions faisant sens du point de vue géographique, j'ai regroupé arbitrairement certains pays
#  dans la fonction 'region_monde'. Ainsi, la recherche de la solution optimale automatique était légèrement contrainte
region_monde <- function(d) {
  df <- d
  # df$region[is.element(df$Pays,c('AG','TS','MO','LY'))] <- 'Maghreb'
  df$region[is.element(df$Pays,c('BN','UV','BY','CM','CT','CN','CF','CG','IV','DJ','GB','GV','MA','ML','MR','NG','RW','SG','CD','TO','SF','AO', 'BC', 'CV', 'ET', 'GH', 'PU', 'EK', 'KE', 'MZ', 'WA', 'NI', 'UG', 'SE', 'SU', 'ZA', 'ZI', 'MA','TZ', 'CN', 'SO', 'SL', 'ER', 'LI', 'LT', 'MI'))] <- 'Afrique'
  df$region[is.element(df$Pays,c('RP','ID','AS','NZ','PP','MY','SN','BX','FJ','NH'))] <- 'Pacifique'
  df$region[is.element(df$Pays,c('CU','DR','HA','JM','TD','CS','HO','NU', 'MX', 'GT', 'PM', 'CO', 'BH', 'RN', 'ST', 'TB', 'VE', 'BR', 'AR', 'CI', 'BL', 'PE', 'EC', 'NS', 'GY', 'UY', 'PA'))] <- 'Latino'
  df$region[is.element(df$Pays,c('UK','EI','NL', 'IC', 'GL'))] <- 'EuropeOccidentale'
  df$region[is.element(df$Pays,c('TW','JA', 'KS', 'CH', 'VN', 'KN'))] <- 'AsieOrientale'
  # df$region[is.element(df$Pays,c('MA','TZ', 'CN'))] <- 'AfriqueEst'
  df$region[is.na(df$region)] <- df$Pays[is.na(df$region)]
  df2 <- aggregate(cbind(inscrits, Macron, exprimes) ~ region, df, sum)
  df2$Pays <- df2$region
  df2$Pays[df2$region=="AsieOrientale"] <- "CH"
  df2$Pays[df2$region=="Afrique"] <- "ML"
  df2$Pays[df2$region=="EuropeOccidentale"] <- "NL"
  df2$Pays[df2$region=="Latino"] <- "PM"
  df2$Pays[df2$region=="Pacifique"] <- "MY"
  df2 <- merge(df, df2, by="Pays", all.x=TRUE, sort=FALSE)
  return(df2)
}
monde3 <- region_monde(monde2)
names(monde3) <- c('Pays', 'Nom', 'inscrits', 'Macron', 'exprimes', 'region', 'region2', 'inscrits_region', 'Macron_region', 'exprimes_region')
monde3 <- merge(monde, monde3, by="Pays", all.x=TRUE, sort=FALSE)
monde3 <- monde3[,c(1,6,14:17,19:21)]
monde3$circo <- monde3$region
write.dbf(monde3, "Monde/étranger2017_regionalise.dbf")
# réunion des régions sur QGis dans étranger2017_regionalise.shp (edit > merged selected...).
# utiliser "résultat moins naif.shp" et les colonnes inscrits_region et region, sur autoredistrict


##### Préparation: populations #####
# Populations légales 2014 par communes (inclut la population de plus de 15 ans mais pas la population majeure, absente des données de l'INSEE)
# Source: https://www.insee.fr/fr/statistiques/2862200
pop <- read_excel("pop_communes_2014.xls")
pop <- pop[-c(1:5),]
names(pop) <- c("insee", "pop", "pop15")
pop$pop15 <- as.numeric(pop$pop) - as.numeric(pop$pop15)
# Je rajoute à la main les populations des arrondissements de Lyon à partir de l'excel définissant pop (elles sont dans une autre feuille du document):
pop[nrow(pop)+1,] <- c("69381", 29539,29539-4259)
pop[nrow(pop)+1,] <- c("69382", 29999, 29999-3882)
pop[nrow(pop)+1,] <- c("69383", 99819,99819-15869)
pop[nrow(pop)+1,] <- c("69384", 36101,36101-5840)
pop[nrow(pop)+1,] <- c("69385", 47302,47302-6936)
pop[nrow(pop)+1,] <- c("69386", 50419,50419-7343)
pop[nrow(pop)+1,] <- c("69387", 80993,80993-11224)
pop[nrow(pop)+1,] <- c("69388", 83619,83619-14433)
pop[nrow(pop)+1,] <- c("69389", 48824,48824-9016)

# Population par circonscription actuelle (2013) Source: https://www.insee.fr/fr/statistiques/2508230
pop_circos <- read_excel("pop_circos_2013.xls")
pop_circos <- as.numeric(pop_circos[-c(1:7),]$X__3)
# Ajout des français de l'étranger à partir de Wikipédia: https://fr.wikipedia.org/wiki/Circonscriptions_l%C3%A9gislatives_des_Fran%C3%A7ais_%C3%A9tablis_hors_de_France
# Renormalisation en termes de population totale et non d'inscrits. Source: , http://www.vie-publique.fr/focus/elections-2017-combien-electeurs-inscrits.html, https://fr.wikipedia.org/wiki/D%C3%A9mographie_de_la_France
ratio_pop_inscrits <- 67/45.7
pop_circos <- sort(c(pop_circos,ratio_pop_inscrits*c(259390,101084,173477,185502,112029,179597,151576,137560,171290,158312,152371))) 


##### Préparation: résultats électoraux #####
# election2017 <- read_excel("Résultats présidentielle/2017.xls")
# election2017$canton <- paste(election2017[[1]], election2017$X__2)
# election2017$Macron <- as.numeric(election2017$X__22)
# election2017$LePen <- as.numeric(election2017$X__29)
# election2017 <- election2017[-c(1:3),-c(1:32)]

election2012 <- read_excel("Résultats présidentielle/2012.xls")
election2012$canton <- paste(election2012[[1]], election2012[[3]])
election2012$Hollande <- election2012$Voix
election2012$Sarkozy <- election2012$Voix__1
election2012 <- election2012[,-c(1:27)]

# Résultat présidentielle	2012: circos 2012: 333/577 i.e. 57,71% / total national : 51,64	%	i.e. 279.96/577 pour Hollande
# Résultat présidentielle	2017: circos 2017: 532/577 i.e. 92,20% / total national : 61,10	%	i.e. 352.55/577 pour Macron

##### Préparation: IRIS #####
# Source des données géographiques (2015): http://professionnels.ign.fr/contoursiris
# Source des données démographqiues (2010): https://public.opendatasoft.com/explore/dataset/france-donnees-population-a-liris/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true
# /!\ Les deux jeux de données ne coïncident pas: 63 géographies n'ont pas de population, et 1544 géographies n'ont pas de population
# Le problème vient que les données de population ne sont pas à jour, et que les contours des IRIS ont changé
# On simplifie d'abord la géographie au maximum grâce à mapshaper.org, en lui donnant le .shp et le .dbf, etc.

# Il y a un problème quand autoredistrict calcule les centroids, donc on les calcule depuis QGis: 
#   Properties > Field > Field calculator > "x(centroid($geometry))" ...

# read.csv2("IRIS/population.csv")
# pop_iris$p15 <- pop_iris$p10_f15p + pop_iris$p10_h15p
# pop_iris <- pop_iris[,c(1:3,24)]
# names(pop_iris) <- c("id", "CODE_IRIS", "pop", "pop15") # dciris est le code de l'IRIS, p15 la population de 15 ans et plus
# iris_original <- read.dbf("IRIS/Métropole/deprecated/IRIS_simplifie.dbf")
# length(which(!is.element(iris$CODE_IRIS, pop_iris$CODE_IRIS)))
# length(which(!is.element(pop_iris$CODE_IRIS, iris$CODE_IRIS)))
# iris <- join(iris_original, pop_iris)
# iris$pop[is.na(iris$pop)] <- floor(mean(pop_iris$pop))
# # for (i in length(iris$CODE_IRIS)) { iris$pop[i] <- as.numeric(pop_iris$tot[pop_iris$CODE_IRIS==iris$CODE_IRIS[i]]) }
# # iris$pop[is.na(iris$pop)] <- floor(mean(pop_iris$pop))
# iris$id <- iris$CODE_IRIS
# iris <- iris[,c("CODE_IRIS", "id", "pop")]
# # names(iris) <- c("code", "id", "pop")
# (sum(pop_iris$pop)-sum(iris$pop))/sum(pop_iris$pop) # /!\ manque 3.5% de la population! => c'est sûrement les DOM (checker)
# sum(iris$pop)/293 # 212823 habitants par circo (cf. Répartition des députés...)
# rm(pop_iris)
# write.dbf(iris, "IRIS/Métropole/IRIS.dbf")


pop_iris <- read_xls("IRIS/population 2013.xls", skip=5)
pop_iris$p18 <- pop_iris$P13_POP - pop_iris$P13_POP0002 - pop_iris$P13_POP0305 - pop_iris$P13_POP0610 - pop_iris$P13_POP1117
pop_iris <- pop_iris[,c("IRIS", "p18")]
names(pop_iris) <- c("CODE_IRIS", "p18")
pop_iris$CODE_IRIS[pop_iris$CODE_IRIS=="800210601"] <- "800210606"
pop_iris$id <- pop_iris$CODE_IRIS
iris_original <- read.dbf("IRIS/Métropole/IRIS 2015 original.dbf")
length(which(!is.element(iris_original$CODE_IRIS, pop_iris$CODE_IRIS))) # 0
length(which(!is.element(pop_iris$CODE_IRIS, iris_original$CODE_IRIS))) # 690
iris <- join(iris_original, pop_iris)
iris <- iris[,c("CODE_IRIS", "p18", "id")]
sum(iris$p18)/293 # 170000 majeurs par circo (cf. Répartition des députés...)
rm(pop_iris)
write.dbf(iris, "IRIS/Métropole/IRIS 2015.dbf")


##### Paris #####
# Paris n'est pas divisé en cantons, mais chacun des 20 arrondissements contient 4 quartiers.
# J'ai ainsi choisi les quartiers comme unité de base pour créer les circonscriptions.
# Source des données démographiques (2012): http://opendata.apur.org/datasets/iris-demographie/geoservice (http://carto2.apur.org/apur/rest/services/OPENDATA/IRIS_DEMOGRAPHIE/MapServer/0/query?where=1%3D1&outFields=N_QU,P12_POP,P12_H0014,P12_F0014&returnGeometry=false&outSR=4326&f=json)
# Source des données géographiques: http://opendata.apur.org/datasets/quartier
quartiers_Paris <- read.csv("Paris/IRIS_DEMOGRAPHIE.csv")
quartiers_Paris <- quartiers_Paris[!is.na(quartiers_Paris$N_QU)&!is.na(quartiers_Paris$P12_POP),c("N_QU", "P12_POP", "P12_H0014", "P12_F0014")]
quartiers_Paris <- aggregate(cbind(P12_POP, P12_POP-P12_H0014-P12_F0014) ~ N_QU, quartiers_Paris, sum)
names(quartiers_Paris) <- c("quartier", "pop", "pop15")
write.dbf(quartiers_Paris, "Paris/QUARTIER.dbf")

## Initialement, j'avais calculé à part les circonscriptions parisiennes ##
# Le résultat est visible à Paris/circos_proposées.png (ou .ggs)
sum(quartiers_Paris$pop15)/160000 # 12 circonscriptions
regroupement_quartiers <- read.dbf("Paris/résultat.dbf", as.is = TRUE)
circonscriptions_Paris_proposees <- aggregate(cbind(pop, pop15) ~ quartier, regroupement_quartiers, sum)
regroupement_quartiers <- regroupement_quartiers[,-c(1)]
names(regroupement_quartiers) <- c("pop", "pop15", "circo_proposee")
quartiers_Paris$pop_rounded <- round(quartiers_Paris$pop)
regroupement_quartiers$pop_rounded <- round(regroupement_quartiers$pop)
quartiers_Paris <- merge(regroupement_quartiers, quartiers_Paris, by=c("pop_rounded"))
rm(regroupement_quartiers)
# Statistiques sur les résultats
sd(circonscriptions_Paris_proposees$pop15)/mean(circonscriptions_Paris_proposees$pop15) # 3,4%
(max(circonscriptions_Paris_proposees$pop15) - min(circonscriptions_Paris_proposees$pop15))/mean(circonscriptions_Paris_proposees$pop15) # 11%


##### Lyon Métropole ####
# Lyon Métropole n'est pas divisée en cantons. J'ai choisi comme unité de base les arrondissements lyonnais et les communes hors Lyon
# Source données géographiques: https://data.grandlyon.com/limitesadministratives/commune-du-grand-lyon-voies-et-adresses/ et https://data.grandlyon.com/limitesadministratives/arrondissement-de-lyon-voies-et-adresses/
# J'ai uni les données géographiques dans GQis, créant "Lyon/lyon reconstitué.shp"
# Ci-dessous, je combine ces données avec les données de population
communes_Lyon_dbf <- read.dbf("Lyon/lyon reconstitué.dbf", as.is = TRUE)
communes_Lyon_dbf$insee[is.na(communes_Lyon_dbf$insee)] <- communes_Lyon_dbf$inseecommu[is.na(communes_Lyon_dbf$insee)]
communes_Lyon <- communes_Lyon_dbf
communes_Lyon$nomreduit[is.na(communes_Lyon$nomreduit)] <- communes_Lyon$nomredui_2[is.na(communes_Lyon$nomreduit)]
# communes_Lyon$insee[is.na(communes_Lyon$insee)] <- communes_Lyon$inseecommu[is.na(communes_Lyon$insee)]
communes_Lyon$gid[is.na(communes_Lyon$gid)] <- communes_Lyon$gid_2[is.na(communes_Lyon$gid)]
communes_Lyon <- aggregate(communes_Lyon, list(communes_Lyon$insee), function(vec) { return(vec[1]) } )
communes_Lyon <- communes_Lyon[,c(3,4,6)]
names(communes_Lyon) <- c("commune", "insee", "gid")
communes_Lyon <- merge(communes_Lyon, pop)
communes_Lyon$circou_nouvelle <- communes_Lyon$insee
write.dbf(communes_Lyon, "Lyon/lyon.dbf")

## Initialement, j'avais calculé à part les circonscriptions parisiennes ##
sum(as.numeric(communes_Lyon$pop))/200000 # 7 circonscriptions à déterminer (6.77)
circos_Lyon_nouveau <- read.dbf("Lyon/résultat.dbf", as.is = TRUE)
circos_Lyon_nouveau <- aggregate(cbind(pop, pop15) ~ circou_nou, circos_Lyon_nouveau, sum)
# Statistiques sur les résultats
sd(circos_Lyon_nouveau$pop15)/mean(circos_Lyon_nouveau$pop15) # 3.0%
(max(circos_Lyon_nouveau$pop15) - min(circos_Lyon_nouveau$pop15))/mean(circos_Lyon_nouveau$pop15) # 8.8%


##### Inclusion de Paris et Lyon dans l'Hexagone #####
# Regroupement des données Paris, Lyon Métropole et l'Hexagone
paris <- read.dbf("Paris/QUARTIER.dbf", as.is=TRUE)
lyon <- read.dbf("Lyon/lyon.dbf", as.is=TRUE)
france <- read.dbf("cantons/cantons simple.dbf", as.is=TRUE)
paris$atom <- paris$quartier
lyon$atom <- lyon$insee
france$atom <- france$canton
france$pop <- france$population
paris <- paris[c('pop', 'atom')]
lyon <- lyon[c('pop', 'atom')]
france <- france[c('pop', 'atom')]
write.dbf(paris, "métropole/paris.dbf")
write.dbf(lyon, "métropole/lyon.dbf")
write.dbf(france, "métropole/france.dbf")
# Union géographique de lyon et france dans metropole.shp avec QGis, dont nettoyage des doublons lyonnais dans les attributs (Vector > Geoprocessing.. > Union)
metropole <- read.dbf("métropole/metropole.dbf", as.is=TRUE)
metropole$atom[is.na(metropole$atom)] <- metropole$atom_2[is.na(metropole$atom)]
metropole$pop[is.na(metropole$pop)] <- metropole$pop_2[is.na(metropole$pop)]
metropole <- metropole[c('pop', 'atom')]
write.dbf(metropole, "métropole/metropole.dbf")
# Union de paris et metropole dans tout.shp après chgt du CRS de paris (save as...), puis nettoyage des doublons dans Qgis (s'aider de duplicated(tout$atom))
tout <- read.dbf("métropole/tout2.dbf", as.is=TRUE)
tout$atom[is.na(tout$atom)] <- tout$atom_2[is.na(tout$atom)]
tout$pop[is.na(tout$pop)] <- tout$pop_2[is.na(tout$pop)]
tout <- tout[c('pop', 'atom')]
tout$circo <- tout$atom
tout$pop <- round(as.numeric(tout$pop))
write.dbf(tout, "métropole/tout.dbf")


##### Répartition des députés entre métropole et le reste #####
# Il y a 313 circonscriptions à répartir d'après ce rapport p.28 : http://www2.assemblee-nationale.fr/static/reforme-an/Rapport-1-GT.pdf
# Je fais le choix de séparer l'Hexagone, la Corse, chaque DROM, la Polynésie et les français établis à l'étranger dans des ensembles de circonscriptions distincts
# Je n'ai pas inclus la Nouvelle-Calédonie, dont le statut va sûrement changé en 2018.
# Dans un souci de cohérence avec le découpage automatique, c'est la population totale qui est considérée.
# Dans l'absolu, il faudrait répartir les nombre de sièges à partir de la population majeure.
# Les données non contenues dans pop ont été obtenues sur Wikipedia: https://fr.wikipedia.org/wiki/Collectivit%C3%A9_d%27outre-mer et https://fr.wikipedia.org/wiki/Fran%C3%A7ais_%C3%A9tablis_hors_de_France
pop_tot <- sum(as.numeric(pop$pop))+240000+331834+1710945 # Population totale 2014, où j'ai rajouté Mayotte, les COM et les français de l'étranger (2015) à la main
pop_par_circo <- pop_tot/313  # 219478 habitants par circo
sum(as.numeric(pop$pop[as.numeric(pop$insee)<96000|is.na(as.numeric(pop$insee))]))/pop_par_circo # 294 circos pour métropole + Corse (294.04)
1710945/pop_par_circo # 8 circos pour français de l'étranger (7.80)
331834/pop_par_circo # 2 circos pour les COM (Polynésie, etc.) (1.51), mais je propose de ne pas mettre dans une même circo des îles de l'Atlantique et du Pacifique :
(268707+12197)/pop_par_circo # 1 circo pour la Polynésie + Wallis-et-Futuna (1.28)
sum(as.numeric(pop$pop[is.na(as.numeric(pop$insee))]))/pop_par_circo # 1 circo pour la Corse (1.48)
(240000+sum(as.numeric(pop$pop[as.numeric(pop$insee)>96000&!is.na(as.numeric(pop$insee))])))/pop_par_circo # 10 circos pour les DROM (9.69)
sum(as.numeric(pop$pop[as.numeric(pop$insee)<96000&!is.na(as.numeric(pop$insee))]))/pop_par_circo # 293 circos pour l'Hexagone (métropole hors Corse) (292.56)
# Je fais le choix de séparer les COM et de créer une circonscription Polynésie+Wallis-et-Futuna (dans le Pacifique) 
# et de ratacher les îles de l'Atlantique à des circonscriptions de la Martinique et de la Guadeloupe
# On aboutit bien à un total de 313 circos (ou 314 si on laisse 2 circos à la Corse), ce qui laisse 90 (ou 89) députés à la proportionnelles 
# Le découpage est effectué automatiquement pour l'Hexagone, pour les français de l'étranger et pour la Réunion
# Pour la Corse, la Guyane, la Martinique, la Guadeloupe, Mayotte, et les autres îles, j'ai effectué le découpage moi-même, car il n'y avait qu'une issue cohérente
# La répartition dans les DROM est la suivante:
sum(as.numeric(pop$pop[grepl('971', pop$insee)]))/pop_par_circo # 2 Guadeloupe (1.82)
sum(as.numeric(pop$pop[grepl('972', pop$insee)]))/pop_par_circo # 2 Martinique (1.75)
(sum(as.numeric(pop$pop[grepl('971', pop$insee)]))+sum(as.numeric(pop$pop[grepl('972', pop$insee)]))+331834-268707)/pop_par_circo #  Guadeloupe, Martinique et COM autres que Polynésie*: 3.86 *(Walis-et-Futuna 12 197, Saint-Martin 35 594, Saint-Barthélémy 9 279 et Saint-Pierre-et-Miquelon 6 057)
sum(as.numeric(pop$pop[grepl('973', pop$insee)]))/pop_par_circo # 1 Guyane (1.15)
sum(as.numeric(pop$pop[grepl('974', pop$insee)]))/pop_par_circo # 4 Réunion (3.84)

## Nouveaux calculs en prenant comme base 343 circonscriptions (0.85*404):
pop_par_circo <- pop_tot/343  # 200282 habitants par circo
sum(as.numeric(pop$pop[as.numeric(pop$insee)<96000|is.na(as.numeric(pop$insee))]))/pop_par_circo # 322 circos pour métropole + Corse (322.22)
1265255/pop_par_circo # 6 circos pour français de l'étranger (6.32)
331834/pop_par_circo # 2 circos pour les COM (Polynésie, etc.) (1.66), mais je propose de ne pas mettre dans une même circo des îles de l'Atlantique et du Pacifique :
(268707+12197)/pop_par_circo # 1 circo pour la Polynésie + Wallis-et-Futuna (1.40) (les COM de l'Atlantique sont attachés aux DROM, cf. ci-dessus)
sum(as.numeric(pop$pop[is.na(as.numeric(pop$insee))]))/pop_par_circo # 2 circos pour la Corse (1.62)
(240000+sum(as.numeric(pop$pop[as.numeric(pop$insee)>96000&!is.na(as.numeric(pop$insee))])))/pop_par_circo # 11 circos pour les DROM (10.58)
sum(as.numeric(pop$pop[as.numeric(pop$insee)<96000&!is.na(as.numeric(pop$insee))]))/pop_par_circo # 321 circos pour l'Hexagone (métropole hors Corse) (320.60)
# il serait mieux d'avoir un autre nombre de circonscriptions, pour que les arrondis aillent dans le bon sens


##### Autoredistrict.org #####
# Exécuter autoredistricting à partir de "tout.shp", "Monde/étranger2017_regionalise.shp" et "cantons/Réunion.shp"
# Pour ce faire: en ligne de commande, se placer dans le bon dossier, et pour lancer autoredistricting:
# java -jar autoredistrict.jar -Xmx4096M -Xms1024M
# J'ai utilisé les paramètres suivants pour l'Hexagone : num. of districts: 293, pop. column: pop, district column: circo
# anneal rate: 3%, mutation: 95%, elitism: 40%, elites mutated: 100%
# geometric: 100%, contiguous: 80% (non coché), compact: 5%, equal pop: 100% (coché)
# Ces paramètres ont été utilisés à la fin des 50.000 itérations (ce qui a pris un jour de calculs), avant, j'avais pris
# les mêmes paramètres sauf elitism: 5%, elites mutated: 0%, et equal pop non coché.


##### Résultats: préparation #####
cantons_resultat <- read.dbf("Résultats/résultat.dbf", as.is = TRUE) # À chaque canton est associé une circo
circos_nouvelles <- aggregate(pop ~ circo, cantons_resultat, sum) # Les populations de chaque circo
# Ajout des régions hors Hexagone
circos_monde <- read.dbf("Monde/resultat moins naif.dbf", as.is = TRUE) # À chaque pays/région est associé une circo des français de l'étranger
circos_monde$circo <- paste('etr', circos_monde$c.AC..Anti)
circos_monde <- aggregate(cbind(inscrits_r, exprimes_r) ~ circo, circos_monde, sum) # Les populations de chaque circo
# Renormalisation des populations des français de l'étranger, seules exprimées en nombre d'inscrits plutôt qu'en population totale
circos_monde$pop <- circos_monde$inscrits_r*mean(circos_nouvelles$pop)/mean(circos_monde$inscrits_r)
circos_nouvelles <- merge(circos_nouvelles, circos_monde[,c(1,4)], all=T)
# Choix d'une circonscription corse, on aurait pu en mettre deux
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "Corse", pop = sum(as.numeric(pop$pop[grepl('2A',pop$insee)|grepl('2B',pop$insee)])))
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "Polynésie&Walis", pop = 268707+12197)
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "Mayotte", pop = 240000)
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "Guyane", pop = sum(as.numeric(pop$pop[grepl('973', pop$insee)])))
# La Guadeloupe et la Martinique sont divisées en deux circonscriptions ayant autant d'arrondissements
# On attribue Saint-Barthélémy et Saint-Pierre-et-Miquelon à Basse-Terre (Guadeloupe) et Saint-Martin à Fort-de-France (Martinique)
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "St-Barthélémy&St-Pierre-et-Miquelon&Guadeloupe_Basse-Terre", pop = 9279+6057+190255)
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "Guadeloupe_Pointe-a-Pitre", pop = 209931)
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "Martinique_Marin&Trinite", pop = 119693+80415)
circos_nouvelles[length(circos_nouvelles$pop)+1,] <- c(circo = "St-Martin&Martinique_Fort-France&St-Pierre", pop = 35594+160498+23305)
reunion_resultat <- read.dbf("cantons/Réunion.dbf", as.is = TRUE)
circos_reunion <- aggregate(population ~ ref, reunion_resultat, sum)
circos_reunion$circo <- paste('reu', circos_reunion$ref)
circos_reunion <- circos_reunion[,c(2,3)]
names(circos_reunion) <- c('pop', 'circo')
circos_nouvelles <- merge(circos_nouvelles, circos_reunion, all=T)
circos_nouvelles$pop <- as.numeric(circos_nouvelles$pop)


##### Résultats: statistiques #####
# Exemples de circos non contigues : 33 19 (!) / 18 1 / 44 4 /40 5 / 42 4 / 6 11 / 77 12 / 59 12 / 7 13 / 15 11 / 4 9 / 17 4 / 2 3 / 12 3 / 34 10 / 9 11 

# Différence de population entre les extrêmes (en proportion de la moyenne), actuel:
(max(pop_circos)-min(pop_circos))/mean(pop_circos) #  Max-min: 313%
(pop_circos[576]-pop_circos[4])/mean(pop_circos) #  Max-min: 174% Hexagone seulement
# et dans le redécoupage proposé:
(max(circos_nouvelles$pop) - min(circos_nouvelles$pop))/mean(circos_nouvelles$pop) #  Max-min: 65%
(max(circos_nouvelles$pop[circos_nouvelles$pop<270000]) - min(circos_nouvelles$pop))/mean(circos_nouvelles$pop[circos_nouvelles$pop<270000])
#  Max-min: 32% en enlevant la Corse et la Polynésie&Wallis

# Average absolute difference (i.e. écart moyen entre les populations de deux circonscriptions aléatoires: E[|X-Y|]):
aad <- aad_nou <- 0
for (i in 1:577) {  for (j in 1:577) {    aad <- aad + abs(pop_circos[i] - pop_circos[j])  } }
for (i in 1:313) {  for (j in 1:313) {    aad_nou <- aad_nou + abs(circos_nouvelles$pop[i] - circos_nouvelles$pop[j])  } }
aad <- aad/577^2/mean(pop_circos) # actuel: 17.7% de la moyenne
aad_nou <- aad_nou/313^2/mean(circos_nouvelles$pop) # proposé: 6.1% de la moyenne

# Écart-type actuel (en proportion de la moyenne)
sd(pop_circos)/mean(pop_circos) # sd: 20.5%
# et dans le redécoupage proposé:
sd(circos_nouvelles$pop)/mean(circos_nouvelles$pop)
# sd: 6.2% de 216k, la taille moyenne
sd(circos_nouvelles$pop[circos_nouvelles$pop<270000])/mean(circos_nouvelles$pop[circos_nouvelles$pop<270000])
# sd: 5.2% en enlevant la Corse et la Polynésie&Wallis

# Nombre de circonscriptions dont la population s'écarte de plus de 20% de la moyenne, par le dessus puis par le dessous
nb_inf <- length(which(pop_circos<0.8*mean(pop_circos))) # 47, soit 8.1%
nb_sup <- length(which(pop_circos>1.2*mean(pop_circos))) # 24, soit (24+47)/577=12.3%
# et dans le redécoupage proposé:
nb_inf_nou <- length(which(circos_nouvelles$pop<0.8*mean(circos_nouvelles$pop))) # 0
nb_sup_nou <- length(which(circos_nouvelles$pop>1.2*mean(circos_nouvelles$pop))) # 2


##### Graphique #####
plot(seq(0,1, by=1/576), quantile(pop_circos/mean(pop_circos), probs=seq(0,1, by=1/576)), axes=FALSE, ylab="Population (par rapport à la moyenne)", xlab="Circonscriptions, des moins peuplées au plus peuplées",  type='s', lwd=2) + grid()
lines(seq(0,(nb_inf-1)/576, by=1/576), pop_circos[1:nb_inf]/mean(pop_circos), col="red", type="s", lwd=2)
lines(seq((577-nb_sup)/576,1, by=1/576), pop_circos[(578-nb_sup):577]/mean(pop_circos), col="red", type="s", lwd=2)
legend("topleft", title="Population des circonscriptions en proportion de la moyenne", text.col=c("black", "green", "red"), lwd=2, lty=1, col=c("black", "green", "red"), legend=c("Découpage actuel", "Découpage proposé", "Taille s'écartant d'au moins 20% de la moyenne"))
lines(seq(0,1,by=1/312), sort(circos_nouvelles$pop)/mean(circos_nouvelles$pop), col="green", lwd=2, type='s')
lines(seq(311/312,1,by=1/312), sort(circos_nouvelles$pop[which(circos_nouvelles$pop>1.2*mean(circos_nouvelles$pop))])/mean(circos_nouvelles$pop), col="red", lwd=2, type='s')
abline(h=seq(0.8,1.2,by=0.4), col="grey",lty=2) + abline(h=1, col="grey",lty=3)
axis(2, at=c(0,0.4,0.8, 1.2,1.6,2,2.4,2.8)) + axis(1) + box()


##### Corrélations taille/résultats électoraux #####
# Données électorales de 2012 par canton. Source: https://www.data.gouv.fr/s/resources/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-2nd-tour/20170511-092258/Presidentielle_2017_Resultats_Tour_2_c.xls
res_2012 <- read.csv2("Résultats présidentielle/2012_minimal.csv")
res_2012$Hollande <- as.numeric(as.vector(res_2012$Hollande))
sum(res_2012$Hollande>50)/577 # Hollande aurait obtenu 57.71% de sièges théoriques contre 51.64% de score national
plot(res_2012$Inscrits, res_2012$Hollande) # On voit que Saint-Pierre-et-Miquelon profite nettement à Hollande. Pour le reste, l'effet est moins marqué, mais quand même présent (cf. ci-dessous)
cor(res_2012$Hollande, res_2012$Inscrits) # -0.19 de corrélation entre le nombre d'inscrits et le score de Hollande
summary(lm(res_2012$Hollande ~ res_2012$Inscrits)) # -1.3e-4*** Score de Hollande régressé sur nombre d'inscrits
# !!! 12660 inscrits en moins est associé avec 1% de plus pour Hollande
summary(lm(I(res_2012$Hollande>50) ~ res_2012$Inscrits)) # -3.7e-6* Linear probability model:
# i.e. 37k inscrits en plus augmentent la proba de victoire d'1% toutes choses égales par ailleurs
proba_Hollande <- glm(I(Hollande>50) ~ Inscrits, data=res_2012, family = binomial(link = "probit")) # Régression probit
summary(proba_Hollande) # -9.7e-6* Résultat significatif mais dont la taille est non interprétable (cf. ci-dessous)
probitmfx(I(Hollande>50) ~ Inscrits, data=res_2012) # average marginal effect at mean: -3.8e-6*. Même résultat qu'au-dessus, mais interprétable:
# i.e. on average 38k (1.75sd) inscrits en plus augmentent la proba de victoire d'1% toutes choses égales par ailleurs (résultat semblable au modèle linéaire)

effet <- aad*mean(pop_circos)/(12660*ratio_pop_inscrits) # 1.14 Effet sur le score de Hollande (en %) associé à une population de circonscription réduite de l'écart moyen entre deux circonscriptions aléatoires
length(which(election2012$Hollande/(election2012$Hollande+election2012$Sarkozy) < 0.5+effet/100 & 0.5 < election2012$Hollande/(election2012$Hollande+election2012$Sarkozy)))/length(election2012$canton) 
# dans 4.6% des circos, Hollande a gagné avec moins de 51.14%, et il aurait donc perdu si la circo avait été plus grande


##### Résultats électoraux dans le redécoupage proposé #####



##### Résultats IRIS #####
resultats_iris <- read.dbf("IRIS/Métropole/hexagone2015.dbf")
circos_iris <- aggregate(p18 ~ id, resultats_iris, sum) # Les populations de chaque circo
plot(seq(0,1, by=1/576), quantile(pop_circos/mean(pop_circos), probs=seq(0,1, by=1/576)), axes=FALSE, ylab="Population (par rapport à la moyenne)", xlab="Circonscriptions, des moins peuplées au plus peuplées",  type='s', lwd=2) + grid()
lines(seq(0,1,by=1/292), sort(circos_iris$pop)/mean(circos_iris$pop), col="green", lwd=2, type='s')
# Différence de population entre les extrêmes (en proportion de la moyenne), actuel:
(pop_circos[576]-pop_circos[4])/mean(pop_circos) #  Max-min: 174% Hexagone seulement
# et dans le redécoupage proposé:
(max(circos_iris$pop) - min(circos_iris$pop))/mean(circos_iris$pop) #  Max-min: 65%

# Average absolute difference (i.e. écart moyen entre les populations de deux circonscriptions aléatoires: E[|X-Y|]):
aad <- aad_nou <- 0
for (i in 1:577) {  for (j in 1:577) {    aad <- aad + abs(pop_circos[i] - pop_circos[j])  } }
for (i in 1:293) {  for (j in 1:293) {    aad_nou <- aad_nou + abs(circos_iris$pop[i] - circos_iris$pop[j])  } }
aad <- aad/577^2/mean(pop_circos) # actuel: 17.7% de la moyenne
aad_nou <- aad_nou/293^2/mean(circos_iris$pop) # proposé: 10.0% de la moyenne

# Écart-type actuel (en proportion de la moyenne)
sd(pop_circos)/mean(pop_circos) # sd: 20.5%
# et dans le redécoupage proposé:
sd(circos_iris$pop)/mean(circos_iris$pop)
# sd: 9.4% de 212k, la taille moyenne

# Nombre de circonscriptions dont la population s'écarte de plus de 20% de la moyenne, par le dessus puis par le dessous
nb_inf <- length(which(pop_circos<0.8*mean(pop_circos))) # 47, soit 8.1%
nb_sup <- length(which(pop_circos>1.2*mean(pop_circos))) # 24, soit (24+47)/577=12.3%
# et dans le redécoupage proposé:
nb_inf_nou <- length(which(circos_iris$pop<0.8*mean(circos_iris$pop))) # 4
nb_sup_nou <- length(which(circos_iris$pop>1.2*mean(circos_iris$pop))) # 6



##### Sauvegarde #####
save.image("env.RData")

## TODO
# Comparer les résultats des dernières élections présidentielles avec le découpage actuel et mon découpage.
# Remplacer pop par majeurs
# Calculer les résultats électoraux sur Paris


##### Préparation IRIS Guy Lifshitz #####
iris1_2 <- read.dbf("IRIS_Guy_final/decoupage1-2 original.dbf")
iris1_2 <- iris1_2[,c("CODE_IRIS", "P14_POP", "centroid_l", "centroid_1", "departemen", "c", "w")]
write.dbf(iris1_2, "IRIS_Guy_final/decoupage1-2.dbf")

iris1_2['iris'] <- iris1_2['CODE_IRIS']
iris1_2 <- iris1_2[c('P14_POP', 'iris')]
resultats_iris1_2 <- read.dbf("IRIS_Guy_final/iris_decoupage1-5.dbf")
resultats_iris1_2 <- merge(resultats_iris1_2, iris1_2)
circos_iris <- aggregate(P14_POP ~ c, resultats_iris1_2, sum) # Les populations de chaque circo

plot(seq(0,1, by=1/576), quantile(pop_circos/mean(pop_circos), probs=seq(0,1,by=1/576)), axes=FALSE, ylab="Population (par rapport à la moyenne)", xlab="Circonscriptions, des moins peuplées au plus peuplées",  type='s', lwd=2) + grid()
lines(seq(0,(nb_inf-1)/576, by=1/576), pop_circos[1:nb_inf]/mean(pop_circos), col="red", type="s", lwd=2)
lines(seq((577-nb_sup)/576,1, by=1/576), pop_circos[(578-nb_sup):577]/mean(pop_circos), col="red", type="s", lwd=2)
legend("topleft", title="Population des circonscriptions en proportion de la moyenne", text.col=c("black", "green", "red"), lwd=2, lty=1, col=c("black", "green", "red"), legend=c("Découpage actuel", "Découpage proposé", "Taille s'écartant d'au moins 20% de la moyenne"))
lines(seq(0,1,by=1/319), sort(circos_iris$P14_POP)/mean(circos_iris$P14_POP), col="green", lwd=2, type='s')
lines(seq(0,(nb_inf_nou-1)/319, by=1/319), sort(circos_iris$P14_POP)[1:nb_inf_nou]/mean(circos_iris$P14_POP), col="red", type="s", lwd=2)
abline(h=seq(0.8,1.2,by=0.4), col="grey",lty=2) + abline(h=1, col="grey",lty=3)
axis(2, at=c(0,0.4,0.8, 1.2,1.6,2,2.4,2.8)) + axis(1) + box()

circos <- read.dbf("cantons_Guy_final/cantons_decoupage2-5.dbf")
circos <- aggregate(pop ~ c, circos, sum) # Les populations de chaque circo

plot(seq(0,1, by=1/576), quantile(pop_circos/mean(pop_circos), probs=seq(0,1, by=1/576)), axes=FALSE, ylab="Population (par rapport à la moyenne)", xlab="Circonscriptions, des moins peuplées au plus peuplées",  type='s', lwd=2) + grid()
lines(seq(0,(nb_inf-1)/576, by=1/576), pop_circos[1:nb_inf]/mean(pop_circos), col="red", type="s", lwd=2)
lines(seq((577-nb_sup)/576,1, by=1/576), pop_circos[(578-nb_sup):577]/mean(pop_circos), col="red", type="s", lwd=2)
legend("topleft", title="Population des circonscriptions en proportion de la moyenne", text.col=c("black", "green", "red"), lwd=2, lty=1, col=c("black", "green", "red"), legend=c("Découpage actuel", "Découpage proposé", "Taille s'écartant d'au moins 20% de la moyenne"))
lines(seq(0,1,by=1/319), sort(circos$pop)/mean(circos$pop), col="green", lwd=2, type='s')
lines(seq(0,(nb_inf_nou-1)/319, by=1/319), sort(circos$pop)[1:nb_inf_nou]/mean(circos$pop), col="red", type="s", lwd=2)
abline(h=seq(0.8,1.2,by=0.4), col="grey",lty=2) + abline(h=1, col="grey",lty=3)
axis(2, at=c(0,0.4,0.8, 1.2,1.6,2,2.4,2.8)) + axis(1) + box()

# Différence de population entre les extrêmes (en proportion de la moyenne), actuel:
(pop_circos[576]-pop_circos[4])/mean(pop_circos) #  Max-min: 174% Hexagone seulement
# et dans le redécoupage proposé:
(max(circos_iris$P14_POP) - min(circos_iris$P14_POP))/mean(circos_iris$P14_POP) #  Max-min: 42%

# Average absolute difference (i.e. écart moyen entre les populations de deux circonscriptions aléatoires: E[|X-Y|]):
aad <- aad_nou <- 0
for (i in 1:577) {  for (j in 1:577) {    aad <- aad + abs(pop_circos[i] - pop_circos[j])  } }
for (i in 1:320) {  for (j in 1:320) {    aad_nou <- aad_nou + abs(circos_iris$P14_POP[i] - circos_iris$P14_POP[j])  } }
aad <- aad/577^2/mean(pop_circos) # actuel: 17.7% de la moyenne
aad_nou <- aad_nou/320^2/mean(circos_iris$P14_POP) # proposé: 6.3% de la moyenne

# Écart-type actuel (en proportion de la moyenne)
sd(pop_circos)/mean(pop_circos) # sd: 20.5%
# et dans le redécoupage proposé:
sd(circos_iris$P14_POP)/mean(circos_iris$P14_POP)
# sd: 5.7% de 200k, la taille moyenne

# Nombre de circonscriptions dont la population s'écarte de plus de 20% de la moyenne, par le dessus puis par le dessous
nb_inf <- length(which(pop_circos<0.8*mean(pop_circos))) # 47, soit 8.1%
nb_sup <- length(which(pop_circos>1.2*mean(pop_circos))) # 24, soit (24+47)/577=12.3%
# et dans le redécoupage proposé:
nb_inf_nou <- length(which(circos_iris$P14_POP<0.8*mean(circos_iris$P14_POP))) # 2
nb_sup_nou <- length(which(circos_iris$P14_POP>1.2*mean(circos_iris$P14_POP))) # 0

##### Biais du futur redécoupage gouvernemental #####
# Source: https://query.wikidata.org/#%23%20D%C3%A9put%C3%A9s%20fran%C3%A7ais%20de%20la%20XVe%20l%C3%A9gislature%0A%23SELECT%20%3Fdepute%20%3FdeputeNom%20%3FcirconscriptionNom%20%3FdateDebutMandat%20%3FdateFinMandat%20%3FdateNaissance%20%3FgroupeParlementaireNom%20%28SAMPLE%28%3Fphoto%29%20AS%20%3Fphotographie%29%20%3Farticle%0ASELECT%20%3FdeputeNom%20%3FcirconscriptionNom%20%3Fnumero%20%3Fpopulation%20%3FgroupeParlementaireNom%0AWHERE%20%7B%0A%20%20%3Fdepute%20wdt%3AP39%20wd%3AQ3044918%20.%0A%20%20%3Fdepute%20p%3AP39%20%3Fmandat%20.%0A%20%20%3Fmandat%20pq%3AP2937%20wd%3AQ24939798%20.%0A%20%20%3Fmandat%20pq%3AP580%20%3FdateDebutMandat%20.%0A%20%20OPTIONAL%20%7B%20%3Fdepute%20rdfs%3Alabel%20%3FdeputeNom%20.%20FILTER%28%20LANG%28%3FdeputeNom%29%20%3D%20%22fr%22%20%29%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fdepute%20wdt%3AP734%20%3FnomFamille%20.%20%3FnomFamille%20rdfs%3Alabel%20%3FnomFamilleNom%20.%20FILTER%28%20LANG%28%3FnomFamilleNom%29%20%3D%20%22fr%22%20%29%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fmandat%20pq%3AP582%20%3FdateFinMandat%20%7D%20.%0A%20%20%23FILTER%20NOT%20EXISTS%20%7B%20%3Fmandat%20pq%3AP582%20%3FdateFinMandat%20%7D%20.%20%23%20D%C3%A9commenter%20pour%20avoir%20les%20mandats%20en%20cours%20%28c-%C3%A0-d%20sans%20date%20de%20fin%29%0A%20%20%23%3Fmandat%20pq%3AP582%20%3FdateFinMandat%20.%20%23%20D%C3%A9commenter%20pour%20avoir%20les%20mandats%20termin%C3%A9s%20%28c-%C3%A0-d%20avec%20une%20date%20de%20fin%29%0A%20%20FILTER%28%20%3FdateDebutMandat%20%3D%20%222017-06-21%22%5E%5Exsd%3AdateTime%20%29%20.%20%23%20D%C3%A9commenter%20pour%20avoir%20les%20mandats%20commenc%C3%A9s%20apr%C3%A8s%20le%2021%20juin%202017%0A%20%20OPTIONAL%20%7B%20%3Fmandat%20pq%3AP768%20%3Fcirconscription%20.%20%3Fcirconscription%20rdfs%3Alabel%20%3FcirconscriptionNom%20.%20FILTER%28%20LANG%28%3FcirconscriptionNom%29%20%3D%20%22fr%22%20%29%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fcirconscription%20wdt%3AP131%20%3Fdepartement%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fdepartement%20wdt%3AP1082%20%3Fpopulation%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fdepartement%20wdt%3AP2586%20%3Fnumero%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fdepute%20wdt%3AP569%20%3FdateNaissance%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fmandat%20pq%3AP1365%20%3Fremplace%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fmandat%20pq%3AP1366%20%3Fremplacepar%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Fmandat%20pq%3AP4100%20%3FgroupeParlementaire%20.%20%3FgroupeParlementaire%20rdfs%3Alabel%20%3FgroupeParlementaireNom%20.%20FILTER%28%20LANG%28%3FgroupeParlementaireNom%29%20%3D%20%22fr%22%20%29%20%7D%20.%0A%20%20%23OPTIONAL%20%7B%20%3Fdepute%20wdt%3AP18%20%3Fphoto%20%7D%20.%0A%20%20OPTIONAL%20%7B%20%3Farticle%20schema%3Aabout%20%3Fdepute%3B%20schema%3AisPartOf%20%3Chttps%3A%2F%2Ffr.wikipedia.org%2F%3E%20%7D%20.%0A%20%20BIND%28%20REPLACE%28%20UCASE%28%20REPLACE%28%20%3FnomFamilleNom%2C%20%22%5Ede%20%22%2C%20%22%22%20%29%20%29%2C%20%22%C3%89%22%2C%20%22E%22%20%29%20AS%20%3Fnom%20%29%20.%0A%7D%0A%23GROUP%20BY%20%3Fdepute%20%3FdeputeNom%20%3FcirconscriptionNom%20%3FdateDebutMandat%20%3FdateFinMandat%20%3FdateNaissance%20%3FgroupeParlementaireNom%20%3Farticle%0AORDER%20BY%20%3FdateFinMandat%20%3FdateDebutMandat%20%3Fnom
deputes <- read.csv(file = "deputes2017.csv")
deputes$departement <- deputes$numero
deputes <- deputes[order(deputes$population),]
deputes <- deputes[!is.na(deputes$population),]
deputes <- deputes[!duplicated(deputes$circonscriptionNom),]
deputes$groupe <- deputes$groupeParlementaireNom
deputes$groupe <- revalue(deputes$groupe, c('groupe Gauche démocrate et républicaine'='GDR', 'groupe La France insoumise'='LFI', 
                          'groupe La République en marche !'='EM', 'groupe Les Républicains'='LR', 'groupe Mouvement démocrate'='Modem',
                          'groupe Nouvelle Gauche'='NG', 'groupe UDI, Agir et indépendants'='UDI'))
groupes <- levels(deputes$groupe)[levels(deputes$groupe)!=""]
for (grp in groupes) deputes[grp] <- deputes$groupe==grp
temp <- aggregate(cbind(GDR, LFI, NG, Modem, UDI, EM, LR) ~ departement, deputes, mean)
for (grp in groupes) temp[paste('prop_',grp,sep='')] <- temp[grp]
temp <- temp[,names(temp)[which(!is.element(names(temp),groupes))]]
deputes <- merge(deputes, temp, by='departement', all.x=TRUE)

seuil <- function(r) sqrt(floor(r)*ceiling(r))
parlementaires_par_departement <- function(nb_senateurs=244, nb_deputes=335) {
  pop_total <- sum(as.numeric(deputes$population[which(!duplicated(deputes$departement))]))
  departements <- dpts <- deputes[which(!duplicated(deputes$departement)),]
  prepare <- function(dpts, pop_parl) {
    dpts$ratio_parl <- dpts$population/pop_parl
    dpts$floor_parl <- floor(dpts$ratio_parl)
    dpts$ceil_parl <- ceiling(dpts$ratio_parl)
    dpts$nb_parl <- dpts$floor_parl*(seuil(dpts$ratio_parl)>dpts$ratio_parl) + dpts$ceil_parl*(seuil(dpts$ratio_parl)<=dpts$ratio_parl)
    return(dpts)
  }
  for (parl in c('dep', 'sen')) {
    nb_parl <- nb_deputes*(parl=='dep')+nb_senateurs*(parl=='sen')
    pop_parl <- pop_total/nb_parl
    dpts <- prepare(dpts, pop_parl)
    # pop_senateur <- pop_total/nb_senateurs
    # dpts$ratio_sen <- dpts$population/pop_senateur
    # dpts$floor_sen <- floor(dpts$ratio_sen)
    # dpts$ceil_sen <- ceiling(dpts$ratio_sen)
    # if (!(all(dpts$floor_dep!=dpts$ratio_dep) & all(dpts$floor_sen!=dpts$ratio_sen))) print('/!\ integer ratio')
    # dpts$nb_sen <- dpts$floor_sen*(seuil(dpts$ratio_sen)>dpts$ratio_sen) + dpts$ceil_sen*(seuil(dpts$ratio_sen)<=dpts$ratio_sen)
    nb_parl_iter <- sum(dpts$nb_parl)
    step <- pop_parl/2
    while ((nb_parl_iter != nb_parl) & step > 10^-3) {
      if (nb_parl_iter < nb_parl) {
        pop_parl <- pop_parl - step
        dpts <- prepare(dpts, pop_parl)
      } else {
        pop_parl <- pop_parl + step
        dpts <- prepare(dpts, pop_parl)
      }
      step <- step / 2
      nb_parl_iter <- sum(dpts$nb_parl)
    }
    if (nb_parl_iter != nb_parl) print(paste('Fail to match number of', parl))
    if (parl=='dep') departements$nb_dep <- dpts$nb_parl
    else departements$nb_sen <- dpts$nb_parl
    print(paste('Target population by ', parl, ': ', round(pop_parl), ' (instead of average of: ', round(pop_total/nb_parl), ')', sep=''))
    for (grp in groupes) departements[paste('nb_',parl,'_',grp,sep='')] <- dpts$nb_parl*dpts[paste('prop_',grp,sep='')]
  }
  return(departements)
}
parl_par_departement <- parlementaires_par_departement()

for (grp in groupes) { print(paste('Pourcentage de députés', grp, 'actuels:', round(sum(deputes[,grp])/5.66,2), 'vs.', 
              round(sum(parl_par_departement[paste('prop_',grp,sep='')]*parl_par_departement$population/(1.06*mean(parl_par_departement$population))),2), 
              "dans l'idéal vs.", round(sum(parl_par_departement[paste('nb_dep_',grp,sep='')])/3.35,2), '(ou',
              round(sum(parl_par_departement[paste('nb_sen_',grp,sep='')])/2.44,2), 'sénateurs) futurs hypothétiques:')) }
(0.5463-0.5406)*244 # 1.4 sénateurs en plus pour EM
(0.5483-0.5406)*335 # 2.6 députés en plus pour EM

parl_par_departement$prop_gauche <- parl_par_departement$prop_GDR + parl_par_departement$prop_LFI + parl_par_departement$prop_NG
parl_par_departement$prop_centre <- parl_par_departement$prop_EM + parl_par_departement$prop_UDI + parl_par_departement$prop_Modem
View(parl_par_departement[,c('departement', 'population', 'nb_dep', 'nb_sen', 'prop_gauche', 'prop_centre', 'prop_EM', 'prop_LR', 'circonscriptionNom')])
# Écart max entre le nombre d'habitants par sénateur (resp. par député) de Lozère et des Landes (resp. de l'Orne): 
403234/76309 # 5.28 Sénat (actuellement, c'est de 4 entre la Seine-Saint-Denis et la Creuse, cf. https://fr.wikipedia.org/wiki/Nombre_de_parlementaires_sous_la_Cinqui%C3%A8me_R%C3%A9publique_fran%C3%A7aise#Nombre_de_d%C3%A9put%C3%A9s_et_de_s%C3%A9nateurs_par_d%C3%A9partement_ou_territoire)
286618/76309 # 3.76 Assemblée
round(sum(deputes$GDR+deputes$LFI+deputes$NG)/5.66,2) # 11.3% de députés de gauche
round(sum(deputes$EM)/5.66,2) # 54.1% de députés EM
round(sum(deputes$UDI+deputes$Modem+deputes$EM)/5.66,2) # 67.8% de députés de gauche
round(sum(deputes$LR)/5.66,2) # 17.8% de députés de gauche


##### Inégalités Cantons #####
cantons_osm$dep[cantons_osm$dep=="2A"] <- 210
cantons_osm$dep[cantons_osm$dep=="2B"] <- 211
cantons_osm$dep <- as.numeric(cantons_osm$dep)
cantons_osm$population <- as.numeric(cantons_osm$population)
dep_gap_max <- 0
threshold <- 1.5
seuil <- 0.15
gap_max <- 1
count <- 0
for (d in unique(cantons_osm$dep)) { 
  gap <- max(cantons_osm$population[cantons_osm$dep==d])/min(cantons_osm$population[cantons_osm$dep==d])
  if ((max(cantons_osm$population[cantons_osm$dep==d])>(1+seuil)*mean(cantons_osm$population[cantons_osm$dep==d]))
      | (min(cantons_osm$population[cantons_osm$dep==d])<(1-seuil)*mean(cantons_osm$population[cantons_osm$dep==d]))) {
    count <- count + 1
  }
  if (gap > threshold) {
    print(paste("Le département", d, "présente un écart de population dans ses cantons de 1 à", round(gap,2)))
    # count <- count + 1
  }
  if (gap > gap_max) {
    gap_max <- gap
    dep_gap_max <- d
  }
}
print(paste(count, "départements présentent un écart de population de leurs cantons supérieur à", threshold)) # 34 pour 1.5 (79 pour 1.4)
max(cantons_osm$population)/min(cantons_osm$population) # 19 sur toute la France
print(paste("Le département", dep_gap_max, "présente l'écart de population maximal entre ses cantons: de 1 à", round(gap_max,2)))

##### Sénat : Collège électoral #####
conseillers_municipaux <- function(nb_hab) {
  return((nb_hab<100)*7 + (nb_hab>=100)*11 + (nb_hab>=500)*4 + (nb_hab>=1500)*4 + (nb_hab>=2500)*4 + (nb_hab>=3500)*4 + (nb_hab>=5000)*2 + (nb_hab>=10000)*4
    + (nb_hab>=20000)*2 + (nb_hab>=30000)*4  + (nb_hab>=40000)*4  + (nb_hab>=50000)*2  + (nb_hab>=60000)*4  + (nb_hab>=80000)*4  
    + (nb_hab>=100000)*2  + (nb_hab>=150000)*4  + (nb_hab>=200000)*2 + (nb_hab>=250000)*4  + (nb_hab>=300000)*4 
    )  
}
grand_electeur <- function(nb_hab) {
  return((nb_hab<500) + (nb_hab>=500)*3 + (nb_hab>=1500)*2 + (nb_hab>=2500)*2 + (nb_hab>=3500)*8 + (nb_hab>=9000)*14 + (nb_hab>=10000)*4
    + (nb_hab>=20000)*2 + (nb_hab>=30000)*4  + (nb_hab>=40000)*4  + (nb_hab>=50000)*2  + (nb_hab>=60000)*4  + (nb_hab>=80000)*4  
    + (nb_hab>=100000)*2  + (nb_hab>=150000)*4  + (nb_hab>=200000)*2 + (nb_hab>=250000)*4  + (nb_hab>=300000)*4 
    + pmax(floor((nb_hab-30000)/800), 0)
    )
}
sous40k <- seq(0,40000,by=100)
sous60k <- seq(0,60000,by=100)
sous100k <- seq(0,95000,by=1000)
sous500k <- seq(0,500000,by=1000)
plot(sous40k, grand_electeur(sous40k), type='l', main="Nombre de grands électeurs par commune", xlab="Population de la commune", ylab="Nombre de grands électeurs")
plot(sous60k, grand_electeur(sous60k), type='l', main="Nombre de grands électeurs par commune", xlab="Population de la commune", ylab="Nombre de grands électeurs")
plot(sous100k, grand_electeur(sous100k), type='l', main="Nombre de grands électeurs par commune", xlab="Population de la commune", ylab="Nombre de grands électeurs")
lines(sous40k, conseillers_municipaux(sous40k), type='l', col='blue')
lines(sous500k, floor(1+sous500k/800), type='l', col='red')
lines(sous100k, 1+sous100k/600, type='l', col='orange')
abline(v = 27000)
abline(v = 30000)
abline(v = seq(0,100000,by=10000), lty=3, col="grey")
abline(h = seq(0,120,by=10), lty=3, col="grey")
legend(x="topleft", legend = c("Actuel", "Proposé"), col=c("black", "red"), lwd=1)

##### 6 circos de l'étranger #####
etranger <- read.dbf('Monde/resultat moins naif.dbf')
etranger <- aggregate(inscrits_r ~ Pays, etranger, sum)
