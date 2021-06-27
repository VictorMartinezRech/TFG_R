rm(list=ls())
load('dades.RData')

##########################################################
# Library
##########################################################
library(tidyverse)
library(knitr)
library(kableExtra)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(cowplot)
library(gapminder)
library(ggrepel)
library(ggridges)
library(openxlsx)
library(kml)
library(kml3d)
library(data.table)
library(factoextra)
library(cluster)
library(ggplot2)
library(egg)

##########################################################
# Filtrar por minutos
##########################################################
##-- Filter players with more than XX minutes
min_min <- 30                                        # Minutos mínimos
min_sum <- tapply(d2021$Minutos,d2021$player_id,sum) # Minutos por jugadora
boxplot(min_sum)                                     # Distribución minutos temporada  
max(min_sum)
min(min_sum)
length(min_sum)                                     # Num. jugadoras
sum(min_sum<min_min)                                 # Jugadoras que no llegan a minutos mínimos
sel_players <- as.numeric(names(min_sum[min_sum>=min_min]))
d2021b <- d2021 %>% filter(player_id %in% sel_players)


##########################################################
# Data_frame por k_means
##########################################################
d2021b_kmeans <- as.data.table(d2021b)
d2021b_kmeans <- d2021b_kmeans[,.(Puntos_min=mean(Puntos_min,na.rm=TRUE),
                                  T2_Anotados_min=mean(T2_Anotados_min,na.rm=TRUE),
                                  T2_Lanzados_min=mean(T2_Lanzados_min,na.rm=TRUE),
                                  Perc_T2_min=mean(Perc_T2_min,na.rm=TRUE),
                                  T3_Anotados_min= mean(T3_Anotados_min,na.rm=TRUE),
                                  T3_Lanzados_min=mean(T3_Lanzados_min,na.rm=TRUE),
                                  Per_T3_min=mean(Per_T3_min,na.rm=TRUE),
                                  T1_Anotados_min=mean(T1_Anotados_min,na.rm=TRUE),
                                  T1_Lanzados_min=mean(T1_Lanzados_min,na.rm=TRUE),
                                  Per_T1_min= mean(Per_T1_min,na.rm=TRUE),
                                  Reb_Of_min= mean(Reb_Of_min,na.rm=TRUE),
                                  Reb_Def_min=mean(Reb_Def_min,na.rm=TRUE),
                                  Reb_tot_min= mean(Reb_tot_min,na.rm=TRUE),
                                  Asist_min=mean(Asist_min,na.rm=TRUE),
                                  Robos_min=mean(Robos_min,na.rm=TRUE),
                                  Perdidas_min=mean(Perdidas_min,na.rm=TRUE),
                                  Tapones_min=mean(Tapones_min,na.rm=TRUE),
                                  Tapones_Reci_min=mean(Tapones_Reci_min,na.rm=TRUE),
                                  Mates_min=mean(Mates_min,na.rm=TRUE),
                                  Faltas_Com_min=mean(Faltas_Com_min,na.rm=TRUE),
                                  Faltas.Rec_min=mean(Faltas.Rec_min,na.rm=TRUE),
                                  Valoracion_min=mean(Valoracion_min,na.rm=TRUE),
                                  FG_min=mean(FG_min,na.rm=TRUE),
                                  FGA_min=mean(FGA_min,na.rm=TRUE),
                                  FT_min=mean(FT_min,na.rm=TRUE),
                                  FTA_min=mean(FTA_min,na.rm=TRUE),
                                  eFG_min=mean(eFG_min,na.rm=TRUE),
                                  TSP_min=mean(TSP_min,na.rm=TRUE),
                                  Winscore_min=mean(Winscore_min,na.rm=TRUE),
                                  Valoracion_without_P_min=mean(Valoracion_without_P_min,na.rm=TRUE),
                                  FTrati=mean(FTrati,na.rm=TRUE),
                                  PTC_min=mean(PTC_min,na.rm=TRUE),
                                  UR_min=mean(UR_min,na.rm=TRUE)),
                               by=.(player_id,Nombre,nombre_short,team_id,equipo_short,Equipo)]

#c(7,29:36,38:39)

set.seed(0420)

#Miramos las k gráficamente
calc_withinss <- function(data, centers) {
  km_fit <- select(data, c(7,29:36,38:39)) %>%
    kmeans(centers = centers, nstart = 10)
  km_fit$tot.withinss
}

tibble(centers = 1:15) %>%
  mutate(
    within_sum_squares = map_dbl(
      centers, ~calc_withinss(d2021b_kmeans, .x)
    )
  ) %>%
  ggplot(aes(centers, within_sum_squares)) +
  geom_point() +
  geom_line() +
  theme_bw()

#Parece que el codo se empieza a notar con 4-5 centers, pero hay un segundo codo en los 8 centers

set.seed(0420)
km_fit <- d2021b_kmeans %>% 
  select(c(7,29:36,38:39)) %>%
  kmeans(centers = 8, nstart = 10)

km_fit #Con 8 centers se obtiene un 91.5% de la variabilidad explicada

km_fit$cluster #cluster donde se encuentra cada jugadora

km_fit$centers #medias por cluster

#Gráficos descriptivos numéricos

comp.df <- cbind(d2021b_kmeans, km_fit$cluster) #añadimos el clúster a la base de datos
names(comp.df)[40] <- "cluster"

comp.df$cluster <- as.factor(comp.df$cluster)

#Puntos por minuto

boxplot(km_fit$centers[,1], main = "Puntos por minuto")

P_min <- ggplot(comp.df, aes(x = cluster, y = Puntos_min)) + 
  geom_boxplot()

P_min

#FG por minuto

boxplot(km_fit$centers[,2], main = "FG por minuto")

FG_min <- ggplot(comp.df, aes(x = cluster, y = FG_min)) + 
  geom_boxplot()

FG_min

#FGA por minuto

boxplot(km_fit$centers[,3], main = "FGA por minuto")

FGA_min <- ggplot(comp.df, aes(x = cluster, y = FGA_min)) + 
  geom_boxplot()

FGA_min

#FT por minuto

boxplot(km_fit$centers[,4], main = "FT por minuto")

FT_min <- ggplot(comp.df, aes(x = cluster, y = FT_min)) + 
  geom_boxplot()

FT_min

#FTA por minuto

boxplot(km_fit$centers[,5], main = "FTA por minuto")

FTA_min <- ggplot(comp.df, aes(x = cluster, y = FTA_min)) + 
  geom_boxplot()

FTA_min

#eFG por minuto

boxplot(km_fit$centers[,6], main = "eFG por minuto")

eFG_min <- ggplot(comp.df, aes(x = cluster, y = eFG_min)) + 
  geom_boxplot()

eFG_min

#TSP por minuto

boxplot(km_fit$centers[,7], main = "TSP por minuto")

TSP_min <- ggplot(comp.df, aes(x = cluster, y = TSP_min)) + 
  geom_boxplot()

TSP_min

#Winscore por minuto

boxplot(km_fit$centers[,8], main = "Winscore por minuto")

Winscore_min <- ggplot(comp.df, aes(x = cluster, y = Winscore_min)) + 
  geom_boxplot()

Winscore_min

#VWP por minuto

boxplot(km_fit$centers[,9], main = "Valoración sin puntos por minuto")

VWP_min <- ggplot(comp.df, aes(x = cluster, y = Valoracion_without_P_min)) + 
  geom_boxplot()

VWP_min

#PTC por minuto

boxplot(km_fit$centers[,10], main = "PTC por minuto")

PTC_min <- ggplot(comp.df, aes(x = cluster, y = PTC_min)) + 
  geom_boxplot()

PTC_min

#UR por minuto

boxplot(km_fit$centers[,11], main = "UR por minuto")

UR_min <- ggplot(comp.df, aes(x = cluster, y = UR_min)) + 
  geom_boxplot()

UR_min

ggarrange(P_min, FG_min, FGA_min, FT_min, FTA_min, eFG_min, ncol = 2)
ggarrange(TSP_min, Winscore_min, VWP_min, PTC_min, UR_min, ncol = 2)
ggarrange(P_min, FG_min, FGA_min, FT_min, FTA_min, eFG_min,TSP_min, Winscore_min, ncol =2)
ggarrange(FTA_min, eFG_min,TSP_min, Winscore_min, VWP_min, PTC_min, UR_min, ncol = 2)

#Dendograma de los clusters
set.seed(0420)
res2 <- hcut(d2021b_kmeans[,c(7,29:36,38:39)], k = 8, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5)

#Gráfico para visualizar los clústers mediante los componentes principales (1a y 2a dimensión)
fviz_cluster(km_fit, data = d2021b_kmeans[,c(7,29:36,38:39)],
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

fviz_cluster(km_fit, data = d2021b_kmeans[,c(7,29:36,38:39)])

d2021b_kmeans[c(52,40,146),] #en teoria las mejores jugadoras

#Gráfico para visualizar los puntos y los centroides en los clústers
fviz_cluster(km_fit, data = d2021b_kmeans[,c(7,29:36,38:39)], ellipse.type = "euclid",repel = TRUE,star.plot = TRUE)

#Gráfico para mirar los puntos más cercanos entre clústers
fviz_cluster(res.km, data = d2021b_kmeans[,c(7,29:36,38:39)], ellipse.type = "norm")

pc1 <- prcomp(d2021b_kmeans[,c(7,29:36,38:39)], scale=TRUE)
print(pc1)

Psi <- pc1$x[,1:2]

Phi <- cor(d2021b_kmeans[,c(7,29:36,38:39)],Psi)
Phi

par(mfrow = c(2,2))
plot(comp.df$cluster[comp.df$team_id == 42], main = "Perfumerías Avenida")
plot(comp.df$cluster[comp.df$team_id == 44], main = "Spar Girona")
plot(comp.df$cluster[comp.df$team_id == 46], main = "Valencia B.C")



##########################################################
# Data_frame por k_means longitudinal
##########################################################
##-- Trayectories
VAR <- c('Puntos_min','T2_Anotados_min')   # Hecho por 2 variables, se deben añadir todas las estandarizadas por minuto
N_player <- length(table(d2021b$Nombre))   # Número de jugadoras
N_jorn <- max(d2021b$Jornada_num)          # Número de jornadas
N_ind <- length(VAR)                       # Número de indicadores

d2021b_kmeans_3d <- array(dim = c(N_player,N_jorn,N_ind))
for(v in VAR){
  z = which(VAR==v)
  d2021b_kmeans_3d[,,z] <- tapply(d2021b[,v],list(d2021b$Nombre,d2021b$Jornada_num),mean,na.rm=TRUE)  
}

M <- d2021b_kmeans_3d

##-- Object to cluster
cld3d <- clusterLongData3d(traj=M,
                           idAll=levels(as.factor(d2021b$Nombre)),
                           time=1:29,
                           varNames=c("P","T2"),
                           maxNA=10)
##-- Kmaens longitudinal
set.seed(12345)
kml3d(cld3d, nbClusters = 2:4, nbRedrawing = 5, toPlot = "traj") # Slow porque dibujas trajectorias
choice(cld3d)
getClusters(cld3d, 3) ##--> Obtener los clusters y representarlos





