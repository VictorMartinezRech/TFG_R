##########################################################################
# April 2020
#
# The following R code allows to replicate all the analyses and examples
# in the book "Basketball Data Science" (by P. Zuccolotto and M. Manisera),
# forthcoming as a CRC Press publication.
# It is based on the "BasketballAnalyzeR" package developed with M. Sandri.
# See 
# https://bdsports.unibs.it/basketballanalyzer/
# for further explanations and updates
###########################################################################

##############################################
#Warning: If you want to reproduce the figures
#contained in the book and if the version of
#your R machine is > 3.6.0, you need to type
#RNGkind(sample.kind="Rounding")
#at the beginning of your working session
##############################################

rm(list=ls())
# install.packages("devtools", repos="https://cran.stat.unipd.it/")
# devtools::install_github("sndmrc/BasketballAnalyzeR",force=TRUE)
library(BasketballAnalyzeR)
data(package="BasketballAnalyzeR")

load('dades.RData')
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
library(gridExtra)

min_min <- 30                                        # Minuts mínims
min_sum <- tapply(d2021$Minutos,d2021$player_id,sum) # Minuts per jugadora
boxplot(min_sum)                                     # Distribució minuts temporada  
max(min_sum)
min(min_sum)
length(min_sum)                                     # Num. jugadores
sum(min_sum<min_min)                                 # Jugadores que no arriben a minuts mínims
sel_players <- as.numeric(names(min_sum[min_sum>=min_min]))
d2021b <- d2021 %>% filter(player_id %in% sel_players)

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

d2021b2 <- as.data.table(d2021b)
d2021b2 <- d2021b2[,.(Minutos=mean(Minutos,na.rm=TRUE),
                      Puntos=mean(Puntos,na.rm=TRUE),
                      P_min=mean(Puntos_min,na.rm=TRUE),
                      T2_Anotados=mean(T2_Anotados,na.rm=TRUE),
                      T2_Lanzados=mean(T2_Lanzados,na.rm=TRUE),
                      T3_Anotados= mean(T3_Anotados,na.rm=TRUE),
                      T3_Lanzados=mean(T3_Lanzados,na.rm=TRUE),
                      T1_Anotados=mean(T1_Anotados,na.rm=TRUE),
                      T1_Lanzados=mean(T1_Lanzados,na.rm=TRUE),
                      Reb_Of= mean(Reb_Of,na.rm=TRUE),
                      Reb_Def=mean(Reb_Def,na.rm=TRUE),
                      Reb_tot= mean(Reb_tot,na.rm=TRUE),
                      Asist=mean(Asist,na.rm=TRUE),
                      Robos=mean(Robos,na.rm=TRUE),
                      Perdidas=mean(Perdidas,na.rm=TRUE),
                      Tapones=mean(Tapones,na.rm=TRUE),
                      Tapones_Reci=mean(Tapones_Reci,na.rm=TRUE),
                      Mates=mean(Mates,na.rm=TRUE),
                      Faltas_Com=mean(Faltas_Com,na.rm=TRUE),
                      Faltas.Rec=mean(Faltas.Rec,na.rm=TRUE),
                      Valoracion=mean(Valoracion,na.rm=TRUE),
                      FG=mean(FG,na.rm=TRUE),
                      FGA=mean(FGA,na.rm=TRUE),
                      FT=mean(FT,na.rm=TRUE),
                      FTA=mean(FTA,na.rm=TRUE)),
                   by=.(player_id,Nombre,nombre_short,team_id,equipo_short,Equipo)]

d2021b2 <- cbind(d2021b2, (d2021b2$T2_Anotados/d2021b2$T2_Lanzados))
names(d2021b2)[32] <- "Per_T2"
d2021b2 <- cbind(d2021b2, (d2021b2$T3_Anotados/d2021b2$T3_Lanzados))
names(d2021b2)[33] <- "Per_T3"
d2021b2 <- cbind(d2021b2, (d2021b2$T1_Anotados/d2021b2$T1_Lanzados))
names(d2021b2)[34] <- "Per_T1"

#Bar line plot

Pbox.HR <- subset(d2021b2, player_id==613 |
                          player_id==627 |
                          player_id==634 |
                          player_id==693 |
                          player_id==733)
barline(data=Pbox.HR, id="Nombre",
        bars=c("Per_T2","Per_T3","Per_T1"), line="Minutos",
        order.by="P_min", labels.bars=c("2P%","3P%","FT%"),
        title="Porcentajes de Acierto Mejor Quinteto")
??barline
#Radial plot (Mejor quinteto)

Pbox.PG <- subset(d2021b2, player_id==613 |
                    player_id==627 |
                    player_id==634 |
                    player_id==693 |
                    player_id==733)

attach(Pbox.PG)
X <- data.frame(T2_Anotados, T3_Anotados, T1_Anotados, Reb_tot, Asist,
                Robos, Tapones)/Minutos
detach(Pbox.PG)
radialprofile(data=X, title=Pbox.PG$Nombre, std=FALSE)

radialprofile(data=X, title=Pbox.PG$Nombre, std=TRUE)


#Scatterplot (relacionar métricas, marcar jugadoras de un equipo)

Pbox.sel <- subset(d2021b2, Minutos>= 20)
attach(Pbox.sel)
X <- data.frame(Asist, Perdidas, Puntos)/Minutos
X <- data.frame(X, Minutos)
detach(Pbox.sel)
mypal <- colorRampPalette(c("blue","yellow","red"))

scatterplot(X, data.var=c("Asist","Puntos"), z.var="Minutos",
            labels=Pbox.sel$Nombre, palette=mypal)

SPAR <- which(Pbox.sel$equipo_short=="SPAR")
scatterplot(X, data.var=c("Asist","Puntos"), z.var="Minutos",
            labels=Pbox.sel$player_id, palette=mypal,
            subset=SPAR)


#Bubbleplot (relacionar métricas por equipos)

Pbox.PER.VAL <- subset(d2021b2,
                       equipo_short=="PERFUMERIAS" &
                         Minutos>=20 |
                         equipo_short =="VALENCIA" &
                         Minutos>=20)
attach(Pbox.PER.VAL)
X <- data.frame(ID=Nombre, Equipo, V1=Reb_Def/Minutos, V2=Robos/Minutos,
                V3=Tapones/Minutos, V4=Minutos)
detach(Pbox.PER.VAL)
labs <- c("Rebotes Defensivos por minuto","Robos por minuto","Tapones por minuto",
          "Minutos jugados por partido")
bubbleplot(X, id="ID", x="V1", y="V2", col="V3",
           size="V4", text.col="Equipo", labels=labs,
           title="Perfumerias Avenida y Valencia B.C durante la temporada regular",
           text.legend=TRUE, text.size=3.5, scale.size=FALSE)

#Índice de Gini (medir la desigualdad existente entre las jugadoras de los equipos en función de los puntos conseguidos en la temporada)

Pbox.PER <- subset(d2021b2, equipo_short=="PERFUMERIAS")
ineqPER <- inequality(Pbox.PER$Puntos, nplayers=12)
p1 <- plot(ineqPER, title="Perfumerias Avenida")
p1

Pbox.VAL <- subset(d2021b2, equipo_short=="VALENCIA")
ineqVAL <- inequality(Pbox.VAL$Puntos, nplayers=12)
p2 <- plot(ineqVAL, title="Valencia B.C")
p2

grid.arrange(p1, p2, nrow=1)

# MDS map (escalado multidimensional --> visualizar X variables en un mapa bidimensional (similitudes entre jugadoras))

attach(d2021b2)
data <- data.frame(Puntos, T3_Anotados, T2_Anotados, Reb_tot, Asist, Perdidas,
                Robos, Tapones)
detach(d2021b2)
data <- subset(data, d2021b2$Minutos>=20)
id <- d2021b2$player_id[d2021b2$Minutos>=20]

mds <- MDSmap(data)
plot(mds, labels=id)

selp <- which(id==613 |
              id==627 |
              id==634 |
              id==693 |
              id==733)

plot(mds, labels=id, subset=selp, col.subset="tomato")

par(mfrow=c(2, 2))
plot(mds, labels=id, z.var=c("T3_Anotados"),
     level.plot=FALSE, palette=topo.colors)
plot(mds, labels=id, z.var=c("T3_Anotados"),
     level.plot=FALSE, palette=topo.colors, subset=selp, col.subset="tomato")


# Densidad conjunta de dos variables
data <- subset(d2021b2, Minutos>=20)
attach(data)
X <- data.frame(Puntos, T3_Anotados, T2_Anotados, Reb_tot, Asist)/Minutos
detach(data)

scatterplot(X, data.var=1:5,
            lower=list(continuous="density"),
            diag=list(continuous="densityDiag"))

# Clustering (agrupar jugadoras perfiles)

attach(d2021b2)
data <- data.frame(Puntos, T3_Anotados, T2_Anotados, Reb_tot, Asist, Perdidas,
                   Robos, Tapones, Faltas_Com)

detach(d2021b2)

data <- subset(data, d2021b2$Minutos>=20)
id <- d2021b2$player_id[d2021b2$Minutos>=20]

hclu1 <- hclustering(data)
plot(hclu1)

hclu2 <- hclustering(data, labels=id, k=8)
plot(hclu2, profiles=TRUE)

# Regresión lineal simple

Pbox.sel <- subset(d2021b2, Minutos>=20)
attach(Pbox.sel)
X <- Puntos/Minutos
Y <- Asist/Minutos
Pl <- Nombre
detach(Pbox.sel)
out <- simplereg(x=X, y=Y, type="lin")
xtitle <- "Puntos por minuto"
ytitle <- "Asistencias por minuto"

plot(out, labels=Pl, subset="quant",
     Lx=0, Ux=0.97, Ly=0, Uy=0.97,
     xtitle=xtitle, ytitle=ytitle)

# Gaussian kernel smoothing

data <- subset(d2021b2, Minutos>=20)
attach(data)
X <- data.frame(Puntos, T3_Anotados, T2_Anotados, Reb_tot, Asist)/Minutos
detach(data)

scatterplot(X, data.var=1:5,
            lower=list(continuous="smooth_loess"),
            diag=list(continuous="barDiag"))

# Gráficos interactivos

library(plotly)
Pbox.sel <- subset(d2021b2, Minutos>=20)
attach(Pbox.sel)
X <- data.frame(Asist,Perdidas, PTSpm=Puntos)/Minutos
detach(Pbox.sel)
mypal <- colorRampPalette(c("blue","yellow","red"))
p5 <- scatterplot(X, data.var=c("Asist","Perdidas"),
                  z.var="PTSpm", palette=mypal)
ggplotly(p5, tooltip="text")

data <- d2021b2[c(136:147,183:194), c("Puntos", "T3_Anotados", "T2_Anotados","Reb_tot","Equipo")]
p6 <- scatterplot(data, data.var=1:4, z.var="Equipo")
ggplotly(p6)
