
#Mirar 1er funció findFn del paquet sos (veure sobretot darrer apartat "Combining search results"
#https://cran.r-project.org/web/packages/sos/vignettes/sos.pdf)
install.packages('sos')
library(sos) 
#PL <- findFn('Petal.Length')

#setwd("F:/2017_18/Articles_escrits/R_packages_Sports/TFG")


sports <- findFn('sports')
football <- findFn('football')
basketball <- findFn('basketball')
soccer <- findFn('soccer')
tennis <- findFn('tennis')
volleyball <- findFn('volleyball')
handball <- findFn('handball')
water_polo <- findFn('water polo')
hockey <- findFn('hockey')
baseball <- findFn('baseball')
martial <- findFn('martial')
judo <- findFn('judo')
sumo <- findFn('sumo')
taekwondo <- findFn('taekwondo')
karate <- findFn('karate')
aikido <- findFn('aikido')
jujutsu <- findFn('jujutsu')
kendo <- findFn('kendo')
capoeira <- findFn('capoeira')
athletics <- findFn('athletics')
running <- findFn('running') #care with not sports packages
jumping <- findFn('jumping') 
climbing <- findFn('climbing')
swimming <- findFn('swimming')
throwing <- findFn('throwing')
walking <- findFn('walking')
cycling <- findFn('cycling')
riding <- findFn('riding')
surfing <- findFn('surfing')
racing <- findFn('racing')
diving <- findFn('diving')
archery <- findFn('archery')
rugby <- findFn('rugby')
badminton <- findFn('badminton')
cricket <- findFn('cricket')
croquet <- findFn('croquet')
softball <- findFn('softball')
chess <- findFn('chess')
futsal <- findFn('futsal')
golf <- findFn('golf')
squash <- findFn('squash')
polo <- findFn('polo')
paddle <- findFn('paddle')
gymnastics <- findFn('gymnastics')
alpinism <- findFn('alpinism')
skiing <- findFn('skiing')
wrestling <- findFn('wrestling')
boxing <- findFn('boxing')
calisthenics <- findFn('calisthenics')
curling <- findFn('curling')
javelin <- findFn('javelin')
marathon <- findFn('marathon')
darts <- findFn('darts')
nfl <- findFn('NFL');
nba <- findFn('NBA') 
mlb <- findFn('MLB') 
nhl <- findFn('NHL') 
mls <- findFn('MLS')
acb <- findFn('ACB')
liga <- findFn('liga')
league <- findFn('league')
ligue <- findFn('ligue')
calcio <- findFn('calcio')
serie_a <- findFn('serie A')
f1 <- findFn('f1')
moto_gp <- findFn('moto gp')
nascar <- findFn('nascar')
champions <- findFn('champions')
uefa <- findFn('uefa')
olympics <- findFn('olympics')
game <- findFn('game')#care with not sports packages
player <- findFn('player')
accelerometer <- findFn('accelerometer')
ball <- findFn('ball')#care with not sports packages
team <- findFn('team')
cup <- findFn('cup')#care with not sports packages
score <- findFn('score')#care with not sports packages
tournament <- findFn('tournament')
kayak <- findFn('kayak')
canoe <- findFn('canoe')
racket <- findFn('racket')
fifa <- findFn('fifa')
fiba <- findFn('fiba')
espn <- findFn('espn')
athlon <- findFn('athlon')
ncaa <- findFn("ncaa")

all <- sports | football | basketball | soccer | tennis | volleyball | handball | water_polo | hockey | baseball |
       martial | judo | sumo | taekwondo | karate | aikido | jujutsu | kendo | capoeira | athletics| running | 
       jumping | climbing | swimming | throwing | walking | cycling | riding | surfing | racing | diving | 
       archery | rugby | badminton | cricket | croquet | softball | chess | futsal | golf | squash | polo | 
       paddle | gymnastics | alpinism | skiing | wrestling | boxing | calisthenics | curling | javelin | marathon |
       darts | nfl | nba | mlb | nhl | mls | acb | liga | league | ligue | calcio | serie_a | f1 | moto_gp | 
       nascar | champions | uefa | olympics | game | player | accelerometer | ball | team | cup | score | 
       tournament | kayak | canoe | racket | fifa | fiba | espn | athlon | ncaa

#installPackages(all, minCount = 1)

#writeFindFn2xls(all)

llistat<-unique(all$Package)  # Paquets únics
N_packages <- length(llistat)
N_packages <- length(unique_packages)    # Número de paquets únics
intersect(llistat, llistatV) #per veure els que coincideixen
llistat[! llistat %in% llistatV] #en el llistat trobat però no en el nostra manual excel


install.packages("pkgsearch")
??pkgsearch

pkg_search()


