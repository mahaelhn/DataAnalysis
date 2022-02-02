air1=unique(ontime_february$AIRLINE_ID)
nbr=c()
nbr_c=c()
nbr_d=c()

for(co in air1)
  {
  s=subset(ontime_february, ontime_february$AIRLINE_ID==co)
  #nombre d'annulation
  nbr_c=c(nbr_c, sum(s$CANCELLED==1,na.rm=TRUE))
  nbr_d=c(nbr_d, sum(s$ARR_DELAY_NEW>0,na.rm=TRUE))
  nbr=c(nbr,nrow(s))
}

airline=data.frame(Airline=air1,nbr=nbr,nbr_cancelled=nbr_c, nbr_deplayed=nbr_d)
airline

air <- table(ontime_february$AIRLINE_ID)
air

#Q2
airline$prc_c=airline$nbr_cancelled/airline$nbr
airline$prc_d=airline$nbr_deplayed/airline$nbr

#Q3
index=which.min(airline$prc_d)
X=airline$Airline[index]

View(airline)
#Q4
min(airline$prc_d)*(airline$nbr)
(1-min(airline$prc_d))*(airline$nbr)

#???4a

#on peut pas faire test hypothes sur echantillion prit la ligne 6 , mettre na.rm==TRUE
