gfortran -o waves_logfile_rfmt waves_logfile_rfmt.f
ls *LOG*
ls *log*
echo "Enter input filename from WAVESMON program?"
read file1
rm $file1.jpg
echo "Enter output filename for analysis using R?"
read wavefile
rm wave.bat
echo $file1 > wave.bat
echo $wavefile >> wave.bat
echo 9 >> wave.bat
./waves_logfile_rfmt < wave.bat
echo "Enter corresponding adcp curent meter deployment data?"
echo "  Enter cruise number now ie vr067?      "
echo ""
read cruno
echo ""
echo "  Enter station number now?    "
echo ""
read station
rm adcprequest.sql
rm aadcprequest.sql
rm waveplot2.deck.Rout
rm waveplot2.deck
echo "
drop table adcpplot;
create table adcpplot as select
losierr.adcpplan.cruno,losierr.adcpplan.station,deploy,segment,depthsur,depthbot,
speed,direction,
veleast,velnorth,intensity,
unit_magvar,area_magvar
from losierr.adcpvel,losierr.adcpplan where
losierr.adcpvel.cruno='$cruno' and
losierr.adcpplan.cruno='$cruno' and
losierr.adcpplan.station=$station and
losierr.adcpvel.station=$station;
update adcpplot
set area_magvar=(area_magvar*-1.0);
update adcpplot
set direction=direction+360 where direction<area_magvar;
update adcpplot
set direction=direction-area_magvar;
SET VERIFY OFF;
SET ECHO OFF;
SET FEEDBACK OFF;
SET HEADING ON;
SET WRAP ON;
set trim on;
SET SHOW OFF;
SET VERIFY OFF;
SET PAGESIZE 0;
SET LINESIZE 132;
set numwidth 4;
SET TERMOUT OFF;
column latitude format 999.99999;
column longitude format 999.99999;
column depthbot format 999.99;
column depthsur format 999.99;
COLUMN SPEED  FORMAT 999.9;
column seg format 99999;
COLUMN dir FORMAT 9999;
column cordir format 999.9;
column ve FORMAT 999.9;
column vn FORMAT 999.9;
COLUMN mag FORMAT 999.9;
column ddmmyyyyhhmiss format a20;
column depth format 999.99;
column int format 999.9;
SPOOL adcp$cruno\_$station.lis;
SELECT deploy,I.segment seg,depthbot,depthsur,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddmmyyyyhhmiss ,
speed,direction dir,intensity int,
sin(direction*2.0*3.141592654/360)*speed ve,
cos(direction*2.0*3.141592654/360)*speed vn,
latitude,longitude,tidehgt,depth
FROM losierr.adcpinf I,adcpplot V
where
I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment" > aadcprequest.sql
if [ "$file1" = "adcpvr067_a_log9.TXT" ]
then
echo "and V.segment < 3956" >> aadcprequest.sql
fi
if [ "$file1" = "adcpvr067_b_log9.TXT" ]
then
echo "and V.segment >= 3956" >> aadcprequest.sql
fi
echo "order by seg,depthbot;
exit;
" >> aadcprequest.sql
sqlplus losierr/For5tat5@ptran @aadcprequest.sql
mv adcp$cruno\_$station.lis adcp$cruno\_$station.rose
echo  YOUR OUTPUT FILE IS adcp$cruno\_$station.rose
echo "
set pagesize 500;
select cruno,station,depthbot,count(depthbot) from losierr.adcpvel where cruno='$cruno' and station=$station
group by cruno,station,depthbot order by cruno,station,depthbot;
exit;
" > adcprequest.sql
sqlplus losierr/For5tat5@ptran @adcprequest.sql
echo " Plots displays CM data and Wave data. What depthbot do you wish to plot?"
read depplot
echo "
rosefile <- paste(\"adcp\",\"$cruno\",\"_\",\"$station\",\".rose\",sep=\"\")
cm <- matrix(scan(rosefile),ncol=19,byrow=T)
cmminutes <- cm[,9][cm[,9]>=50 & cm[,3] == $depplot]
cmsegment <- cm[,2][cm[,9]>=50 & cm[,3] == $depplot]
cmdepthbot <- cm[,3][cm[,9]>=50 & cm[,3] ==  $depplot]
cmdepth <- cm[,4][cm[,9]>=50 & cm[,3] ==  $depplot]
cmve <- cm[,14][cm[,9]>=50 & cm[,3] == $depplot]
cmvn <- cm[,15][cm[,9]>=50 & cm[,3] == $depplot]
cmday <- cm[,5][cm[,9]>=50 & cm[,3] == $depplot]
cmmon <- cm[,6][cm[,9]>=50 & cm[,3] == $depplot]
cmyr <- cm[,7][cm[,9]>=50 & cm[,3] == $depplot]
cmhr <- cm[,8][cm[,9]>=50 & cm[,3] == $depplot]
cmsec <- cm[,10][cm[,9]>=50 & cm[,3] == $depplot]
cmspeed <- cm[,11][cm[,9]>=50 & cm[,3] == $depplot]
cmdir <- cm[,12][cm[,9]>=50 & cm[,3] == $depplot]
timeint <- c(rep(0.0,length(cmmon)))
timeint2 <- c(rep(0.0,length(cmmon)))
#initialize time interval period between sampling
vndel <- c(rep(0,length(cmmon)))
vedel <- c(rep(0,length(cmmon)))
dmy <- paste(cmday,cmmon,cmyr,sep=\"/\")
# concatenate my DMY together with a / separator to set into Splus format
HMS <- paste(cmhr,cmminutes,cmsec,sep=\":\")
# concatenate my HMS together with a : separator to set into Splus format
dmyHMS <- paste(dmy,HMS,sep=\" \")
mydate <- strptime(dmyHMS,\"%d/%m/%Y %H:%M:%S\")
tittlen <- paste(\"\",dmy[1],\"to\",dmy[length(dmy)])
tittlen2 <- paste($depplot,\"meters from bottom\")
# produce the vector mydate using the datetime function chron
Totaltime <- mydate[length(cmmon)] - mydate[1]
# 
ve <- sin(cmdir*2.0*pi/360.)*cmspeed
vn <- cos(cmdir*2.0*pi/360.)*cmspeed
# calculating vectors ve and vn from input matrix
timecor <- 0
timecor[1] <- 0
for(x in 2:length(cmday)){
timecor[x] <- difftime(mydate[x],mydate[x-1],units=\"mins\")}
timecor <- timecor*60
#convert to decimal seconds
firstx <- 2
lastx <- length(cmsec)
vndel[1] <- vn[1]*timecor[2]/100000
vedel[1] <- ve[1]*timecor[2]/100000
for(i in firstx:lastx){
 vndel[i] <- ((vn[i]*timecor[i]/100000)+vndel[i-1])
 vedel[i] <- ((ve[i]*timecor[i]/100000)+vedel[i-1])
 timeint[i] <- difftime(mydate[i],mydate[i-1],units=\"mins\")+timeint[i-1]
  }
timeint <- timeint/60/24
jpegfile <- paste(\"$file1\",\".jpg\",sep=\"\")
jpeg(jpegfile, height = 700, width = 700, quality = 100,pointsize=17)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE),c(4,4),c(3.5,3.5), TRUE)
par(mar=c(3,4,2,2))
  #screen(1)
  maxdis <- max(vedel,vndel)
  mindis <- min(vedel,vndel)
  absdel <- max(abs(mindis),abs(maxdis))
  #computing x and y axis limits
  plot(vedel,vndel,type=\"l\",
  xlab=\"\",ylab=\"\",
  xlim=c(-absdel,absdel),ylim=c(-absdel,absdel))
  mtext(\"Vn Kilometers\",side=2,line=2,cex=.9)
  mtext(\"Ve Kilometers\",side=1,line=2,cex=.9)
  segments(-absdel,0,absdel,0)
  segments(0,-absdel,0,absdel)

  #screen(2)

plot(cmdir,cmspeed,type=\"p\",
xlab=\" \",ylab=\"\")
mtext(\"Direction\",side=1,line=2,cex=.9)
mtext(\"Speed cm/sec\",side=2,line=2,cex=.9)
rosefile <- paste(\"adcp\",\"$cruno\",\"_\",\"$station\",\".rose\",sep=\"\")
wavefile <- paste(\"adcp\",\"$cruno\",\"_\",\"$station\",\".wave\",sep=\"\")
x <- scan(\"$wavefile\",
what=list(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),fill=TRUE,flush=TRUE)
burst <- x[[1]]
yr <- x[[2]]
mon <- x[[3]]
day <- x[[4]]
hr <- x[[5]]
minutes <- x[[6]]
hs <- x[[9]]
tp <- x[[10]]
dp <- x[[11]]
pres <- x[[12]]
hssea <- x[[13]]
tpsea <- x[[14]]
dpsea <- x[[15]]
hsswell <- x[[16]]
tpswell <- x[[17]]
dpswell <- x[[18]]
hsmax <- x[[19]]
tpmax <- x[[20]]
hs10 <- x[[21]]
tp10 <- x[[22]]
hs33 <- x[[23]]
tp33 <- x[[24]]
hsmean <- x[[25]]
tpmean <- x[[26]]
dpmean <- x[[27]]
spd1 <- x[[29]]
dir1 <- x[[30]]
spd2 <- x[[31]]
dir2 <- x[[32]]
spd3 <- x[[33]]
dir3 <- x[[34]]
spd4 <- x[[35]]
dir4 <- x[[36]]
spd5 <- x[[37]]
dir5 <- x[[38]]
spd6 <- x[[39]]
dir6 <- x[[40]]
spd7 <- x[[41]]
dir7 <- x[[42]]
spd8 <- x[[43]]
dir8 <- x[[44]]
spd9 <- x[[45]]
dir9 <- x[[46]]
spd10 <- x[[47]]
dir10 <- x[[48]]
maxy=max(hs,hssea,hsswell,hs10,hs33,hsmax)
miny=0
plot(burst,hsmean,type=\"l\",col=\"cyan\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,xlab=\"\",ylab=\"\")
par(new=T)
plot(burst[hsmax > 0],hsmax[hsmax > 0],type=\"p\",col=\"red\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.9,xlab=\"\",ylab=\"\")
mtext(\"Wave Height (m)\",side=2,line=2,cex=.9)
mtext(\"Hourly Burst\",side=1,line=2,cex=.9)
par(new=T)
plot(burst,hs,type=\"l\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,
xlab=\" \",ylab=\"\")
par(new=T)
#plot(burst[hssea > 0],hssea[hssea > 0],type=\"l\",col=\"cyan\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,xlab=\"\",ylab=\"\")
#par(new=T)
plot(burst,hsswell,type=\"l\",col=\"lightgreen\",ylim=c(miny,maxy),xlim=c(min(burst),max(burst)),cex=.2,xlab=\"\",ylab=\"\")
par(new=T)
legend(\"topleft\",legend=c(\"hs\",\"hsswell\",\"hsmax\",\"hsmean\"),col=c(\"black\",\"lightgreen\",\"red\",\"cyan\"),pt.cex=1,
cex=.8,bty='n',lty=c(1,1,0,1),pch=c(-1,-1,1,-1))
maxyy=max(tp,tpsea,tpswell,tp10,tp33)
minyy=0
maxyy=20
plot(burst,tpmean,type=\"l\",col=\"cyan\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,
xlab=\"\",ylab=\"\")
par(new=T)
plot(burst,tpswell,type=\"l\",col=\"lightgreen\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
par(new=T)
plot(burst[tpmax > 0],tpmax[tpmax > 0],type=\"p\",col=\"red\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
legend(\"topleft\",legend=c(\"tp\",\"tpswell\",\"tpmax\",\"tpmean\"),col=c(\"black\",\"lightgreen\",\"red\",\"cyan\"),pt.cex=1,
cex=.8,,bty=\"n\",lty=c(1,1,0,1),pch=c(-1,-1,1,-1))
mtext(\"Wave period (sec)\",side=2,line=2,cex=.9)
mtext(\"Hourly Burst\",side=1,line=2,cex=.9)
par(new=T)
plot(burst,tp,type=\"l\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,
xlab=\"\",ylab=\"\")
par(new=T)
#plot(burst,tpsea,type=\"l\",col=\"red\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
#par(new=T)
#plot(burst,tp10,type=\"p\",col=\"cyan\",ylim=c(minyy,maxyy),xlim=c(min(burst),max(burst)),cex=.7,xlab=\"\",ylab=\"\")
#par(new=T)

#Hs Significant Wave Height (mean of highest 1/3 of all waves)
#Tp mean of the periods of Hs
#Dp Peak Wave Direction
#H10 mean od 10% of the largest waves
#Tmean mean Period
#Dmean mean direction
title(main=tittlen,outer=T,line=-2.5,cex.main=1.3)
title(main=tittlen2,outer=T,line=-3.5,cex.main=1.3)
q()
"> waveplot2.deck
R CMD BATCH waveplot2.deck                             
echo "Your jpg file is $file1.jpg"
echo "Press Return to continue"
read return
gimp $file1.jpg
