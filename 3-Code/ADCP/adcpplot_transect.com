echo ""
echo ""
echo "  This script will produce a plot of ADCP current data.  "
echo ""
echo "  Enter cruise and station for processing.  "
echo ""
echo "  Enter cruise number      "
echo ""
read cruno
echo ""
echo "  Enter station number    "
echo ""
read station
echo ""
echo "  Do you want depth selection relative to the bottom or surface b/s? "
read botsur
case $botsur in
 [bB]) botsur=depthbot;;
 [sS]) botsur=depthsur;;
esac
echo "  Do you want to select a range of segments y/n? "                   
read ensyn 
if [ "$ensyn" = "Y" -o "$ensyn" = "y" ]
then
echo "
select min(segment),max(segment)
from adcpinf
where cruno='$cruno' and station=$station;
exit;
" > ppp.sql
sqlplus losierr/rmtj01 @ ppp.sql
echo ""
echo ""
echo "Enter minimum segment number now "
read minensemble
echo ""
echo ""
echo "Enter maximum segment number now "
read maxensemble
fi
echo ""
echo "
select distinct($botsur), count($botsur) from adcpvel
where adcpvel.cruno='$cruno' and
adcpvel.station=$station
group by $botsur  order by $botsur ;
EXIT;
" > xx.sql
sqlplus losierr/rmtj01 @xx.sql
echo "Enter Depth for processing now from above list? "
read depth       
title=$cruno\_$station
echo "
drop table adcpplot;
create table adcpplot as select
adcpplan.cruno,adcpplan.station,deploy,segment,depthsur,depthbot,
speed,direction,
veleast,velnorth,
unit_magvar,area_magvar
from adcpvel,adcpplan where
adcpvel.cruno='$cruno' and
adcpplan.cruno='$cruno' and
adcpplan.station=$station and
adcpvel.station=$station
and adcpvel.$botsur=$depth;
update adcpplot
set area_magvar=(area_magvar*-1.0);
update adcpplot
set direction=direction+360 where direction<area_magvar;
SET VERIFY OFF;
SET ECHO OFF;
SET FEEDBACK OFF;
SET HEADING OFF;
SET WRAP ON;
set trim on;
SET SHOW OFF;
SET VERIFY OFF;
SET PAGESIZE 0;
SET LINESIZE 100;
set numwidth 6;
set null NA;
SET TERMOUT OFF;
column depthbot format 999.99;
column depthsur format 999.99;
COLUMN SPEED  FORMAT 999.9;
COLUMN direction FORMAT 999;
column cordir format 999.9;
COLUMN heading FORMAT 999;
COLUMN temp FORMAT 999.9;
COLUMN longitude   FORMAT 999.99999;
COLUMN latitude    FORMAT 999.99999;
COLUMN tidehgt FORMAT 99.9;
column depthsur 999.99;
column veleast FORMAT 999.9;
column velnorth FORMAT 999.9;
column velvert FORMAT 999.9;
column btveast  FORMAT 999.9;
column btvnorth FORMAT 999.9;
column btvert   FORMAT 999.9;
column longitude FORMAT 999.99999;
column latitude FORMAT 999.99999;
COLUMN mag FORMAT 999.9;
column area format a20;
column data_mode format a2;
column ddd format a20;
column depth format 9999.9;
column tidehgt format 99.9;
column flow format a1;
column intensity format 9999.9;
column flow format a1;
SPOOL adcpplot_transect.lis;
SELECT deploy,I.segment,depthsur,depthbot,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddd ,
speed,direction-area_magvar cordir,latitude,longitude,tidehgt,flow
FROM adcpinf I,adcpplot V
where $botsur=$depth and
I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment " > xx.sql
if [ "$ensyn" = "Y" -o "$ensyn" = "y" ]
then
echo " and I.segment >= $minensemble and I.segment <= $maxensemble" >> xx.sql
fi
echo " order by I.segment;
EXIT;
" >> xx.sql
sqlplus losierr/rmtj01 @xx.sql
echo " Enter vector scaling factor now (use .1 as default) now?"
read scale 
echo " "
echo "WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING"
echo " "
echo " If plot generates Line segments out of bound errors repeat run with"
echo " a smaller scaling value"
echo " "
echo "WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING"
echo " "
echo "
map <- matrix(scan(\"/disc2/users/losierr/drifter/passgm.ref\"),ncol=3,byrow=T)
#This scan allows me to enter character data as defined by the quotes
x <- scan(\"adcpplot_transect.lis\",what=list(0,0,0,0,0,0,0,0,\"\",0,0,0,0,0,0,\"\"))
#This assigns names to my matrix data
long <- map[,1]
lat <- map[,2]
pen <- map[,3]
#This assigns names to my vector data
day <- x[[5]]
mon <- x[[6]]
yr <- x[[7]]
hr <- x[[8]]
minutes <- x[[9]]
sec <- x[[10]]
speed <- x[[11]]
dir <- x[[12]]
tracklat <- x[[13]]
tracklong <- x[[14]]
tidehgt <- x[[15]]
flow <- x[[16]]
#defines all aspects of the chron library which allows print to
#display all the date and time fields
library(chron)
#assigning mapping coordinate values for the axis.
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
#assigning a small buffer to the axis
latmin <- latmin -0.0050
latmax <- latmax +0.0050
longmin <- longmin -0.0050
longmax <- longmax +0.0050
#getting the midpoint for the data along the longitudinal axis.
latdeg=latmax-latmin
longdeg=(longmin-longmax)/2
midlong=longmin-longdeg
#scaling the x axis to the page size and the aspect ratio for lats and longs
longunits=(7.256*latdeg)/(0.71*5.216)
#reset the longitude scaling for the axis and centering the data in the plot.
longmin=midlong-(longunits/2)
longmax=midlong+(longunits/2)
ps.options(max.vertices=60000)
#initialize time interval period between sampling
vndel <- c(rep(0,length(mon)))
vedel <- c(rep(0,length(mon)))
dmy <- paste(day,mon,yr,sep=\"/\")
# concatenate my DMY together with a / separator to set into Splus format
HMS <- paste(hr,minutes,sec,sep=\":\")
HM <- paste(hr,minutes,sep=\":\")
mydate <- chron(dmy[1],HMS[1],format=c(\"d/mon/yyyy\",\"h:m:s\"))
# concatenate my HMS together with a : separator to set into Splus format
ve <- sin(dir*2.0*pi/360.)*speed
vn <- cos(dir*2.0*pi/360.)*speed
vebar <- sin(270*2.0*pi/360)*50
vnbar <- cos(270*2.0*pi/360)*50
# calculating vectors ve and vn from input matrix
#initialize variable timecor
maxvevn <- max(abs(ve),abs(vn))
firstx <- 1
lastx <- length(sec)
ve2 <- c(rep(0,length(sec)))
vn2 <- c(rep(0,length(sec)))
vn <- vn*1.41
for(i in firstx:lastx){
ve2[i] <- (ve[i]/(maxvevn/(longmax-longmin))*$scale+tracklong[i])
vn2[i] <- (vn[i]/(maxvevn/(latmax-latmin))*$scale+tracklat[i])}
ve2bar <- (vebar/(maxvevn/(longmax-longmin))*$scale+longmax)
vn2bar <- (vnbar/(maxvevn/(latmax-latmin))*$scale+latmin)
  postscript()
  #screen(1)
firstlat <- tracklat[1]
firstlong <- tracklong[1]
lastlat <- tracklat[length(sec)]
lastlong <- tracklong[length(sec)]
hourtime <- c(rep(0,length(24)))
hourlat <- c(rep(0,length(24)))
hourlong <- c(rep(0,length(24)))
hourtime[1] <- HM[1]
hourlat[1] <- tracklat[1]
hourlong[1] <- tracklong[1]
j <- 2
for(i in 2:lastx){
if(hr[i] > hr[i-1])hourtime[j]<- HM[i]
if(hr[i] > hr[i-1])hourlat[j]<-tracklat[i]
if(hr[i] > hr[i-1])hourlong[j]<-tracklong[i]
if(hr[i] > hr[i-1])j<- j+1}
for(i in 1:lastx){
if(flow[i] ==\"S\")sllat<-tracklat[i]
if(flow[i] ==\"S\")sllong<-tracklong[i]
if(flow[i] ==\"S\")flowage2<-\"Slack\"}
if(flow[10] ==\"E\")flowage<-\"Ebb\"
if(flow[10] ==\"F\")flowage<-\"Flood\"
plot(tracklong,tracklat,type='l',lwd=3,xlim=c(longmin,longmax),err=1,
ylim=c(latmin,latmax),xlab=\"Longitude\",ylab=\"Latitude\",pin=par(pin=c(7.256,5.216)))
title2 <- paste(as.character(mydate),\"$depth m\")
title(main=title2)
par(new=T,xaxs=\"d\",yaxs=\"d\")
polygon(long,lat,density=-1,angle=45,border=T,col=6,err=-1)
par(new=T,xaxs=\"d\",yaxs=\"d\")
axiscoor <- par(\"usr\")
segments(tracklong,tracklat,ve2,vn2,lwd=.8)
segments(longmax,latmin,ve2bar,vn2bar,lwd=.8)
text((longmax+ve2bar)/2,(axiscoor[3]+latmin)/2,\"50cm/sec\")
text(sllong,sllat,flowage2,cex=.9)
text(tracklong[10],tracklat[10],flowage,cex=.9)
text(hourlong,hourlat,hourtime,cex=.9)
stamp(string=\"$title\", print=T, plot=T)
box()
q()
" > adcpplot_transect.splus
rsh globec "(DISPLAY=142.2.15.150:0.0;export display;cd quoddy2mount/adcp/;/usr/local/bin/Splus5 < adcpplot_transect.splus)"
echo ""
echo " This is it !! "
