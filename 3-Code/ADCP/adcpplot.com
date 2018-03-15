
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
sqlplus losierr/For5tat5@ptran @ppp.sql
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
set pages 100;
select distinct($botsur), count($botsur) from adcpvel
where adcpvel.cruno='$cruno' and
adcpvel.station=$station
group by $botsur order by $botsur;
exit;
" > xx3.sql
sqlplus losierr/For5tat5@ptran @xx3.sql
if [ "$botsur" = "depthbot" ]
then
echo "Enter Depth from the bottom now?"
read dbot
else
echo " From the min(botsur) value listed above enter depth selection?"
echo " example 2.5,3.5,4.5 etc "
echo " Enter min(botsur)?"
read minbot
fi
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
adcpvel.station=$station" > xxx.sql
if [ "$botsur" = "depthbot" ]
then
echo "and adcpvel.$botsur=$dbot;" >> xxx.sql
else
echo "and adcpvel.$botsur >= $minbot - .5 and adcpvel.$botsur <= $minbot + .5;" >> xxx.sql
fi
echo "
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
SET LINESIZE 80;
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
column btverr   FORMAT 999.9;
COLUMN mag FORMAT 999.9;
column area format a20;
column data_mode format a2;
column ddd format a20;
column depth format 9999.9;
column intensity format 9999.9;
column flow format a1;
SPOOL adcpplot.lis;
SELECT deploy,I.segment,depthsur,depthbot,
to_char(sdate,'dd mm yyyy hh24 mi ss') ddd ,
speed,direction-area_magvar cordir
FROM adcpinf I,adcpplot V " >> xxx.sql
if [ "$botsur" = "depthbot" ]
then
echo "where $botsur=$dbot and" >> xxx.sql
else
echo "where $botsur >= $minbot - .5 and $botsur <= $minbot + .5 and" >> xxx.sql
fi
echo "I.cruno = V.cruno and
I.station = V.station and
I.segment = V.segment " >> xxx.sql
if [ "$ensyn" = "Y" -o "$ensyn" = "y" ]
then
echo " and I.segment >= $minensemble and I.segment <= $maxensemble" >> xxx.sql
fi
echo "order by I.segment;
EXIT;
" >> xxx.sql
sqlplus losierr/For5tat5@ptran @xxx.sql
echo " Enter title for your plot"
read title
echo "
x <- matrix(scan(\"adcpplot.lis\"),ncol=12,byrow=T)
day <- x[,5]
mon <- x[,6]
yr <- x[,7]
hr <- x[,8]
minutes <- x[,9]
sec <- x[,10]
speed <- x[,11]
dir <- x[,12]
timeint <- c(rep(0.0,length(mon)))
timeint2 <- c(rep(0.0,length(mon)))
#initialize time interval period between sampling
vndel <- c(rep(0,length(mon)))
vedel <- c(rep(0,length(mon)))
dmy <- paste(day,mon,yr,sep=\"/\")
# concatenate my DMY together with a / separator to set into Splus format
HMS <- paste(hr,minutes,sec,sep=\":\")
# concatenate my HMS together with a : separator to set into Splus format
dmyHMS <- paste(dmy,HMS,sep=\" \")
mydate <- strptime(dmyHMS,\"%d/%m/%Y %H:%M:%S\")
# produce the vector mydate using the datetime function chron
Totaltime <- mydate[length(mon)] - mydate[1]
# Total amount of time for deployment in decimal days
ve <- sin(dir*2.0*pi/360.)*speed
vn <- cos(dir*2.0*pi/360.)*speed
# calculating vectors ve and vn from input matrix
timecor <- 0
#initialize variable timecor
if(minutes[2] > minutes[1]) timecor <- (minutes[2] - minutes[1])
if(minutes[1] > minutes[2]) timecor <- ((minutes[2]+60) - minutes[1])
#determine time interval of data sampling
timecor <- timecor*60
#convert to decimal seconds
firstx <- 2
lastx <- length(sec)
vndel[1] <- vn[1]*timecor/100000
vedel[1] <- ve[1]*timecor/100000
for(i in firstx:lastx){
 vndel[i] <- ((vn[i]*timecor/100000)+vndel[i-1])
 vedel[i] <- ((ve[i]*timecor/100000)+vedel[i-1])
  timeint[i] <- (mydate[i] - mydate[i-1])+timeint[i-1]
  }
timeint <- timeint/60/24
# loop calculation to compute Progressive vector plot.
X11(display = \"\", width = 7, height = 9.5)
#postscript(file=\"adcpplot.ps\",horizontal=F,height = 11, width = 8.5)
#bitmap(\"adcpplot.jpeg\", type = \"jpeg\", height = 11.0, width = 8.5, res = 144,pointsize=10)
pdf(\"adcpplot.pdf\", height = 11.0, width = 8.5,pointsize=10)
a<-layout(matrix(c(1,2,7,7,6,6,5,5,4,4,3,3),6,2,byrow=T),
widths=lcm(c(8,8)), heights=lcm(c(8,2.8,2.8,2.8,2.8,2.8)))
#layout.show(a)
par(mar=c(5,4,0,2))
  #screen(1)
  maxdis <- max(vedel,vndel)
  mindis <- min(vedel,vndel)
  absdel <- max(abs(mindis),abs(maxdis))
  #computing x and y axis limits
  plot(vedel,vndel,type=\"l\",mgp=c(1.4,.25,0),
  xlab=\"Ve Kilometers\",ylab=\"Vn Kilometers\",
  xlim=c(-absdel,absdel),ylim=c(-absdel,absdel))
  segments(-absdel,0,absdel,0)
  segments(0,-absdel,0,absdel)

  #screen(2)
  maxven <- max(ve,vn)
  minven <- min(ve,vn)
  absven <- max(abs(minven),abs(maxven))
  #computing x and y axis limits
  vve2 <- 0
  vvn2 <- 0
  plot(vve2,vvn2,type='l',mgp=c(1.4,.25,0),lwd=.2,
  xlab=\"Ve cm/sec\",ylab=\"Vn cm/sec\",
  xlim=c(-absven,absven),ylim=c(-absven,absven))
  #initializes the plot area
  par(lwd=.2)
  segments(0,0,ve,vn)
  #segments allows line segments to a plot using polar coordinates
  #this plots the lines instantly instead of doing a line by line plot

  #screen(3)
  par(mar=c(2,2.2,0,.8))
  plot(timeint,speed,type=\"l\",xlab=\"Days\",ylab=\"Speed(cm/sec)\",mgp=c(1.1,.25,0),tcl=-.3)

  #screen(4)
  par(mar=c(1.5,2.2,0,.8))
  plot(mydate,vn,type=\"l\",mgp=c(1.4,.25,0),tcl=-.3,
  xaxt=\"n\",xlab=\" \",ylab=\"Vn\",ylim=c(-absven,absven))
  zero <- rep(0,length(ve))
  lines(mydate,zero)

  #screen(5)
  plot(mydate,ve,type=\"l\",mgp=c(1.4,.25,0),tcl=-.3,
  xaxt=\"n\",xlab=\" \",ylab=\"Ve\",ylim=c(-absven,absven))
  lines(mydate,zero)

  #screen(6)
  plot(mydate,dir,type=\"l\",mgp=c(1.4,.25,0),tcl=-.3,
  xaxt=\"n\",yaxt=\"n\",xlab=\" \",ylab=\"Direction\",ylim=c(0,360))
  axis(2,at=c(0,90,180,270,360),labels=c(\"0\",\"90\",\"180\",\"270\",\"360\"),las=2,tcl=.3,
  mgp=c(1.4,.25,0))

  #screen(7)
  duration <- max(timeint)
  ve2 <- c(rep(0,length(mon)))
  for(i in firstx:lastx){
   ve2[i] <- (((ve[i]*0.6/5.7)*(duration/maxven)) + timeint[i])}
  #ve is scaled by the axis lengths for proper vector lengths
  xlimx <- timeint[2]
  ylimx <- (timeint[2])+Totaltime
  plot(0,0,type='l',mgp=c(1.4,.25,0),tcl=-.3,
  ylim=c(-absven,absven),xlim=c(xlimx,ylimx),xaxt=\"n\",
  xlab=\" \",ylab=\"Velocity cm/sec\")
  par(lwd=.2)
  segments(timeint,0,ve2,vn)
  par(cex=1.5)
  title(\"$title\",outer=T,line=-3)

  #mai= margin sizes specified in inches(bot,left,top,right)
  #mgp= margin size in units of mex (axistitle,axislabels,axisline)
  #mex= coordinates for addressing locations
  #omi= size of outer margins in inches(bot,left,top,right)
  #cex= character scaling
  #source(\"adcpplot.splus\") allows reading of script files.
" > adcpplot.deck
R CMD BATCH adcpplot.deck
echo ""
echo " This is it !! "
gimp adcpplot.pdf
