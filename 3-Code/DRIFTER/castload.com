echo 'Do you want to edit,load or verify drifter data e/l/p? \c'    
read reply2
if [ $reply2 = e -o $reply2 = E -o $reply2 = p -o $reply2 = P ]
  then
  conseq=9999
 else
  echo "
  select max(deploy),max(sdate) from castdrft group by deploy order by deploy;
  exit;
  ">xxx.sql
  sqlplus losierr/rmtj01 @xxx.sql
  echo "From the Oracle Run above, what is your next deployment number?"
  echo 'Enter deployment number now ? \c'
  read conseq
fi
echo " "
echo  " ******************************* "
echo  " ***  1 = PASSAMAQUODDY     **** "
echo  " ***  2 = PEI               **** "
echo  " ***  3 = MUSQUASH          **** "
echo  " ******************************* "
echo 'What area were the drifters released 1,2,3? \c'
read area
case $area in
 1) map=passgm.ref;;
 2) map=tracadie_map.dat;;
 3) map=passgm_reduce.dat;;   
esac
if [ $reply2 = e -o $reply2 = l ]
  then
  ls [0-9]*.txt
  ls [0-9]*.TXT
  echo " "
  echo 'Enter day,month,year of release ex. 22,07,99 or 31,01,00 ? \c '
  read depdate
  echo $depdate | cut -c1-2,4-5,7-8 > depdate2 
  depdate3=`cat depdate2`
fi
if [ $reply2 = e ]
  then
  echo 'Do you want to edit these Drifter files now y/n? \c'
  read reply
  if [ $reply = y ]
    then
    echo " "
    echo "Edits require that all character lines are removed."
    echo "Edits require that data is space delimited (no commas or colons)."
    echo " "
    echo " in vi do :1,\$s/,/ /g"
    echo " in vi do :1,\$s/:/ /g"
    echo " "
    echo "Press enter to continue."
    read enter
    echo " "
    for file in $depdate3*
    do
    echo "
    track <- matrix(scan(\"$file\"),ncol=8,byrow=T)
    gooddata <- track[,7]
    tracklat <- ((track[,5]/200000)-90)[gooddata == 1]
    tracklong <- ((track[,6]/200000)-180)[gooddata == 1] " > drifter.deck
if [ $area = 3 ]
  then
    echo "
    map <- matrix(scan(\"$map\"),ncol=2,byrow=T) " >> drifter.deck
    else
    echo "
    map <- matrix(scan(\"$map\"),ncol=3,byrow=T) " >> drifter.deck
fi
    echo "
    long <- map[,1]
    lat <- map[,2]
    latmin <- min(na.rm = T,tracklat)
    latmax <- max(na.rm = T,tracklat)
    longmin <- min(na.rm = T,tracklong)
    longmax <- max(na.rm = T,tracklong)
    latmin <- latmin - 0.005
    latmax <- latmax + 0.005
    longmin <- longmin - 0.005
    longmax <- longmax + 0.005
    latdeg=latmax-latmin
    latunits=(7.256*latdeg)/(0.71*5.216)
    units=longmax-latunits
    if(units > longmin)longdeg=longmax-longmin
    if(units > longmin)longunits=(5.216*longdeg)/(1.41*7.256)
    if(units > longmin)units=latmin+longunits
    if(units > longmin)latmax=units
    if(units < longmin)longmin=units
    postscript(horizontal=T)
    plot(tracklong,tracklat,type='l',xlim=c(longmin,longmax),
    ylim=c(latmin,latmax),xlab=\"Longitude\",ylab=\"Latitude\",pin=par(pin=c(7.256,5.216)))
    par(new=T)
    title(main=\"$file\")
    polygon(long,lat,density=0,border=T,col=6)
    par(new=T)
    plot(tracklong,tracklat,type='l',xlim=c(longmin,longmax),
    ylim=c(latmin,latmax),xlab=\"\",ylab=\"\",pin=par(pin=c(7.256,5.216)))
    for (i in seq(along=tracklong))
    arrows(tracklong[i],tracklat[i],tracklong[i+1],tracklat[i+1],open=T,
    rel=F,size=.1)
    q()
    " >> drifter.deck
    ans=y
    while [ "$ans" = y ]
    do
    dos2unix $file
    vi $file
    R CMD BATCH drifter.deck
    echo " "
    echo 'You have a plot printout on the lexmark3 printer'
    echo 'Do you want to repeat this edit y/n? \c'
    read ans  
    done
    done
  fi
  echo "You may now rerun the script in load mode"
  exit 1
fi
if [ $reply2 = l ]
  then
  echo  " ******************************* "
  echo  " ***  1 = Hardwood Island   **** "
  echo  " ***  2 = Tongue Shoal      **** "
  echo  " ***  3 = Big Letite        **** "
  echo  " ***  4 = Little Letite     **** "
  echo  " ***  5 = Eastport          **** "
  echo  " ***  6 = Deadmans Harbour  **** "
  echo  " ***  7 = Hills Island      **** "
  echo  " ***  8 = Lime Kiln Bay     **** "
  echo  " ***  9 = Letang Harbour    **** " 
  echo  " *** 10 = Back Bay          **** "
  echo  " *** 11 = SDDF              **** " 
  echo  " *** 12 = AQUA FISH FARMS   **** "
  echo  " *** 13 = Ronnie Hawkins    **** "
  echo  " *** 14 = Fisherman Cove    **** "
  echo  " *** 15 = Stolts Sea Farms  **** "
  echo  " *** 16 = Cunningham Beach  **** "
  echo  " *** 17 = Ministers Island  **** "
  echo  " *** 18 = Bocabec Bay       **** "
  echo  " *** 19 = Oven Head         **** "
  echo  " *** 20 = Mascarene Shore   **** "
  echo  " *** 21 = Pendleton Pass    **** "
  echo  " *** 22 = Tracadie Bay PEI  **** "
  echo  " *** 23 = Seal Cove GM      **** "
  echo  " *** 24 = Outer Wood Island **** "
  echo  " ******************************* "
  echo  " ******************************* "
  echo 'Enter number for area of deployment from above list? \c'
  read area2
  case $area2 in
   1) foreman=41;;
   2) foreman=40;;
   3) foreman=29;;
   4) foreman=29;;
   5) foreman=24;;
   6) foreman=44;;
   7) foreman=44;;
   8) foreman=33;;
   9) foreman=33;;
   10) foreman=30;;
   11) foreman=33;;
   12) foreman=33;;
   13) foreman=33;;
   14) foreman=33;;
   15) foreman=33;;
   16) foreman=41;;
   17) foreman=40;;
   18) foreman=41;;
   19) foreman=41;;
   20) foreman=40;;
   21) foreman=29;;
   22) foreman=1915;;
   23) foreman=5;;
   24) foreman=1;;
  esac
  echo  " *** 1 = cluster release    **** "
  echo  " *** 2 = transect release   **** "
  echo  " *** 3 = individual release **** "
  echo 'Enter number for deployment type from above list? \c'
  read deptype
  case $deptype in
   1) deployt=CL;;
   2) deployt=TR;;
   3) deployt=IN;;
  esac
  rm castall.dat
  rm xxxcast*
  echo "
  select max(deploy),max(sdate) from castdrft;
  exit;
  ">xxx.sql
  sqlplus losierr/rmtj01 @xxx.sql
  echo " "
  echo "****************************************************************"
  echo "NOTE... Your last deployment number in the above ORACLE selection"
  echo " If this value = $conseq then this script should be aborted "
  echo " unless you are appending drogue data to deployment "
  echo "****************************************************************"
  echo " "
  echo " Abort Script y/n ? \c"
  read reply
  if [ "$reply" = "y" ]
    then
    echo "        This job has been aborted because you are attempting to load"
    echo "        a previously used deployment . See your DBASE administrator."
    exit 1
  fi
fi
if [ $reply2 = e -o $reply2 = l ]
  then
  echo " "
  ls $depdate3*
  echo " "
  echo 'Note above listed files and enter return to continue'
  read return
  echo 'Enter the barrel ID number to process ex. 01 04 06 07 ? '
  echo 'These are the last 2 numereic values in the filenames. \c ' 
  read barrel
# for cast in  03 05 08 09 10  holey socks ; drogue depth 6
# for cast in  01 02 04 06 07  barrels ; depth 0
  for cast in $barrel
  do 
  mv $depdate3$cast'.TXT' $depdate3$cast'.txt'
  echo $depdate3$cast'.txt' > xxx.scr
  echo xxxcast$cast >> xxx.scr
# value for Drogue number
  echo $cast >> xxx.scr
# value for Deployment number
  echo $conseq >> xxx.scr
# value for Drogue area release
  echo $area2 >> xxx.scr
# value for Drogue depth     
  echo 0 >> xxx.scr
# value for GMT adjustment *No longer required (12 Mar 98)
# echo 3 >> xxx.scr
# value for Drogue date           
  echo $depdate >> xxx.scr
# value to stop listing tidal stations
  echo '9' >> xxx.scr
# value for Freeman tidal costituents
  echo $foreman >> xxx.scr
  f95 -f77 -o casttide casttide.f
  casttide < xxx.scr
  done
  echo " "
  echo "Check value for Drogue depth - surface value is 0"
  echo "GMT adjustment is not necessary "
  echo 'Enter return to continue'
  read return
  cat xxxcast* > castall.dat
  more castall.dat
  echo "......................................................."
  echo "     This Proceedure file allows you to load"
  echo "     edited drifter data to castdrft oracle"
  echo "     database. "
  echo "......................................................."
  echo " "
  echo " "
  echo "Loading file castall.dat  with deployment number $conseq \c"; date
  echo " "
  echo " This will delete all CAST deployments $conseq from the database "
  echo " Are you sure you want to proceed with the load y/n ? /c"
  echo " Answer no if loading drogue data into existing deployment /c"
  echo " This will append data to the deployment in database /c"
  read answer
  if [ $answer = "y" ]
    then
    echo "
     -- cleaning out Deployment $conseq from ORACLE DATABASE
     delete from castdrft where deploy = $conseq;
     exit;
     ">clean.sql
    sqlplus losierr/rmtj01 @clean.sql
    rm clean.sql
  fi
  echo "
  -- Loading cast data info for $conseq into ORACLE
  -- THIS IS AN EXAMPLE OF A COMMENT LINE IN SQL*LOADER
  -- THIS PROGRAM ALLOWS YOU TO MOVE EXTERNAL FILES INTO
  -- TABLES IN AN ORACLE DATABASE
  LOAD DATA
  -- REQUIRED AT THE BEGINNING OF THE CONTROL FILE
  INFILE castall.dat
  -- INFILE SPECIFIES THE DATA TO BE LOADED
  -- INFILE *    -THE DATA IS FOUND WITHIN THIS COMMAND FILE AFTER
  -- COMMAND WORD BEGINDATA.
  APPEND INTO TABLE castdrft         
  -- INTO TABLE CASTTIDE REPLACE ...IF YOU WANT TO REWRITE THE FILE.
  -- EXTERNAL IS DECLARED IN ORDER TO PROPERLY LOAD DATA INTO THE
  -- TABLES. THIS CONVERTS YOUR DATA INTO CHAR FORMAT AND RECODES
  -- THE DATA INTO NUMBER FORMAT WHEN WRITTEN. OTHERWISE THE DATA
  -- IS NOT WRITTEN.
  -- APPEND INTO TABLE CASTTIDE   -WILL LOAD DATA INTO EXISTING TABLES
  (deploy        POSITION(01:05) INTEGER EXTERNAL,
   drgid         POSITION(06:10) INTEGER EXTERNAL,             
   depth         POSITION(11:14) DECIMAL EXTERNAL,                 
   tidestn       POSITION(15:19) INTEGER EXTERNAL,                     
   sdate         POSITION(20:37) date 'ddmmyyhh24miss',                 
   hours         POSITION(29:31) INTEGER EXTERNAL,
   minutes       POSITION(32:34) INTEGER EXTERNAL,
   seconds       POSITION(35:37) INTEGER EXTERNAL,          
   gpslat        POSITION(38:46) INTEGER EXTERNAL,
   gpslong       POSITION(47:55) INTEGER EXTERNAL,                 
   latitude      POSITION(56:64) DECIMAL EXTERNAL,                 
   longitude     POSITION(65:73) DECIMAL EXTERNAL,                 
   gfix          POSITION(74:75) INTEGER EXTERNAL,                 
   satconf       POSITION(76:83) INTEGER EXTERNAL,                 
   tide          POSITION(84:89) DECIMAL EXTERNAL,                 
   area          POSITION(91:107) char,                         
   distance      POSITION(108:114) DECIMAL EXTERNAL,                 
   velocity      POSITION(115:121) DECIMAL EXTERNAL,                 
   flow          POSITION(123:123) CHAR)
   ">castall.ctl
   sqlldr losierr/rmtj01 castall.ctl,castall.log,castall.bad
   more castall.log
   echo "
   -- inserting deployment type into database.       
   Update castdrft set deptype = '$deployt' where deploy = $conseq;
   exit;
   ">insert.sql
   sqlplus losierr/rmtj01 @insert.sql $conseq
   rm insert.sql
fi
if [ $reply2 = e -o $reply2 = l ]
then
echo "
set verify off;
set echo off;
set pagesize 0;
set termout off;
set feedback off;
set heading off;
set wrap on;
set show off;
SET NUMWIDTH 7;
COLUMN gpslat format 99999999;
COLUMN gpslong format 99999999;
COLUMN tide FORMAT 99.99;
COLUMN velocity FORMAT 99.999;
COLUMN distance FORMAT 9999.9;
COLUMN deploy FORMAT 999;
column flow format a2;
SPOOL oradrft.dat;
SELECT deploy,drgid,gpslat,gpslong,tide,distance,velocity,flow
FROM castdrft where deploy=$conseq
order by drgid,sdate,hours,minutes;    
EXIT;
">deploy_plot.sql
sqlplus losierr/rmtj01 @deploy_plot.sql
echo ' '
echo '                       YOUR DATAFILE IS CALLED oradrft.dat'
echo '                       USE PROGRAM drifter_oracle for plotting.'
echo ' '
echo "
track <- scan(\"oradrft.dat\",what=list(0,0,0,0,0,0,0,\"\"))
drgid <- track[[2]]
tracklat <- ((track[[3]]/200000)-90)
tracklong <- ((track[[4]]/200000)-180)
map <- matrix(scan(\"$map\"),ncol=3,byrow=T)
long <- map[,1]
lat <- map[,2]
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
latmin <- latmin - 0.005
latmax <- latmax + 0.005
longmin <- longmin - 0.005
longmax <- longmax + 0.005
latdeg=latmax-latmin
latunits=(7.256*latdeg)/(0.71*5.216)
units=longmax-latunits
if(units > longmin)longdeg=longmax-longmin
if(units > longmin)longunits=(5.216*longdeg)/(1.41*7.256)
if(units > longmin)units=latmin+longunits
if(units > longmin)latmax=units
if(units < longmin)longmin=units
postscript(,horizontal=F,width=7.5,height=10)
plot(tracklong,tracklat,type='l',xlim=c(longmin,longmax),err=1,
ylim=c(latmin,latmax),pin=par(pin=c(7.256,5.216)))
par(new=T)
title(main=\"$file\")
polygon(long,lat,density=0,border=T,col=6,err=-1)
par(new=T)
xyplot(tracklat~tracklong | drgid,type=\"l\",xlab=\"Longitude\",ylab=\"Latitude\",
xlim=c(longmin,longmax),err=1,ylim=c(latmin,latmax))
polygon(long,lat,density=0,border=T,col=6,err=-1)
text(tracklong[1],tracklat[1],flow[1],cex=.8)
par(new=T)
" > drifter2.deck
ans=y
fi
if [ $reply2 = p -o $reply2 = P ]
  then
  echo 'Enter deployment number you wish to plot now ? \c'
  read conseq
echo "
select area, deploy, drgid from castdrft where deploy=$conseq 
group by area,deploy,drgid;
exit;
" > xxx.sql
sqlplus losierr/rmtj01 @xxx.sql
echo " "
echo " "
echo "                From the ORACLE selection above, note the drgid"
echo " "
echo "             If you want all of them enter 9999 at the next prompt"
echo " "
echo "What drifter do you want to plot? \c"
read drftype
echo "Do you want a postscript plot (p) or a Word metafile plot (w) ? \c"
read plttype
echo "
set verify off;
set echo off;
set pagesize 0;
set termout off;
set feedback off;
set heading off;
set wrap on;
set show off;
SET NUMWIDTH 7;
COLUMN gpslat format 99999999;
COLUMN gpslong format 99999999;
COLUMN tide FORMAT 99.99;
COLUMN velocity FORMAT 99.999;
COLUMN distance FORMAT 9999.9;
COLUMN deploy FORMAT 999;
column  hratlib format 999.99;
column flow format a2;
drop view timelag;
create view timelag as select
deploy,drgid,min(sdate) minsdate from castdrft where deploy=$conseq group by deploy,drgid;
drop table oradrft;
create table oradrft as select deploy,drgid,gpslat,gpslong,tide,distance,velocity,flow,sdate
FROM castdrft where deploy=$conseq;
SPOOL oradrft.dat;
SELECT timelag.deploy,timelag.drgid,gpslat,gpslong,tide,distance,velocity,flow,(sdate-minsdate)*24 hratlib">deploy_plot.sql 
if [ $drftype = 9999 ]
then
echo "FROM oradrft,timelag where" >>deploy_plot.sql
else
echo "FROM oradrft,timelag where drgid=$drftype">>deploy_plot.sql
fi
echo "oradrft.drgid=timelag.drgid(+)
order by timelag.drgid,(sdate-minsdate)*24;
EXIT;">>deploy_plot.sql

sqlplus losierr/rmtj01 @deploy_plot.sql
echo " "
echo "                       YOUR DATAFILE IS CALLED oradrft.dat"
echo " "
if [ $drftype = 9999 ]
then
echo "
track <- scan(\"oradrft.dat\",what=list(0,0,0,0,0,0,0,\"\",0))
drgid <- track[[2]]
tracklat <- ((track[[3]]/200000)-90)
tracklong <- ((track[[4]]/200000)-180)
timelag <- track[[9]]
tide <-  track[[5]]
map <- matrix(scan(\"/passgm.ref\"),ncol=3,byrow=T)
long <- map[,1]
lat <- map[,2]
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
latmin <- latmin - 0.005
latmax <- latmax + 0.005
longmin <- longmin - 0.005
longmax <- longmax + 0.005
latdeg=latmax-latmin
latunits=(7.256*latdeg)/(0.71*5.216)
units=longmax-latunits
if(units > longmin)longdeg=longmax-longmin
if(units > longmin)longunits=(5.216*longdeg)/(1.41*7.256)
if(units > longmin)units=latmin+longunits
if(units > longmin)latmax=units
if(units < longmin)longmin=units
postscript(horizontal=T,width=9.7,height=7.1)
lastx <- length(tracklat)
hourtime <- c(rep(0,length(tracklat)))
hourtime[1] <- 0
j <- 1
for(i in 2:lastx){
if(trunc(timelag[i]) > trunc(timelag[i-1]))hourtime[i] <- timelag[i]
if(trunc(timelag[i]) <= trunc(timelag[i-1]))hourtime[i] <- 0}
#Produce a data Object
newdrgid <- as.factor(drgid)
hourtime <- trunc(hourtime)
randy.df <- data.frame(tracklat,tracklong,drgid,newdrgid,hourtime)

xyplot(tracklat ~ tracklong | newdrgid, data=randy.df,pch=\" \",xlab=\"Longitude\",ylab=\"Latitude\",
xlim=c(longmin,longmax),ylim=c(latmin,latmax),
as.table=T,
main=\"Deployment $conseq\",
strip = function(...) strip.default(..., strip.names=F, style=1),
par.strip.text=list(cex=1.1),
panel=function(x,y, subscripts, ...){
lines(x,y)
text.default(x,y+0.0005,labels=ifelse(randy.df\$hourtime[subscripts]!=0,randy.df\$hourtime[subscripts],
\" \"),cex=.7)
#points(x[randy.df\$hourtime[subscripts]!=0],y[randy.df\$hourtime[subscripts]!=0],pch=3)
points(x[1],y[1],pch=16,csi=.14)
polygon(long,lat,density=0,border=T,col=6,)
})
par(fig=c(0,1,0,1))
pltdrgid <- drgid[timelag==max(timelag)]
plot(timelag[drgid==pltdrgid], tide[drgid==pltdrgid],
csi=.15,mex=par(mex=.30),mgp=c(1.5,.1,0),cex=.4,
type=\"l\",ylab=\"Tide\",xlab=\"Hours\",fig=par(fig=c(.72,.90,-.02,.13)))   
q() 
" > drifter2.deck
echo "
track <- scan(\"oradrft.dat\",what=list(0,0,0,0,0,0,0,\"\",0))
drgid <- track[[2]]
drgcnt <- length(unique(drgid))
tracklat <- ((track[[3]]/200000)-90)
tracklong <- ((track[[4]]/200000)-180)
timelag <- track[[9]]
tide <-  track[[5]]
map <- matrix(scan(\"/passgm.ref\"),ncol=3,byrow=T)
long <- map[,1]
lat <- map[,2]
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
latmin <- latmin - 0.005
latmax <- latmax + 0.005
longmin <- longmin - 0.005
longmax <- longmax + 0.005
latdeg=latmax-latmin
longdeg=longmax-longmin
#scaling the x axis to the page size and the aspect ratio for lats and longs
drgcount <- length(unique(drgid))
if(drgcount == 3)longunits=(2.5*latdeg)/(0.713*4.1)
if(drgcount == 3)latunits=(4.1*longdeg)/(1.41*2.5)
if(drgcount == 4)longunits=(3.63*latdeg)/(0.713*1.88)
if(drgcount == 4)latunits=(1.88*longdeg)/(1.41*3.63)
if(drgcount == 1)longunits=(7.256*latdeg)/(0.713*5.216)
if(drgcount == 1)latunits=(5.216*longdeg)/(1.41*7.256)
if(drgcount == 2 || drgcount > 4)longunits=latdeg/0.713
if(drgcount == 2 || drgcount > 4)latunits=longdeg/1.41
#reset the longitude scaling for the axis and centering the data in the plot.
if(longdeg > longunits)longunits=longdeg
if(latdeg > latunits)latunits=latdeg
midlong=longmax-(longdeg/2)
midlat=latmin+(latdeg/2)
longmin=midlong-(longunits/2)
longmax=midlong+(longunits/2)
latmin=midlat-(latunits/2)
latmax=midlat+(latunits/2)
wmf.graph(file=\"Deploy$conseq.wmf\",width=11,height=7.5,color=F,pointsize=24)
lastx <- length(tracklat)
hourtime <- c(rep(0,length(tracklat)))
hourtime[1] <- 0
j <- 1
for(i in 2:lastx){
if(trunc(timelag[i]) > trunc(timelag[i-1]))hourtime[i] <- timelag[i]
if(trunc(timelag[i]) <= trunc(timelag[i-1]))hourtime[i] <- 0}
#Produce a data Object
newdrgid <- as.factor(drgid)
hourtime <- trunc(hourtime)
randy.df <- data.frame(tracklat,tracklong,drgid,newdrgid,hourtime)

xyplot(tracklat ~ tracklong | newdrgid, data=randy.df,pch=\" \",xlab=\"Longitude\",ylab=\"Latitude\",
xlim=c(longmin,longmax),ylim=c(latmin,latmax),
as.table=T,
main=\"Deployment $conseq\",
strip = function(...) strip.default(..., strip.names=F, style=1),
par.strip.text=list(cex=1.1),
panel=function(x,y, subscripts, ...){
lines(x,y)
text.default(x,y+0.0005,labels=ifelse(randy.df\$hourtime[subscripts]!=0,randy.df\$hourtime[subscripts],
\" \"),cex=.7)
#points(x[randy.df\$hourtime[subscripts]!=0],y[randy.df\$hourtime[subscripts]!=0],pch=3)
points(x[1],y[1],pch=16,csi=.14)
polygon(long,lat,density=90,lwd=2,angle=0,border=T,col=6)
par(new=T)
plot(long,lat,type=\"l\",xlab=\" \",ylab=\" \",axes=F)
})
par(fig=c(0,1,0,1))
pltdrgid <- drgid[timelag==max(timelag)]
plot(timelag[drgid==pltdrgid], tide[drgid==pltdrgid],
csi=.15,mex=par(mex=.30),mgp=c(1.5,.1,0),cex=.4,
type=\"l\",ylab=\"Tide\",xlab=\"Hours\",fig=par(fig=c(.72,.90,-.02,.13)))
frame()
q()
" > drifter2wmf.deck
if [ $plttype = p -o $plttype = P ]
then
R CMD BATCH drifter2.deck
fi
#if [ $plttype = w -o $plttype = W ]
#then
#Splus < drifter2wmf.deck
#fi
echo "
track <- scan(\"oradrft.dat\",what=list(0,0,0,0,0,0,0,\"\",0))
drgid <- track[[2]]
tracklat <- ((track[[3]]/200000)-90)
tracklong <- ((track[[4]]/200000)-180)
map <- matrix(scan(\"$map\"),ncol=3,byrow=T)
long <- map[,1]
lat <- map[,2]
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
latmin <- latmin - 0.005
latmax <- latmax + 0.005
longmin <- longmin - 0.005
longmax <- longmax + 0.005
latdeg=latmax-latmin
latunits=(7.256*latdeg)/(0.71*5.216)
units=longmax-latunits
if(units > longmin)longdeg=longmax-longmin
if(units > longmin)longunits=(5.216*longdeg)/(1.41*7.256)
if(units > longmin)units=latmin+longunits
if(units > longmin)latmax=units
if(units < longmin)longmin=units
wmf.graph(file=\"Deployall$conseq.wmf\",width=11,height=7.5,color=F,pointsize=24)
grpdrg <- unique(drgid)
for(i in 1:length(grpdrg)){
tracklon1<-c(rep(0,length(tracklong)))
tracklat1<-c(rep(0,length(tracklat)))
tracklon1<-tracklong[drgid==grpdrg[i]]
tracklat1<-tracklat[drgid==grpdrg[i]]
plot(tracklon1,tracklat1,type='l',xlim=c(longmin,longmax),
ylim=c(latmin,latmax),xlab=\"Longitude\",ylab=\"Latitude\",pin=par(pin=c(7.256,5.216)))
points(tracklon1[1],tracklat1[1],pch=16);
par(new=T)}
polygon(long,lat,density=100,angle=0,border=T,col=6)
par(new=T)
plot(long,lat,type=\"l\",xlab=\" \",ylab=\" \",axes=F) 
par(new=T)
title(main=\"Deployment $conseq\")
box()
frame()
q()
" > drifter4wmf.deck
echo "
track <- scan(\"oradrft.dat\",what=list(0,0,0,0,0,0,0,\"\",0))
drgid <- track[[2]]
tracklat <- ((track[[3]]/200000)-90)
tracklong <- ((track[[4]]/200000)-180)
map <- matrix(scan(\"$map\"),ncol=3,byrow=T)
long <- map[,1]
lat <- map[,2]
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
latmin <- latmin - 0.005
latmax <- latmax + 0.005
longmin <- longmin - 0.005
longmax <- longmax + 0.005
latdeg=latmax-latmin
latunits=(7.256*latdeg)/(0.71*5.216)
units=longmax-latunits
if(units > longmin)longdeg=longmax-longmin
if(units > longmin)longunits=(5.216*longdeg)/(1.41*7.256)
if(units > longmin)units=latmin+longunits
if(units > longmin)latmax=units
if(units < longmin)longmin=units
postscript(horizontal=T,width=9.7,height=7.1)
grpdrg <- unique(drgid)
for(i in 1:length(grpdrg)){
tracklon1<-c(rep(0,length(tracklong)))
tracklat1<-c(rep(0,length(tracklat)))
tracklon1<-tracklong[drgid==grpdrg[i]]
tracklat1<-tracklat[drgid==grpdrg[i]]
plot(tracklon1,tracklat1,type='l',xlim=c(longmin,longmax),
ylim=c(latmin,latmax),xlab=\"Longitude\",ylab=\"Latitude\",pin=par(pin=c(7.256,5.216)))
points(tracklon1[1],tracklat1[1],pch=16);
par(new=T)}
polygon(long,lat,density=0,border=T,col=6)
par(new=T)
title(main=\"Deployment $conseq\")
box()
q()
" > drifter4.deck
if [ $plttype = d -o $plttype = D ]
then
R CMD BATCH drifter4.deck
fi
if [ $plttype = w -o $plttype = W ]
then
R CMD BATCH drifter4wmf.deck
fi
if [ $plttype = p -o $plttype = P ]
then
R CMD BATCH drifter4.deck
fi
else
echo "
track <- scan(\"oradrft.dat\",what=list(0,0,0,0,0,0,0,\"\",0))
drgid <- track[[2]]
tracklat <- ((track[[3]]/200000)-90)
tracklong <- ((track[[4]]/200000)-180)
flow <- track[[8]]
map <- matrix(scan(\"$map\"),ncol=3,byrow=T)
long <- map[,1]
lat <- map[,2]
latmin <- min(na.rm = T,tracklat)
latmax <- max(na.rm = T,tracklat)
longmin <- min(na.rm = T,tracklong)
longmax <- max(na.rm = T,tracklong)
latmin <- latmin - 0.005
latmax <- latmax + 0.005
longmin <- longmin - 0.005
longmax <- longmax + 0.005
latdeg=latmax-latmin
latunits=(7.256*latdeg)/(0.71*5.216)
units=longmax-latunits
if(units > longmin)longdeg=longmax-longmin
if(units > longmin)longunits=(5.216*longdeg)/(1.41*7.256)
if(units > longmin)units=latmin+longunits
if(units > longmin)latmax=units
if(units < longmin)longmin=units
postscript(horizontal=T)
plot(tracklong,tracklat,type='l',xlim=c(longmin,longmax),
ylim=c(latmin,latmax),xlab=\"Longitude\",ylab=\"Latitude\",pin=par(pin=c(7.256,5.216)))
par(new=T)
title(main=\"$drftype\")
polygon(long,lat,density=0,border=T,col=6)
text(tracklong[1],tracklat[1],flow[1],cex=.8)
for (i in seq(along=tracklong))
arrows(tracklong[i],tracklat[i],tracklong[i+1],tracklat[i+1],open=T,
rel=F,size=.1)
box()
q()
" > drifter2.deck
R CMD BATCH drifter2.deck
fi
fi
