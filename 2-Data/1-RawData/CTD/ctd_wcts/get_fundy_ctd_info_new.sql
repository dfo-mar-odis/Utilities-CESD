/* Page Foramtting Section */
set pages 0;
set linesize 108;
set newpage 1;
set feedback off;
set heading off;
set wrap off;
set trim off;
set show off;
set verify off;
set termout off;
set heading off;
column deploy format 99999999;
column station format a8;
column longitude format 9999.99999999;
column latitude format 9999.99999999;
column date_str format a17;
column a_str format a40;


spool fundy_ctd_list_new.dat
select deploy,
/*       station, */
       -abs(longitude),
       latitude,
       to_char(sdate,'yyyy-mm-dd hh24:mi') date_str,
       concat(concat(replace(trim(project),' ','_'),'_'),replace(trim(area),' ','_')) a_str
from sabs_osdinf
   where  -abs(longitude) > -75
     and  -abs(longitude) < -62
     and latitude < 45.5
     and latitude > 44.0
order by sdate,deploy;
spool off
exit
