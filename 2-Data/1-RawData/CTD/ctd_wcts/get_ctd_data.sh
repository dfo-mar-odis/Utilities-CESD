#!/bin/bash

echo Name of file containing list of ctd deployments
read filenm


exec 3<$filenm
while read inputline1
do
   #get cruno and station from current line
   deploy="$(echo $inputline1 | cut -d' ' -f1)"

  sqlplus haigh/sql_quoddy3@ptran @/home/haigh/quoddy3/sql_scripts/get_ctd_data.sql $deploy
done <$filenm

exit 0
