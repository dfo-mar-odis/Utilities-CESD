      program ctd_rfmt
      character*3 mon(12)
      character*11 sal,sig,oxymll,oxymgl,par 
      character deploy*5
      character filename*132,stype*2,monn*3,record*132,upcast*1
      integer x,rec,j,min,hr,sec
      integer day,year,y       
      real temp,depth,cdepth,mdepth
      character*11 oxypsat,oxysatmgl,turb,wetlabs           
      data mon/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &'Sep','Oct','Nov','Dec'/
  100 format(a132)
  101 format(a2)
  109 format(a5)
  103 format(a1)
      oxypsat=' '
      oxysatmgl=' '
      turb=' '
      par=' '
      wetlabs=' '
      oxymll=' '
      oxymgl=' '
      type *,'Enter input filename'
      read(5,100)filename
      type *,'Enter deployment number'      
      read(5,109)deploy
      open(unit=10,status='old',readonly,
     &file=filename)
      open(unit=11,status='unknown',
     &file='ctd_rfmt.dat',recl=180)
      open(unit=12,status='unknown',
     &file='ctdinf.dat',recl=180)
      rec = 1
      x=1
      read(10,100)record
      type *,record
      type *,' '
      type *,'MMM DD YYYY HH:MM:SS      T090         Pr    WetStar
     &   SeaTurbMtr0      Sal00  Sigma-M-i00'
      type *,' '
      type *,'WARNING... These displayed variables must 
     &match for proper output'
   11 format(1x,a3,i3,i5,i3,1x,i2,1x,i2,f11.4,f9.1,2x,
     &9a11)
   12 format(a5,i3,i3,i5,3I3,f6.1,f7.3,9a11)
   14 format(a5,i3,i3,i5,3i3)
      read(10,11,end=102)monn,day,year,hr,min,sec,temp,depth,
     &wetlabs,oxymll,oxymgl,oxypsat,oxysatmgl,turb,par,sal,sig
      y=1
      do while (y .le. 12)
         if (monn .eq. mon(y) ) then
           moni=y
         end if
         y=y+1
      end do
      write(12,14)deploy,moni,day,year,hr,min,sec
      type *,moni,monn,day,year,hr,min,sec,temp,depth,wetlabs
      backspace 10
      type *,'   Do you want to reprocess for the up cast y/n?'
      read(5,103)upcast
      if(upcast .eq. 'N' .or. upcast .eq. 'n')then
        cdepth=0.0
        upcast='N'
       else
        mdepth=0.0
        upcast='Y'
        do while(rec .eq. 1)
          read(10,11,end=115)monn,day,year,hr,min,sec,temp,depth,
     &    wetlabs,oxymll,oxymgl,oxypsat,oxysatmgl,turb,par,sal,sig
          if(depth .gt. mdepth)then
            mdepth=depth
          end if
        end do
  115   continue
        rewind 10
        read(10,100)record
      end if
      if(upcast .eq. 'Y')then
        goto 112
      end if
      do while(rec .eq. 1)
        read(10,11,end=102)monn,day,year,hr,min,sec,temp,depth,
     &  wetlabs,oxymll,oxymgl,oxypsat,oxysatmgl,turb,par,sal,sig
        if(depth .gt. cdepth)then
          cdepth=depth
         else
          goto 102
        end if
        y=1
        do while(y .le. 12)       
          if(monn .eq. mon(y))then
            moni=y
            goto 111
          end if
          y=y+1
        end do
  111   write(11,12)deploy,day,moni,year,hr,min,sec,depth,temp,sal,
     &  sig,oxymll,oxymgl,oxypsat,oxysatmgl,turb,wetlabs,par
      end do
  112 do while(rec .eq. 1)
        read(10,11,end=102)monn,day,year,hr,min,sec,temp,depth,
     &  wetlabs,oxymll,oxymgl,oxypsat,oxysatmgl,turb,par,sal,sig
        if(depth .lt. mdepth)then
          goto 112
         else
          mdepth=-1.0
        end if
        y=1
        do while(y .le. 12)       
          if(monn .eq. mon(y))then
            moni=y
            goto 114
          end if
          y=y+1
        end do
  114   write(11,12)deploy,day,moni,year,hr,min,sec,depth,temp,sal,
     &  sig,oxymll,oxymgl,oxypsat,oxysatmgl,turb,wetlabs,par
        end do
  102   stop
	end
