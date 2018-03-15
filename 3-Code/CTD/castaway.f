      program castawayrfmt
      character allarr*178,char1*8,filename*60
      character*3 mmon(12),mint(12),filename2*60,filename3*60
      character allarr1*178
      real pres,depth,temp,cond,scond,sal,sound,density
      integer x,y,rec
      rec=1
   12 format(a40,4f10.5)
  101 format(a178)
      write(*,*)"Enter raw header information file now ?"
      read(5,101)filename
c     write(*,*)"Enter processed castaway file now ?"
c     read(5,101)filename2
      write(*,*)"Enter output filename for ORACLE Load ?"
      read(5,101)filename3
      open(unit=10,file=filename,status='unknown')
c     open(unit=11,file=filename2,status='unknown')
      open(unit=12,file=filename3,status='unknown')
c  Extract date and time data from header file      
      allarr1(1:40)='                                        '
      do while(rec .eq. 1)
        read(10,101)allarr
        y=1
        x=1
        write(*,*)allarr(8:11)
        if (allarr(8:11) .eq. 'time')then
          do while(x .le. 40)
            if (allarr(x:x) .eq. ',')then
              allarr1(1:18)=allarr(x+1:x+18)
              x=41
            end if
            x=x+1
          end do
          y=1
          goto 100
        end if
      end do
c Extract latitude and longitude from Header File
  100 continue
      write(*,*)allarr1(1:20)
      do while(rec .eq. 1)
        read(10,101)allarr
        write(*,*)allarr(3:7)
        y=1
        x=1
        if (allarr(3:7) .eq. 'Start')then
          do while(x .le. 40)
            if (allarr(x:x) .eq. ',')then
              allarr1(20:28)=allarr(x+1:x+9)
              read(10,101)allarr
              x=1
              do while(x .le. 40)
                if (allarr(x:x) .eq. ',')then
                  allarr1(30:38)=allarr(x+1:x+9)
                  x=41
                end if
                x=x+1
              end do
              write(*,*)allarr1(1:40)
             else
              x=x+1
            end if
          end do
          goto 200
        end if
      end do
  200 do while(rec .eq. 1)
        read(10,101,end=999)allarr
        y=1
        do while(y .le. 40)
          if (allarr1(y:y) .eq. '-' .or. allarr1(y:y) .eq. ':'
     &      .or. allarr1(y:y) .eq. ',' .or. allarr1(y:y) .eq. '/')then
             allarr1(y:y)=' ' 
          end if
          y=y+1
        end do
        if (allarr(1:4) .eq. 'Pres')then
          checkdepth=0
          read(10,*)pres,depth,temp,cond,scond,sal,sound,density
          density=density-1000
          write(12,12)allarr1,depth,temp,sal,density
          do while(depth .ge. checkdepth)
            read(10,*)pres,depth,temp,cond,scond,sal,sound,density
            density=density-1000
            if(depth .gt. checkdepth)then
              write(12,12)allarr1,depth,temp,sal,density
              checkdepth=depth
            end if
          end do
        end if
      end do
  999 stop
      end
