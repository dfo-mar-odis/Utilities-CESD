      program visit_drifter
      character record*80,filename*80,filenam1*80
      real long,lat
      integer drgid,rec,count,numdrg,vecstr(10),vecend(10)
      integer drgid2,kseq(10000),veccount,tcount,drgid3
      rec=1
   10 format(a80)
      filename="oradrft.dat"
      open(unit=10,file=filename,recl=80,
     &status='unknown')
      type *,'Enter Visit Drifter output data file ?'
      read(5,10)filenam1
      open(unit=11,file=filenam1,status='unknown')
      count=0
      read(10,*,end=888)long,lat,drgid
      rewind(10)
      do while(rec .eq. 1)
      read(10,*,end=888)long,lat,drgid2
      count=count+1
      kseq(count)=count
      end do
  888 rewind 10
      record="# vtk DataFile Version 3.0"
      write(11,10)record
      record="Drifter Data"                 
      write(11,10)record
      record="ASCII"                       
      write(11,10)record
      record="DATASET POLYDATA"              
      write(11,10)record
  102 format("POINTS",i5," float")          
      write(11,102)count
      x=1
      numdrg=1
      do while(rec .eq. 1)
      read(10,*,end=999)long,lat,drgid2
      drgid3=0
      write(11,*)long,lat,drgid3
      x=x+1
      if(drgid .ne. drgid2)then
        vecend(numdrg)=x-3
        vecstr(numdrg+1)=x-2
        numdrg=numdrg+1
        drgid=drgid2
      end if
      end do      
  999 vecstr(1)=0
      vecend(numdrg)=count-1
  103 format("LINES",i3,i5)
      tcount=numdrg+count
      write(11,103)numdrg,tcount
      write(*,*)vecstr
      write(*,*)vecend
      x=1
  104 format(300(I4))
      do while(x .le. numdrg)
        veccount=(vecend(x)-vecstr(x))+1
        write(11,104) veccount,(kseq(k),k=vecstr(x),vecend(x))
        x=x+1
      end do
      stop
      end
