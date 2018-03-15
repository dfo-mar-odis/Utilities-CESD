      Program castawaysal
      real*8 KCL,r,r2,sal,ds,ds2,a0,a1,a2,a3,a4,a5
      real*8 b0,b1,b2,b3,b4,b5
      write(*,*)'Enter Conductivity value?'
      read(*,*)XR
      write(*,*)'Enter Temperature  value'
      read(*,*)XT
      temp=xt
      cond=xr
      a0=0.008;
      a1=-0.1692;
      a2=25.3851;
      a3=14.0941;
      a4=-7.0261;
      a5=2.7081;
      b0=0.0005;
      b1=-0.0056;
      b2=-0.0066;
      b3=-0.0375;
      b4=0.0636;
      b5=-0.0144;
      c0=0.6766097;
      c1=0.0200564;
      c2=0.0001104259;
      c3=-0.00000069698;
      c4=0.0000000010031;
c     KCL=-0.0267243*(XT)**3 + 4.6636947*(XT)**2 + 861.3027640*XT + 
c    &29035.1640851
c     CR=XR/KCL   
c     Sal=a0 + (a1*(CR)**.5) + (a2*CR) + (a3*(CR)**1.5) + (a4*(CR)**2) + 
c    &(a5*(CR)**2.5)
c     DSAL=(XT-15)/(1+0.0162*(XT-15))*(b0+b1*(CR)**.5 + 
c    &b2*CR + b3*(CR)*1.5 + b4*(CR)**2 + B5*(CR)*2.5)
c     Sal=Sal+DSAL
      r=cond/42914;
      r2=r/(c0+temp*(c1+temp*(c2+temp*(c3+temp*c4))))
      r2=sqrt(r2)
      ds=b0+r2*(b1+r2*(b2+r2*(b3+r2*(b4+r2*b5))))
      ds2=((temp-15.0)/(1.0+0.0162*(temp-15.0)))*ds
      sal=a0+r2*(a1+r2*(a2+r2*(a3+r2*(a4+r2*a5))))+ds2
c     write(*,*)KCL,CR,DSAL,SAL
      write(*,*)r,r2,ds,ds2,sal      
      stop
      end
