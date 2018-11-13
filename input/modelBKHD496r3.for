      MODULE modelBKHD496r3_SGMOD                                                                                                   
C      MA=MACH NO, AL=ALTITUDE, DMA=DELTA MACH, DAL=DELTA ALTITUDE                                                                  
       REAL:: MA(17),AL(9),DMA,DAL,M                                                                                                
       INTEGER::I,J,L,M1,IAL(9)                                                                                                     
C                                                                                                                                   
      END MODULE modelBKHD496r3_SGMOD                                                                                               
C PC SINDA/G FOR COMPAQ FORTRAN, SINGLE PRECISION                                                                                   
      PROGRAM SINDA                                                                                                                 
      USE modelBKHD496r3_SGMOD                                                                                                      
      CHARACTER*6 H                                                                                                                 
      COMMON /TITLE/H(20)                                                                                                           
      COMMON /TEMP/T(    786)                                                                                                       
      COMMON /CAP/C(    258)                                                                                                        
      COMMON /SOURCE/Q(    262)                                                                                                     
      COMMON /COND/G(    1931)                                                                                                      
      COMMON /KONST/K(      1)                                                                                                      
      COMMON /ARRAY/A(    4831)                                                                                                     
      COMMON /PC1/LSQ1(   13416)                                                                                                    
      COMMON /PC2/LSQ2(       1)                                                                                                    
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA                                                                   
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS                                                                              
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR                                                                  
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR,GUI                                                                       
      COMMON /XSPACE/NDIM,NTH,X(   11722)                                                                                           
      COMMON /FIXCON/                                                                                                               
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,                                                                      
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,                                                                      
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,                                                                      
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,                                                                      
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,                                                                      
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,                                                                      
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,                                                                      
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,                                                                      
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,                                                                      
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,                                                                      
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,                                                                      
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,                                                                      
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,                                                                      
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,                                                                      
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,                                                                      
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG,SPARE8,SPARE9,SSTERM                                                                       
      DIMENSION XK(      1),NX(   11722),IA(    4831)                                                                               
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))                                                                            
      LOGICAL ASCI,FLUD,GNRL,CHAR,GUI                                                                                               
      CHARACTER * 14 PRNAME                                                                                                         
      COMMON /PROBNAME/PRNAME                                                                                                       
      COMMON /MODNAME/MODNAME                                                                                                       
      CHARACTER *50 MODNAME                                                                                                         
      COMMON /IMODNAME/MODNSTRT,MODNEND,IFILESYS                                                                                    
      COMMON /NewPCS/nDimC,nDimQ,nDimG                                                                                              
      COMMON /PcsC/lsqC(     786)                                                                                                   
      COMMON /PcsQ/lsqQ(     524)                                                                                                   
      COMMON /PcsG/lsqG(    7724)                                                                                                   
      LOGICAL lRecall,lParam                                                                                                        
       COMMON /File22/lRecall,lParam                                                                                                
      MODNAME="modelBKHD496r3"                                                                                                      
      MODNSTRT=  1                                                                                                                  
      MODNEND= 14                                                                                                                   
      IFILESYS=  1                                                                                                                  
      LPARAM=.FALSE.                                                                                                                
      PRNAME=MODNAME(MODNSTRT:MODNEND)                                                                                              
      GUI=.TRUE.                                                                                                                    
      NIN=5                                                                                                                         
      LDAT=2                                                                                                                        
      LDIC=4                                                                                                                        
      ASCI=.FALSE.                                                                                                                  
      CHAR=.FALSE.                                                                                                                  
      FLUD=.FALSE.                                                                                                                  
      GNRL=.FALSE.                                                                                                                  
      T(1)=0.                                                                                                                       
      C(1)=0.                                                                                                                       
      Q(1)=0.                                                                                                                       
      G(1)=0.                                                                                                                       
      LSQ1(1)=0                                                                                                                     
      LSQ2(1)=0                                                                                                                     
      K(1)=0                                                                                                                        
      A(1)=0.                                                                                                                       
      X(1)=0.                                                                                                                       
      NOUT=   6                                                                                                                     
      CALL RUNJOB(PRNAME,MODNAME,0)                                                                                                 
      CALL CLOSE_H5                                                                                                                 
      END                                                                                                                           
      SUBROUTINE EXECT                                                                                                              
      use msflib                                                                                                                    
      USE modelBKHD496r3_SGMOD                                                                                                      
      CHARACTER*6 H                                                                                                                 
      COMMON /TITLE/H(20)                                                                                                           
      COMMON /TEMP/T(    786)                                                                                                       
      COMMON /CAP/C(    258)                                                                                                        
      COMMON /SOURCE/Q(    262)                                                                                                     
      COMMON /COND/G(    1931)                                                                                                      
      COMMON /KONST/K(      1)                                                                                                      
      COMMON /ARRAY/A(    4831)                                                                                                     
      COMMON /PC1/LSQ1(   13416)                                                                                                    
      COMMON /PC2/LSQ2(       1)                                                                                                    
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA                                                                   
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS                                                                              
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR                                                                  
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR,GUI                                                                       
      COMMON /XSPACE/NDIM,NTH,X(   11722)                                                                                           
      COMMON /FIXCON/                                                                                                               
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,                                                                      
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,                                                                      
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,                                                                      
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,                                                                      
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,                                                                      
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,                                                                      
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,                                                                      
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,                                                                      
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,                                                                      
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,                                                                      
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,                                                                      
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,                                                                      
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,                                                                      
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,                                                                      
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,                                                                      
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG,SPARE8,SPARE9,SSTERM                                                                       
      DIMENSION XK(      1),NX(   11722),IA(    4831)                                                                               
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))                                                                            
      LOGICAL ASCI,FLUD,GNRL,CHAR,GUI                                                                                               
      CHARACTER * 14 PRNAME                                                                                                         
      COMMON /PROBNAME/PRNAME                                                                                                       
       OPEN(77, FILE='RESULTS_BKHD496.DAT')                                                                                         
C Write section #1                                                                                                                  
       WRITE(77,883)'$ BKHD 496'                                                                                                    
C load the variable arrays                                                                                                          
       DAL = 5000.                                                                                                                  
       DMA = .1                                                                                                                     
       DO J = 1,9                                                                                                                   
           AL(J) = DAL*(J-1)                                                                                                        
       ENDDO                                                                                                                        
        DO I = 1,17                                                                                                                 
             MA(I) = DMA*(I-1)                                                                                                      
        ENDDO                                                                                                                       
C Write section #2                                                                                                                  
       WRITE(77,884)NND                                                                                                             
C Write section #3                                                                                                                  
       WRITE(77,885)(NA(L),L=1,NND)                                                                                                 
C Run the model                                                                                                                     
        DO M1=1,3                                                                                                                   
        M=M1                                                                                                                        
        DO I=1,17                                                                                                                   
          DO J=1,9                                                                                                                  
      CALL   SNSOR                                                                                                                  
C Write section #4                                                                                                                  
       IAL(J)=AL(J)                                                                                                                 
       WRITE(77,886)MA(I),IAL(J)                                                                                                    
       WRITE(77,887)(T(L),L=1,NND)                                                                                                  
          ENDDO                                                                                                                     
        ENDDO                                                                                                                       
        ENDDO                                                                                                                       
C Write section #5                                                                                                                  
       WRITE(77,883)'$ENDDATA  '                                                                                                    
 883    FORMAT(A10)                                                                                                                 
 884    FORMAT(I8)                                                                                                                  
 885    FORMAT(10I8)                                                                                                                
 886    FORMAT(F8.2,I8)                                                                                                             
 887    FORMAT(10F8.2)                                                                                                              
C These calls can be modified in the "append.sin file.                                                                              
C This COMMON is shared with the RESPAT routine                                                                                     
C and is called to write nodal results for MSC.Patran                                                                               
CF     COMMON /PATRAN/ IFRAME, NUNIT                                                                                                
C   IFRAME is the initial result file number                                                                                        
C   NUNIT  is the unit number for output                                                                                            
      IFRAME = 0                                                                                                                    
      NUNIT = 90                                                                                                                    
C Call default steady state routine                                                                                                 
CF     CALL STDSTL                                                                                                                  
CF     CALL TPRINT                                                                                                                  
C Call RESPAT to create nodal results for MSC.Patran to read                                                                        
      CALL RESPAT                                                                                                                   
      RETURN                                                                                                                        
      END                                                                                                                           
      SUBROUTINE VARBL1                                                                                                             
      use msflib                                                                                                                    
      USE modelBKHD496r3_SGMOD                                                                                                      
      CHARACTER*6 H                                                                                                                 
      COMMON /TITLE/H(20)                                                                                                           
      COMMON /TEMP/T(    786)                                                                                                       
      COMMON /CAP/C(    258)                                                                                                        
      COMMON /SOURCE/Q(    262)                                                                                                     
      COMMON /COND/G(    1931)                                                                                                      
      COMMON /KONST/K(      1)                                                                                                      
      COMMON /ARRAY/A(    4831)                                                                                                     
      COMMON /PC1/LSQ1(   13416)                                                                                                    
      COMMON /PC2/LSQ2(       1)                                                                                                    
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA                                                                   
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS                                                                              
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR                                                                  
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR,GUI                                                                       
      COMMON /XSPACE/NDIM,NTH,X(   11722)                                                                                           
      COMMON /FIXCON/                                                                                                               
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,                                                                      
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,                                                                      
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,                                                                      
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,                                                                      
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,                                                                      
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,                                                                      
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,                                                                      
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,                                                                      
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,                                                                      
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,                                                                      
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,                                                                      
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,                                                                      
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,                                                                      
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,                                                                      
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,                                                                      
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG,SPARE8,SPARE9,SSTERM                                                                       
      DIMENSION XK(      1),NX(   11722),IA(    4831)                                                                               
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))                                                                            
      LOGICAL ASCI,FLUD,GNRL,CHAR,GUI                                                                                               
      CHARACTER * 14 PRNAME                                                                                                         
      COMMON /PROBNAME/PRNAME                                                                                                       
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(273))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(275))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(277))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(279))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(281))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(282))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(283))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(284))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(285))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(286))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(287))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(288))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(897), T(289))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(290))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(301))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(303))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(305))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(307))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(309))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(310))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(311))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(312))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(313))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(314))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(315))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(316))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1345), T(317))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(318))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(319))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(320))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(321))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(322))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(323))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(324))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(325))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(326))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(327))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(328))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(329))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(330))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(331))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(333))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(334))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(335))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(337))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(338))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(339))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(340))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(341))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(342))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(343))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(344))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(345))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(346))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(347))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(348))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(349))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(350))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(351))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(352))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(353))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(354))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(355))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(356))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(357))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(358))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(359))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(360))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(361))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(362))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(363))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(364))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(365))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(366))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(367))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(368))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(369))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(370))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(371))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(372))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(373))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(374))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(375))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(376))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(377))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(378))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(379))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(380))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(381))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(382))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(383))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(384))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(385))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(386))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(387))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(388))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(389))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(390))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(391))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(392))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(393))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(394))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(395))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(396))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(397))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(398))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(399))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(400))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(401))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(402))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(403))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(404))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(405))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(406))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(407))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(408))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(409))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(410))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(411))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(412))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(413))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(414))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(415))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(416))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(417))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(418))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(419))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(420))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(421))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(422))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(423))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(424))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(425))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(426))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(427))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(428))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(429))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(430))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(431))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(432))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(433))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(434))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(435))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(436))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(437))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(438))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(439))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(440))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(441))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(442))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(443))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(444))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(445))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(446))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(447))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(448))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(449))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(450))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(451))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(452))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(453))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(454))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(455))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(456))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(457))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(458))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(459))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(460))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(461))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(462))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(463))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(464))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(465))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(466))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(467))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(468))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(469))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(470))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(471))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(472))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(473))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(474))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(475))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(476))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(477))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(478))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(479))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(480))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(481))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(482))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(483))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(484))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(485))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(486))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(487))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(488))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(489))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(490))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(491))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(492))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(493))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(494))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(495))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(496))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(497))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(498))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(499))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(500))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(501))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(502))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(503))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(504))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(505))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(506))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(507))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(508))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(509))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(510))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(511))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(512))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(513))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(514))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(515))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(516))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(517))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(518))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(519))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(520))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(521))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(522))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(523))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(524))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(525))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(526))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(527))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(528))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(529))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(530))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(531))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(532))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(533))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(534))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(535))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(536))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(537))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(538))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(539))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(540))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(541))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(542))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(543))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(544))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(545))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(546))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(547))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(548))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(549))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(550))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(551))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(2689), T(552))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(553))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(554))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(555))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(556))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(557))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(558))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(559))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(560))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(561))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(562))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(563))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(564))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(565))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(566))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(567))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(568))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(569))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(570))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(571))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(572))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(573))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(574))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(575))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(576))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(577))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(578))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(579))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(580))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(581))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(582))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(583))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(584))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(585))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(586))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(587))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(588))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(589))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(590))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(591))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(592))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(593))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(594))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(595))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(596))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(597))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(598))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(599))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(600))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(601))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(602))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(603))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(604))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(605))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(606))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(607))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(608))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(609))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(610))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(611))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(612))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(613))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(614))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(615))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(616))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(617))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(618))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(619))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(620))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(621))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(622))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(623))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(624))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(625))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(626))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(627))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(628))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(629))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(630))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(631))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(632))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(633))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(634))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(635))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(636))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(637))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(638))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(639))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(640))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(641))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(642))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(643))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(644))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(645))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(646))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(647))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(648))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(649))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(650))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(651))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(652))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(653))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(654))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(655))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(656))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(657))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(658))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(659))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(660))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(661))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(662))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(663))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(664))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(665))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(666))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(667))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(668))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(669))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(670))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(671))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(672))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(673))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(2241), T(674))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(675))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(676))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(677))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(678))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(679))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(680))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(681))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(682))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(683))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(684))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(685))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(686))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(687))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(688))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(689))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(690))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(691))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(692))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(693))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(694))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(695))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(696))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(697))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(698))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(699))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(701))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(703))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(705))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(707))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(708))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(709))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(710))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(711))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(712))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(713))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(714))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(715))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(716))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(717))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(718))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(719))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(720))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(721))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(722))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(723))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(724))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(725))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(726))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(727))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(728))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(729))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(730))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(731))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(733))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(735))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(737))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(739))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(740))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(741))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(742))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(743))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(744))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(745))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(746))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(747))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(748))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(749))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(750))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(751))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(752))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(753))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(754))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(755))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(756))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(757))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(758))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(759))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(760))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(761))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(762))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(763))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(765))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(767))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(768))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(769))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(770))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(771))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(772))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(773))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(774))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(775))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(776))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(777))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(778))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(779))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(780))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(781))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(782))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(449), T(783))                                                                                  
      CALL D3DEG1(AL(J), MA(I), M, A(3046), T(784))                                                                                 
      CALL D3DEG1(AL(J), MA(I), M, A(1), T(785))                                                                                    
      CALL D3DEG1(AL(J), MA(I), M, A(1793), T(786))                                                                                 
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61915278, G(904))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61915278, G(905))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(908))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(909))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00020259, G(912))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00020259, G(913))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061310, G(916))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061310, G(917))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00041061, G(920))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00041061, G(921))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00094293, G(924))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00094293, G(925))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(928))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(929))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(932))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(933))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001229, G(936))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001229, G(937))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02620140, G(938))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02620140, G(939))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61915278, G(960))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61915278, G(961))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(964))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(965))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00020260, G(968))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00020260, G(969))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.62163192, G(972))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.62163192, G(973))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.62222916, G(976))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.62222916, G(977))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60415280, G(980))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60415280, G(981))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.58696532, G(984))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.58696532, G(985))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(988))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(989))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001229, G(992))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001229, G(993))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02620140, G(994))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02620140, G(995))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.45409027, G(996))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.45409027, G(997))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573959, G(998))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573959, G(999))                                                                        
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.49077082, G(1000))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.49077082, G(1001))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1002))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1003))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1004))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1005))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02761114, G(1006))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02761114, G(1007))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.45409027, G(1008))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.45409027, G(1009))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573959, G(1010))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573959, G(1011))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.49077082, G(1012))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.49077082, G(1013))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1014))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1015))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1016))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1017))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02761114, G(1018))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02761114, G(1019))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1020))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1021))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00048765, G(1024))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00048765, G(1025))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01087152, G(1026))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01087152, G(1027))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1028))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1029))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1032))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1033))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01816667, G(1034))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01816667, G(1035))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000029, G(1036))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000029, G(1037))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02527430, G(1038))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02527430, G(1039))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1040))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1041))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01349306, G(1042))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01349306, G(1043))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.46598610, G(1044))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.46598610, G(1045))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01804861, G(1046))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01804861, G(1047))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.44665277, G(1048))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.44665277, G(1049))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.05721180, G(1050))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.05721180, G(1051))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1052))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1053))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00903126, G(1054))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00903126, G(1055))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1056))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1057))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000347, G(1058))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000347, G(1059))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1060))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1061))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01241667, G(1062))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01241667, G(1063))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000917, G(1064))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000917, G(1065))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00848960, G(1066))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00848960, G(1067))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1068))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1069))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000347, G(1070))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000347, G(1071))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000024, G(1072))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000024, G(1073))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01242014, G(1074))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01242014, G(1075))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00002140, G(1076))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00002140, G(1077))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00873960, G(1078))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00873960, G(1079))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1080))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1081))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1084))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1085))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02728473, G(1086))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02728473, G(1087))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1088))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1089))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1090))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1091))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48049307, G(1092))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48049307, G(1093))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1096))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1097))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04345486, G(1098))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04345486, G(1099))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1100))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1101))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03578125, G(1102))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03578125, G(1103))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.38600695, G(1104))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.38600695, G(1105))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03351389, G(1106))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03351389, G(1107))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000008, G(1108))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000008, G(1109))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03861458, G(1110))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03861458, G(1111))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000029, G(1112))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000029, G(1113))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03498610, G(1114))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03498610, G(1115))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1116))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1117))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03233333, G(1118))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03233333, G(1119))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41651389, G(1120))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41651389, G(1121))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1122))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1123))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41675696, G(1124))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41675696, G(1125))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03216320, G(1126))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03216320, G(1127))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41798609, G(1128))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41798609, G(1129))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03396874, G(1130))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03396874, G(1131))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1132))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1133))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02227084, G(1134))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02227084, G(1135))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1136))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1137))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02545834, G(1138))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02545834, G(1139))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1140))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1141))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02050347, G(1142))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02050347, G(1143))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1144))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1145))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03625695, G(1146))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03625695, G(1147))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1148))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1149))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1152))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1153))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1156))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1157))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03540278, G(1158))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03540278, G(1159))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1160))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1161))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04322222, G(1162))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04322222, G(1163))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1164))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000002, G(1165))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04416667, G(1166))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04416667, G(1167))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00081743, G(1168))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00081743, G(1169))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1172))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1173))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00045486, G(1174))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00045486, G(1175))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1176))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1177))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00053125, G(1178))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00053125, G(1179))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000276, G(1180))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000276, G(1181))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467015, G(1182))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467015, G(1183))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001494, G(1184))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001494, G(1185))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467708, G(1186))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467708, G(1187))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1188))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1189))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1192))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1193))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00044445, G(1194))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00044445, G(1195))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1196))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1197))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02512153, G(1198))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02512153, G(1199))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1200))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1201))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02470834, G(1202))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02470834, G(1203))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1204))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1205))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1208))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1209))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061111, G(1210))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061111, G(1211))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1212))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1213))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573264, G(1214))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573264, G(1215))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1216))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1217))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02466318, G(1218))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02466318, G(1219))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1220))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1221))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1224))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1225))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00035764, G(1226))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00035764, G(1227))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61977780, G(1228))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61977780, G(1229))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1232))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1233))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1236))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1237))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000026, G(1240))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000026, G(1241))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1244))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1245))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1248))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1249))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04775695, G(1250))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04775695, G(1251))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1252))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1253))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01184028, G(1254))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01184028, G(1255))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1256))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1257))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1260))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1261))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1264))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1265))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04709028, G(1266))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04709028, G(1267))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1268))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1269))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01161806, G(1270))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01161806, G(1271))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1272))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1273))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1276))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1277))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1280))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1281))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02262848, G(1282))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02262848, G(1283))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1284))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1285))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02354862, G(1286))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02354862, G(1287))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1288))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1289))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1292))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1293))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60143059, G(1296))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60143059, G(1297))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60088193, G(1300))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60088193, G(1301))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57701385, G(1304))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57701385, G(1305))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57673615, G(1308))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57673615, G(1309))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1312))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1313))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03630902, G(1314))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03630902, G(1315))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1316))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1317))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1318))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1319))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1320))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1321))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1324))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1325))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1328))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1329))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03608680, G(1330))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03608680, G(1331))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1332))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1333))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1334))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1335))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1336))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1337))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1340))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1341))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1344))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1345))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02446874, G(1346))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02446874, G(1347))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1348))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1349))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1350))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1351))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1352))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1353))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1356))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1357))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1360))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1361))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1364))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1365))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57652777, G(1368))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57652777, G(1369))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57631248, G(1372))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57631248, G(1373))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1376))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1377))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1378))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1379))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1380))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1381))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02001389, G(1382))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02001389, G(1383))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1384))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1385))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1388))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1389))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1392))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1393))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1394))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1395))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1396))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1397))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02437153, G(1398))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02437153, G(1399))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1400))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1401))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.47995833, G(1404))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.47995833, G(1405))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.44988191, G(1408))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.44988191, G(1409))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1410))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1411))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1412))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1413))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02727779, G(1414))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02727779, G(1415))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43318751, G(1416))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43318751, G(1417))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1420))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1421))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1424))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000005, G(1425))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1428))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1429))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1432))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000003, G(1433))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02121180, G(1434))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02121180, G(1435))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000013, G(1436))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000013, G(1437))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000013, G(1440))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000013, G(1441))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00871527, G(1442))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00871527, G(1443))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000029, G(1444))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000029, G(1445))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02992707, G(1446))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02992707, G(1447))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1448))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1449))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000040, G(1452))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000040, G(1453))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574305, G(1454))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574305, G(1455))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000019, G(1456))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000019, G(1457))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574305, G(1458))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574305, G(1459))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000056, G(1460))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000056, G(1461))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00143053, G(1464))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00143053, G(1465))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061311, G(1468))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061311, G(1469))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00041061, G(1472))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00041061, G(1473))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00094308, G(1476))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00094308, G(1477))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(1480))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000657, G(1481))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43761110, G(1484))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43761110, G(1485))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01358333, G(1486))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01358333, G(1487))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.59935415, G(1488))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.59935415, G(1489))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00312848, G(1490))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00312848, G(1491))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00048766, G(1492))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00048766, G(1493))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01087152, G(1494))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01087152, G(1495))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57569444, G(1496))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57569444, G(1497))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00598611, G(1498))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00598611, G(1499))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1500))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1501))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01816667, G(1502))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01816667, G(1503))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1504))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1505))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02527430, G(1506))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02527430, G(1507))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1508))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1509))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01349306, G(1510))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01349306, G(1511))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.46598610, G(1512))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.46598610, G(1513))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01805208, G(1514))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01805208, G(1515))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000049, G(1516))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000049, G(1517))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.05721874, G(1518))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.05721874, G(1519))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1520))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000016, G(1521))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00903126, G(1522))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00903126, G(1523))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1524))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1525))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000039, G(1528))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000039, G(1529))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01242014, G(1530))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01242014, G(1531))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000932, G(1532))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000932, G(1533))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00848960, G(1534))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00848960, G(1535))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1536))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1537))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1540))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1541))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01242014, G(1542))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01242014, G(1543))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00002151, G(1544))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00002151, G(1545))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00873960, G(1546))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00873960, G(1547))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1548))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1549))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1552))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1553))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02728473, G(1554))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02728473, G(1555))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1556))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1557))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1558))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01472223, G(1559))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48049307, G(1560))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48049307, G(1561))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.37241665, G(1564))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.37241665, G(1565))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04345486, G(1566))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04345486, G(1567))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1568))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1569))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03578125, G(1570))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03578125, G(1571))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1572))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1573))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03351389, G(1574))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03351389, G(1575))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.39702085, G(1576))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.39702085, G(1577))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03861458, G(1578))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03861458, G(1579))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1580))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1581))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03498610, G(1582))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03498610, G(1583))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1584))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1585))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03233333, G(1586))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03233333, G(1587))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41651389, G(1588))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41651389, G(1589))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1590))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1591))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41675696, G(1592))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41675696, G(1593))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03216320, G(1594))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03216320, G(1595))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41798609, G(1596))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41798609, G(1597))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03396874, G(1598))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03396874, G(1599))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1600))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1601))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02227084, G(1602))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02227084, G(1603))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1604))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1605))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02545834, G(1606))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02545834, G(1607))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.42561805, G(1608))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.42561805, G(1609))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02050347, G(1610))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02050347, G(1611))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1612))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1613))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03625347, G(1614))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03625347, G(1615))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1616))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1617))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1620))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1621))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41074306, G(1624))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.41074306, G(1625))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03540278, G(1626))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03540278, G(1627))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00081758, G(1628))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00081758, G(1629))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000037, G(1632))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000037, G(1633))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00045486, G(1634))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00045486, G(1635))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000026, G(1636))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000026, G(1637))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00053125, G(1638))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00053125, G(1639))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000276, G(1640))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000276, G(1641))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467015, G(1642))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467015, G(1643))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001505, G(1644))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00001505, G(1645))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467708, G(1646))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02467708, G(1647))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1648))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1649))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1652))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1653))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00044445, G(1654))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00044445, G(1655))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1656))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1657))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02512153, G(1658))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02512153, G(1659))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1660))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1661))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02470834, G(1662))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02470834, G(1663))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48230559, G(1664))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48230559, G(1665))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1668))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1669))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061111, G(1670))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00061111, G(1671))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.45343053, G(1672))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.45343053, G(1673))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573264, G(1674))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02573264, G(1675))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1676))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1677))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02466318, G(1678))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02466318, G(1679))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43952778, G(1680))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43952778, G(1681))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1684))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1685))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00035764, G(1686))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00035764, G(1687))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61977780, G(1688))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.61977780, G(1689))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1692))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1693))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60197914, G(1696))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60197914, G(1697))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1700))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1701))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57728469, G(1704))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57728469, G(1705))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1708))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1709))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04775695, G(1710))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04775695, G(1711))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1712))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1713))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01184028, G(1714))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01184028, G(1715))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1716))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1717))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1720))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1721))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1724))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1725))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04709028, G(1726))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.04709028, G(1727))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1728))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1729))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01161806, G(1730))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01161806, G(1731))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1732))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1733))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1736))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1737))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1740))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1741))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02262848, G(1742))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02262848, G(1743))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1744))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1745))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02354862, G(1746))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02354862, G(1747))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1748))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1749))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1752))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1753))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60143059, G(1756))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60143059, G(1757))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60088193, G(1760))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.60088193, G(1761))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57701385, G(1764))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57701385, G(1765))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57673615, G(1768))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57673615, G(1769))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1772))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1773))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03630902, G(1774))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03630902, G(1775))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1776))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1777))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1778))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1779))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1780))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1781))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1784))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1785))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1788))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1789))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03608680, G(1790))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03608680, G(1791))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1792))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1793))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1794))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1795))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1796))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48106945, G(1797))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1800))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1801))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1804))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1805))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02446874, G(1806))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02446874, G(1807))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1808))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1809))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1810))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02164235, G(1811))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1812))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1813))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1816))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1817))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1820))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1821))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1824))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1825))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57652777, G(1828))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57652777, G(1829))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57631248, G(1832))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57631248, G(1833))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1836))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1837))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1838))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1839))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1840))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1841))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02001389, G(1842))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02001389, G(1843))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1844))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1845))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1848))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.53155553, G(1849))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1852))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1853))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1854))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1855))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1856))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.51052082, G(1857))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02437153, G(1858))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02437153, G(1859))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1860))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.48044443, G(1861))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.47995833, G(1864))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.47995833, G(1865))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.44988191, G(1868))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.44988191, G(1869))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1870))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01881251, G(1871))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1872))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000021, G(1873))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02727779, G(1874))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02727779, G(1875))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43318751, G(1876))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43318751, G(1877))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1880))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1881))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.59969449, G(1884))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.59969449, G(1885))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57614583, G(1888))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.57614583, G(1889))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1892))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.55259025, G(1893))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02121180, G(1894))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02121180, G(1895))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1896))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1897))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1900))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000011, G(1901))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00871527, G(1902))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00871527, G(1903))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1904))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000032, G(1905))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02992707, G(1906))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.02992707, G(1907))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1908))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1909))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1912))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1913))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574305, G(1914))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574305, G(1915))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000049, G(1916))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000049, G(1917))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574652, G(1918))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.03574652, G(1919))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1920))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00000042, G(1921))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00143058, G(1924))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.00143058, G(1925))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43761110, G(1928))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.43761110, G(1929))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01358333, G(1930))                                                                       
      CALL D2D1WM(AL(J), MA(I), A(4747), 0.01358333, G(1931))                                                                       
      RETURN                                                                                                                        
      END                                                                                                                           
      SUBROUTINE VARBL2                                                                                                             
      use msflib                                                                                                                    
      USE modelBKHD496r3_SGMOD                                                                                                      
      CHARACTER*6 H                                                                                                                 
      COMMON /TITLE/H(20)                                                                                                           
      COMMON /TEMP/T(    786)                                                                                                       
      COMMON /CAP/C(    258)                                                                                                        
      COMMON /SOURCE/Q(    262)                                                                                                     
      COMMON /COND/G(    1931)                                                                                                      
      COMMON /KONST/K(      1)                                                                                                      
      COMMON /ARRAY/A(    4831)                                                                                                     
      COMMON /PC1/LSQ1(   13416)                                                                                                    
      COMMON /PC2/LSQ2(       1)                                                                                                    
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA                                                                   
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS                                                                              
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR                                                                  
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR,GUI                                                                       
      COMMON /XSPACE/NDIM,NTH,X(   11722)                                                                                           
      COMMON /FIXCON/                                                                                                               
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,                                                                      
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,                                                                      
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,                                                                      
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,                                                                      
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,                                                                      
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,                                                                      
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,                                                                      
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,                                                                      
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,                                                                      
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,                                                                      
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,                                                                      
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,                                                                      
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,                                                                      
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,                                                                      
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,                                                                      
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG,SPARE8,SPARE9,SSTERM                                                                       
      DIMENSION XK(      1),NX(   11722),IA(    4831)                                                                               
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))                                                                            
      LOGICAL ASCI,FLUD,GNRL,CHAR,GUI                                                                                               
      CHARACTER * 14 PRNAME                                                                                                         
      COMMON /PROBNAME/PRNAME                                                                                                       
      RETURN                                                                                                                        
      END                                                                                                                           
      SUBROUTINE OUTCAL                                                                                                             
      use msflib                                                                                                                    
      USE modelBKHD496r3_SGMOD                                                                                                      
      CHARACTER*6 H                                                                                                                 
      COMMON /TITLE/H(20)                                                                                                           
      COMMON /TEMP/T(    786)                                                                                                       
      COMMON /CAP/C(    258)                                                                                                        
      COMMON /SOURCE/Q(    262)                                                                                                     
      COMMON /COND/G(    1931)                                                                                                      
      COMMON /KONST/K(      1)                                                                                                      
      COMMON /ARRAY/A(    4831)                                                                                                     
      COMMON /PC1/LSQ1(   13416)                                                                                                    
      COMMON /PC2/LSQ2(       1)                                                                                                    
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA                                                                   
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS                                                                              
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR                                                                  
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR,GUI                                                                       
      COMMON /XSPACE/NDIM,NTH,X(   11722)                                                                                           
      COMMON /FIXCON/                                                                                                               
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,                                                                      
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,                                                                      
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,                                                                      
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,                                                                      
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,                                                                      
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,                                                                      
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,                                                                      
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,                                                                      
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,                                                                      
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,                                                                      
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,                                                                      
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,                                                                      
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,                                                                      
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,                                                                      
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,                                                                      
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG,SPARE8,SPARE9,SSTERM                                                                       
      DIMENSION XK(      1),NX(   11722),IA(    4831)                                                                               
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))                                                                            
      LOGICAL ASCI,FLUD,GNRL,CHAR,GUI                                                                                               
      CHARACTER * 14 PRNAME                                                                                                         
      COMMON /PROBNAME/PRNAME                                                                                                       
      RETURN                                                                                                                        
      END                                                                                                                           
      SUBROUTINE RESPAT                                                                                                             
C*************************************************                                                                                  
C    The RESPAT subroutine writes out nodal temperature files                                                                       
C    that can be read directly into MSC.Patran.  These files will                                                                   
C    have the format "nr#.sin" where # represents the output or frame                                                               
C    number.  This routine can be called from within a SINDA input                                                                  
C    file just like a call to TPRINT would be made (typically OUTPUT CALLS                                                          
C    or EXECUTION).                                                                                                                 
C    This routine was generated for  Gaski SINDA                                                                                    
C*************************************************                                                                                  
C      MSC.Software                                                                                                                 
C     Variable Definitions:                                                                                                         
C       H      Contains title specified in first "BCD" title line (54 char)                                                         
C       TITLE1 first line of title information converted into integer                                                               
C       TITLE2 second line of title information converted into integer                                                              
C       TITLE3 third line of title information converted into integer                                                               
C       STIMEN TIMEN variable converted into string format                                                                          
C       NUNIT  is the unit opened for writing nodal result files                                                                    
C       IFRAME is the frame counter for each nodal result file.                                                                     
      CHARACTER*6 H                                                                                                                 
      CHARACTER   STIMEN*20, SFRAME*15                                                                                              
      CHARACTER*80 EROR, FILNAM, STITLE1, STITLE2, STITLE3                                                                          
      INTEGER   MAXNOD, NDMAX, WIDTH                                                                                                
      INTEGER   TITLE1( 80 )                                                                                                        
      INTEGER   TITLE2( 80 )                                                                                                        
      INTEGER   TITLE3( 80 )                                                                                                        
      REAL      DEFMAX                                                                                                              
      COMMON /TITLE/ H( 20 )                                                                                                        
      COMMON /DIMENS/ NND, NNC, NNT, NGL, NGT, NGE, NCC,                                                                            
     + NUC, NCT, NAT, LENA, NSQ1, NSQ2, NPC, NPT, NSQ3,                                                                             
     + NVL, NPM, NTE, NSQ4, NCS, LCS                                                                                                
      COMMON /TEMP/ T( 1 )                                                                                                          
      COMMON /FIXCON/  TIMEN , DTIMEU, TIMEND, CSGFAC, NLOOP ,                                                                      
     + DTMPCA, ITROUT, DTIMEH, DAMPA , DAMPD , ATMPCA, BACKUP,                                                                      
     + TIMEO , TIMEM , DTMPCC, ATMPCC, CSGMIN, OUTPUT, ARLXCA,                                                                      
     + LOOPCT, DTIMEL, DTIMEI, CSGMAX, CSGRAL, CSGRCL, DRLXCA,                                                                      
     + DRLXCC, NLINE , NPAGE , ARLXCC, LSPCS , ENGBAL, BALENG,                                                                      
     + ATSLIM, NCSGMN, NDTMPC, NARLXC, NATMPC, ITEST , JTEST ,                                                                      
     + KTEST , LTEST , MTEST , RTEST , STEST , TTEST , UTEST ,                                                                      
     + VTEST , LAXFAC, SIGMA , TMPZRO, NDRLXC, TDERV , NTDERV,                                                                      
     + BENODE, EBNODE, NODEEB, EXTLIM, NFLOOP, PRLXCA, PRLXCC,                                                                      
     + LOOPFC, GRVCON, PZERO , NCSGMX, NTEST , ATEST , BTEST ,                                                                      
     + CTEST , DTEST , ETEST , FTEST , GTEST , HTEST , OTEST ,                                                                      
     + PTEST , QTEST , WTEST , XTEST , YTEST , ZTEST , NTROSS,                                                                      
     + ISNUNC, NLINPP, LOTEMP, ERRMAX, ERRMIN, SENGIN, DBLPRC,                                                                      
     + MPCNTL, IPCNT1, IPCNT2                                                                                                       
      COMMON /PATRAN/ IFRAME, NUNIT                                                                                                 
C      Assign unit 90 if none is given                                                                                              
      IF( NUNIT .LE. 0 ) THEN                                                                                                       
         NUNIT = 90                                                                                                                 
      ENDIF                                                                                                                         
C      ________________________________________________                                                                             
C      Write the  title data for the nodal results file                                                                             
C      Convert the real TIMEN variable to string                                                                                    
      WRITE( STIMEN , 1 ) TIMEN                                                                                                     
 1    FORMAT( 1PE20.6 )                                                                                                             
C      A title length of BCD 9 will be used allowing for 54 characters                                                              
      STITLE1 = H(1)//H(2)//H(3)//H(4)//H(5)//H(6)//H(7)//H(8)//H(9)                                                                
      STITLE2 = 'MSC.Software - SINDA Model Generated using MSC.Patran'                                                             
      STITLE3 = 'TIME  =  '// STIMEN                                                                                                
      READ( STITLE1, 302 ) ( TITLE1( I ), I=1,80 )                                                                                  
      READ( STITLE2, 302 ) ( TITLE2( I ), I=1,80 )                                                                                  
      READ( STITLE3, 302 ) ( TITLE3( I ), I=1,80 )                                                                                  
302   FORMAT( 80A1 )                                                                                                                
      WRITE( SFRAME, 305 ) IFRAME                                                                                                   
305   FORMAT( I15 )                                                                                                                 
C     MAXNOD, DEFMAX, NDMAXcan be set to zero for thermal results                                                                   
      MAXNOD = 0                                                                                                                    
      DEFMAX = 0                                                                                                                    
      NDMAX = 0                                                                                                                     
C     WIDTH is the number of columns of data ( = 1 for temperatures)                                                                
      WIDTH = 1                                                                                                                     
      EROR = '*** ERROR opening the SINDA input file ***'                                                                           
C     -----------------------                                                                                                       
C     Creating the file name:                                                                                                       
C     -----------------------                                                                                                       
C     NCHAR0 returns the left most character location                                                                               
C     NCHAR2 returns the right most character location                                                                              
      NL = NCHAR0 ( SFRAME, 1, 15 )                                                                                                 
      NR = NCHAR2 ( SFRAME, 1, 15 )                                                                                                 
      NR = MAX( NL, NR )                                                                                                            
      FILNAM = 'nr' // SFRAME(NL:NR) // '.sin'                                                                                      
C     Open & Write the MSC.Patran Nodal Results                                                                                     
      EROR = '**** ERROR **** Unable to open a MSC.Patran results file'                                                             
      NR = NCHAR2 ( FILNAM, 1, 80 )                                                                                                 
      OPEN ( NUNIT, FILE=FILNAM(1:NR), STATUS='NEW', ERR=401 )                                                                      
      WRITE( NUNIT, 302 ) TITLE1                                                                                                    
      WRITE( NUNIT, 303 ) NNT, MAXNOD, DEFMAX, NDMAX, WIDTH                                                                         
303   FORMAT( I9, I9, 1PE15.6, I9, I9 )                                                                                             
      WRITE( NUNIT, 302 ) TITLE2                                                                                                    
      WRITE( NUNIT, 302 ) TITLE3                                                                                                    
      DO I = 1, NNT                                                                                                                 
         WRITE ( NUNIT, 304 ) NA(I), T(I)                                                                                           
      ENDDO                                                                                                                         
304   FORMAT( I8, 1PE13.6 )                                                                                                         
      CLOSE ( NUNIT, STATUS='KEEP' )                                                                                                
      IFRAME = IFRAME +1                                                                                                            
      RETURN                                                                                                                        
401   WRITE( *, * ) '****** ERROR ******'                                                                                           
      WRITE( *, * ) EROR                                                                                                            
      END                                                                                                                           
C#################################################                                                                                  
C     F U N C T I O N    N C H A R 2                                                                                                
C#################################################                                                                                  
      INTEGER FUNCTION NCHAR2( STRING, LEFT, RIGHT )                                                                                
C#################################################                                                                                  
C     This function returns the character position of the rightmost                                                                 
C            non-blank character in the substring STRING(LEFT:RIGHT)                                                                
C            relative to STRING(1:1).  If no non-blank characters are                                                               
C            found, NCHARS returns as zero.                                                                                         
C     ARGUMENTS:                                                                                                                    
C            string        -->  character string being processed.                                                                   
C            left          -->  leftmost character position to be searched.                                                         
C            right         -->  rightmost character position to be searched.                                                        
C     Declare the arguments.                                                                                                        
      CHARACTER*(*) STRING                                                                                                          
      INTEGER LEFT, RIGHT                                                                                                           
C$END                                                                                                                               
C#################################################                                                                                  
C     Declare the local variables.                                                                                                  
      INTEGER I                                                                                                                     
C#################################################                                                                                  
C     Begin searching.                                                                                                              
      DO 1 I=RIGHT,LEFT,-1                                                                                                          
         IF( STRING(I:I) .NE. ' ' ) THEN                                                                                            
             NCHAR2 = I                                                                                                             
             RETURN                                                                                                                 
         ENDIF                                                                                                                      
    1 CONTINUE                                                                                                                      
C#################################################                                                                                  
C     If we have fallen through to here, no non-blank characters were                                                               
C            found.  Set NCHAR2 to 0 and return.                                                                                    
      NCHAR2 = 0                                                                                                                    
C#################################################                                                                                  
      RETURN                                                                                                                        
      END                                                                                                                           
C#################################################                                                                                  
C     F U N C T I O N    N C H A R 0                                                                                                
C#################################################                                                                                  
      INTEGER FUNCTION NCHAR0( STRING, LEFT, RIGHT )                                                                                
C#################################################                                                                                  
C     This function returns the character position of the leftmost                                                                  
C            non-blank character in the substring STRING(LEFT:RIGHT)                                                                
C            relative to STRING(1:1).  If no non-blank characters are                                                               
C            found, NCHARS returns as zero.                                                                                         
C     ARGUMENTS:                                                                                                                    
C            string        -->  character string being processed.                                                                   
C            left          -->  leftmost character position to be searched.                                                         
C            right         -->  rightmost character position to be searched.                                                        
C     Declare the arguments.                                                                                                        
      CHARACTER*(*) STRING                                                                                                          
      INTEGER LEFT, RIGHT                                                                                                           
C$END                                                                                                                               
C#################################################                                                                                  
C     Declare the local variables.                                                                                                  
      INTEGER I                                                                                                                     
C#################################################                                                                                  
C     Begin searching.                                                                                                              
      DO 1 I = LEFT, RIGHT                                                                                                          
         IF( STRING(I:I) .NE. ' ' ) THEN                                                                                            
             NCHAR0 = I                                                                                                             
             RETURN                                                                                                                 
         ENDIF                                                                                                                      
    1 CONTINUE                                                                                                                      
C#################################################                                                                                  
C     If we have fallen through to here, no non-blank characters were                                                               
C            found.  Set NCHAR0 to 1 and return.                                                                                    
      NCHAR0 = 1                                                                                                                    
C#################################################                                                                                  
      RETURN                                                                                                                        
      END                                                                                                                           
      SUBROUTINE VARBLF                                                                                                             
      RETURN                                                                                                                        
      END                                                                                                                           
