HEADER FPROP DATA, 6290, ENG, 0.0
C
C   THIS FLUID DESCRIPTION WAS CREATED BY RAPPR FOR FLUID  290
C   AND IS ACCURATE FOR TEMPERATURES NEAR   539.67
C   MAXIMUM ERROR IN LIQUID PROPERTIES = 25.00 PERCENT
C   MAXIMUM ERROR IN VAPOR PROPERTIES  = 25.00 PERCENT
C   MAXIMUM LIQUID (SATURATION) TEMP. =   613.92371    
C
         TCRIT =     665.95001    ,    PCRIT =     617.40002    
         TGMAX =     683.11926    ,    PGMAX =     355.00684    
         TMIN =      450.98657    
C
VINIT
       DOUBLE PRECISION POLD
       SAVE TOLD,TTOC,A,B,BINV,C,CINV,TREF,DLIQ,DLDT
       SAVE CPVAP,CPVDT,REFF,BC,POLD,TTOCX,TSAT
       DATA A,B,BINV/  11.588919    , -8576.3330    ,-1.16599949E-04/
       DATA C,CINV,TREF/ -1.1391749    ,-0.87782830    ,  539.67004    /
       DATA DLIQ,DLDT/  30.704151    ,-5.43199517E-02/
       DATA CPVAP,CPVDT,TOLD,TSAT/ 0.51817322    , 1.92324433E-03,2*0.0/
       DATA REFF,BC,POLD/ 0.19677515    ,  9769.9434    ,0.0D0/
C
VSV
       V      = REFF*T/SNGL(P)
C
VDL
       D      = DLIQ + (T-TREF)*DLDT
C
VCPV
       CP     = CPVAP + (T-TREF)*CPVDT
C
VTAV2
       T      = TLOOKS(H,CPVAP,CPVDT,TREF,0.0,0.0)
       V      = REFF*T/SNGL(P)
C
VH
       H      = HINTS(CPVAP,CPVDT,TREF,0.0,T)
C
VTS
       IF(P .NE. POLD)THEN
            TTOCX  = (ALOG(SNGL(P))-A)*BINV
            TSAT   = TTOCX**CINV
            POLD   = P
       ENDIF
       T      = TSAT
C
VDPDT
       IF(P .NE. POLD)THEN
            TTOCX  = (ALOG(SNGL(P))-A)*BINV
            TSAT   = TTOCX**CINV
            POLD   = P
       ENDIF
       DPDT   = BC*SNGL(P)*TTOCX/TSAT
C
VHFG
       IF(T .NE. TOLD)THEN
            IF(T .EQ. TSAT)THEN
                 TTOC   = TTOCX
                 TOLD   = TSAT
            ELSE
                 TTOC   = T**C
                 TOLD   = T
            ENDIF
       ENDIF
       VLIQ   = 1.0/(DLIQ + (T-TREF)*DLDT) 
       HFG    = BC*SNGL(P)*(V-VLIQ)*TTOC* 0.18504094    
C
VPS
       IF(T .NE. TOLD)THEN
            IF(T .EQ. TSAT)THEN
                 TTOC   = TTOCX
                 TOLD   = TSAT
            ELSE
                 TTOC   = T**C
                 TOLD   = T
            ENDIF
       ENDIF
       P      = DBLE(EXP(A+B*TTOC)) 
C
VDPDTT
       IF(T .NE. TOLD)THEN
            IF(T .EQ. TSAT)THEN
                 TTOC   = TTOCX
                 TOLD   = TSAT
            ELSE
                 TTOC   = T**C
                 TOLD   = T
            ENDIF
       ENDIF
       DPDT   = BC*EXP(A+B*TTOC)*TTOC/T 
C
VCONDF
       COND   =  5.55987470E-02 + (T-TREF)*(-1.53802204E-04)
C
VVISCF
       VISC   =  0.25264844     + (T-TREF)*(-1.49023009E-03)
C
VCONDV
       COND   =  1.04732616E-02 + (T-TREF)*( 3.51756134E-05)
C
VVISCV
       VISC   =  1.99820325E-02 + (T-TREF)*( 3.59671430E-05)
C
VST
       ST     =  4.64590266E-04 + (T-TREF)*(-4.64570121E-06)
