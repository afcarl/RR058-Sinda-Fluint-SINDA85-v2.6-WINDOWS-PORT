       PROGRAM ASTAP
C
       PARAMETER(MXNODE= 20000, MXCOND= 100000, MXUARY=  5000)
       PARAMETER(MXTARY=  10000,MXMODS= 100,  MXTOKN=  80)
       PARAMETER(MXCNTR=  76,MXUCON= 1000,  MXNCON= 5000)
       PARAMETER(MXFRES= 196,MXFILE=  17)
       PARAMETER(MXFLID=  30,MXMODF= 100)
        COMMON /FLUDAT/ FI(90),FDATA(1700)
        COMMON /FLUARY/ NFPARY,FPARAY(        1)
        INTEGER FI
       COMMON /BLANK1/  NDIM1    , NTH1  , EXTRA1(MXNODE)
       COMMON /BLANK2/  NDIM2    , NTH2  , EXTRA2(MXNODE)
       COMMON /BLANK3/  NDIM3    , NTH3  , EXTRA3(MXNODE)
       COMMON /BLANK4/  NDIM4    , NTH4  , EXTRA4(MXNODE)
       COMMON /BLANK5/  NDIM5    , NTH5  , EXTRA5(MXNODE)
       COMMON /BLANK6/  NDIM6    , NTH6  , EXTRA6(MXNODE)
C
       COMMON /BLDAT  /   NSEQ(MXMODS)
       COMMON /BLDAT1 /   FIG
       CHARACTER*8        FIG
C
       COMMON /CCONST /   MMODS   , NNOD    , ABSZRO  , SIGMA
     +                  , NMACT   , DATE    , TIMDY   , LINECT
     +                  , PAGECT  , DTIMES  , TIMEN   , TIMEO
     +                  , TIMEM   , TIMEND  , NLOOPS  , LOOPCT
     +                  , MLINE
       INTEGER            LINECT  , PAGECT
C
       COMMON /DIMENS /   NND     , NNA     , NNH     , NNT
     +                  , NGT     , NCON    , NARYP   , NSEQU
     +                  , NSEQB   , NVC     , NVG
C
       COMMON /HEAD   /   NVRSN   , LMDTE   , NDTE    , NTME
     +                  , NJOB    , MDLNNM  , NTITLE  , ITITLE
       CHARACTER*6        NVRSN   , LMDTE   , NDTE    , NTME
       CHARACTER*8        NJOB    , MDLNNM
       CHARACTER*66       NTITLE  , ITITLE
C
       COMMON /TAPES  /   NIN     , NOUT    , NCOND   , NNODE
     +                  , NOSORC  , NOARRY  , NOCARY  , NOTARY
     +                  , NVCD    , NVGD    , NOUSER  , NOCNTR
     +                  , NOUSE   , NETQF   , NXTRA1  , NXTRA2
     +                  , NTSV    , NUSER1  , NUSER2  , NRSI  
     +                  , NRSO    , NPARAM  , NFLOW   , NFLOW2
     +                  , NFLOW3  , NOOPT   , NSCR1   , NSCR2 
     +                  , NSCR3   , NSCR4   , NQMP    , NFPROP
C
       COMMON /BEGEND /   IBEGC(MXMODS) , IENDC(MXMODS)
     +                  , IBEGG(MXMODS) , IENDG(MXMODS)
C
       COMMON /USER/  USRDAT(      26)
       COMMON /CNTRL1 /
     +       DTIMEU(MXMODS) , DTIMEI(MXMODS) , DTIMEL(MXMODS)
     +     , DTIMEH(MXMODS) , OUTPUT(MXMODS) , DTMPCC(MXMODS)
     +     , DTMPCA(MXMODS) , NDTMPN(MXMODS) , ITHOLD(MXMODS)
     +     , ATMPCA(MXMODS) , NATMPN(MXMODS) , DRLXCA(MXMODS)
     +     , NDRLXN(MXMODS) , ARLXCA(MXMODS) , ARLXCC(MXMODS)
     +     , NARLXN(MXMODS) , CSGMIN(MXMODS) , CSGFAC(MXMODS)
     +     , CSGMAX(MXMODS) , NCSGMN(MXMODS) , NCGMAN(MXMODS)
     +     , EBALSA(MXMODS) , EBALNA(MXMODS) , EBALSC(MXMODS)
     +     , EBALNC(MXMODS) , NEBALN(MXMODS) , NLOOPT(MXMODS)
     +     , EXTLIM(MXMODS) , ITERXT(MXMODS) , ITEROT(MXMODS)
     +     , BACKUP(MXMODS) , OPEITR(MXMODS) , DRLXCC(MXMODS)
     +     , ATMPCC(MXMODS) , ESUMIS(MXMODS) , ESUMOS(MXMODS)
       INTEGER            OPEITR
C
       COMMON /CNTRL2 /   NDTMPC(MXMODS) , NATMPC(MXMODS)
     +     , NDRLXC(MXMODS) , NARLXC(MXMODS) , NCSGMC(MXMODS)
     +     , NCGMAC(MXMODS) , NEBALC(MXMODS)
       CHARACTER*8        NDTMPC      , NATMPC
     +                  , NDRLXC      , NARLXC      , NCSGMC
     +                  , NCGMAC      , NEBALC
C
       COMMON /MODNM1 /   MNAMES(MXMODS)
       COMMON /MODNM2 /   MLLINK(MXMODS)
       COMMON /MODNM3 /   MRLINK(MXMODS)
       COMMON /MODNM4 /   MACT(MXMODS)
       COMMON /MODNM5 /   MROOT
       CHARACTER*8        MNAMES
C
       COMMON /TITL   /   TITLE
       CHARACTER*120      TITLE
C
       COMMON /NUMCON /   K(       1)
       COMMON /ARAYS  /   A(       1)
       COMMON /CARAYS /   UCA(       1)
       DIMENSION          XK(1)   , NA(1)
       CHARACTER*128 UCA
       EQUIVALENCE (XK(1), K(1))
       EQUIVALENCE (NA(1), A(1))
C
       COMMON /CDAT1  /   PT(       7)
       COMMON /CDAT2  /   PG(       7)
       COMMON /CDAT3  /   NLIN(       4)
       COMMON /CDAT4  /   NRAD(       4)
       COMMON /CDAT7  /   NCTOT(       1)
       COMMON /CDAT8  /   NGSTRT(       1)
       COMMON /CDAT9  /   G(       3)
       INTEGER            PT      , PG      , NLIN    , NRAD
       INTEGER            NCTOT   , NGSTRT
C
       COMMON /VARC   /   VC(       1)
C
       COMMON /VARG   /   VG(       1)
C
       COMMON /NDAT1  /   T(       4)
       COMMON /NDAT2  /   C(       4)
       COMMON /NDAT3  /   Q(       4)
       COMMON /NDAT4  /   NMOD(       1)
       COMMON /NDAT5  /   NSTRT(       1)
       COMMON /NDAT6  /   NMDIF(       1)
       COMMON /NDAT7  /   NMARI(       1)
       COMMON /NDAT8  /   NMHT(       1)
       COMMON /NDAT9  /   NMBD(       1)
       COMMON /NDAT10 /   NDNAM(       4)
       COMMON /NDAT11 /   NDINT(       4)
       COMMON /NDAT12 /   TOLD(       4)
       COMMON /NDAT13 /   QOLD(       4)
       CHARACTER*8        NMOD    ,NDNAM
C
       COMMON /DATQVT/ QVTDAT(8,       1)
       COMMON /MODQVT/ MODLQS(2,       1)
       COMMON /TARY1 / TARY(     1)
       COMMON /TARY2 / NUMREC(     1)
       COMMON /TARY3 / NPNT(2,     1)
       COMMON /TARY4 / NTREC(     1)
       COMMON /TARY5 / TIME1(     1)
       COMMON /TARY6 / TIMEH(     1)
       COMMON /TARY7 / TARYL(     1)
       COMMON /TARY8 / TIME1L(     1)
       COMMON /TARY9/ TIMEHL(     1)
        COMMON /SAVCPA/ HAVCPA
        COMMON /SAVCP1/ CPAFID(2,    1)
        COMMON /SAVCP2/ CPIARY(    1)
        INTEGER HAVCPA,CPAFID
        REAL CPIARY
       EXTERNAL BLKDAT
 
       include '../prepro/data.fi'
 
 
       projectpath1 = 'bar'
 
       call params_default
       call filenames_default
 
       CALL INITAL
       CALL READIN
       CALL OPER
C
       call filenames_cleanup_prep
C
       call filenames_cleanup
C
       STOP
       END
       SUBROUTINE SOR001
       PARAMETER(MXNODE= 20000, MXCOND= 100000, MXUARY=  5000)
       PARAMETER(MXTARY=  10000,MXMODS=100,  MXTOKN=80)
       PARAMETER(MXCNTR=76,MXUCON= 1000,  MXNCON= 5000)
       PARAMETER(MXFRES=196,MXFILE=17,  MXFLID= 30)
       PARAMETER(MXMODF=100)
        COMMON /FLUDAT/ FI(90),FDATA(1700)
        INTEGER FI
        DOUBLE PRECISION VPS,VPGMAX
       COMMON /CNTRL1 /
     +       DTIMEU(MXMODS) , DTIMEI(MXMODS) , DTIMEL(MXMODS)
     +     , DTIMEH(MXMODS) , OUTPUT(MXMODS) , DTMPCC(MXMODS)
     +     , DTMPCA(MXMODS) , NDTMPN(MXMODS) , ITHOLD(MXMODS)
     +     , ATMPCA(MXMODS) , NATMPN(MXMODS) , DRLXCA(MXMODS)
     +     , NDRLXN(MXMODS) , ARLXCA(MXMODS) , ARLXCC(MXMODS)
     +     , NARLXN(MXMODS) , CSGMIN(MXMODS) , CSGFAC(MXMODS)
     +     , CSGMAX(MXMODS) , NCSGMN(MXMODS) , NCGMAN(MXMODS)
     +     , EBALSA(MXMODS) , EBALNA(MXMODS) , EBALSC(MXMODS)
     +     , EBALNC(MXMODS) , NEBALN(MXMODS) , NLOOPT(MXMODS)
     +     , EXTLIM(MXMODS) , ITERXT(MXMODS) , ITEROT(MXMODS)
     +     , BACKUP(MXMODS) , OPEITR(MXMODS) , DRLXCC(MXMODS)
     +     , ATMPCC(MXMODS) , ESUMIS(MXMODS) , ESUMOS(MXMODS)
       INTEGER            OPEITR
C
       COMMON /CNTRL2 /   NDTMPC(MXMODS) , NATMPC(MXMODS)
     +     , NDRLXC(MXMODS) , NARLXC(MXMODS) , NCSGMC(MXMODS)
     +     , NCGMAC(MXMODS) , NEBALC(MXMODS)
       CHARACTER*8        NDTMPC      , NATMPC
     +                  , NDRLXC      , NARLXC      , NCSGMC
     +                  , NCGMAC      , NEBALC
C
       COMMON /NDAT1  /   T(       4)
       COMMON /NDAT2  /   C(       4)
       COMMON /NDAT3  /   Q(       4)
       COMMON /NDAT4  /   NMOD(       1)
       COMMON /NDAT5  /   NSTRT(       1)
       COMMON /NDAT6  /   NMDIF(       1)
       COMMON /NDAT7  /   NMARI(       1)
       COMMON /NDAT8  /   NMHT(       1)
       COMMON /NDAT9  /   NMBD(       1)
       COMMON /NDAT10 /   NDNAM(       4)
       COMMON /NDAT11 /   NDINT(       4)
       COMMON /NDAT12 /   TOLD(       4)
       CHARACTER*8        NMOD    ,NDNAM
        COMMON /TAPES  /   NIN    ,NOUT   ,NCOND  ,NNODE
     +                ,NOSORC ,NOARRY ,NOCARY ,NOTARY
     +                ,NVCD   ,NVGD   ,NOUSER ,NOCNTR
     +                ,NOUSE  ,NETQF  ,NXTRA1 ,NXTRA2
     +                ,NTSV   ,NUSER1 ,NUSER2 ,NRSI
     +                ,NRSO   ,NPARAM ,NFLOW  ,NFLOW2
     +                ,NFLOW3 ,NOOPT  ,NSCR1  ,NSCR2
     +                ,NSCR3  ,NSCR4  ,NQMP   ,NFPROP
       COMMON /CDAT1  /   PT(       7)
       COMMON /CDAT2  /   PG(       7)
       COMMON /CDAT3  /   NLIN(       4)
       COMMON /CDAT4  /   NRAD(       4)
       COMMON /CDAT5  /   NFLO(      13)
       COMMON /CDAT7  /   NCTOT(       1)
       COMMON /CDAT8  /   NGSTRT(       1)
       COMMON /CDAT9  /   G(       3)
       INTEGER            PT      , PG      , NLIN    , NRAD
       INTEGER            NCTOT   , NGSTRT
            COMMON /CCONST /   MMODS  ,NNOD   ,ABSZRO ,SIGMA  ,NMACT
     +                  ,DATE   ,TIMDY  ,LINECT ,PAGECT ,DTIMES
     +                  ,TIMEN  ,TIMEO  ,TIMEM  ,TIMEND ,NLOOPS
     +                  ,LOOPCT ,MLINE
        INTEGER LINECT,PAGECT
        COMMON /NUMCON /   K(       1)
       COMMON /ARAYS  /   A(       1)
       COMMON /CARAYS /   UCA(       1)
       DIMENSION          XK(1)   , NA(1)
       CHARACTER*128 UCA
       EQUIVALENCE (K(1), XK(1))
       EQUIVALENCE (A(1), NA(1))
      COMMON/USER/ ATEST,BTEST,CTEST,DTEST,ETEST,FTEST,GTEST,HTEST,ITEST
     +,JTEST,KTEST,LTEST,MTEST,NTEST,OTEST,PTEST,QTEST,RTEST,STEST,TTEST
     +,UTEST,VTEST,WTEST,XTEST,YTEST,ZTEST
       EXTERNAL CTF,CTK,FTC,FTK,FTR,KTC,KTF,RTF
       EXTERNAL TPRINT,QPRINT,CPRINT,GPRINT
       COMMON /SOLTYP/ NSOL
        RETURN
        END
       SUBROUTINE VA1001
       CALL QVTEMP('SUB1    ')
       CALL GVTEMP('SUB1    ')
       CALL CVTEMP('SUB1    ')
       RETURN
       END
       SUBROUTINE VA2001
       RETURN
       END
       SUBROUTINE VA0001
       CALL QVTIME('SUB1    ')
       CALL GVTIME('SUB1    ')
!       CALL CVTIME('SUB1    ')
       RETURN
       END
       SUBROUTINE OPER
       PARAMETER(MXNODE= 20000, MXCOND= 100000, MXUARY=  5000)
       PARAMETER(MXTARY=  10000,MXMODS=100,  MXTOKN=80)
       PARAMETER(MXCNTR=76,MXUCON= 1000,  MXNCON= 5000)
       PARAMETER(MXFRES=196,MXFILE=17,  MXFLID= 30)
       PARAMETER(MXMODF=100)
        COMMON /FLUDAT/ FI(90),FDATA(1700)
        INTEGER FI
        DOUBLE PRECISION VPS,VPGMAX
       COMMON /CNTRL1 /
     +       DTIMEU(MXMODS) , DTIMEI(MXMODS) , DTIMEL(MXMODS)
     +     , DTIMEH(MXMODS) , OUTPUT(MXMODS) , DTMPCC(MXMODS)
     +     , DTMPCA(MXMODS) , NDTMPN(MXMODS) , ITHOLD(MXMODS)
     +     , ATMPCA(MXMODS) , NATMPN(MXMODS) , DRLXCA(MXMODS)
     +     , NDRLXN(MXMODS) , ARLXCA(MXMODS) , ARLXCC(MXMODS)
     +     , NARLXN(MXMODS) , CSGMIN(MXMODS) , CSGFAC(MXMODS)
     +     , CSGMAX(MXMODS) , NCSGMN(MXMODS) , NCGMAN(MXMODS)
     +     , EBALSA(MXMODS) , EBALNA(MXMODS) , EBALSC(MXMODS)
     +     , EBALNC(MXMODS) , NEBALN(MXMODS) , NLOOPT(MXMODS)
     +     , EXTLIM(MXMODS) , ITERXT(MXMODS) , ITEROT(MXMODS)
     +     , BACKUP(MXMODS) , OPEITR(MXMODS) , DRLXCC(MXMODS)
     +     , ATMPCC(MXMODS) , ESUMIS(MXMODS) , ESUMOS(MXMODS)
       INTEGER            OPEITR
C
       COMMON /CNTRL2 /   NDTMPC(MXMODS) , NATMPC(MXMODS)
     +     , NDRLXC(MXMODS) , NARLXC(MXMODS) , NCSGMC(MXMODS)
     +     , NCGMAC(MXMODS) , NEBALC(MXMODS)
       CHARACTER*8        NDTMPC      , NATMPC
     +                  , NDRLXC      , NARLXC      , NCSGMC
     +                  , NCGMAC      , NEBALC
C
       COMMON /NDAT1  /   T(       4)
       COMMON /NDAT2  /   C(       4)
       COMMON /NDAT3  /   Q(       4)
       COMMON /NDAT4  /   NMOD(       1)
       COMMON /NDAT5  /   NSTRT(       1)
       COMMON /NDAT6  /   NMDIF(       1)
       COMMON /NDAT7  /   NMARI(       1)
       COMMON /NDAT8  /   NMHT(       1)
       COMMON /NDAT9  /   NMBD(       1)
       COMMON /NDAT10 /   NDNAM(       4)
       COMMON /NDAT11 /   NDINT(       4)
       COMMON /NDAT12 /   TOLD(       4)
       CHARACTER*8        NMOD    ,NDNAM
        COMMON /TAPES  /   NIN    ,NOUT   ,NCOND  ,NNODE
     +                ,NOSORC ,NOARRY ,NOCARY ,NOTARY
     +                ,NVCD   ,NVGD   ,NOUSER ,NOCNTR
     +                ,NOUSE  ,NETQF  ,NXTRA1 ,NXTRA2
     +                ,NTSV   ,NUSER1 ,NUSER2 ,NRSI
     +                ,NRSO   ,NPARAM ,NFLOW  ,NFLOW2
     +                ,NFLOW3 ,NOOPT  ,NSCR1  ,NSCR2
     +                ,NSCR3  ,NSCR4  ,NQMP   ,NFPROP
       COMMON /CDAT1  /   PT(       7)
       COMMON /CDAT2  /   PG(       7)
       COMMON /CDAT3  /   NLIN(       4)
       COMMON /CDAT4  /   NRAD(       4)
       COMMON /CDAT5  /   NFLO(      13)
       COMMON /CDAT7  /   NCTOT(       1)
       COMMON /CDAT8  /   NGSTRT(       1)
       COMMON /CDAT9  /   G(       3)
       INTEGER            PT      , PG      , NLIN    , NRAD
       INTEGER            NCTOT   , NGSTRT
            COMMON /CCONST /   MMODS  ,NNOD   ,ABSZRO ,SIGMA  ,NMACT
     +                  ,DATE   ,TIMDY  ,LINECT ,PAGECT ,DTIMES
     +                  ,TIMEN  ,TIMEO  ,TIMEM  ,TIMEND ,NLOOPS
     +                  ,LOOPCT ,MLINE
        INTEGER LINECT,PAGECT
        COMMON /NUMCON /   K(       1)
       COMMON /ARAYS  /   A(       1)
       COMMON /CARAYS /   UCA(       1)
       DIMENSION          XK(1)   , NA(1)
       CHARACTER*128 UCA
       EQUIVALENCE (K(1), XK(1))
       EQUIVALENCE (A(1), NA(1))
      COMMON/USER/ ATEST,BTEST,CTEST,DTEST,ETEST,FTEST,GTEST,HTEST,ITEST
     +,JTEST,KTEST,LTEST,MTEST,NTEST,OTEST,PTEST,QTEST,RTEST,STEST,TTEST
     +,UTEST,VTEST,WTEST,XTEST,YTEST,ZTEST
       EXTERNAL CTF,CTK,FTC,FTK,FTR,KTC,KTF,RTF
       EXTERNAL TPRINT,QPRINT,CPRINT,GPRINT
       COMMON /SOLTYP/ NSOL
       CHARACTER*8 NBNAM( 100)
       CHARACTER*8 NFNAM( 100)
       NBNAM(  1) = 'SUB1    '
       CALL BUILD ('TEST    ',  1, NBNAM)
        CALL FWDBCK
       RETURN
       END
       SUBROUTINE OUT001
       PARAMETER(MXNODE= 20000, MXCOND= 100000, MXUARY=  5000)
       PARAMETER(MXTARY=  10000,MXMODS=100,  MXTOKN=80)
       PARAMETER(MXCNTR=76,MXUCON= 1000,  MXNCON= 5000)
       PARAMETER(MXFRES=196,MXFILE=17,  MXFLID= 30)
       PARAMETER(MXMODF=100)
        COMMON /FLUDAT/ FI(90),FDATA(1700)
        INTEGER FI
        DOUBLE PRECISION VPS,VPGMAX
       COMMON /CNTRL1 /
     +       DTIMEU(MXMODS) , DTIMEI(MXMODS) , DTIMEL(MXMODS)
     +     , DTIMEH(MXMODS) , OUTPUT(MXMODS) , DTMPCC(MXMODS)
     +     , DTMPCA(MXMODS) , NDTMPN(MXMODS) , ITHOLD(MXMODS)
     +     , ATMPCA(MXMODS) , NATMPN(MXMODS) , DRLXCA(MXMODS)
     +     , NDRLXN(MXMODS) , ARLXCA(MXMODS) , ARLXCC(MXMODS)
     +     , NARLXN(MXMODS) , CSGMIN(MXMODS) , CSGFAC(MXMODS)
     +     , CSGMAX(MXMODS) , NCSGMN(MXMODS) , NCGMAN(MXMODS)
     +     , EBALSA(MXMODS) , EBALNA(MXMODS) , EBALSC(MXMODS)
     +     , EBALNC(MXMODS) , NEBALN(MXMODS) , NLOOPT(MXMODS)
     +     , EXTLIM(MXMODS) , ITERXT(MXMODS) , ITEROT(MXMODS)
     +     , BACKUP(MXMODS) , OPEITR(MXMODS) , DRLXCC(MXMODS)
     +     , ATMPCC(MXMODS) , ESUMIS(MXMODS) , ESUMOS(MXMODS)
       INTEGER            OPEITR
C
       COMMON /CNTRL2 /   NDTMPC(MXMODS) , NATMPC(MXMODS)
     +     , NDRLXC(MXMODS) , NARLXC(MXMODS) , NCSGMC(MXMODS)
     +     , NCGMAC(MXMODS) , NEBALC(MXMODS)
       CHARACTER*8        NDTMPC      , NATMPC
     +                  , NDRLXC      , NARLXC      , NCSGMC
     +                  , NCGMAC      , NEBALC
C
       COMMON /NDAT1  /   T(       4)
       COMMON /NDAT2  /   C(       4)
       COMMON /NDAT3  /   Q(       4)
       COMMON /NDAT4  /   NMOD(       1)
       COMMON /NDAT5  /   NSTRT(       1)
       COMMON /NDAT6  /   NMDIF(       1)
       COMMON /NDAT7  /   NMARI(       1)
       COMMON /NDAT8  /   NMHT(       1)
       COMMON /NDAT9  /   NMBD(       1)
       COMMON /NDAT10 /   NDNAM(       4)
       COMMON /NDAT11 /   NDINT(       4)
       COMMON /NDAT12 /   TOLD(       4)
       CHARACTER*8        NMOD    ,NDNAM
        COMMON /TAPES  /   NIN    ,NOUT   ,NCOND  ,NNODE
     +                ,NOSORC ,NOARRY ,NOCARY ,NOTARY
     +                ,NVCD   ,NVGD   ,NOUSER ,NOCNTR
     +                ,NOUSE  ,NETQF  ,NXTRA1 ,NXTRA2
     +                ,NTSV   ,NUSER1 ,NUSER2 ,NRSI
     +                ,NRSO   ,NPARAM ,NFLOW  ,NFLOW2
     +                ,NFLOW3 ,NOOPT  ,NSCR1  ,NSCR2
     +                ,NSCR3  ,NSCR4  ,NQMP   ,NFPROP
       COMMON /CDAT1  /   PT(       7)
       COMMON /CDAT2  /   PG(       7)
       COMMON /CDAT3  /   NLIN(       4)
       COMMON /CDAT4  /   NRAD(       4)
       COMMON /CDAT5  /   NFLO(      13)
       COMMON /CDAT7  /   NCTOT(       1)
       COMMON /CDAT8  /   NGSTRT(       1)
       COMMON /CDAT9  /   G(       3)
       INTEGER            PT      , PG      , NLIN    , NRAD
       INTEGER            NCTOT   , NGSTRT
            COMMON /CCONST /   MMODS  ,NNOD   ,ABSZRO ,SIGMA  ,NMACT
     +                  ,DATE   ,TIMDY  ,LINECT ,PAGECT ,DTIMES
     +                  ,TIMEN  ,TIMEO  ,TIMEM  ,TIMEND ,NLOOPS
     +                  ,LOOPCT ,MLINE
        INTEGER LINECT,PAGECT
        COMMON /NUMCON /   K(       1)
       COMMON /ARAYS  /   A(       1)
       COMMON /CARAYS /   UCA(       1)
       DIMENSION          XK(1)   , NA(1)
       CHARACTER*128 UCA
       EQUIVALENCE (K(1), XK(1))
       EQUIVALENCE (A(1), NA(1))
      COMMON/USER/ ATEST,BTEST,CTEST,DTEST,ETEST,FTEST,GTEST,HTEST,ITEST
     +,JTEST,KTEST,LTEST,MTEST,NTEST,OTEST,PTEST,QTEST,RTEST,STEST,TTEST
     +,UTEST,VTEST,WTEST,XTEST,YTEST,ZTEST
       EXTERNAL CTF,CTK,FTC,FTK,FTR,KTC,KTF,RTF
       EXTERNAL TPRINT,QPRINT,CPRINT,GPRINT
       COMMON /SOLTYP/ NSOL
        IF ( T(     2) .GE. 200.00000 ) THEN
        CALL TPRINT( 'SUB1' )
        TIMEND = TIMEN
        ENDIF
       RETURN
       END
       SUBROUTINE FLOGI0(MODNO)
       RETURN
       END
       SUBROUTINE FLOGI1(MODNO)
       RETURN
       END
       SUBROUTINE FLOGI2(MODNO)
       RETURN
       END
       SUBROUTINE VARBL0(MODNO)
       IF(MODNO .EQ. 1)   THEN
                CALL VA0001
       END IF
       RETURN
       END
       SUBROUTINE VARBL1(MODNO)
       IF(MODNO .EQ. 1)   THEN
                CALL VA1001
       END IF
       RETURN
       END
       SUBROUTINE VARBL2(MODNO)
       IF(MODNO .EQ. 1)   THEN
                CALL VA2001
       END IF
       RETURN
       END
       SUBROUTINE QVTEMP(MODNAM)
       CHARACTER*(*) MODNAM
       PARAMETER(MXMODS=100)
       COMMON /MODNM1 /   MNAMES(MXMODS)
       COMMON /MODNM2 /   MLLINK(MXMODS)
       COMMON /MODNM3 /   MRLINK(MXMODS)
       COMMON /MODNM4 /   MACT(MXMODS)
       COMMON /MODNM5 /   MROOT
       CHARACTER*8 MNAMES
       INTEGER SERCHN
       MODNO = SERCHN(MODNAM,MROOT,MLLINK,MRLINK,MACT,MNAMES)
       IF(MODNO .EQ. 1)   THEN
                CALL SOR001
       END IF
       RETURN
       END
       SUBROUTINE OUTCAL(MODNO)
       IF(MODNO .EQ. 1)   THEN
                CALL OUT001
       END IF
       RETURN
       END
       SUBROUTINE FASTFS(A)
       DIMENSION A(1)
       RETURN
       END
       SUBROUTINE FLUSET(D,A)
       DIMENSION A(1)
       RETURN
       END
       LOGICAL FUNCTION RDOFLU(D,I)
       RDOFLU = .FALSE.
       RETURN
       END
       SUBROUTINE FLUSLV(D,D1,A,A1,*)
       DIMENSION A(1),A1(1)
       RETURN
       END
       SUBROUTINE PREFLO(A,A1,A2,A3)
       DIMENSION A(1),A1(1),A2(1),A3(1)
       RETURN
       END
       SUBROUTINE FLUINF(A)
       DIMENSION A(1)
       RETURN
       END
       SUBROUTINE UPDATF(A)
       DIMENSION A(1)
       RETURN
       END
       SUBROUTINE FCNVRG(A,A1,D,A2,A3)
       DIMENSION A(1),A1(1),A2(1),A3(1)
       RETURN
       END
