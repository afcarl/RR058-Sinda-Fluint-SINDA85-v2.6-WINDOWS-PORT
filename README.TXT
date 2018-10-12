

=========================================
SINDA/FLUINT (SINDA85) v2.6 WINDOWS-PORT:
=========================================

SINDA/FLUINT Systems Improved Numerical Differencing Analyzer and Fluid Integrator (SINDA85) version 2.6. 

The Systems Integrated Numerical Differencing Analyzer — 1985 Version (formerly called SINDA '85) is an upgraded version of the SINDA program. This new version consists of an entirely new preprocessor and a processor library that is much simpler than its predecessor in terms of the number of network solution routines available to the user. Major improvements over older SINDA codes include sub models and a comprehensive simultaneous fluid flow analysis capability: FLUINT.

SINDA/FLUINT has been well-received since its first introduction in 1985, having effective supplanted other Inversions, becoming a bestseller and a NASA Space Act Award winner in the process. Originally intended to satisfy the ever-changing modeling needs of the spacecraft and launch vehicle thermal community, the program has diffused into other industries such as nuclear, aircraft, WAC, and automotive, and into other specialties and subsystems such as propulsion and environmental control.

SINDA is intended primarily for analyzing thermal systems represented in electrical analog, lumped parameter form, although its use may be extended to include other classes of physical systems which can be modeled in this form.


1. Pre-Requisits/Reference:

1a. Compaq Visual Fortran v. 6.6C


2. Documentation:

2a. User's Manual located in "./sindaf_documentation/"


3. Example problems are located in "./examples/":


4. Example problem: "Aquaduct" located in "./examples/aquaduct/":
4a. INPUT FILES: "AQUADUCT.INP" & "AQUADUCT.INC".

==================================
START: Input File: "AQUADUCT.INP"
==================================
HEADER OPTIONS DATA
TITLE FLUINT SAMPLE PROBLEM 2 - SIPHON IN A ROMAN AQUADUCT
	OUTPUT	= aquaduct.out
	USER1	= snowmelt.out
	USER2   = earthquake.out
C
HEADER FPROP DATA,9718,ENG,-460.0
C
C MOST COMPLETE LIQUID WATER DESCRIPTION
C
	TMIN	= 32.0,	TMAX	= 706.
AT,V,	32.0,4.28,	40.0,3.69,	50.0,3.12,	60.0,2.68,	
	70.0,2.32,	80.0,2.03,	90.0,1.79,	100.0,1.6,	
	120.0,1.30,	140.0,1.093,	160.0,0.938,	180.0,0.813,	
	200.0,0.717,	220.0,0.638,	240.0,0.574,	260.0,0.521,	
	280.0,0.476,	300.0,0.439,	320.0,0.406,	340.0,0.379,	
	360.0,0.355,	400.0,0.315,	500.0,0.251,	600.0,0.200,	
	700.0,0.121,	706.0,0.101

... <content removed> ...

HEADER OUTPUT CALLS, PIPE1
C
	IF(ITRAN .EQ. 1)THEN
		WRITE(NUSER1,10)TIMEN,TL99,PIPE2.TL99,PIPE3.TL99,
     .		PIPE4.TL99,PIPE5.TL99,PIPE6.TL99,PIPE7.TL99,PIPE8.TL99
	ELSEIF(ITRAN .EQ. 2)THEN
		WRITE(NUSER2,10)TIMEN,FR99,PIPE2.FR99,PIPE3.FR99,
     .		PIPE4.FR99,PIPE5.FR99,PIPE6.FR99,PIPE7.FR99,PIPE8.FR99
	ENDIF
10	FORMAT(1X,1PG13.6,8G12.3)
END OF DATA
==================================
END: Input File: "AQUADUCT.INP"
==================================


4b. Pre-processing: Run "./prepro_w_utility/Debug/prepro_w_utility.exe"

4c. OUTPUT FILES: <copy located in "./examples/aquaduct/prepro_output/*.*"

4d. Pre-processor creates "aquaduct.F"


4e. Open Workspace file: "aquaduct.dsw" in Compaq Visual Fortran GUI:

4f. Build executable: "aquaduct.exe" located in "./examples/aquaduct/Debug/"

4g. OUTPUT FILES: "aquaduct.out", "earthquake.out", & "snowmelt.out".


==================================
START: Output File: "AQUADUCT.OUT"
==================================
                            SYSTEMS IMPROVED NUMERICAL DIFFERENCING ANALYZER '85 (SINDA '85)               PAGE     2

 MODEL = SINDA85                  FLUINT SAMPLE PROBLEM 2 - SIPHON IN A ROMAN AQUADUCT       
                                                                   


  BUILDING FLOW MODEL AQUA     WITH SUBMODEL(S) - 
          PIPE1   , PIPE2   , PIPE3   , PIPE4   , PIPE5   , PIPE6   , PIPE7   , PIPE8   , 

  SUBMODEL NAME = WALL5   


   ARLXCA =  1.000000E-02 ARLXCC =   1.01000        ATMPCA =  1.000000E+30    ATMPCC =   0.00000        BACKUP =   0.00000    
   CSGFAC =   0.00000     CSGMAX =   0.00000        CSGMIN =   0.00000        DRLXCA =  1.000000E-02    DRLXCC =   1.01000    
   DTIMEH =  1.000000E+30 DTIMEI =   0.00000        DTIMEL =   0.00000        DTIMEU =   0.00000        DTMPCA =   2.00000    
   DTMPCC =   0.00000     EBALNA =   0.00000        EBALNC =   0.00000        EBALSA =   0.00000        EBALSC =  1.000000E+30
   ESUMIS =   0.00000     ESUMOS =   0.00000        EXTLIM =   5.00000        ITHOLD =         10       NARLXN =          0
   NATMPN =          0    NCGMAN =          0       NCSGMN =          0       NDRLXN =          0       NDTMPN =          0
   NEBALN =          0    NLOOPT =          0       ITEROT =          0       ITERXT =          3       OPEITR =          0
   OUTPUT =   1000.00    

... <content removed> ...

                                     PATH PARAMETER TABULATION FOR SUBMODEL PIPE8   

    PATH  TYPE    LMP 1   LMP 2   DUP I      DUP J     STAT   XL UPSTRM    FLOWRATE     DELTA PRES   FLOW REGIMES OR REYNOLDS NO.

      2   TUBE       98       2    1.0        1.0      NORM    0.000        35.00      -1.6896E+05                   2.33192E+05
      4   TUBE        2       4    1.0        1.0      NORM    0.000        35.00      -1.6896E+05                   2.33200E+05
      6   TUBE        4       6    1.0        1.0      NORM    0.000        35.00      -1.6896E+05                   2.33212E+05
      8   TUBE        6       8    1.0        1.0      NORM    0.000        35.00      -1.6896E+05                   2.33220E+05
     10   TUBE        8      10    1.0        1.0      NORM    0.000        35.00      -1.6896E+05                   2.33229E+05
     11   TUBE       10      11    1.0        1.0      NORM    0.000        35.00        6600.        2.33229E+05    2.33233E+05
     13   TUBE       11      13    1.0        1.0      NORM    0.000        35.00       1.3200E+04    2.33233E+05    2.33241E+05
     15   TUBE       13      15    1.0        1.0      NORM    0.000        35.00       1.1831E+04    2.33241E+05    2.33249E+05
     17   TUBE       15      17    1.0        1.0      NORM    0.000       -5.278       -1575.         35171.         35173.    
     19   TUBE       17      19    1.0        1.0      NORM    0.000       -5.278       -1303.         35173.         35174.    
     21   TUBE       19      20    1.0        1.0      NORM    0.000       -5.278       -651.7         35174.         35175.    
     22   TUBE       20      22    1.0        1.0      NORM    0.000       -5.278       1.6323E+05                    35176.    
     24   TUBE       22      24    1.0        1.0      NORM    0.000       -5.278       1.6323E+05                    35177.    
     26   TUBE       24      26    1.0        1.0      NORM    0.000       -5.278       1.6323E+05                    35179.    
     28   TUBE       26      28    1.0        1.0      NORM    0.000       -5.278       1.6323E+05                    35180.    
     30   TUBE       28      99    1.0        1.0      NORM    0.000       -5.278       1.6323E+05                    35162.    
     98   LOSS     1000      98    1.0        1.0      NORM    0.000        35.00        580.5                                  
     99   LOSS       99    2000    1.0        1.0      NORM    0.000       -5.278       -22.48                                  
   3000   CTLVLV     15    3000    1.0        1.0      NORM    0.000        40.28       8.1259E+05                              
==================================
END: Output File: "AQUADUCT.OUT"
==================================



