      PROGRAM create_emis

      IMPLICIT NONE

!...  INCLUDES:
      INCLUDE 'PARMS3.EXT'        !  I/O API parameters
      INCLUDE 'IODECL3.EXT'       !  I/O API function declarations
      INCLUDE 'FDESC3.EXT'        !  I/O API file description data structures.

!...........   EXTERNAL FUNCTIONS and their descriptions:
      LOGICAL      DSCGRID
      EXTERNAL     DSCGRID
      INTEGER, EXTERNAL :: ENVINT, WKDAY, JULIAN

!... Program I/O files
! Program name
      CHARACTER(len=16)  :: PROGNAME  = 'LIUEMIS'
! input file
      CHARACTER(len=16) :: POW_FILE = 'POW_FILE'
      CHARACTER(len=16) :: IND_FILE = 'IND_FILE'
      CHARACTER(len=16) :: RES_FILE = 'RES_FILE'
      CHARACTER(len=16) :: TRA_FILE = 'TRA_FILE'
      CHARACTER(len=16) :: AGR_FILE = 'AGR_FILE'
      CHARACTER(len=16) :: BIO_FILE = 'BIO_FILE'
      CHARACTER(len=16) :: SHP_FILE = 'SHP_FILE'
      !      CHARACTER(len=16) :: OTH_FILE = 'OTH_FILE'
      !! Add by Evan
      CHARACTER(len=16) :: BOM_FILE = 'BOM_FILE'
      CHARACTER(len=16) :: BOL_FILE = 'BOL_FILE'
      CHARACTER(len=16) :: BLD_FILE = 'BLD_FILE'
      CHARACTER(len=16) :: DST_FILE = 'DST_FILE'
      CHARACTER(len=16) :: ELC_FILE = 'ELC_FILE'
      CHARACTER(len=16) :: ROD_FILE = 'ROD_FILE'
      CHARACTER(len=16) :: NRO_FILE = 'NRO_FILE'
      CHARACTER(len=16) :: PET_FILE = 'PET_FILE'
      CHARACTER(len=16) :: SOL_FILE = 'SOL_FILE'
      CHARACTER(len=16) :: OTH_FILE = 'OTH_FILE'
! Output file
      CHARACTER(len=16) :: EMIS_1     = 'EMIS_1'
! Parameters for file units
      INTEGER :: logdev

! I/O API file parameters
      INTEGER       jdate          ! Looping date YYYYDDD
      INTEGER       jtime          ! Looping time HHMMSS
!      INTEGER       ncols          ! Number of columns
!      INTEGER       nrows          ! Number of rows
!      INTEGER       mxrec          ! Total number of time steps
      INTEGER       sdate          ! Start date YYYYDDD
      INTEGER       stime          ! Start time HHMMSS
      INTEGER       tstep          ! Time step

! I/O API file parameters for reading emission file
      INTEGER       rdjdate
      INTEGER       rdjtime
      INTEGER       rdtstep

!... Internal parameters
! internal paramters (status and buffer)
      INTEGER   ios                               ! i/o status
      CHARACTER(len=256) :: MESG                  ! Message buffer

! local variables and their descriptions:
!     CHARACTER(len=16)  :: GDNAM
      CHARACTER(len=16)  :: CNAME
! Count number: t for time and s for variable(variables)
      INTEGER :: t,s,z,i,j
! Current date: year, month, day, week and the first day of week
      INTEGER :: YEAR, MONTH, DAY, WEEK, WEEK_FIRST
! number of days in week and number of days in month
      INTEGER :: WEEK_NUM(7),MON_NUM(12)
! Week factor of POWER, INDUSTRY, RESIDENTIAL and TRANSPORT
      REAL :: week_fac_pow(7),week_fac_ind(7)
      REAL :: week_fac_res(7),week_fac_tra(7)
      REAL :: week_fac_shp(7)
      !! Add by Evan
      REAL :: week_fac_bom(7),week_fac_bol(7)
      REAL :: week_fac_bld(7),week_fac_dst(7)
      REAL :: week_fac_rod(7),week_fac_nro(7)
      REAL :: week_fac_elc(7),week_fac_pet(7)
      REAL :: week_fac_sol(7),week_fac_oth(7)
! Hourly factor of POWER, INDUSTRY, RESIDENTIAL and TRANSPORT
      REAL :: hour_fac_pow(25),hour_fac_ind(25)
      REAL :: hour_fac_res(25),hour_fac_tra(25)
      REAL :: hour_fac_agr(25),hour_fac_shp(25)
      !! Add by Evan
      REAL :: hour_fac_bom(25),hour_fac_bol(25)
      REAL :: hour_fac_bld(25),hour_fac_dst(25)
      REAL :: hour_fac_rod(25),hour_fac_nro(25)
      REAL :: hour_fac_elc(25),hour_fac_pet(25)
      REAL :: hour_fac_sol(25),hour_fac_oth(25)
! Vertical factor of POWER, INDUSTRY, RESIDENTIAL and TRANSPORT 
      REAL :: vert_fac_pow(8),vert_fac_ind(8)
      REAL :: vert_fac_res(8),vert_fac_tra(8)
      REAL :: vert_fac_agr(8),vert_fac_bio(8)
      REAL :: vert_fac_shp(8)   !vert_fac_oth(8)
      !! Add by Evan
      REAL :: vert_fac_bom(8),vert_fac_bol(8)
      REAL :: vert_fac_bld(8),vert_fac_dst(8)
      REAL :: vert_fac_rod(8),vert_fac_nro(8)
      REAL :: vert_fac_elc(8),vert_fac_pet(8)
      REAL :: vert_fac_sol(8),vert_fac_oth(8)
! Day factor of POWER, INDUSTRY, RESIDENTIAL and TRANSPORT
      REAL :: day_fac_pow, day_fac_ind
      REAL :: day_fac_res, day_fac_tra, day_fac_shp
      !! Add by Evan
      REAL :: day_fac_bom, day_fac_bol
      REAL :: day_fac_bld, day_fac_dst
      REAL :: day_fac_rod, day_fac_nro
      REAL :: day_fac_elc, day_fac_pet
      REAL :: day_fac_sol, day_fac_oth
! Variables list in input nc file
      CHARACTER(len=16)  :: GCVARLIST(54), AEVARLIST(19)
      CHARACTER(len=16)  :: NRVARLIST(1), BIOVARLIST(31)
! Variables index from input file to output file
      INTEGER :: ant_index(74), agr_index(1)
      INTEGER :: bio_index(31)   ! oth_index(2)
! emission data from input file
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_POW, VAR_IND
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_TRA, VAR_RES
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_BIO, VAR_SHP
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_AGR     ! VAR_OTH
      !! Add by Evan
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_BOM, VAR_BOL
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_BLD, VAR_DST
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_ROD, VAR_NRO
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_ELC, VAR_PET
      REAL, ALLOCATABLE, DIMENSION (:,:,:,:) :: VAR_SOL, VAR_OTH
! emission data IBUFF(NCOL,NROW,LAYER,TIME,VAR)
      REAL,ALLOCATABLE :: IBUFF(:,:,:,:,:),tmpBUFF(:,:,:,:,:) 


! temporary number
      INTEGER :: tmpint

!***********************************************************************

!=======================================================================
!...  Begin program
!=======================================================================


!-----------------------------------------------------------------------
!.....1) File set up and assign I/O parameters
!-----------------------------------------------------------------------
!... Get input parameters from run script
      logdev = init3()

      CALL ENVSTR( 'GRID_NAME', MESG, 'LIU', GDNAM3D, IOS )
      IF( .NOT. DSCGRID( GDNAM3D, CNAME, GDTYP3D,
     &              P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D,
     &              XORIG3D, YORIG3D, XCELL3D, YCELL3D,
     &              NCOLS3D, NROWS3D, NTHIK3D ) ) THEN
         MESG = 'Could not get grid description.'
         CALL M3EXIT ( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

!... Get today's date
      sdate = ENVINT( 'STDATE', MESG, 0, ios )
      stime = 0
      tstep = 10000
      jdate = sdate
      jtime = stime

!      write(*,*)GDNAM,CNAME,GDTYP3D,P_ALP3D,P_BET3D,P_GAM3D,XCENT3D,
!     &               YCENT3D,XORIG3D,YORIG3D,XCELL3D,YCELL3D,NCOLS3D,
!     &               NROWS3D,NTHIK3D


!-----------------------------------------------------------------------
!.....2) Compute the temporal and verticle factors
!-----------------------------------------------------------------------
!... Load data set
      mon_num=(/31,28,31,30,31,30,31,31,30,31,30,31/)

      week_fac_pow=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_ind=(/0.159,0.159,0.159,0.159,0.159,0.103,0.103/)
      week_fac_res=(/0.159,0.159,0.159,0.159,0.159,0.103,0.103/)
      week_fac_tra=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_shp=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      !! Add by Evan
      week_fac_bom=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_bol=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_bld=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_dst=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_elc=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_rod=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_nro=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_pet=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_sol=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)
      week_fac_oth=(/0.154,0.154,0.154,0.154,0.154,0.115,0.115/)

      hour_fac_pow=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_ind=(/0.0450,0.0680,0.0680,0.0680,0.0680,0.0680,0.0680,
     &               0.0680,0.0680,0.0660,0.0630,0.0370,0.0370,0.0370,
     &               0.0370,0.0370,0.0260,0.0070,0.0070,0.0070,0.0070,
     &               0.0070,0.0070,0.0290,0.0450/)
      hour_fac_res=(/0.0380,0.0380,0.0300,0.0450,0.0450,0.0380,0.0300,
     &               0.0300,0.0380,0.0750,0.0750,0.0750,0.0750,0.0540,
     &               0.0180,0.0180,0.0180,0.0180,0.0180,0.0180,0.0180,
     &               0.0380,0.0750,0.0750,0.0380/)
      hour_fac_tra=(/0.0599,0.0590,0.0594,0.0540,0.0501,0.0587,0.0600,
     &               0.0620,0.0594,0.0574,0.0557,0.0490,0.0454,0.0417,
     &               0.0308,0.0216,0.0173,0.0132,0.0138,0.0150,0.0155,
     &               0.0157,0.0290,0.0564,0.0599/)
      hour_fac_agr=(/0.0285,0.0485,0.0681,0.0853,0.0986,0.1051,0.1037,
     &               0.0934,0.0755,0.0525,0.0307,0.0213,0.0193,0.0180,
     &               0.0171,0.0164,0.0158,0.0153,0.0149,0.0145,0.0142,
     &               0.0139,0.0136,0.0158,0.0285/)
      hour_fac_shp=(/0.0599,0.0590,0.0594,0.0540,0.0501,0.0587,0.0600,
     &               0.0620,0.0594,0.0574,0.0557,0.0490,0.0454,0.0417,
     &               0.0308,0.0216,0.0173,0.0132,0.0138,0.0150,0.0155,
     &               0.0157,0.0290,0.0564,0.0599/)
      !! Add by Evan
      hour_fac_bom=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_bol=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_bld=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_dst=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_elc=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_rod=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_nro=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_pet=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_sol=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)
      hour_fac_oth=(/0.0433,0.0457,0.0478,0.0486,0.0494,0.0497,0.0501,
     &               0.0500,0.0497,0.0489,0.0477,0.0473,0.0466,0.0440,
     &               0.0397,0.0353,0.0325,0.0300,0.0288,0.0284,0.0292,
     &               0.0316,0.0354,0.0403,0.0433/)

      vert_fac_pow=(/0.00,0.05,0.10,0.30,0.20,0.20,0.10,0.05/)
      vert_fac_ind=(/0.50,0.30,0.20,0.00,0.00,0.00,0.00,0.00/)
      vert_fac_res=(/1,0,0,0,0,0,0,0/)
      vert_fac_tra=(/1,0,0,0,0,0,0,0/)
      vert_fac_agr=(/1,0,0,0,0,0,0,0/)
      vert_fac_shp=(/1,0,0,0,0,0,0,0/)
      vert_fac_bio=(/1,0,0,0,0,0,0,0/)
!      vert_fac_oth=(/1,0,0,0,0,0,0,0/)
      !! Add by Evan
      vert_fac_bom=(/1,0,0,0,0,0,0,0/)
      vert_fac_bol=(/0.00,0.05,0.10,0.30,0.20,0.20,0.10,0.05/)
      vert_fac_bld=(/0.50,0.30,0.20,0.00,0.00,0.00,0.00,0.00/)
      vert_fac_dst=(/1,0,0,0,0,0,0,0/)
      vert_fac_elc=(/0.00,0.05,0.10,0.30,0.20,0.20,0.10,0.05/)
      vert_fac_rod=(/1,0,0,0,0,0,0,0/)
      vert_fac_nro=(/1,0,0,0,0,0,0,0/)
      vert_fac_pet=(/0.50,0.30,0.20,0.00,0.00,0.00,0.00,0.00/)
      vert_fac_sol=(/1,0,0,0,0,0,0,0/)
      vert_fac_oth=(/1,0,0,0,0,0,0,0/)


      print*,'print sum of factors'
      print*,sum(week_fac_pow),sum(week_fac_ind),sum(week_fac_res),
     &       sum(week_fac_tra),sum(hour_fac_pow),sum(hour_fac_ind),
     &       sum(hour_fac_res),sum(hour_fac_tra),sum(hour_fac_agr),
     &       sum(vert_fac_pow),sum(vert_fac_ind),sum(vert_fac_res),
     &       sum(vert_fac_tra),sum(vert_fac_agr),sum(vert_fac_bio)
!     &       sum(vert_fac_oth)

!... Get date and time
      YEAR = jdate/1000
      CALL DAYMON( jdate, MONTH, DAY ) 
      WEEK = WKDAY( jdate )
      WEEK_FIRST = WKDAY( YEAR*1000+JULIAN(YEAR, MONTH, 1) )

!... if this is a leap year
      if ( ( MOD(YEAR,4)==0 .and. MOD(YEAR,100)/=0 ) .or.
     &                          MOD(YEAR,400)==0 ) then
        MON_NUM(2)=29
      end if

!... Calculate the number of week in this month
      WEEK_NUM(:)=4
      if ( MON_NUM( MONTH ) /= 28 ) then
        do t=WEEK_FIRST,WEEK_FIRST+MON_NUM( MONTH )-29
          tmpint=MOD(t,7)
          if (tmpint==0) tmpint=7
          WEEK_NUM( tmpint )=WEEK_NUM( tmpint )+1
        end do
      endif

!... Calculate the factor today
      day_fac_pow = week_fac_pow( WEEK )/SUM( WEEK_NUM*week_fac_pow )
      day_fac_ind = week_fac_ind( WEEK )/SUM( WEEK_NUM*week_fac_ind )
      day_fac_res = week_fac_res( WEEK )/SUM( WEEK_NUM*week_fac_res )
      day_fac_tra = week_fac_tra( WEEK )/SUM( WEEK_NUM*week_fac_tra )
      day_fac_shp = week_fac_shp( WEEK )/SUM( WEEK_NUM*week_fac_shp )
      !! Add by Evan
      day_fac_bom = week_fac_bom( WEEK )/SUM( WEEK_NUM*week_fac_bom )
      day_fac_bol = week_fac_bol( WEEK )/SUM( WEEK_NUM*week_fac_bol )
      day_fac_bld = week_fac_bld( WEEK )/SUM( WEEK_NUM*week_fac_bld )
      day_fac_dst = week_fac_dst( WEEK )/SUM( WEEK_NUM*week_fac_dst )
      day_fac_elc = week_fac_elc( WEEK )/SUM( WEEK_NUM*week_fac_elc )
      day_fac_rod = week_fac_rod( WEEK )/SUM( WEEK_NUM*week_fac_rod )
      day_fac_nro = week_fac_nro( WEEK )/SUM( WEEK_NUM*week_fac_nro )
      day_fac_pet = week_fac_pet( WEEK )/SUM( WEEK_NUM*week_fac_pet )
      day_fac_sol = week_fac_sol( WEEK )/SUM( WEEK_NUM*week_fac_sol )
      day_fac_oth = week_fac_oth( WEEK )/SUM( WEEK_NUM*week_fac_oth )

!      print*, day_fac_pow, day_fac_ind, day_fac_res, day_fac_tra


!-----------------------------------------------------------------------
!.....3) Get the data from input file
!-----------------------------------------------------------------------
!... Allocate the data from input file
      ALLOCATE ( VAR_POW( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_IND( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_RES( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_TRA( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_SHP( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_AGR( NCOLS3D, NROWS3D,  1, 12 ) )
      ALLOCATE ( VAR_BIO( NCOLS3D, NROWS3D, 31, 25 ) )
!      ALLOCATE ( VAR_OTH( NCOLS3D, NROWS3D,  2, 12 ) )
      !! Add by Evan
      ALLOCATE ( VAR_BOM( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_BOL( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_BLD( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_DST( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_ELC( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_ROD( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_NRO( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_PET( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_SOL( NCOLS3D, NROWS3D, 74, 12 ) )
      ALLOCATE ( VAR_OTH( NCOLS3D, NROWS3D, 74, 12 ) )


!... Load varaible index from input file to output file
      ant_index = (/1:6,74,40:44,27,28,46,47,29,30,31,51,52,33,34,48,
     &            36,35,37,38,49,32,7,14,15,23,12,16,18,8,39,17,9,10,
     &                  21,19,22,13,26,24,25,20,11,50,45,55:73,54,53/)
      agr_index = (/74/)
      bio_index = (/30,51,8,12,2,1,74,14,17,7,9,23,16,10,4,27,40,41,42,
     &             43,44,48,49,46,47,15,28,31,34,50,52/)
!      oth_index = (/54,53/)

!... open input file
      IF ( .NOT. OPEN3( POW_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (POW_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( IND_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (IND_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( RES_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (RES_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( TRA_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (TRA_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( SHP_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (SHP_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( AGR_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (AGR_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
!      IF ( .NOT. OPEN3( OTH_FILE, FSREAD3, PROGNAME ) ) THEN
!         CALL NAMEVAL (OTH_FILE, MESG)  ! get input file name and path
!         MESG = 'Could not open file '//TRIM(MESG)
!         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
!      ENDIF
      IF ( .NOT. OPEN3( BIO_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (BIO_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      !! Add by Evan
      IF ( .NOT. OPEN3( BOM_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (BOM_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( BOL_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (BOL_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( BLD_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (BLD_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( DST_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (DST_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( ELC_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (ELC_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( ROD_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (ROD_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( NRO_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (NRO_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( PET_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (PET_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( SOL_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (SOL_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF
      IF ( .NOT. OPEN3( OTH_FILE, FSREAD3, PROGNAME ) ) THEN
         CALL NAMEVAL (OTH_FILE, MESG)  ! get input file name and path
         MESG = 'Could not open file '//TRIM(MESG)
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF


      print*,"liu1" 
!... read input file
      rdjdate = 2010001
      rdjtime = 0
      rdtstep = 10000
      DO t=1,12
        IF( .NOT. READ3(POW_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_POW(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(POW_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        print*,"liu2"
        IF( .NOT. READ3(IND_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_IND(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(IND_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(RES_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_RES(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(RES_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(TRA_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_TRA(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(TRA_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(SHP_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_SHP(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(SHP_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(AGR_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_AGR(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(AGR_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
!        IF( .NOT. READ3(OTH_FILE,'ALL',-1,rdjdate,rdjtime,
!     &                                        VAR_OTH(:,:,:,t))) THEN
!            MESG = 'Error reading '//TRIM(OTH_FILE)
!            CALL M3EXIT(PROGNAME,0,0,MESG,2)
!        ENDIF
        !! Add by Evan
        IF( .NOT. READ3(BOM_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_BOM(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(BOM_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(BOL_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_BOL(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(BOL_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(BLD_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_BLD(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(BLD_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(DST_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_DST(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(DST_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(ELC_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_ELC(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(ELC_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(ROD_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_ROD(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(ROD_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(NRO_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_NRO(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(NRO_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(PET_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_PET(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(PET_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(SOL_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_SOL(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(SOL_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF
        IF( .NOT. READ3(OTH_FILE,'ALL',-1,rdjdate,rdjtime,
     &                                        VAR_OTH(:,:,:,t))) THEN
            MESG = 'Error reading '//TRIM(OTH_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
        ENDIF

        CALL NEXTIME(rdjdate,rdjtime,rdtstep)
      ENDDO

!... Biogenic emission variables lists
      BIOVARLIST = (/'ISOP','TERP','MEOH','ACET','NO','NO2','NH3',
     &               'CCHO','FACD','HCHO','AACD','BALD','MEK','PACD',
     &               'CO','ETHE','ALK1','ALK2','ALK3','ALK4','ALK5',
     &               'ARO1','ARO2MN','OLE1','OLE2','RCHO','PRPE',
     &               'APIN','TOLU','NAPH','SESQ'/)
      DO t=1,25
        DO s=1,31
          IF( .NOT. READ3(BIO_FILE,BIOVARLIST(s),-1,jdate,jtime,
     &                                       VAR_BIO(:,:,s,t))) THEN
            MESG = 'Error reading '//TRIM(BIO_FILE)
            CALL M3EXIT(PROGNAME,0,0,MESG,2)
          ENDIF
        ENDDO
        CALL NEXTIME(jdate,jtime,tstep)
      ENDDO

!... Allocate the emission data IBUFF
      ALLOCATE ( IBUFF(NCOLS3D,NROWS3D,8,25,74) )

!... initial IBUFF
      IBUFF(:,:,:,:,:) = 0

!... adjust emission
!... store MEIC input data to IBUFF
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_POW(:,:,s,MONTH)*
     &                   day_fac_pow*hour_fac_pow(t)*vert_fac_pow(z) )*
     &                         ( VAR_IND(:,:,s,MONTH)*
     &                   day_fac_ind*hour_fac_ind(t)*vert_fac_ind(z) )*
     &                         ( VAR_RES(:,:,s,MONTH)*
     &                   day_fac_res*hour_fac_res(t)*vert_fac_res(z) )*
     &                         ( VAR_TRA(:,:,s,MONTH)*
     &                   day_fac_tra*hour_fac_tra(t)*vert_fac_tra(z) )*
     &                         ( VAR_SHP(:,:,s,MONTH)*
     &                   day_fac_shp*hour_fac_shp(t)*vert_fac_shp(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO


      DO s=1,1
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,agr_index(s)) = IBUFF(:,:,z,t,agr_index(s)) +
     &                          VAR_AGR(:,:,s,MONTH)/
     &                              MON_NUM(MONTH)*hour_fac_agr(t)*
     &                              vert_fac_agr(z)*1000000/3600
          ENDDO
        ENDDO
      ENDDO

      DO s=1,31
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,bio_index(s)) = IBUFF(:,:,z,t,bio_index(s)) +
     &                          VAR_BIO(:,:,s,t)*vert_fac_bio(z)
          ENDDO
        ENDDO
      ENDDO

      !! Add by Evan
!... store LEAQ input data to IBUFF 
      ! 1.BOM
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_BOM(:,:,s,MONTH)*
     &                   day_fac_bom*hour_fac_bom(t)*vert_fac_bom(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 2.BOL
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_BOL(:,:,s,MONTH)*
     &                   day_fac_bol*hour_fac_bol(t)*vert_fac_bol(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 3.BLD
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_BLD(:,:,s,MONTH)*
     &                   day_fac_bld*hour_fac_bld(t)*vert_fac_bld(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 4.DST
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_DST(:,:,s,MONTH)*
     &                   day_fac_dst*hour_fac_dst(t)*vert_fac_dst(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 5.ELC
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_ELC(:,:,s,MONTH)*
     &                   day_fac_elc*hour_fac_elc(t)*vert_fac_elc(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 6.ROD
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_ROD(:,:,s,MONTH)*
     &                   day_fac_rod*hour_fac_rod(t)*vert_fac_rod(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 7.NRO
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_NRO(:,:,s,MONTH)*
     &                   day_fac_nro*hour_fac_nro(t)*vert_fac_nro(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 8.PET
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_PET(:,:,s,MONTH)*
     &                   day_fac_pet*hour_fac_pet(t)*vert_fac_pet(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 9.SOL
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_SOL(:,:,s,MONTH)*
     &                   day_fac_sol*hour_fac_sol(t)*vert_fac_sol(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO
      ! 10.OTH
      DO s=1,74
        DO t=1,25
          DO z=1,8
            IBUFF(:,:,z,t,ant_index(s)) = IBUFF(:,:,z,t,ant_index(s)) +
     &                         ( VAR_OTH(:,:,s,MONTH)*
     &                   day_fac_oth*hour_fac_oth(t)*vert_fac_oth(z) )*
     &                           1000000/3600
          ENDDO
        ENDDO
      ENDDO

!... variable
!....               1    2    3      4    5    6       7      8
      GCVARLIST=(/'NO2','NO','HONO','CO','SO2','SULF','HCHO','MEOH',
!....               9     10     11     12     13     14     15
     &            'AACD','PACD','RNO3','ACET','CRES','CCHO','RCHO',
!....              16     17     18     19     20     21     22
     &            'MEK','FACD','PRD2','MGLY','IPRD','GLY','BACL',
!....              23     24     25     26     27     28     29
     &            'BALD','MACR','MVK','ACRO','ETHE','PRPE','BDE13',
!....              30     31     32     33     34     35     36
     &            'ISOP','APIN','ACYE','BENZ','TOLU','MXYL','OXYL',
!....              37     38     39     40     41     42     43
     &            'PXYL','B124','ETOH','ALK1','ALK2','ALK3','ALK4',
!....              44     45       46     47     48     49
     &            'ALK5','SOAALK','OLE1','OLE2','ARO1','ARO2MN',
!....              50     51     52     53     54   
     &            'NAPH','TERP','SESQ','CL2','HCL'/)
!....              55     56     57     58     59   60    61   62
      AEVARLIST=(/'PSO4','PNO3','PCL','PNH4','PNA','PMG','PK','PCA',
!....               63     64     65    66    67    68    69    70
     &             'POC','PNCOM','PEC','PFE','PAL','PSI','PTI','PMN',
!....               71     72       73
     &             'PH2O','PMOTHR','PMC'/)
!....              74
      NRVARLIST=(/'NH3'/)
      print*,GCVARLIST,AEVARLIST,NRVARLIST

!-----------------------------------------------------------------------
!...... emission change test
!-----------------------------------------------------------------------
!      IBUFF(:,:,:,:,7:52)=IBUFF(:,:,:,:,7:52)*0.8  !VOC EMISSION*2
!      IBUFF(:,:,:,:,55:73)=IBUFF(:,:,:,:,55:73)*1.2

!-----------------------------------------------------------------------
!.....4) Store the emission data
!-----------------------------------------------------------------------
!... Rset jdate and jtime
      jdate = sdate
      jtime = stime

!... set attribute
      FTYPE3D = GRDDED3
      SDATE3D = sdate
      STIME3D = stime
      TSTEP3D = tstep
      MXREC3D = 25
      NLAYS3D = 8
      NVARS3D = 74
      VGTYP3D = 7
      VGTOP3D = 5000
      VGLVS3D(1:9) = (/1.000, 0.995, 0.992, 0.980, 0.960, 0.950, 0.930,
     &                 0.910, 0.890/)
      VTYPE3D(1:NVARS3D) = M3REAL
      VNAME3D(1:54) = GCVARLIST(:)
      VNAME3D(55:73)= AEVARLIST(:)
      VNAME3D(74)   = NRVARLIST(1)
      VDESC3D  = VNAME3D
      DO s=1,54
        UNITS3D(s) = 'moles/s         '
      ENDDO
      DO s=55,73
        UNITS3D(s) = 'g/s             '
      ENDDO
      UNITS3D(74)  = 'moles/s         '

      write(*,*) FTYPE3D, SDATE3D, STIME3D, TSTEP3D, NCOLS3D, NROWS3D,
     &           NLAYS3D, NVARS3D, NTHIK3D, GDTYP3D, P_ALP3D, P_BET3D,
     &           P_GAM3D, XORIG3D, YORIG3D, XCELL3D, YCELL3D,
     &           XCENT3D, YCENT3D

      IF ( .NOT. OPEN3( EMIS_1, FSCREA3, PROGNAME ) ) THEN
        MESG = 'Could not open file '//TRIM(EMIS_1)
        CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

      DO t=1,25
        DO s=1,74
            IF ( .NOT. WRITE3(EMIS_1,VNAME3D(s),jdate,jtime,
     &                                        IBUFF(:,:,:,t,s)    )  ) THEN
               MESG = 'Error writing to file: '//TRIM(EMIS_1)
               CALL M3EXIT(PROGNAME,jdate,jtime,MESG,2)
            ENDIF
        ENDDO
        CALL NEXTIME(jdate,jtime,tstep)
      ENDDO

!=======================================================================
!...  End program
!=======================================================================
      ENDPROGRAM
