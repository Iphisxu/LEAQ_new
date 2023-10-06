#!/bin/csh -f
echo "Current working directory: `pwd`"
echo "-------------------------------------"

# ===================== CCTMv5.4.X Run Script ========================= 
# Usage: run.cctm >&! cctm_Bench_2016_12SE1.log &                                
#
# To report problems or request help with this script/program:     
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================  

# ===================================================================
#> Runtime Environment Options
# ===================================================================

echo 'Start Model Run At ' `date`

#> Toggle Diagnostic Mode which will print verbose information to 
#> standard output
 setenv CTM_DIAG_LVL 0

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 if ( ! $?compiler ) then
   setenv compiler intel
 endif
 if ( ! $?compilerVrsn ) then
   setenv compilerVrsn Empty
 endif

#> Source the config.cmaq file to set the build environment
 cd ../..
 source ./config_cmaq.csh $compiler $compilerVrsn
 cd CCTM/scripts

# =====================================================================
#> CCTM Configuration Options
# =====================================================================

 set APPL      = CN3AH_135X138  #> Application Name (e.g. Gridname)
 setenv GRID_NAME $APPL         #> check GRIDDESC file for GRID_NAME options

#> Set Start and End Days for looping
 set START_DATE = "2023-08-29"     #> beginning date (July 1, 2016)
 set END_DATE   = "2023-08-29"     #> ending date    (July 1, 2016)

#> Set Timestepping Parameters
set STTIME     = 000000            #> beginning GMT time (HHMMSS)
set NSTEPS     = 240000            #> time duration (HHMMSS) for this run
set TSTEP      = 010000            #> output time step interval (HHMMSS)


# =====================================================================
#> Begin Loop Through Simulation Days
# =====================================================================
set rtarray = ""

set TODAYG = ${START_DATE}
set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set START_DAY = ${TODAYJ} 
set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set NDAYS = 0

while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ
  
  set NDAYS = `echo "${NDAYS} + 1" | bc -l`

  #> Retrieve Calendar day Information
  set YYYYMMDD = `date -ud "${TODAYG}" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYMMDD
  set YYYYMM = `date -ud "${TODAYG}" +%Y%m`     #> Convert YYYY-MM-DD to YYYYMM
  set YYMMDD = `date -ud "${TODAYG}" +%y%m%d`   #> Convert YYYY-MM-DD to YYMMDD
  set MM = `date -ud "${TODAYG}" +%m`           #> Convert YYYY-MM-DD to MM  
  set YYYYJJJ = $TODAYJ

  #> Calculate Yesterday's Date
  set YESTERDAY = `date -ud "${TODAYG}-1days" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYJJJ

# =====================================================================
#> Calculate Anthropogenic Emissions -- user-defined
# =====================================================================

  cd ../../PREP/emis
  setenv STDATE $YYYYJJJ
  ./run_${GRID_NAME}.emis_LEAQ >& emis.log
  cd ../../CCTM/scripts

exit
