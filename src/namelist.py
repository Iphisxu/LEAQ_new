
progdir = 'D:/data/Project_Anqing/'
datadir = progdir + 'Local_emis_2021/create_emis_new/'

# local emis
emisdir   = datadir + 'step0_original/'
emispoint  = emisdir + 'AQ_2021_point.xlsx'
emistemp  = emisdir + 'AQ_2020_area_temp.xlsx'
emisarea  = emisdir + 'AQ_2020_area.xlsx'

# sector mapping file
secmap = datadir + 'SectorMapping.xlsx'
secmap_area = datadir + 'SectorMapping_area.xlsx'

# local emis netcdf
emis_nc_dir = datadir + 'step1_preliminary/meic_category/sum/'
local_ind_file = emis_nc_dir + 'Industry.nc'
local_pow_file = emis_nc_dir + 'Power.nc'
local_tra_file = emis_nc_dir + 'Transportation.nc'
local_res_file = emis_nc_dir + 'Residential.nc'
local_agr_file = emis_nc_dir + 'Agriculture.nc'

# meic emis
meicdir = datadir + 'MEIC_AQ/'
meic_ind_file = meicdir + 'emis.CN3AH_135X138.ind.ncf'
meic_pow_file = meicdir + 'emis.CN3AH_135X138.pow.ncf'
meic_res_file = meicdir + 'emis.CN3AH_135X138.res.ncf'
meic_tra_file = meicdir + 'emis.CN3AH_135X138.tra.ncf'
meic_agr_file = meicdir + 'emis.CN3AH_135X138.agr.ncf'
meic_shp_file = meicdir + 'emis.CN3AH_135X138.shp.ncf'

# allocated emis
allocated_dir = datadir + 'step2_allocated/'
leaq_ind_file = allocated_dir + 'ind.nc'
leaq_pow_file = allocated_dir + 'pow.nc'
leaq_tra_file = allocated_dir + 'tra.nc'
leaq_res_file = allocated_dir + 'res.nc'
leaq_agr_file = allocated_dir + 'agr.nc'

# =============================================================

# for TEST_SIM
# timestart = '2023-06-11T00'
# timeend   = '2023-06-16T23'

# mcip_file   = progdir + 'TEST_SIM/AQ_test_mcip.nc'
# meic_ncfile = progdir + 'TEST_SIM/AQ_test_meic.nc'
# lex_ncfile  = progdir + 'TEST_SIM/AQ_test_lex.nc'

# shapefile
shpaq   = progdir + 'shapefile/Anqing/Anqing.shp'
shpub = progdir + 'shapefile/Anqing_urban/urban.shp'
shpmap   = progdir + 'shapefile/Anqing_district/anqing_district.shp'