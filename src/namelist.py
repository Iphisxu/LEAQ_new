
progdir = 'D:/data/Project_Anqing/'
datadir = progdir + 'Local_emis_2021/create_emis_new/'

# local emis
emisdir   = datadir + 'step0_original/'
emisptemp  = emisdir + 'AQ_2021_point_temp.xlsx'
emispoint  = emisdir + 'AQ_2021_point.xlsx'
emisatemp  = emisdir + 'AQ_2020_area_temp.xlsx'
emisarea  = emisdir + 'AQ_2020_area.xlsx'

# sector mapping file
secmap = datadir + 'SectorMapping_new.xlsx'

# local emis netcdf
emis_nc_dir = datadir + 'step1_preliminary/leaq_category/sum/'
local_bio_file = emis_nc_dir + 'Biomass.nc'
local_bol_file = emis_nc_dir + 'Boiler.nc'
local_bld_file = emis_nc_dir + 'Building.nc'
local_dst_file = emis_nc_dir + 'Dust.nc'
local_elc_file = emis_nc_dir + 'Electric.nc'
local_rod_file = emis_nc_dir + 'Road.nc'
local_nro_file = emis_nc_dir + 'NonRoad.nc'
local_pet_file = emis_nc_dir + 'Petrochemicals.nc'
local_sol_file = emis_nc_dir + 'Solvent.nc'
local_oth_file = emis_nc_dir + 'Other.nc'

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
leaq_bio_file = allocated_dir + 'emis.CN3AH_135X138.bio.ncf'
leaq_bol_file = allocated_dir + 'emis.CN3AH_135X138.bol.ncf'
leaq_bld_file = allocated_dir + 'emis.CN3AH_135X138.bld.ncf'
leaq_dst_file = allocated_dir + 'emis.CN3AH_135X138.dst.ncf'
leaq_elc_file = allocated_dir + 'emis.CN3AH_135X138.elc.ncf'
leaq_rod_file = allocated_dir + 'emis.CN3AH_135X138.rod.ncf'
leaq_nro_file = allocated_dir + 'emis.CN3AH_135X138.nro.ncf'
leaq_pet_file = allocated_dir + 'emis.CN3AH_135X138.pet.ncf'
leaq_sol_file = allocated_dir + 'emis.CN3AH_135X138.sol.ncf'
leaq_oth_file = allocated_dir + 'emis.CN3AH_135X138.oth.ncf'

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