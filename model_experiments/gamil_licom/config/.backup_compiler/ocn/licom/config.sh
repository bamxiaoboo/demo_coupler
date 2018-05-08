#!/bin/csh -f

# === Note by Li Ruizhe ===
# Required paramters:
#   NAMELIST_DST_DIR
#   DATA_DST_DIR
#   DATA_SRC_DIR
#
#   RUNTYPE
#   GRID
#   RUN_REFCASE
#   RUN_START_DATE
#   RAMP_CO2_START_YMD
#   DOUT_L_MSNAME
# =========================

set called=($_)
if ("$called" != "") then
    set me = "$called[2]"    # the script was sourced from this location
endif
if ("$0" != "csh") then
    set me = "$0"                # the script was run from this location
endif
set me = `readlink -f $me`
set MYPATH = `dirname "$me"`

cd $NAMELIST_DST_DIR

if ($BYPASS_CONFIGURATION == 'FALSE' ) then
    create_symbol_copy $CODEROOT/demo/gamil_licom/licom $DATA_DST_DIR/../licom_demo > /dev/null
endif
cp $DATA_DST_DIR/../licom_demo/*.nc .

