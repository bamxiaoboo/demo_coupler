#!/bin/sh -f

cat > ${CASEROOT}/CCPL_dir/config/all/env_run.xml << END
<?xml version="1.0" ?>
<Time_setting
    case_name="$CASE_NAME"
    model_name="The coupled version of MPAS and WWATCH3"
    run_type="$RUN_TYPE"
    leap_year="$LEAP_YEAR"
    start_date="${START_DATE//-/}"
    start_second="$START_SECOND"
    rest_freq_unit="$REST_FREQ_UNIT"
    rest_freq_count="$REST_FREQ_COUNT"
    rest_ref_case="C-Coupler testing"
    rest_ref_date="00040401"
    rest_ref_second="0"
    stop_option="date"
    stop_date="${STOP_DATE//-/}"
    stop_second="$STOP_SECOND"
    stop_n="3"
/>
END
