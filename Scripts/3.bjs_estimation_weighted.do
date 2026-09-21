* ============================================================
* BJS Estimation - Using PNADc sample weights
* ============================================================

clear all
set more off

* ---- WORKING DIRECTORY ----
global root "."
global data "$root\Data"
global results "$root\Results"

* ============================================================
* 1. OVERALL ATT
* ============================================================
use "$data\dta_pnadc_agg_full.dta", clear
replace cohort = 999 if cohort == .

did_imputation informal_w v4013 t cohort, ///
    autosample ///
    wtr(n_weighted) ///
    cluster(v4013) ///
    minn(0)

estimates store overall_att_w
regsave using "$results\bjs_overall_att_w.dta", replace tstat pval ci

* ============================================================
* 2. OVERALL EVENT STUDY
* ============================================================
use "$data\dta_pnadc_agg_full.dta", clear
replace cohort = 999 if cohort == .

did_imputation informal_w v4013 t cohort, ///
    autosample ///
    wtr(n_weighted) ///
    pretrend(6) ///
    horizons(0/9) ///
    cluster(v4013) ///
    minn(0)

estimates store overall_es_w
regsave using "$results\bjs_overall_es_w.dta", replace tstat pval ci

* ============================================================
* 3. COHORT 3 ATT
* ============================================================
use "$data\dta_pnadc_agg_cohort3.dta", clear
replace cohort = 999 if cohort == .

did_imputation informal_w v4013 t cohort, ///
    autosample ///
    wtr(n_weighted) ///
    cluster(v4013) ///
    minn(0)

estimates store cohort3_att_w
regsave using "$results\bjs_cohort3_att_w.dta", replace tstat pval ci

* ============================================================
* 4. COHORT 3 EVENT STUDY
* ============================================================
use "$data\dta_pnadc_agg_cohort3.dta", clear
replace cohort = 999 if cohort == .

did_imputation informal_w v4013 t cohort, ///
    autosample ///
    wtr(n_weighted) ///
    pretrend(2) ///
    horizons(0/9) ///
    cluster(v4013) ///
    minn(0)

estimates store cohort3_es_w
regsave using "$results\bjs_cohort3_es_w.dta", replace tstat pval ci

* ============================================================
* 5. COHORT 7 ATT
* ============================================================
use "$data\dta_pnadc_agg_cohort7.dta", clear
replace cohort = 999 if cohort == .

did_imputation informal_w v4013 t cohort, ///
    autosample ///
    wtr(n_weighted) ///
    cluster(v4013) ///
    minn(0)

estimates store cohort7_att_w
regsave using "$results\bjs_cohort7_att_w.dta", replace tstat pval ci

* ============================================================
* 6. COHORT 7 EVENT STUDY
* ============================================================
use "$data\dta_pnadc_agg_cohort7.dta", clear
replace cohort = 999 if cohort == .

did_imputation informal_w v4013 t cohort, ///
    autosample ///
    wtr(n_weighted) ///
    pretrend(6) ///
    horizons(0/5) ///
    cluster(v4013) ///
    minn(0)

estimates store cohort7_es_w
regsave using "$results\bjs_cohort7_es_w.dta", replace tstat pval ci