* Set the main path (replace this with the appropriate path)
local main_path "/path/to/replication"  


* -------------------------------------------------------- * 
 * produce the partial r-squared estimates for the universe poi sample using the data aggregated at the cbg level * 
 * -------------------------------------------------------- * 
 
import delimited "`main_path'/data/clean/outcomes_blckgrp_clean_allpois_popweighted.csv", clear



sum access_idx_crosscity_wt
sum usage_pct_all_cats_home_bg_based


* compute variables to map to the ones created by timur *
gen pct_white = 100 * cs_share_white
gen pop_density_log = log(1 + epa_d1b)
gen hu_density_log = log(1 + epa_d1a)
gen pct_ed_ba_plus = 100 * (cs_b15003e22 + cs_b15003e23 + cs_b15003e24 + cs_b15003e25) / cs_b15003e1
gen median_income_log = log(1 + cs_b19013e1)
gen epa_transit_freq_sqm = epa_d4d


* ranges between 0 and 100 *
gen usage = 100 * usage_pct_all_cats_home_bg_based
gen access = 100 * access_idx_crosscity_wt


* column 1 * 
reghdfe usage access, absorb(urb_geoid)
local b=_b[access]
cor usage access, covariance
matrix C=r(C)
local p_1 = (C[1,2] / C[1,1]) * `b'
display `p_1'


* column 2 * 
reghdfe usage access pop_density_log median_income_log pct_ed_ba_plus pct_white, absorb(urb_geoid)
local b=_b[access]
cor usage access, covariance
matrix C=r(C)
local p_2 = (C[1,2] / C[1,1]) * `b'
display `p_2'


* column 3 * 
reghdfe usage access pop_density_log median_income_log pct_ed_ba_plus pct_white epa_transit_freq_sqm avg_dist_work, absorb(urb_geoid)
local b=_b[access]
cor usage access, covariance
matrix C=r(C)
local p_3 = (C[1,2] / C[1,1]) * `b'
display `p_3'


* -------------------------------------------------------- * 
 * produce the partial r-squared estimates for the universe poi sample using the data aggregated at the urban level * 
 * -------------------------------------------------------- * 
 
import delimited "`main_path'/data/clean/outcomes_urb_clean_allpois_popweighted.csv", clear

 

sum access_idx_crosscity_wt
sum usage_pct_all_cats_home_bg_based


* compute variables to map to the ones created by timur *
gen pct_white = 100 * cs_share_white
gen pop_density_log = log(1 + epa_d1b)
gen hu_density_log = log(1 + epa_d1a)
gen pct_ed_ba_plus = 100 * (cs_b15003e22 + cs_b15003e23 + cs_b15003e24 + cs_b15003e25) / cs_b15003e1
gen median_income_log = log(1 + cs_b19013e1)
gen epa_transit_freq_sqm = epa_d4d


* ranges between 0 and 100 *
gen usage = 100 * usage_pct_all_cats_home_bg_based
gen access = 100 * access_idx_crosscity_wt


* column 4 * 
reg usage access
local b=_b[access]
cor usage access, covariance
matrix C=r(C)
local p_4 = (C[1,2] / C[1,1]) * `b'
display `p_4'



* column 5 * 
reg usage access pop_density_log median_income_log pct_ed_ba_plus pct_white
local b=_b[access]
cor usage access, covariance
matrix C=r(C)
local p_5 = (C[1,2] / C[1,1]) * `b'
display `p_5'



* column 6 * 
reg usage access pop_density_log median_income_log pct_ed_ba_plus pct_white epa_transit_freq_sqm avg_dist_work
local b=_b[access]
cor usage access, covariance
matrix C=r(C)
local p_6 = (C[1,2] / C[1,1]) * `b'
display `p_6'



display `p_1'
display `p_2'
display `p_3'
display `p_4'
display `p_5'
display `p_6'


