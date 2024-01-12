
** INPUTS
** outcomes_blckgrp_clean_popweighted.csv


** OUTPUTS
** Figure_S7.pdf


*ssc install reghdfe 
*ssc install ftools
*ssc install estout

* Set the main path (replace this with the appropriate path)
local main_path "/path/to/replication"  


import delimited "`main_path'/data/clean/outcomes_blckgrp_clean_popweighted.csv", clear


egen quant  =xtile(cs_b19013e1), n(4) by(urb)
egen deciles=xtile(cs_b19013e1), n(10) by(urb)

// Tag cities in especial sample 
gen samp15=0
replace samp15=1 if urb==78310
replace samp15=1 if urb==69076
replace samp15=1 if urb==79282
replace samp15=1 if urb==20422
replace samp15=1 if urb==79039
replace samp15=1 if urb==11755
replace samp15=1 if urb==80362
replace samp15=1 if urb==15211
replace samp15=1 if urb==16264
replace samp15=1 if urb==9298
replace samp15=1 if urb==51445
replace samp15=1 if urb==78904
replace samp15=1 if urb==10162
replace samp15=1 if urb==84493
replace samp15=1 if urb==63217

// make variable ranges between 0 and 100
replace usage_pct_all_cats_home_bg_based = usage_pct_all_cats_home_bg_based*100
replace access_idx_crosscity_wt = access_idx_crosscity_wt*100
replace psi = psi*100

// Basic correlation between usage and access 
reghdfe usage_pct_all_cats_home_bg_based access_idx_crosscity_wt, absorb(urb) cluster(county)

reghdfe usage_pct_all_cats_home_bg_based access_idx_crosscity_wt if samp15==1, absorb(urb) cluster(county)

reghdfe usage_pct_all_cats_home_bg_based access_idx_crosscity_wt if urb_name=="New York--Newark, NY--NJ--CT", absorb(urb) cluster(county)



// Analysis of segregation 
reghdfe psi i.quant c.usage_pct_all_cats_home_bg_based#ibn.quant if samp15==1, absorb(urb) cluster(county)
estimates store e1

reghdfe psi i.quant c.access_idx_crosscity_wt#ibn.quant  if samp15==1, absorb(urb) cluster(county)
estimates store e2

reghdfe psi i.quant c.usage_pct_all_cats_home_bg_based#ibn.quant , absorb(urb) cluster(county)
estimates store e3

reghdfe psi i.quant c.access_idx_crosscity_wt#ibn.quant , absorb(urb) cluster(county)
estimates store e4

estout e1 e2 e3 e4  using "`main_path'/output/TableS7.tex", style(tex) ///
varlabels(1.quant#c.usage_pct_all_cats_home_bg_based "Usage $\times$ Low income " ///
          2.quant#c.usage_pct_all_cats_home_bg_based "Usage $\times$ Moderate-low income " ///
		  3.quant#c.usage_pct_all_cats_home_bg_based "Usage $\times$ Moderate-high income " ///
		  4.quant#c.usage_pct_all_cats_home_bg_based "Usage $\times$ High income " ///
		  1.quant#c.access_idx_crosscity_wt "Access $\times$ Low income " ///
          2.quant#c.access_idx_crosscity_wt "Access $\times$ Moderate-low income " ///
		  3.quant#c.access_idx_crosscity_wt "Access $\times$ Moderate-high income " ///
		  4.quant#c.access_idx_crosscity_wt "Access $\times$ High income " ) ///
		  cells(b(star fmt(%9.3f)) p(fmt(%9.3f))) ///
		  stats(r2 N, fmt(%7.2f %7.0f) labels( "\\ Urban area fixed effects & \checkmark & \checkmark & \checkmark & \checkmark \\ R-squared" "Observations" )) nolabel replace mlabels(none) collabels(none) starlevels(\$^{*}\$ .1 \$^{**}\$ .05 \$^{***}\$ .01) ///
keep(1.quant#c.usage_pct_all_cats_home_bg_based 2.quant#c.usage_pct_all_cats_home_bg_based  3.quant#c.usage_pct_all_cats_home_bg_based 4.quant#c.usage_pct_all_cats_home_bg_based 1.quant#c.access_idx_crosscity_wt  2.quant#c.access_idx_crosscity_wt 3.quant#c.access_idx_crosscity_wt   4.quant#c.access_idx_crosscity_wt) ///
order(1.quant#c.usage_pct_all_cats_home_bg_based 2.quant#c.usage_pct_all_cats_home_bg_based  3.quant#c.usage_pct_all_cats_home_bg_based 4.quant#c.usage_pct_all_cats_home_bg_based 1.quant#c.access_idx_crosscity_wt  2.quant#c.access_idx_crosscity_wt 3.quant#c.access_idx_crosscity_wt   4.quant#c.access_idx_crosscity_wt)


