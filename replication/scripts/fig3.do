* -------------------------------------------------------- *
* Produces figure 3 in the paper showcasing local trips by income levels.
* -------------------------------------------------------- * 


** INPUTS
** outcomes_blckgrp_clean_popweighted.csv


** OUTPUTS
** Figure_3.pdf


* -------------------------------------------------------- *
				* Install Packages *
* -------------------------------------------------------- *

ssc install colrspace
ssc install palettes
ssc install egenmore

net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots

* -------------------------------------------------------- *
				   * Data Cleaning *
* -------------------------------------------------------- *

* Replace with path to replication folder 
global project "/path/to/replication"

	
* Load cleaned and processed data specific to subset 
import delimited "$project/data/clean/outcomes_blckgrp_clean_popweighted.csv", clear

* Set graph settings for fontface to Helvetica for consistency
graph set window fontface "Helvetica"
graph set ps fontface "Helvetica"

* Generate income quantiles for each urban area
egen quant=xtile(cs_b19013e1), n(10) by(urb)


* Rename variable 
rename usage_pct_all_cats_home_bg_based usage_pct_all_cats_home 

* Aggregate data by quantile and urban area name to get mean and standard deviation
collapse (mean) usage_pct_all_cats_home access_idx_crosscity_wt access_pct_h access_pct_p access_pct_res access_pct_sch access_pct_ser access_pct_g access_pct_d access_pct_rel access_pct_ac access_pct_all_cats (sd) usage_pct_all_cats_home_sd = usage_pct_all_cats_home (count) usage_pct_all_cats_home_count = usage_pct_all_cats_home, by(quant urb_name)


* -------------------------------------------------------- *
				   * Plot Figure *
* -------------------------------------------------------- *


* Compute upper and lower bounds of the 95% confidence interval for the usage measure
gen ci_high = usage_pct_all_cats_home + 1.96 * usage_pct_all_cats_home_sd / sqrt(usage_pct_all_cats_home_count)
gen ci_low = usage_pct_all_cats_home - 1.96 * usage_pct_all_cats_home_sd / sqrt(usage_pct_all_cats_home_count)

* Create a variable to identify the first quantile
sum quant
gen tick = 1 if quant == 1

* Rank urban areas based on usage percentage for the first quantile
egen rank = rank(usage_pct_all_cats_home) if tick==1, f

* Update ranks based on the highest rank for each urban area
levelsof urb_name, local(lvls)

foreach x of local lvls {
display "`x'"
 * Get the highest rank for the urban area
   qui summ rank if urb_name=="`x'"
   * Replace missing ranks with the highest rank for the urban area
   cap replace rank = `r(max)' if urb_name=="`x'" & rank==.
   }
   
   
* Generate a marker variable for cities based on the first quantile
gen marker = urb_name if tick==1


* Define custom line colors for different ranks
levelsof rank, local(lvls)   
local items = r(r)
foreach x of local lvls {
  
colorpalette   ///
 "183 183 183 %15"  ///
  , ipolate(`items', power(0.8)) reverse nograph 
  local customline `customline' (line usage_pct_all_cats_home quant if rank == `x', lc("`r(p`x')'") lp(solid) lw(*0.8)) ||
  }
  
  
* Plot the graph using the custom line colors and other settings  
summ quant
local start = r(min)
local end   = r(max) 
twoway `customline' ///
(connected usage_pct_all_cats_home quant if urb_name == "Atlanta, GA", msize(medium) color("154 2 78") msize(small) msymbol(circle) lcolor("154 2 78") lwidth(medium) lpattern(solid)) ///
(rarea ci_low ci_high quant if urb_name == "Atlanta, GA", color("154 2 78%20") lwidth(vthin)) ///
(connected usage_pct_all_cats_home quant if urb_name == "New York--Newark, NY--NJ--CT", msize(medium) color("7 93 180") msize(small) msymbol(circle) lcolor("7 93 180") lwidth(medium) lpattern(solid)) ///	
(rarea ci_low ci_high quant if urb_name == "New York--Newark, NY--NJ--CT", color("7 93 180%20") lwidth(vthin)) /// 
(connected usage_pct_all_cats_home quant if urb_name == "Detroit, MI", msize(medium) color("242 93 70") msize(small) msymbol(circle) lcolor("242 93 70") lwidth(medium) lpattern(solid)) ///
(rarea ci_low ci_high quant if urb_name == "Detroit, MI", color("242 93 70%20") lwidth(vthin)),  ///
  xlabel(`start'(1)`end', labsize(medium) tlcolor("black") glcolor("145 168 208"))  ///
  ylabel(0 "0" 0.2 "20" 0.4 "40" 0.6 "60" 0.8 "80", labsize(medium) tlcolor("black") glcolor("145 168 208"))  ///
  ytitle("15-minute Usage (% trips)", size(4)) ///  
  xtitle("Income deciles", size(4)) ///
  text(0.33 9.3 "New York",size(medium) color("7 93 180")) ///
  text(0.03 9.5 "Atlanta",size(medium) color("154 2 78")) ///
  text(0.15 9.5 "Detroit",size(medium) color("242 93 70")) ///
  yscale(lstyle(none)) ///
  xscale(lstyle(none)) ///
  plotregion(lcolor(black) lwidth(thin)) ///
  xsize(8) ysize(6) ///
  legend(off) 
  
  
graph export "$project/output/figure_3.pdf", as(pdf)  replace



 