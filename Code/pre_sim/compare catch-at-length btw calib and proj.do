
import delimited using "$iterative_input_data_cd/baseline_catch_at_length.csv", clear   
gen length_bin = floor(length/5)*5   // groups of 5
collapse (sum) fitted, by(draw state species length_bin)
rename fitted fitted_prob2024
tempfile base
save `base', replace 

import delimited using "$iterative_input_data_cd/projected_catch_at_length.csv", clear   


gen length_bin = floor(length/5)*5   // groups of 5
collapse (sum) fitted, by(draw state species length_bin)
rename fitted fitted_prob2026
merge 1:1 draw state species length_bin using `base'
drop if _merge==2

reshape long fitted_prob, i(length_bin draw state species) j(year) string
destring year, replace


gen domain=state+"_"+species
graph box fitted_prob if species=="bsb" & state=="CT", over(year, label(labsize(vsmall))) over(length_bin, label(labsize(vsmall)))  ///
asyvars  box(1, color(navy)) box(2, color(maroon)) title("catch-at-length probability, BSB North") ///
    legend(position(bottom) rows(1))
graph export "$figure_cd/bsb_north_CaL_2024_2026.png", as(png) replace

graph box fitted_prob if species=="bsb" & state=="MD", over(year, label(labsize(vsmall))) over(length_bin, label(labsize(vsmall)))  ///
asyvars  box(1, color(navy)) box(2, color(maroon)) title("catch-at-length probability, BSB South") ///
    legend(position(bottom) rows(1))
graph export "$figure_cd/bsb_south_CaL_2024_2026.png", as(png) replace

graph box fitted_prob if species=="scup" & state=="NY", over(year, label(labsize(vsmall))) over(length_bin, label(labsize(vsmall)))  ///
asyvars  box(1, color(navy)) box(2, color(maroon)) title("catch-at-length probability, Scup") ///
    legend(position(bottom) rows(1))
graph export "$figure_cd/scup_CaL_2024_2026.png", as(png) replace

graph box fitted_prob if species=="sf" & state=="NY", over(year, label(labsize(vsmall))) over(length_bin, label(labsize(vsmall)))  ///
asyvars  box(1, color(navy)) box(2, color(maroon)) title("catch-at-length probability, SF") ///
    legend(position(bottom) rows(1))
graph export "$figure_cd/sf_CaL_2024_2026.png", as(png) replace