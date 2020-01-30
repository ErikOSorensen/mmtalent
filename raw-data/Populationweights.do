/*******************************************************************************
Title:  population weights mmtalent               
Last modified: 20 February 2018
Contact: Ingvild L. Skarpeid (ingvild.skarpeid@nhh.no)
Purpose: Do-file that cleans populationweights.csv and merges with master data set
		
Files created: censuspop.dta
				populationweights.dta
*******************************************************************************/

/***********
Making censuspopulation dataset from .csv to make compatible with main dataset
***********/

clear all
set more off
clear all
cap log close

if c(os) == "MacOSX" gl user "/Users/`c(username)'"
else if c(os) == "Windows" gl user "C:\Users\\`c(username)'" 
else if c(os) == "Unix" gl user "/usr/`c(username)'"
di "$user

gl root_dir "$user/Documents/svn/mmtalent/estimering"

di "$root_dir/data/processed" 

insheet using "$root_dir/data/raw/populationweights2016.csv", comma

/*/g agegroup = .
replace agegroup == "0" if age_category == ""
replace agegroup == "1" if age_category == ""
replace agegroup == "2" if age_category == ""
replace agegroup == "3" if age_category == ""
replace agegroup == "4" if age_category == ""*/


replace region = "0" if region =="South"
replace region = "1" if region =="Northeast"
replace region = "2" if region =="Midwest"
replace region = "3" if region =="West"

replace gender = "0" if gender == "male"
replace gender ="1" if gender == "female"

destring *, replace

save censuspop.dta, replace

/***********
Making population count for mmtalent dataset
***********/
clear all
di "$root_dir/data/processed" 
use "$root_dir/data/processed/mmtalent_noweights.dta", clear

merge m:1 region gender agegroup using "$root_dir/data/processed/censuspop.dta"
drop _merge popdum
*save "/Users/ingvild/Documents/svn/mmtalent/estimering/data/processed/mmtalent.dta", replace

collapse (first) n2016 (count) uid=uid, by(region agegroup gender)
rename uid =count

save populationweights.dta, replace

/***********
Merging population count and mmtalent dataset
***********/

clear all
use "/Users/ingvild/Documents/svn/mmtalent/estimering/data/processed/mmtalent_noweights.dta", clear
merge m:1 region gender agegroup using populationweights.dta
g popw = uidcount/n2016  
drop _merge uidcount n2016
save "/Users/ingvild/Documents/svn/mmtalent/estimering/data/processed/mmtalent.dta", replace
