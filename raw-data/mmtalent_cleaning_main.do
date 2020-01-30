/*******************************************************************************
Title: Cleaning of mmtalent raw data                
Last modified: 4 June 2018 (added variable names)
Contact: Ingvild L. Skarpeid (ingvild.skarpeid@nhh.no)
Purpose: Cleans raw data from Qualtrics file to be used for analysis in mmtalent
Files created: mmtalent.dta

Modified by Erik Ø. Sørensen, 30 Jan 2020 to abstract away file paths.
*******************************************************************************/

set more off
clear all

insheet using"2017+mmtalent+spectators_December+4%2C+2017_14.06.csv", names
drop in 1/2
destring *, replace


// Overview of how many consented //
//tab consent

// Drop those who were screened out, did not complete or did not consent
drop if eap==. & eai==. & epp==. & epi == . & redist_pref==.
drop if progress!= 100 | redist_pref==. | polpref==. | luck_fair==. | talent_fair==. | effort_fair==. | luck_control==. | talent_control==. | effort_control==.
drop if redist_pref==. & polpref==. & luck_fair==. & talent_fair==. & effort_fair==. & luck_control==. & talent_control==. & effort_control==. & progress==100


// Duplicates check//
duplicates list uid


// Creating the groups of observables used to recruit Representative Sample //
gen agegroup = 0
replace agegroup = 1 if age >=35 & age <=44
replace agegroup = 2 if age >=45 & age <=54
replace agegroup = 3 if age >=55 & age <=64
replace agegroup = 4 if age >=65

label define agegroup 0"18-34" 1 "35-44" 2 "45-54" 3 "55-64" 4 "65+"
label values agegroup agegroup

label define rresidence 1 "Alabama" 2 "Alaska" 3 "Arizona" 4"Arkansas" 5 "California" 6"Colorado" 7 "Connecticut" 8 "Delaware" 9 "DoC" 10 "Florida" 11 "Georgia" ///
12 "Hawaii" 13 "Idaho" 14 "Illinois" 15 "Indiana" 16"Iowa" 17 "Kansas" 18 "Kentucky" 19 "Louisiana" 20 "Maine" 21 "Maryland" 22 "Massachusetts" 23 "Michigan" 24 "Minnesota" ///
25 "Mississippi" 26 "Missouri" 27 "Montana" 28 "Nebraska" 29 "Nevada" 30 "New Hampshire" 31 "New Jersey" 32 "New Mexico" 33 "New York" 34 "North Carolina" 35 "North Dakota" ///
36 "Ohio" 37 "Oklahoma" 38 "Oregon" 39 "Pennsylvania" 40 "Puerto Rico" 41 "Rhode Island"  42 "South Carolina" 43 "South Dakota" 44 "Tennessee" 45 "Texas" 46 "Utah" ///
47"Vermont" 48 "Virginia" 49"Washington" 50"West Virginia"  51 "Wisconsin" 52"Wyoming" 53 " I do not live in the United States"
label values residence rresidence

gen region = 0
replace region = 1 if residence==7 | residence== 20 | residence==22 | residence== 30 | residence ==41 | residence == 47 | residence ==31 | residence == 33 | residence ==39
replace region = 2 if residence==15 | residence== 14 | residence==23 | residence== 36 |residence==51 | residence == 16 | residence ==17 | residence== 24 | residence==26 |residence==28|residence==35|residence==43 
replace region = 3 if residence == 3 | residence == 6| residence ==13 | residence == 27|  residence == 32| residence == 46|  residence == 29 | residence == 52 |  residence ==2 |  residence ==5 | residence == 12|  residence ==38 | residence == 49
label define region 0 " South" 1 "Northeast" 2 "Midwest" 3 "West"
label values region region



// Treatment dummy T
gen T = 1 if exantepersonal==1
replace T=2 if  exanteimpersonal==1
replace T=3 if  expostpersonal==1
replace T=4 if  expostimpersonal==1


// Timing dummy Ex Post
gen EP =0
replace EP=1 if T ==3 | T == 4

// Personal dummy P
gen P = 0
replace P=1 if T==1 | T==3

// Interaction dummy
g PEP =P*EP

// outcome variables

gen gini = abs((paymenthighworker-paymentlowworker)/10)

// Equalredistribution dummy
gen equaldum = 0 
replace equaldum=1 if gini ==0

// No redistribution dummy
gen redistdum = 0 
replace redistdum=1 if redistribute ==0

// Rightwing dummy and interaction
gen rightwing = 0
replace rightwing = 1 if polpref==4|polpref==5

label define rw 0 "Neutral or left wing" 1 "Right wing"
label values rightwing rw

gen REP =rightwing*EP

label var REP "Ex Post x Right Wing " 
label define rep 0 " " 1 "Ex Post x Right Wing"
label values REP rep

g RP = rightwing*P

label var RP "Personal x Right Wing " 
label define rp 0 " " 1 "Personal x Right Wing"
label values RP rp

g GEP=gender*EP
g GP = gender*P

label var GEP "Ex Post x Female " 
label define gep 0 " " 1 "Ex Post x Female"
label values GEP gep

label var GP "Personal x Female " 
label define gp 0 " " 1 "Personal x Female"
label values GP gp

label var T "Treatments"
label define treatment 1 "Ex Ante Personal" 2 "Ex Ante Impersonal" 3 "Ex Post Personal"  4"Ex Post Impersonal"
label values T treatment


label var EP "Ex Post " 
label define ea 0 "Ex Ante" 1 "Ex Post"
label values EP ea

label var P "Personal "
label define P 0 "Impersonal" 1 "Personal"
label values P P

label var PEP "Personal x Ex Post"

label var equaldum "\specialcell{Full \\ Redistribution}"
label define equaldum 0 "Some or no redistribution" 1 "Full redistribution"
label values equaldum equaldum

label var redistdum "\specialcell{No \\ Redistribution}"
label define rd 0 "Some redistribution" 1 "No redistribution"
label values redistdum rd

label var gender "Female"
label define gender 0 "Male" 1 "Female"
label values gender gender 

label define edu 1 "No High School" 2 "High School/GED" 3 "Some college" 4 "Associate's Degree" 5 "Bachelor" 6 "Masters" 7 "Professional (JD/MD)" 8 "PhD"
label values edu edu 

label define income 1 "Less than 29 999" 2 " 30 k- 59 999" 3 "60 k - 99 999" 4 "100k - 149 999" 5 " 150 k +"
label values income income

label define tenscale 0 " Very fair" 10 "Very unfair"
label values luck_fair talent_fair  effort_fair tenscale

label define fivescale 1 "Strongly Agree" 2 "Somewhat agree" 3 "Neither" 4 "Somewhat disagree" 5 "Strongly disagree"
label values redist_pref fivescale

label define polpref 1 "Very left wing" 2 "somewhat left wing" 3 "Neutral" 4 "Somewhat right wing" 5 "Very right wing"
label values polpref polpref

label define control 0 "Beyond control" 10 "Within control"
label values talent_control effort_control luck_control control

save "mmtalent_noweights.dta", replace



