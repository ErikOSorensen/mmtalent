/*******************************************************************************
Title: Cleaning of mmtalent raw data                
Last modified: 4 June 2018 (added variable names)
Contact: Ingvild L. Skarpeid (ingvild.skarpeid@nhh.no)
Purpose: Cleans raw data from Qualtrics file to be used for analysis in mmtalent
Files created: mmtalent.dta

Modified by Erik Ø. Sørensen, 30 Jan 2020 to abstract away file paths.

Modified by Erik Ø. Sørensen, 12 Nov 2021 to remove some extraneous variables,
and the file created is now mmtalent_noweights.dta
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
label var agegroup "Age group"
label var age "Age (in years)"

// Removing survey technical variables.
drop status progress finished recordeddate responseid distributionchannel userlanguage ///
    consent uid epp eap epi eai redistribute

label define residence 1 "Alabama" 2 "Alaska" 3 "Arizona" 4"Arkansas" 5 "California" 6 "Colorado" ///
    7 "Connecticut" 8 "Delaware" 9 "DoC" 10 "Florida" 11 "Georgia" 12 "Hawaii" 13 "Idaho" ///
    14 "Illinois" 15 "Indiana" 16 "Iowa" 17 "Kansas" 18 "Kentucky" 19 "Louisiana" 20 "Maine" ///
    21 "Maryland" 22 "Massachusetts" 23 "Michigan" 24 "Minnesota" ///
    25 "Mississippi" 26 "Missouri" 27 "Montana" 28 "Nebraska" 29 "Nevada" 30 "New Hampshire" ///
    31 "New Jersey" 32 "New Mexico" 33 "New York" 34 "North Carolina" 35 "North Dakota" ///
    36 "Ohio" 37 "Oklahoma" 38 "Oregon" 39 "Pennsylvania" 40 "Puerto Rico" 41 "Rhode Island"  ///
    42 "South Carolina" 43 "South Dakota" 44 "Tennessee" 45 "Texas" 46 "Utah" 47 "Vermont" ///
    48 "Virginia" 49 "Washington" 50 "West Virginia"  51 "Wisconsin" 52 "Wyoming" ///
    53 "I do not live in the United States"
label values residence residence
label var residence "US state or territory"

gen region = .
replace region = 1 if inlist(residence, 7, 20, 22, 30, 41, 47, 31, 33, 39)
replace region = 2 if inlist(residence, 15, 14, 23, 36, 51, 16, 17, 24, 26, 28, 35, 43)
replace region = 3 if inlist(residence, 8, 9, 10, 11, 21, 34, 42, 48, 50, 1, 18, 25, 44, 4, 19, 37, 45)
replace region = 4 if inlist(residence, 3, 6, 13, 32, 27, 46, 29, 52, 2, 5, 12, 38, 49)

label define region 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label values region region
label var region "US Census region"


// Treatment 
gen byte treatment = 1 if exantepersonal==1
replace  treatment = 2 if exanteimpersonal==1
replace  treatment = 3 if expostpersonal==1
replace  treatment = 4 if expostimpersonal==1
// Personal dummy P
gen byte personal = inlist(treatment, 1, 3)
gen byte expost = inlist(treatment, 3,4)
label var treatment "Treatment"
label define treatment 1 "Ex Ante Personal" 2 "Ex Ante Impersonal" 3 "Ex Post Personal"  4 "Ex Post Impersonal"
label values treatment treatment
label var expost "Indicator Ex Post treatments" 
label define expost 0 "Ex Ante" 1 "Ex Post"
label values expost expost
label var personal "Indicator Personal treatments"
label define personal 0 "Impersonal" 1 "Personal"
label values personal personal
drop exantepersonal exanteimpersonal expostpersonal expostimpersonal  

label var gender "Indicator for female"
label define gender 0 "Male" 1 "Female"
label values gender gender 

label define edu 1 "No High School" 2 "High School/GED" 3 "Some college" 4 "Associate's Degree" 5 "Bachelor" 6 "Masters" 7 "Professional (JD/MD)" 8 "PhD"
label values edu edu 
label var edu "Education"

label define income 1 "Less than 29 999" 2 "30k - 59 999" 3 "60k - 99 999" 4 "100k - 149 999" 5 "150k+"
label values income income
label var income "Household's combined yearly income (gross, USD)"

label define tenscale 0 " Very fair" 10 "Very unfair"
label values luck_fair talent_fair  effort_fair tenscale
label var luck_fair "Is it fair if luck determines a person's income?"
label var talent_fair "Is it fair if talent determines a person's income?"
label var effort_fair "Is it fair if effort determines a person's income?"


label define fivescale 1 "Strongly Agree" 2 "Somewhat agree" 3 "Neither" 4 "Somewhat disagree" 5 "Strongly disagree"
label values redist_pref fivescale
label var redist_pref "Society should aim to equalize income"

label define polpref 1 "Very left wing" 2 "somewhat left wing" 3 "Neutral" 4 "Somewhat right wing" 5 "Very right wing"
label values polpref polpref
label var polpref "Political preference"

label define control 0 "Beyond control" 10 "Within control"
label values talent_control effort_control luck_control control
label var talent_control "Talent mainly reflects factors within individual control"
label var effort_control "Effort mainly reflects factors within individual control"
label var luck_control   "Luck mainly reflects factors within individual control"



label var paymenthighworker "Payment (USD) to high worker (decision)"
label var paymentlowworker  "Payment (USD) to low worker (decision)"

order startdate enddate durationinseconds treatment personal expost paymenthighworker paymentlowworker ///
    luck_fair talent_fair effort_fair ///
    luck_control talent_control effort_control ///
    redist_pref polpref ///
    gender age agegroup edu income residence region 
compress
save "mmtalent_noweights.dta", replace



