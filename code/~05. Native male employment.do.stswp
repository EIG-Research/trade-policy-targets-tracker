
set more off
clear all

* LAST EDIT DATE: 04.10.2025
* LAST EDITOR: sarah@eig.org

* monthly cps samples 1994 - most recent month


********************************************************************************
* employment levels for native born men 25-54
* use /Users/sarah/Documents/GitHub/trade-policy-targets-tracker/data/cps_00030.dta, clear
use /Users/sarah/Downloads/cps_00030.dta, clear

* men
keep if sex == 1

* in labor force
keep if inlist(empstat, 10, 12) // at work, or has job not at work last week

* native born
keep if nativity != 5 // drop foreign born individuals

* prime age workers only
keep if age >=25 & age <= 54

* get employment counts by year, person-level weights
g counter = 1
collapse(sum) employment_level = counter [pw = wtfinl], by(year month)

format employment_level %12.0f

	* quaraterly averages
	g quarter = ""
	replace quarter = "Q1" if inlist(month, 1,2,3)
	replace quarter = "Q2" if inlist(month, 4,5,6)
	replace quarter = "Q3" if inlist(month, 7,8,9)
	replace quarter = "Q4" if inlist(month, 10,11,12)
	
	drop if year == 2025 // data goes through Feb
	
collapse(mean) employment_level, by(year quarter)	
format employment_level %12.0f

g employment_level_in_thousands = employment_level/1000

export excel /Users/sarah/Downloads/employment_prime_age_native_men.xlsx, firstrow(variables) replace

/*
********************************************************************************
* employment rate, native born men
*use /Users/sarah/Documents/GitHub/trade-policy-targets-tracker/data/cps_00030.dta, clear
use /Users/sarah/Downloads/cps_00030.dta, clear

keep if sex==1
keep if nativity != 5
keep if age >=16

g employed = 1*(inlist(empstat, 10, 12))

g counter = 1

collapse(sum) counter [pw = wtfinl], by(year month employed)

bysort year month: egen population = sum(counter)

g employment_rate = counter/population

* get quarterly averages

	* quaraterly averages
	g quarter = ""
	replace quarter = "Q1" if inlist(month, 1,2,3)
	replace quarter = "Q2" if inlist(month, 4,5,6)
	replace quarter = "Q3" if inlist(month, 7,8,9)
	replace quarter = "Q4" if inlist(month, 10,11,12)
	
	keep if employed ==1
	
collapse(mean) employment_rate, by(year quarter)
	
export excel /Users/sarah/Downloads/employment_pop_ratio_age_native_men.xlsx, firstrow(variables) replace


