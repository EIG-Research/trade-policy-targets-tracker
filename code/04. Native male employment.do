
set more off
clear all

* LAST EDIT DATE: 04.17.2025
* LAST EDITOR: sarah@eig.org

* monthly cps samples 1994 - most recent month

********************************************************************************
* employment levels
use /Users/sarah/Downloads/cps_00031.dta, clear

* in labor force
keep if inlist(empstat, 10, 12) // at work, or has job not at work last week

* native born
keep if nativity != 5 // drop foreign born individuals

* 18-64 workers
keep if age >=16

* get employment counts by year, person-level weights
g counter = 1
collapse(sum) employment_level = counter [pw = wtfinl], by(year month)

format employment_level %12.0f

/*	* quaraterly averages
	g quarter = ""
	replace quarter = "Q1" if inlist(month, 1,2,3)
	replace quarter = "Q2" if inlist(month, 4,5,6)
	replace quarter = "Q3" if inlist(month, 7,8,9)
	replace quarter = "Q4" if inlist(month, 10,11,12)
*/	
*collapse(mean) employment_level, by(year quarter)	
format employment_level %12.0f 

export excel /Users/sarah/Downloads/employment_level_native.xlsx, firstrow(variables) replace

********************************************************************************
* employment rate, native born men
use /Users/sarah/Downloads/cps_00031.dta, clear

keep if nativity != 5
keep if age >=16
g employed = 1*(inlist(empstat, 10, 12))

g counter = 1

collapse(sum) counter [pw = wtfinl], by(year month employed)

bysort year month: egen population = sum(counter)

g employment_rate = counter/population

* get quarterly averages

/*	* quaraterly averages
	g quarter = ""
	replace quarter = "Q1" if inlist(month, 1,2,3)
	replace quarter = "Q2" if inlist(month, 4,5,6)
	replace quarter = "Q3" if inlist(month, 7,8,9)
	replace quarter = "Q4" if inlist(month, 10,11,12)
*/
	
	keep if employed ==1

keep year month employment_rate
*collapse(mean) employment_rate, by(year quarter)
	
export excel /Users/sarah/Downloads/employment_rate_native.xlsx, firstrow(variables) replace


