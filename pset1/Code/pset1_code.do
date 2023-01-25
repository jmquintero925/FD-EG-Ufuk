***************************************************************************************
***************************************************************************************
*********** Problem Set 1: Firm Dynamics and Economic Growth  *************************
*********** Author: Jose M. Quintero H.					      *************************
*********** Professor: Ufuk Akcigit 						  *************************
*********** TA: Furkan 								  *************************
***************************************************************************************	
***************************************************************************************

* Housekeeping 
clear all
set more off
cd "/Users/josemiguelquinteroholguin/Library/CloudStorage/Dropbox/UChicago/2nd Year Classes/Firm Dynamics and Econ Growth/pset1"

* Paths
global figs "Figures"
global data "Data"
global tables "Tables"



* Load compustat data
use "$data/ou4cvvrlsxcattnz.dta", clear

*-------------------------------------------------*
*--------------- Sample selection ----------------*
*-------------------------------------------------*

* Interpreting and as intersection
keep if (fic=="USA" & curcd=="USD") & (fyear>=1980 & fyear<=2001)
* Generate duplicates tag for variables not uniquely identifie
duplicates tag fyear gvkey, gen(num_dup)
duplicates report fyear gvkey
* Count how many missings does it has
egen miss_data = rowmiss(exchg emp sale xrd)
* By each observation determine whats the lowest missing value possible
bys gvkey fyear: egen min_miss = min(miss_data)
* Create a dummy if the observation is repeated but it is not the one with the 
* information
gen del = (num_dup>0 & miss_data!=min_miss)
tab del 
drop if del==1
* Finish cleaning duplicates
drop num_dup
duplicates tag fyear gvkey, gen(num_dup)
drop if num_dup>0 & indfmt=="FS"
drop num_dup miss_data min_miss del
duplicates report fyear gvkey
* Make gvkey numeric variable
destring gvkey, replace 
* Save teporal data base 
save temp.dta, replace 

*************************************************
*************************************************

* Load patent data 
use "$data/pat76_06_assg.dta", clear

* Count number of assignees for each patent
by patent: egen nass = count(pdpass)
* Calculate fractional patent ownership
gen npat = 1/nass
* Generate period of interest
gen period = (appyear>=1980) & (appyear<=2001)
* Sort 
sort pdpass appyear
* Merge using dynass data base
merge m:1 pdpass using $data/dynass.dta
keep if _m==3

/************************************************
*************************************************

    Result                           # of obs.
    -----------------------------------------
    not matched                     1,938,314
        from master                 1,938,256  (_merge==1)
        from using                         58  (_merge==2)

    matched                         1,341,253  (_merge==3)
    -----------------------------------------

*************************************************
*************************************************/

* Find the appropriate gvkey to assign the patents
gen gvkey=.

* Loop over the different gvkey
forvalues i=1/5 {
	* Assign the code if the application date is within the year
	replace gvkey = gvkey`i' if (gvkey`i'~=.) & (appyear>=begyr`i') & (appyear<=endyr`i')
}

* Keep only observations I can match 
keep if gvkey~=.
sort gvkey appyear
* Keep only observations within period of interest
keep if period ==1

* Sum over multiple assignees to get patents for each company
collapse (sum) npat, by(gvkey appyear)
* Rename time variable to match compustat variable 
rename appyear fyear
* Merge to compustat data
merge 1:1 gvkey fyear using temp.dta 

/************************************************
*************************************************

    Result                           # of obs.
    -----------------------------------------
    not matched                       161,687
        from master                     2,940  (_merge==1)
        from using                    158,747  (_merge==2)

    matched                            25,459  (_merge==3)
    -----------------------------------------

*************************************************
*************************************************/

* Drop patents that do not show in compustat
drop if _m==1
drop _m
erase temp.dta 

* Merge with Corporate entity file matched to WRDS Compustat file
sort gvkey fyear
merge m:1 gvkey using "$data/pdpcohdr.dta"
* Drop if did not match from using
drop if _m==2
drop _m

* Deal with non patenting firms 
replace npat = 0 if match~=. & npat==.

* Calculate the number of patent per firms across period
bys gvkey: gen totPats = sum(npat)  if fyear<2001
bys gvkey: egen pats = max(totPats) 
drop totPats

* Find innovative firms
gen innovative = (pats>=1)

* Export to table 
tempname dStatistics 
* Descriptive statistics 
foreach vars of var emp sale xrd innovative{ 
	* Summarize variable 
	quietly sum `vars', d
	* Add to matrix 
	mat `dStatistics' = (nullmat(`dStatistics')\(`r(N)',`r(mean)',`r(sd)',`r(min)',`r(p50)',`r(max)'))
}

* Export matrix as table
frmttable using "$tables/tab0", tex replace s(`dStatistics') sd(3) ///
	ctitles("Variable", "Obs.", "Mean", "Std. Dev.", "Min", "Median" ,"Max") ///
    rtitles("Employment" \ "Sales" \ "R\&D" \ "Innovative") hlines(101{0}1) fragment 


* Differences between innovative and non innovative 
tempname compTab 
* Mean difference test for variables of interest 
ttest emp, by(innovative)
* Add to table 
local m_diff = `r(mu_2)'-`r(mu_1)'
mat `compTab' = (nullmat(`compTab'),(`r(N_1)',`r(mu_1)',`r(sd_1)',`r(N_2)',`r(mu_2)',`r(sd_2)',`m_diff',`r(p)'))
* Mean difference test for variables of interest 
ttest sale, by(innovative)
* Add to table 
local m_diff = `r(mu_2)'-`r(mu_1)'
mat `compTab' = (`compTab'\(`r(N_1)',`r(mu_1)',`r(sd_1)',`r(N_2)',`r(mu_2)',`r(sd_2)',`m_diff',`r(p)'))
* Mean difference test for variables of interest 
ttest xrd, by(innovative)
* Add to table 
local m_diff = `r(mu_2)'-`r(mu_1)'
mat `compTab' = (`compTab'\(`r(N_1)',`r(mu_1)',`r(sd_1)',`r(N_2)',`r(mu_2)',`r(sd_2)',`m_diff',`r(p)'))

* Export matrix as table
frmttable using "$tables/tab1", tex replace s(`compTab') sd(3) ///
	ctitles("", "Not Innovative", "", "","Innovative", "", "", "", "" \ "","Obs", "Mean", "Std. Dev.","Obs", "Mean", "Std. Dev.","$\mu_1-\mu_0$","$ p$-value") ///
    multicol(1,2,3;1,5,3) rtitles("Employment" \ "Sales" \ "R\&D") hlines(101{0}1) fragment 


*-------------------------------------------------*
*--------- Firm Growth by Firm Size --------------*
*-------------------------------------------------*

* Generate employment growth
sort gvkey fyear
bys gvkey: gen empgr  = (emp[_n+1] - emp[_n])/(emp[_n])
bys gvkey: gen empgr2 = (emp[_n+1] - emp[_n])/(0.5*(emp[_n]+emp[_n+1]))
drop if fyear==2001

* Truncate above growth
replace empgr  = min(empgr,1000) if empgr!=.
* Find moment when the firm closed
cap gen last = gvkey[_n]!=gvkey[_n+1]
* Replace growth by -1 when it closed
replace empgr  = -1 if (last & fyear!=2000) 
replace empgr2 = -2 if (last & fyear!=2000) 

* Figure 1.b
quietly sum emp if innovative, d
binscatter empgr emp if innovative & emp!=., mc(gray) linetype(connect) msymbols(o) colors(maroon) ylabel(,nogrid format(%3.2f) labs(small) angle(0)) ///
	xlabel(`r(p10)' `r(p25)' `r(p50)' `r(p75)' `r(p90)' `r(p99)', angle(90) format(%3.2f) labs(small)) xscale(log) ///
	ytitle("Forward employment growth") xtitle("Average employee count in size bin") 
	graph export "$figs/Figure1b.pdf", replace
	
	
* Regression 1.b

* Industry
gen  inds = substr(sic,1,2)
egen inds_time_fe = group(inds fyear)

* Generate employment log
gen log_emp = log(emp)
reg empgr log_emp i.inds_time_fe if innovative, nocons cluster(gvkey)
outreg2 using "$tables/reg1.tex", replace dec(4) keep(log_emp)
	

* Figure 1.c
quietly sum emp if innovative, d
binscatter empgr2 emp if innovative & ~last, mc(gray) linetype(connect) msymbols(o) colors(maroon) ylabel(,nogrid format(%3.2f) labs(small) angle(0)) ///
	xlabel(`r(p10)' `r(p25)' `r(p50)' `r(p75)' `r(p90)' `r(p99)', angle(90) format(%3.2f) labs(small)) xscale(log) ///
	ytitle("Forward employment growth") xtitle("Average employee count in size bin") 
	graph export "$figs/Figure1c.pdf", replace

*-------------------------------------------------*
*-------- Innovation Intensity by Firm Size ------*
*-------------------------------------------------*

* When was the first patent
gen run = cond(fyear==1980,0,.) if innovative
by gvkey: replace run 	= cond(npat==0 & fyear!=1980, run[_n-1] + 1, 0,0) if innovative
by gvkey: gen l_run 	= cond(run==0,run[_n-1],.) if innovative
by gvkey: egen m_run 	= mean(l_run) if innovative

* Check how lumpy patenting is
xtset gvkey fyear
xtsum npat m_run if innovative

* Insert here exporting table code
gen period = floor((fyear-1980)/5)+1
replace period = 4 if period==5

* Preserve
preserve 
* Collapse within windows
collapse (sum) npat (mean) emp (first) inds, by(gvkey period innovative)
* Gen patent intensity 
gen pat_emp = npat/(emp) if innovative
* Generate standarized version 
egen pat_emp_std = std(pat_emp) if innovative
* Generate log employment for regression
gen log_emp = log(emp)

* Figure 4.b
quietly quietly sum emp if innovative, d
binscatter pat_emp emp if innovative& emp!=., mc(gray) linetype(connect) msymbols(o) colors(maroon) ylabel(,nogrid format(%3.0f) labs(small) angle(0)) ///
	xlabel(`r(p10)' `r(p25)' `r(p50)' `r(p75)' `r(p90)' `r(p99)', angle(90) format(%3.2f) labs(small)) xscale(log) ///
	ytitle("Patents per employee") xtitle("Average employee count in size bin") 
	graph export "$figs/Figure4b.pdf", replace
	
* Generate industry time fix effects	
egen inds_time_fe = group(inds period)
* Run regression
reg pat_emp_std log_emp i.inds_time_fe if innovative, nocons cluster(gvkey)
* Export results
outreg2 using "$tables/reg2.tex", replace dec(4) keep(log_emp)

* Restore data
restore

*-------------------------------------------------*
*---------- R&D Intensity by Firm Size -----------*
*-------------------------------------------------*

* Generate log R&D expenditure
gen l_rd = log(xrd)

xtset gvkey fyear
gen l_sale = log(sale)

 * Loop over years
 forvalues y = 1981(1)2000 {
	 *Run regressions fixing year
	 areg l_rd l.l_sale if (innovative & fyear==`y'), absorb(inds) vce(cluster gvkey)
	 * Save results
	 estimate store m`y'
 }

 * Figure 5.b
 coefplot 	(m1981, rename(L.l_sale = 1981) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1982, rename(L.l_sale = 1982) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1983, rename(L.l_sale = 1983) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1984, rename(L.l_sale = 1984) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1985, rename(L.l_sale = 1985) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1986, rename(L.l_sale = 1986) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1987, rename(L.l_sale = 1987) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1988, rename(L.l_sale = 1988) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1989, rename(L.l_sale = 1989) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1990, rename(L.l_sale = 1990) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1991, rename(L.l_sale = 1991) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1992, rename(L.l_sale = 1992) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1993, rename(L.l_sale = 1993) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1994, rename(L.l_sale = 1994) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1995, rename(L.l_sale = 1995) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1996, rename(L.l_sale = 1996) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1997, rename(L.l_sale = 1997) citop ciopts(lc(maroon)) mc(gray) mlc(black)) (m1998, rename(L.l_sale = 1998) citop ciopts(lc(maroon)) mc(gray) mlc(black)) ///
			(m1999, rename(L.l_sale = 1999) citop ciopts(lc(maroon)) mc(gray) mlc(black)), vertical drop(_cons) plotregion(fcolor(white)) graphregion(fcolor(white)) ///
			ylabel (,nogrid format(%3.2f) angle(0)) xtitle("Year") legend(off) xlabel(0 "1980" 1 "1981" 2 "1982" 3 "1983" 4 "1984" 5 "1985" 6 "1986" 7 "1987" 8 "1988" 9 "1989" ///
			 10 "1990" 11 "1991" 12 "1992" 13 "1993" 14 "1994" 15 "1995" 16 "1996" 17 "1997" 18 "1998" 19 "1999" 20 "2000",angle(90)) ytitle("Regression coefficient") 
* Save graph 	  
 graph export "$figs/Figure5b.pdf", replace




