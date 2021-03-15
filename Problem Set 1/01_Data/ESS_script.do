****
****
****	ESS Replication File
****
****
****	"Democracy and Depression: A Cross-National Study of Depressive Symptoms and Non-Participation"
****
****	Claudia Landwehr and Christopher Ojeda
****
****	American Political Science Review
****
****
****

* Read-in Data
set maxvar 15000
use "ess_data.dta", clear
svyset [weight=pspwght]


********************************************************************************
** SECTION I: MAIN TEXT RESULTS

* Table 2: Question Wording and Descriptive Statistics of Depressive Symptoms
sum depression [aweight=pspwght]
alpha fltdpr flteeff slprl wrhpp fltlnl enjlf fltsd cldgng

* Table 3: Estimated Coefficient and Standard Error for Depressive Symptoms
svy: logit vote depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: reg motivation depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum if wave == 0 
svy: fracreg logit phy_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum wave
svy: fracreg logit nop_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum wave

* Figure 1: Difference in Political Behavior Between Depressed Quintiles in ESS
* See code at end of file.

* Figure 2: Predicted Probability of Voting Across Depressive Symptoms	  
svy: logit vote depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
margins, at(depression = (1(.2)4)) atmean
matrix B = r(table)'
svmat B, name(ess_vote)

egen seq1 = fill(1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4)
replace seq1 = . if _n > 16
egen seq2 = fill(0 2 4 6 8 0 2 4 6 8)
replace seq2 = . if _n > 16
replace seq2 = seq2/10
gen seq3 = seq1 + seq2

graph twoway (rspike ess_vote5 ess_vote6 seq3, lcolor(black)) ///
			(scatter ess_vote1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle("Predicted Probability of Voting", size(4)) ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4, labsize(2.5)) ///
			title("ESS") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(ess_vote, replace)

* Figure 3: Predicted Level of Political Motivation Across Depressive Symptoms
svy: reg motivation depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum if wave == 0
margins, at(depression = (1(.2)4)) atmean
matrix B = r(table)'
svmat B, name(ess_motivation)

graph twoway (rspike ess_motivation5 ess_motivation6 seq3, lcolor(black)) ///
			(scatter ess_motivation1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle("Predicted Level of Motivation", size(4)) ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4, labsize(2.5)) ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			title("ESS") ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(ess_motivation, replace)


********************************************************************************
** SECTION II: APPENDIX RESULTS

**
** Appendix D: Descriptive Statistics

* Table D.1: European Social Survey
sum vote depression polcmpl poldcs polint gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 [aweight=pspwght]


**
** Appendix E: Full Output of Regression Models

* Table E.1: Voter Turnout Models
svy: logit vote depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum

* Table E.2: Political Motivation Models
svy: reg motivation depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum if wave == 0 

* Table E.3: Political Participation Models
svy: fracreg logit phy_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum wave
svy: fracreg logit nop_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum wave


**
** Appendix G: Post-Treatment Bias and Alternative Model Specifications 

* Table G.1: Results from Reanalysis of ESS Models

* No exclusion			
svy: logit vote depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: reg motivation depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum if wave == 0 
svy: fracreg logit phy_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit nop_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
		
* No income, health, or unemployment
svy: logit vote depression gndr agea eduyrs2 rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum		
svy: reg motivation depression gndr agea eduyrs2 rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit phy_act depression gndr agea eduyrs2 rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit nop_act depression gndr agea eduyrs2 rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum

* Only income
svy: logit vote depression gndr agea eduyrs2 income rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum		
svy: reg motivation depression gndr agea eduyrs2 income rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit phy_act depression gndr agea eduyrs2 income rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit nop_act depression gndr agea eduyrs2 income rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum

* Only health
svy: logit vote depression gndr agea eduyrs2 rlgblg married union_ever health dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum			
svy: reg motivation depression gndr agea eduyrs2 health rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit phy_act depression gndr agea eduyrs2 health rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit nop_act depression gndr agea eduyrs2 health rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum

* Only unemployment
svy: logit vote depression gndr agea eduyrs2 rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum			
svy: reg motivation depression gndr agea eduyrs2 unemploy rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit phy_act depression gndr agea eduyrs2 unemploy rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
svy: fracreg logit nop_act depression gndr agea eduyrs2 unemploy rlgblg married union_ever dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
	

**
** Appendix J: Predictions from the European Social Survey Models
	
* Table J.1: Predicted Probability of Voting
svy: logit vote depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 wave i.cnum
margins, at(depression = (1 4)) atmean
margins, at(age = (18 100)) atmean			
margins, at(eduyrs2 = (0 25)) atmean			
margins, at(income = (0 10)) atmean	
margins, at(health = (1 5)) atmean
margins, at(rlgblg = (0 1)) atmean
margins, at(married = (0 1)) atmean		
margins, at(union_ever = (0 1)) atmean
margins, at(unemploy = (0 1)) atmean

* Table J.2: Predicted Level of Political Motivation
svy: reg motivation depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum if wave == 0 
margins, at(depression = (1 4)) atmean
margins, at(gndr = (0 1)) atmean	
margins, at(age = (18 100)) atmean
margins, at(eduyrs2 = (0 25)) atmean						
margins, at(income = (0 10)) atmean	
margins, at(health = (1 5)) atmean	
margins, at(union_ever = (0 1)) atmean

* Table J.3: Predicted Probability of Physical Acts
svy: fracreg logit phy_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum wave
margins, at(depression = (1 4)) atmean
margins, at(gndr = (0 1)) atmean	
margins, at(age = (18 100)) atmean
margins, at(eduyrs2 = (0 25)) atmean						
margins, at(income = (0 10)) atmean	
margins, at(rlgblg = (0 1)) atmean
margins, at(married = (0 1)) atmean		
margins, at(union_ever = (0 1)) atmean
margins, at(unemploy = (0 1)) atmean

* Table J.4: Predicted Probability of Non-Physical Acts
svy: fracreg logit nop_act depression gndr agea eduyrs2 income health rlgblg married union_ever unemploy dispro_ess gdppc_ess1000 gini_ess1000 unemploy_ess1000 i.cnum wave
margins, at(depression = (1 4)) atmean
margins, at(age = (18 100)) atmean
margins, at(eduyrs2 = (0 25)) atmean						
margins, at(income = (0 10)) atmean	
margins, at(rlgblg = (0 1)) atmean	
margins, at(union_ever = (0 1)) atmean


********************************************************************************
** SECTION III: FIGURE 2

xtile dep_tile=depression, n(5)
keep if dep_tile == 1 | dep_tile == 5
collapse (mean) vote motivation phy_act nop_act [weight=dweight], by(cnum dep_tile)
gen cntry = " "
replace cntry = "AL" if cnum == 1
replace cntry = "BE" if cnum == 2
replace cntry = "BG" if cnum == 3
replace cntry = "CH" if cnum == 4
replace cntry = "CY" if cnum == 5
replace cntry = "CZ" if cnum == 6
replace cntry = "DE" if cnum == 7
replace cntry = "DK" if cnum == 8
replace cntry = "EE" if cnum == 9
replace cntry = "ES" if cnum == 10
replace cntry = "FI" if cnum == 11
replace cntry = "FR" if cnum == 12
replace cntry = "GB" if cnum == 13
replace cntry = "HU" if cnum == 14
replace cntry = "IE" if cnum == 15
replace cntry = "IL" if cnum == 16
replace cntry = "IS" if cnum == 17
replace cntry = "IT" if cnum == 18
replace cntry = "LT" if cnum == 19
replace cntry = "NL" if cnum == 20
replace cntry = "NO" if cnum == 21
replace cntry = "PL" if cnum == 22
replace cntry = "PT" if cnum == 23
replace cntry = "RU" if cnum == 24
replace cntry = "SE" if cnum == 25
replace cntry = "SI" if cnum == 26
replace cntry = "SK" if cnum == 27
replace cntry = "UA" if cnum == 28
replace cntry = "XK" if cnum == 29
replace cntry = "AT" if cnum == 30

gen c_name = " "
replace c_name = "Norway" if cntry == "NO"
replace c_name = "Denmark" if cntry == "DK"
replace c_name = "Iceland" if cntry == "IS"
replace c_name = "Sweden" if cntry == "SE"
replace c_name = "Finland" if cntry == "FI"
replace c_name = "Switzerland" if cntry == "CH"
replace c_name = "Ireland" if cntry == "IE"
replace c_name = "Netherlands" if cntry == "NL"
replace c_name = "Belgium" if cntry == "BE"
replace c_name = "Slovenia" if cntry == "SI"
replace c_name = "Cyprus" if cntry == "CY"
replace c_name = "Austria" if cntry == "AT"
replace c_name = "Israel" if cntry == "IL"
replace c_name = "France" if cntry == "FR"
replace c_name = "Germany" if cntry == "DE"
replace c_name = "Great Britain" if cntry == "GB"
replace c_name = "Spain" if cntry == "ES"
replace c_name = "Poland" if cntry == "PL"
replace c_name = "Italy" if cntry == "IT"
replace c_name = "Estonia" if cntry == "EE"
replace c_name = "Lithuania" if cntry == "LT"
replace c_name = "Slovakia" if cntry == "SK"
replace c_name = "Portugal" if cntry == "PT"
replace c_name = "Czech Republic" if cntry == "CZ"
replace c_name = "Bulgaria" if cntry == "BG"
replace c_name = "Kosovo" if cntry == "XK"
replace c_name = "Russia" if cntry == "RU"
replace c_name = "Hungary" if cntry == "HU"
replace c_name = "Ukraine" if cntry == "UA"
replace c_name = "Albania" if cntry == "AL"

reshape wide vote motivation phy_act nop_act, i(cnum) j(dep_tile)

* Vote Turnout Panel
gen vote_diff = vote1 - vote5
replace vote_diff = vote_diff*100
sort vote_diff
gen n_var = _n
browse c_name vote1 vote5 vote_diff 

graph twoway (dot vote_diff n_var, horizontal mlcolor(black) mfcolor(black) msymbol(o) dcolor(gs12)), ///
			 legend(off) ///
			 xlabel(-10 "-10%" 0 "0%" 10 "10%" 20 "20%" 30 "30%", labsize(2.5)) ///
			 xtitle("Difference Between" "Most/Least Depressed") ///
			 ytitle(" ") ///
			 title("Voter" "Turnout") ///
			 xline(0, lcolor(gs8) lpattern(dash)) ///
			 ylabel(1 "Lithuania" 2 "Ukraine" 3 "Kosovo" 4 "Albania" 5 "Russia" 6 "Portugal" 7 "Cyprus" 8 "Czech Republic" 9 "Bulgaria" 10 "Poland" ///
				11 "Belgium" 12 "Slovakia" 13 "Denmark" 14 "Finland" 15 "Israel" 16 "Spain" 17 "Austria" 18 "Sweden" 19 "Hungary" 20 "Netherlands" ///
				21 "Ireland" 22 "Iceland" 23 "France" 24 "Italy" 25 "Estonia" 26 "Norway" 27 "Slovenia" 28 "Great Britain" 29 "Switzerland" 30 "Germany", angle(horizontal) labsize(2.5)) ///
			 scheme(s1mono) graphregion(fcolor(white)) ///
			 xscale(titlegap(2)) ///
			 name(ess_vote_bivariate, replace)

* Political Motivation Panel
gen mot_diff = motivation1 - motivation5
sort mot_diff
gen n_var_mot = _n

graph twoway (dot mot_diff n_var_mot, horizontal mlcolor(black) mfcolor(black) msymbol(o) dcolor(gs12)), ///
			 legend(off) ///
			 xlabel(-.10 0 .10 .20, labsize(2.5)) ///
			 xtitle("Difference Between" "Most/Least Depressed") ///
			 ytitle(" ") ///
			 title("Political" "Motivation") ///
			 xline(0, lcolor(gs8) lpattern(dash)) ///
			 ylabel(1 "Kosovo" 2 "Albania" 3 "Czech Republic" 4 "Lithuania" 5 "Slovakia" 6 "Ukraine" 7 "Ireland" 8 "Sweden" 9 "Russia" 10 "Norway" ///
					11 "Estonia" 12 "Israel" 13 "Poland" 14 "Italy" 15 "Hungary" 16 "Finland" 17 "Bulgaria" 18 "Great Britain" 19 "France" ///
					20 "Denmark" 21 "Austria" 22 "Netherlands" 23 "Spain" 24 "Cyprus" 25 "Slovenia" 26 "Belgium" 27 "Iceland" 28 "Germany" ///
					29 "Switzerland" 30 "Portugal", angle(horizontal) labsize(2.5)) ///
			 scheme(s1mono) graphregion(fcolor(white)) ///
			 xscale(titlegap(2)) ///
			 name(ess_motivation_bivariate, replace)
			 	 
* Political Participation Panel
gen phy_diff = phy_act1 - phy_act5
gen nop_diff = nop_act1 - nop_act5
sort phy_diff
gen n_var_phy = _n

graph twoway (dot phy_diff n_var_phy, horizontal mlcolor(black) mfcolor(black) msymbol(o) dcolor(gs12)) ///
			(dot nop_diff n_var_phy, horizontal mlcolor(black) mfcolor(white) msymbol(o) dcolor(gs12)) ///
			(scatter n_var_phy phy_diff, mlcolor(black) mfcolor(black) msymbol(o)), ///
			 legend(order(1 2) label(1 "Physical") label (2 "Non-Physical") rows(2) symxsize(4) size(small)) ///
			 xlabel(-.10 -.05 0 .05 .10, labsize(2.5)) ///
			 xtitle("Difference Between" "Most/Least Depressed") ///
			 ytitle(" ") ///
			 xline(0, lcolor(gs8) lpattern(dash)) ///
			 ylabel(1 "Albania" 2 "Lithuania" 3 "Kosovo" 4 "Russia" 5 "Czech Republic" 6 "Cyprus" 7 "Ukraine" 8 "Slovakia" 9 "Portugal" ///
					10 "Bulgaria" 11 "Poland" 12 "Israel" 13 "Sweden" 14 "Ireland"15 "Spain" 16 "Hungary" 17 "Estonia" 18 "Finland" 19 "Great Britain" ///
					20 "France" 21 "Belgium" 22 "Netherlands" 23 "Slovenia" 24 "Denmark" 25 "Austria" 26 "Italy" 27 "Norway" 28 "Switzerland" ///
					29 "Iceland" 30 "Germany", angle(horizontal) labsize(2.5)) ///
			title("Political" "Acts") ///
			 scheme(s1mono) graphregion(fcolor(white)) ///
			 xscale(titlegap(2)) ///
			 name(ess_acts_bivariate, replace)
			 
* Graph Combine			 
graph combine ess_vote_bivariate ess_motivation_bivariate ess_acts_bivariate, row(1) graphregion(fcolor(white))	 
