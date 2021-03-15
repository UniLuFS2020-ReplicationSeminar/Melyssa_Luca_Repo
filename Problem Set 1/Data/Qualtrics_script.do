****
****
****	Qualtrics Replication File
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
use "qualtrics_data.dta", clear


********************************************************************************
** SECTION I: MAIN TEXT RESULTS

* Table 2: Question Wording and Descriptive Statistis of Depressive Symptoms
sum depression
alpha depress1-depress20

* Figure 1 statistics (reported in text) 
xtile dep_tile=depression, n(5)		
tab dep_tile, sum(vote18)
tab dep_tile, sum(motivation)

* Table 3: Estimated Coefficient and Standard Error for Depressive Symptoms
logit vote18 depression female age educ income attend married unemployed black hispanic
reg motivation depression female age educ income attend married unemployed black hispanic

* Figure 2: Predicted Probability of Voting Across Depressive Symptoms
logit vote18 depression female age educ income attend married unemployed black hispanic
margins, at(depression = (1(.2)4)) atmean
matrix B = r(table)'
svmat B, names(qualtrics_vote)

egen seq1 = fill(1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4)
replace seq1 = . if _n > 16
egen seq2 = fill(0 2 4 6 8 0 2 4 6 8)
replace seq2 = . if _n > 16
replace seq2 = seq2/10
gen seq3 = seq1 + seq2

graph twoway (rspike qualtrics_vote5 qualtrics_vote6 seq3, lcolor(black)) ///
			(scatter qualtrics_vote1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle(" ") ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4, labsize(2.5)) ///
			title("Qualtrics") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(qualtrics_vote, replace)
			
* Figure 3: Predicted Level of Political Motivation Across Depressive Symptoms
reg motivation depression female age educ income attend married unemployed black hispanic
margins, at(depression = (1(.2)4)) atmean
matrix B = r(table)'
svmat B, names(qualtrics_motivation)

graph twoway (rspike qualtrics_motivation5 qualtrics_motivation6 seq3, lcolor(black)) ///
			(scatter qualtrics_motivation1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle(" ") ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4, labsize(2.5)) ///
			title("Qualtrics") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(qualtrics_motivation, replace)
			

********************************************************************************
** SECTION II: APPENDIX RESULTS

**
** Appendix D: Descriptive Statistics

* Table D.4: Qualtrics
sum vote18 depression inteff1 inteff2 polint female age educ income attend married unemployed black hispanic


**
** Appendix E: Full Output of Regression Models

* Table E.1: Voter Turnout Models
logit vote18 depression female age educ income attend married unemployed black hispanic

* Table E.2: Political Motivation Models
reg motivation depression female age educ income attend married unemployed black hispanic


**
** Appendix G: Post-Treatment Bias and Alternative Model Specifications 

* Table G.2: Result from Reanalysis of GESIS, BHPS, and Qualtrics Models

* No exclusion
logit vote18 depression female age educ income attend married unemployed black hispanic
reg motivation depression female age educ income attend married unemployed black hispanic

* No income, health, or unemployment
logit vote18 depression female age educ attend married black hispanic
reg motivation depression female age educ attend married black hispanic

* Only income
logit vote18 depression female age educ attend married black hispanic income
reg motivation depression female age educ attend married black hispanic income

* Only unemployment
logit vote18 depression female age educ attend married black hispanic unemployed
reg motivation depression female age educ attend married black hispanic unemployed
