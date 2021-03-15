****
****
****	GESIS Replication File
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
use "gesis_data.dta", clear


********************************************************************************
** SECTION I: MAIN TEXT RESULTS

* Table 2: Question Wording and Descriptive Statistis of Depressive Symptoms
sum mean_depress
alpha depress*

* Figure 1 statistics (reported in text)
xtile dep_tile=mean_depress, n(5)		
tab dep_tile, sum(vote)
tab dep_tile if wave == 1, sum(motivation)

* Table 3: Estimated Coefficient and Standard Error for Depressive Symptoms
logit vote mean_depress female age education income health attend married union underemploy east 
reg motivation mean_depress female age education income health attend married union underemploy east if wave == 1

* Figure 2: Predicted Probability of Voting Across Depressive Symptoms
logit vote mean_depress female age education income health attend married union underemploy east 
margins, at(mean_depress = (1(.2)6)) atmean
matrix B = r(table)'
svmat B, names(gesis_vote)

egen seq1 = fill(1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6)
replace seq1 = . if _n > 26
egen seq2 = fill(0 2 4 6 8 0 2 4 6 8)
replace seq2 = . if _n > 26
replace seq2 = seq2/10
gen seq3 = seq1 + seq2

graph twoway (rspike gesis_vote5 gesis_vote6 seq3, lcolor(black)) ///
			(scatter gesis_vote1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle(" ") ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4 5 6, labsize(2.5)) ///
			title("GESIS") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(gesis_vote, replace)
						
* Figure 3: Predicted Level of Political Motivation Across Depressive Symptoms
reg motivation mean_depress female age education income health attend married union underemploy east if wave == 1
margins, at(mean_depress = (1(.2)6)) atmean
matrix B = r(table)'
svmat B, names(gesis_motivation)

graph twoway (rspike gesis_motivation5 gesis_motivation6 seq3, lcolor(black)) ///
			(scatter gesis_motivation1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle(" ") ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4 5 6, labsize(2.5)) ///
			title("GESIS") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(gesis_motivation, replace)



********************************************************************************
** SECTION II: APPENDIX RESULTS

**
** Appendix D: Descriptive Statistics

* Table D.2: German GESIS Panel
sum vote mean_depress inteff1 inteff2 polint female age education income health attend married union underemploy east


**
** Appendix E: Full Output of Regression Models

* Table E.1: Voter Turnout Models
logit vote mean_depress female age education income health attend married union underemploy east 

* Table E.2: Political Motivation Models
reg motivation mean_depress female age education income health attend married union underemploy east if wave == 1


**
** Appendix G: Post-Treatment Bias and Alternative Model Specifications 

* Table G.2: Result from Reanalysis of GESIS, BHPS, and Qualtrics Models

* No exclusion
logit vote mean_depress female age education income health attend married union underemploy east 
reg motivation mean_depress female age education income health attend married union underemploy east if wave == 1

* No income, health, or unemployment
logit vote mean_depress female age education attend married union east
reg motivation mean_depress female age education attend married union east if wave == 1

* Only income
logit vote mean_depress female age education attend married union east income
reg motivation mean_depress female age education attend married union east income if wave == 1

* Only health
logit vote mean_depress female age education attend married union east health
reg motivation mean_depress female age education attend married union east health if wave == 1

* Only unemployment
logit vote mean_depress female age education attend married union east underemploy
reg motivation mean_depress female age education attend married union east underemploy if wave == 1	


**
** Appendix H: Reanalysis of Political Motivation Index
xtset z000001a
xtreg motivation mean_depress female age education income health attend married union underemploy east wave, re
