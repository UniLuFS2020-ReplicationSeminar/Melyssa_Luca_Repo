****
****
****	BHPS Replication File
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
use "bhps_data.dta", clear
xtset pid


********************************************************************************
** SECTION I: MAIN TEXT RESULTS

* Table 2: Question Wording and Descriptive Statistics of Depressive Symptoms
sum mean_depress
alpha ghqa_r ghqb_r ghqc_r ghqd_r ghqe_r ghqf_r ghqg_r ghqh_r ghqi_r ghqj_r ghqk_r ghql_r

* Figure 1 statistics (reported in text) 
xtile dep_tile=mean_depress, n(5)
tab dep_tile if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 6, sum(vote7_r)	
tab dep_tile if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 6, sum(vote6_r)	

* Table 3: Estimated Coefficient and Standard Error for Depressive Symptoms
xtlogit vote7_r mean_depress female age education inc_log health attend married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16

xtologit vote6_r mean_depress female age education inc_log health married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16
	
* Figure 2: Predicted Probability of Voting Across Depressive Symptoms
xtlogit vote7_r mean_depress female age education inc_log health attend married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
margins, at(mean_depress = (1(.2)4)) atmean
matrix B = r(table)'
svmat B, names(bhps_vote)

egen seq1 = fill(1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4)
replace seq1 = . if _n > 16
egen seq2 = fill(0 2 4 6 8 0 2 4 6 8)
replace seq2 = . if _n > 16
replace seq2 = seq2/10
gen seq3 = seq1 + seq2

graph twoway (rspike bhps_vote5 bhps_vote6 seq3, lcolor(black)) ///
			(scatter bhps_vote1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle(" ") ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4, labsize(2.5)) ///
			title("BHPS") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(bhps_vote, replace)
						
* Figure 3: Predicted Level of Political Motivation Across Depressive Symptoms
xtreg vote6_r mean_depress female age education inc_log health married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16
margins, at(mean_depress = (1(.2)4)) atmean
matrix B = r(table)'
svmat B, names(bhps_motivation)

graph twoway (rspike bhps_motivation5 bhps_motivation6 seq3, lcolor(black)) ///
			(scatter bhps_motivation1 seq3, msymbol(O) mcolor(black) msize(vsmall)), ///
			legend(off) ///
			ytitle(" ") ///
			ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, labsize(2.5) angle(horizontal) nogrid) ///
			yline(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1, lcolor(gs14)) ///
			xtitle("Depressive" "Symptoms", size(4)) ///
			xlabel(1 2 3 4, labsize(2.5)) ///
			title("BHPS") ///
			scheme(s1mono) graphregion(fcolor(white)) ///
			yscale(titlegap(2)) ///
			xscale(titlegap(2)) ///
			name(bhps_motivation, replace)
			
			
********************************************************************************
** SECTION II: APPENDIX RESULTS			
			
**
** Appendix D: Descriptive Statistics

* Table D.3: British Household Panel Study
sum vote7_r mean_depress vote6_r female age education inc_log health attend married union unemploy if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16


**
** Appendix E: Full Output of Regression Models

* Table E.1: Voter Turnout Models
xtlogit vote7_r mean_depress female age education inc_log health attend married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
	
* Table E.2: Political Motivation Models
xtologit vote6_r mean_depress female age education inc_log health married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16

	
**
** Appendix F: Fixed Effects in the British Household Panel Study	
xtlogit vote7_r mean_depress female age education inc_log health married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, fe

xtreg vote6_r mean_depress female age education inc_log health married union unemploy election wave ///
	if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, fe
	
	
**
** Appendix G: Post-Treatment Bias and Alternative Model Specifications 

* Table G.2: Result from Reanalysis of GESIS, BHPS, and Qualtrics Models

* No exclusion
xtlogit vote7_r mean_depress female age education attend married union election wave inc_log health unemploy if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
xtreg vote6_r mean_depress female age education married union election wave inc_log health unemploy if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re

* No income, health, or unemployment
xtlogit vote7_r mean_depress female age education attend married union election wave if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
xtreg vote6_r mean_depress female age education married union election wave if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re

* Only income
xtlogit vote7_r mean_depress female age education attend married union election wave inc_log if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
xtreg vote6_r mean_depress female age education married union election wave inc_log if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re

* Only health
xtlogit vote7_r mean_depress female age education attend married union election wave health if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
xtreg vote6_r mean_depress female age education married union election wave health if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re

* Only unemployment
xtlogit vote7_r mean_depress female age education attend married union election wave unemploy if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re
xtreg vote6_r mean_depress female age education married union election wave unemploy if wave == 2 | wave == 3 | wave == 7 | wave == 8 | wave == 11 | wave == 12 | wave == 15 | wave == 16, re


**
** Appendix H: Reanalysis of Political Motivation Index
xtreg vote6_r mean_depress female age education inc_log health married union unemploy election wave, re
