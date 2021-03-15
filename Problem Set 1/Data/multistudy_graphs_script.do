****
****
****	Multi-Study Graphs Replication File
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

*** All other .do files must be executed before the following commands can be run: ***

* Figure 2: Predicted Probability of Voting	
graph combine ess_vote gesis_vote bhps_vote qualtrics_vote, row(1) graphregion(fcolor(white))
		
* Figure 3: Predicted Values of Political Motivation	
graph combine ess_motivation gesis_motivation bhps_motivation qualtrics_motivation, ycommon row(1) graphregion(fcolor(white))
