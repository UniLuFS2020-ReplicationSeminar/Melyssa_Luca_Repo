
**computing / re-coding variables / descriptives (recoded education due to a coding error in the survey) (used for demographic table in SI-1)
recode education 1/1 = 1 2/2 = 2 3/3 = 3 6/6 = 3 4/4 = 4 5/5 = 5 

tab age 
tab gender 
tab race
tab income
tab education 

gen female = gender
recode female 1/1 = 0 2/2 = 1 3/4 = 0
gen minority = race
recode minority 1/1 = 0 2/6 = 1
gen afam = race
recode afam 1/1 = 0 2/2 = 1 3/6 = 0
gen lat = race
recode lat 1/3 = 0 4/4 = 1 5/6 = 0
gen asian = race
recode asian 1/2 = 0 3/3 = 1 4/6 = 0

sum afam lat asian

**political Knowledge
gen p_knowledge_vetob  = p_knowledge_veto
gen p_knowledge_hofrb = p_knowledge_hofr
gen p_knowledge_constitutionb  = p_knowledge_constitution
gen p_knowledge_vpb = p_knowledge_vp
gen p_knowledge_conservativeb  = p_knowledge_conservative

recode p_knowledge_vetob 0/3 = 0 4/4 = 1 5/6 = 0 ./. = 0
recode p_knowledge_hofrb 1/1 = 1 2/4 = 0 ./. = 0
recode p_knowledge_constitutionb 1/2 = 0 3/3 = 1 4/4 = 0 ./. = 0
recode p_knowledge_vpb 1/2 = 0 3/3 = 1 4/5= 0 ./. = 0
recode p_knowledge_conservativeb 1/1 = 0 2/2 = 1 3/4 = 0 ./. = 0
gen poliknow = p_knowledge_vetob + p_knowledge_hofrb + p_knowledge_constitutionb + p_knowledge_vpb + p_knowledge_conservativeb
sum poliknow 

**risk sick (health vulnerability) and work out of home and Fox
gen childunder4b = childunder4
recode childunder4b 1/1 = 0 2/2 =1
gen pregnantb = pregnant
recode pregnantb 1/1 = 0 2/2 =1
gen riskhealthb = riskhealth
recode riskhealthb 1/1 = 0 2/2 =1 
gen agerisk = age
recode agerisk 1/5 = 0 6/6 = 1

gen risksick0 = agerisk + riskhealthb + childunder4b + pregnantb
tab risksick0
gen risksick = risksick0
recode risksick 0/0 = 0 1/4 = 1

gen workout = 0
recode workout 0/0 = 1 if workleavehome != . & workleavehome > 1

gen tvnews_fox2 = tvnews_fox
recode tvnews_fox2 1/1 = 0 2/2 = 1

**cases
gen casespercapita = day3_avg_cases/countypop

**creating out party scale
alpha feelingtherm_outgroup patriotic_outgroup intelligent_outgroup honest_outgroup openminded_outgroup generous_outgroup hypocritical_outgroup selfish_outgroup mean_outgroup trust_outgroup friends_outgroup neighbors_outgroup marry_outgroup if pid ~= 4
gen allout2=(feelingtherm_outgroup +patriotic_outgroup +intelligent_outgroup +honest_outgroup +openminded_outgroup +generous_outgroup +hypocritical_outgroup + selfish_outgroup +mean_outgroup +trust_outgroup +friends_outgroup +neighbors_outgroup +marry_outgroup)/13
corr allout allout2
gen negparty = (1-allout)
sum negparty

**partisan social identity
alpha party_importance party_describe  party_wethey party_extent
egen psociden = rowmean( party_importance party_describe  party_wethey party_extent)
sum psociden
gen repsociden = rep*psociden
gen demsociden = dem*psociden

***correlating PID and therms across waves
corr feelingtherm_outgroup covfeelingtherm_outgroup if pid !=4 

**interactions
gen repnegparty = rep*negparty
sum negparty repnegparty if  pid !=4 
gen demnegparty = dem*negparty

**experiment analysis
gen expUS = exptrump
recode expUS 0/0 =1 1/1 = 0

sum expconf expprep expfuture if pid !=4 
gen flipexpprep = 5- expprep
gen flipexpfuture = 5 - expfuture
sum expconf flipexpprep flipexpfuture if pid !=4 
alpha expconf flipexpprep flipexpfuture

egen expdv = rowmean(expconf flipexpprep flipexpfuture)

gen expdemnegparty = expUS*demnegparty
gen exprepnegparty = expUS*repnegparty

**regressions for expeiment (Table 1)
regress expdv expUS  if pid!=4 & dem == 1
outreg2 using covidexp, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats") word append 
regress expdv expUS demnegparty expdemnegparty  if pid!=4 & dem == 1
outreg2 using covidexp, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats") word append 
regress expdv expUS  if pid!=4 & rep == 1 
outreg2 using covidexp, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans") word append 
regress expdv expUS  repnegparty exprepnegparty  if pid!=4 & rep == 1 
outreg2 using covidexp, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans") word append 

 
**regressoins for each outcome varaible (Table S3-1)
regress expconf expUS    if pid!=4 & dem == 1
outreg2 using covidexp2, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats Conf") word append 
regress expconf expUS     if pid!=4 & rep == 1 
outreg2 using covidexp2, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans Conf") word append 

regress flipexpprep expUS    if pid!=4 & dem == 1
outreg2 using covidexp2, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats Prep") word append 
regress flipexpprep expUS     if pid!=4 & rep == 1 
outreg2 using covidexp2, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans Prep") word append 

regress flipexpfuture expUS    if pid!=4 & dem == 1
outreg2 using covidexp2, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats Future") word append 
regress flipexpfuture expUS     if pid!=4 & rep == 1
outreg2 using covidexp2, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans Future") word append 

**Table S3-2
regress expconf expUS demnegparty expdemnegparty  if pid!=4 & dem == 1
outreg2 using covidexp3, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats Conf") word append 
regress expconf expUS  repnegparty exprepnegparty  if pid!=4 & rep == 1 
outreg2 using covidexp3, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans Conf") word append 

regress flipexpprep expUS demnegparty expdemnegparty  if pid!=4 & dem == 1 
outreg2 using covidexp3, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats Prep") word append 
regress flipexpprep expUS  repnegparty exprepnegparty  if pid!=4 & rep == 1
outreg2 using covidexp3, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans Prep") word append 

regress flipexpfuture expUS demnegparty expdemnegparty  if pid!=4 & dem == 1
outreg2 using covidexp3, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats Future") word append 
regress flipexpfuture expUS  repnegparty exprepnegparty  if pid!=4 & rep == 1 
outreg2 using covidexp3, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans Future") word append 


**Main Regressions with controls (Table S3-3)
regress expdv expUS casespercapita risksick workout  afam lat asian female ideology  age education poliknow income  tvnews_fox2  if dem == 1 & pid!=4
outreg2 using covidexp4, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats") word append 
regress expdv expUS demnegparty expdemnegparty  casespercapita risksick workout  afam lat asian female ideology  age education poliknow income  tvnews_fox2  if dem == 1 & pid!=4 
outreg2 using covidexp4, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats") word append 
regress expdv expUS  casespercapita risksick workout  afam lat asian female ideology age education poliknow income  tvnews_fox2  if rep == 1 & pid!=4
outreg2 using covidexp4, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans") word append 
regress expdv expUS  repnegparty exprepnegparty casespercapita risksick workout  afam lat asian female ideology age education poliknow income  tvnews_fox2  if rep == 1 & pid!=4 
outreg2 using covidexp4, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans") word append 


**partisanship as social identity (Table 2)
gen expdemsocial = expUS*demsociden
gen exprepsocial = expUS*repsociden


regress expdv expUS demsociden expdemsocial  if pid!=4 & dem == 1 
outreg2 using covidexp5, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Democrats") word append 
regress expdv expUS  repsociden exprepsocial  if pid!=4 & rep == 1
outreg2 using covidexp5, alpha(.01, .05, .1) bdec(3) sdec(3) ctitle("Republicans") word append 

***predictions (Figure 1)
regress expdv expUS demnegparty c.expUS##c.demnegparty  if pid!=4 & dem == 1 
margins, at(expUS=(0 1) demnegparty=(0 1))


regress expdv expUS repnegparty c.expUS##c.repnegparty  if pid!=4 & rep == 1 
margins, at(expUS=(0 1) repnegparty=(0 1))


sum *
