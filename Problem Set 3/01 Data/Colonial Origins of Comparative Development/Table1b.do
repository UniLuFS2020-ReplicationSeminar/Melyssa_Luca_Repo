capture log close
clear
clear matrix
clear mata
set more off, permanently
set mem 2000

log using Table1b, replace

***----Includes user-written programs for outsheeting the results of condivreg and rivtest to a table of the desired format. User-written commands starting with "parse" extract the relevant output from Stata objects so that it can be formatted appropriately for export to a table.

***----Program for parsing the output of rivtest

capture program drop parse_rivtest
program define parse_rivtest
	g temp = "`r(ar_cset)'"
	replace temp=trim(temp)
	*quietly replace temp = regexr(temp, "^[/[]", "")
	replace temp=subinstr(temp, "]", "", .)
	replace temp=subinstr(temp, "[", "", .)
	egen AR1=ends(temp), punct(,) head
	egen AR2=ends(temp), punct(,) last
	destring AR1, replace
	destring AR2, replace
	summ AR1
	global AR1 = `r(mean)'
	summ AR2
	global AR2 = `r(mean)'
	drop temp AR1 AR2

end

***----Program for preparing the first-stage clustered F-stat for export

capture program drop parse_FSF
program define parse_FSF
	estat firststage
	mat x = r(singleresults)
	global FSF =x[1,4]
end


***------Make table

*use "Albouy 2008 data with SJ edits March 19 2011 v10.dta", clear
use "dataset used for final AER rebuttal Sept 11 2011", clear

g rowtitle = ""
for num 1/9: g outcolX = ""
g indexnum = _n

replace rowtitle = "No covariates" if indexnum == 1
replace rowtitle = "AR confidence set" if indexnum == 2
replace rowtitle = "AR confidence set, clustered" if indexnum == 3
replace rowtitle = "F-stat, first stage" if indexnum == 4
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 5


replace rowtitle = "With latitude" if indexnum == 7
replace rowtitle = "AR confidence set" if indexnum == 8
replace rowtitle = "AR confidence set, clustered" if indexnum == 9
replace rowtitle = "F-stat, first stage" if indexnum == 10
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 11


replace rowtitle = "Without neo-Europes" if indexnum == 13
replace rowtitle = "AR confidence set" if indexnum == 14
replace rowtitle = "AR confidence set, clustered" if indexnum == 15
replace rowtitle = "F-stat, first stage" if indexnum == 16
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 17


replace rowtitle = "Without Africa" if indexnum == 19
replace rowtitle = "AR confidence set" if indexnum == 20
replace rowtitle = "AR confidence set, clustered" if indexnum == 21
replace rowtitle = "F-stat, first stage" if indexnum == 22
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 23


replace rowtitle = "With continent dummies" if indexnum == 25
replace rowtitle = "AR confidence set" if indexnum == 26
replace rowtitle = "AR confidence set, clustered" if indexnum == 27
replace rowtitle = "F-stat, first stage" if indexnum == 28
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 29


replace rowtitle = "With continent dummies and latitude" if indexnum == 31
replace rowtitle = "AR confidence set" if indexnum == 32
replace rowtitle = "AR confidence set, clustered" if indexnum == 33
replace rowtitle = "F-stat, first stage" if indexnum == 34
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 35


replace rowtitle = "With percent of European descent in 1975" if indexnum ==37
replace rowtitle = "AR confidence set" if indexnum == 38
replace rowtitle = "AR confidence set, clustered" if indexnum == 39
replace rowtitle = "F-stat, first stage" if indexnum == 40
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 41


replace rowtitle = "With malaria" if indexnum == 43
replace rowtitle = "AR confidence set" if indexnum == 44
replace rowtitle = "AR confidence set, clustered" if indexnum == 45
replace rowtitle = "F-stat, first stage" if indexnum == 46
replace rowtitle = "F-stat, first stage, clustered" if indexnum == 47

replace rowtitle = "" if indexnum == 48 

*COLUMN 1: Original AJR series

local colnum = 1

*column title
replace outcol`colnum' = "Original AJR series"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmort0) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmort0)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmort0) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmort0)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmort0)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmort0) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmort0) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45


*COLUMN 2: Original AJR series, capped at 250

local colnum = 2

*column title
replace outcol`colnum' = "Original AJR series, capped at 250"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortcap250)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortcap250) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortcap250)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortcap250) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortcap250)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortcap250)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortcap250) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortcap250) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45


*COLUMN 3: Benchmarking to Caribbean

local colnum = 3

*column title
replace outcol`colnum' = "Benchmarking to Caribbean"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortjam)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4 


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortjam) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45




*COLUMN 4: Benchmarking to Caribbean, capped at 250

local colnum = 4

*column title
replace outcol`colnum' = "Benchmarking to Caribbean, capped at 250"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam250)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam250) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortjam250)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortjam250) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam250)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortjam250)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortjam250) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortjam250) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45


*COLUMN 5: Using Naval Stations, Method 1

local colnum = 5

*column title
replace outcol`colnum' = "Using Naval Stations, Method 1"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval1)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval1) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45

*COLUMN 6: Using Naval Stations, Method 1, capped at 250

drop mortnaval1250
gen mortnaval1250=mortnaval1
replace mortnaval1250=250 if mortnaval1>250 & mortnaval1~=.
*gen logmortnaval1250=log(mortnaval1250)

local colnum = 6

*column title
replace outcol`colnum' = "Using Naval Stations, Method 1, capped at 250"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1250)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1250) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval1250)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval1250) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1250)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval1250)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval1250) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval1250) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45

*COLUMN 7: Using Naval Stations, Method 2

local colnum = 7

*column title
replace outcol`colnum' = "Using Naval Stations, Method 2"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval2)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval2) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45

*COLUMN 8: Using Naval Stations, Method 2, capped at 250

*gen mortnaval2250=mortnaval2
replace mortnaval2250=250 if mortnaval2>250 & mortnaval2~=.
*gen logmortnaval2250=log(mortnaval2250)


local colnum = 8

*column title
replace outcol`colnum' = "Using Naval Stations, Method 2, capped at 250"  if indexnum == 48

****-----------ROW 1

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 1
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 2

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2250)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 4

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 5

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]"  if  indexnum == 3



****-----------ROW 2

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250) latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 7
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 8

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2250) latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 10

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250) latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 11

*AR confident set, clustered
rivtest, ci gridmult(75) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 9


****-----------ROW 3


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250)  if (neoeuro~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 13
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 14

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval2250)  if (neoeuro~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 16

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250)  if (neoeuro~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 17

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 15



****-----------ROW 4


*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250) if (africa~=1), 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 19
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 20

*--First-stage from condivreg doesn't work when "if" clause in regression
quietly ivregress 2sls loggdp (risk=logmortnaval2250) if (africa~=1)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 22

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250) if (africa~=1), cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 23

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 21




****-----------ROW 5

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250) asia africa other, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 25
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 26

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2250)  asia africa other
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 28

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250)  asia africa other, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 29

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 27




****-----------ROW 6

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250)  asia africa other latitude, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 31
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 32
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 34

*First stage F-stat
quietly ivregress 2sls loggdp (risk=logmortnaval2250)  asia africa other latitude
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 34

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250)  asia africa other latitude, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 35

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 33



****-----------ROW 7

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250) edes1975 , 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 37
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 38
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250) edes1975
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 40

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250) edes1975, cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 41

*AR confident set, clustered
rivtest, ci gridmult(50) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 39



****-----------ROW 8

*Point estimate, AR confidence set
condivreg loggdp (risk=logmortnaval2250) malfal94, 2sls ar
replace outcol`colnum' = string(_b[risk],"%4.2f") if indexnum == 43
replace outcol`colnum' = "[" + string(e(AR_x1),"%4.2f") + "," + string(e(AR_x2),"%4.2f") + "]" if  indexnum == 44
replace outcol`colnum' = string(e(F_first),"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250) malfal94
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 46

*First stage F-stat, clustered
quietly ivregress 2sls loggdp (risk=logmortnaval2250) malfal94 , cluster(logmort0)
parse_FSF
replace outcol`colnum' = string($FSF,"%4.2f") if indexnum == 47

*AR confident set, clustered
rivtest, ci gridmult(25) points(1600)
parse_rivtest
replace outcol`colnum' = "[" + string($AR1,"%4.2f") + "," + string($AR2,"%4.2f") + "]" if  indexnum == 45

outsheet rowtitle outcol1 outcol2 outcol3 outcol4 outcol5 outcol6 outcol7 outcol8 using table1b.out, replace noquote 

