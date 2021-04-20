capture log close
clear
clear matrix
clear mata
set more off, permanently
set mem 2000

log using Table3a, replace

*-Can use this program to add stars to table later, if desired
capture program drop makestars
program define makestars, rclass
	syntax , Pointest(real) PVal(real) [bdec(integer 3)]
	**** Formats the coefficient with stars
	****

	local fullfloat = `bdec' + 1
	
	local outstr = string(`pointest',"%`fullfloat'.`bdec'f")
	
	if `pval' <= 0.01 {
		local outstr = "`outstr'" + "***"
	}
	else if `pval' <= 0.05 {
		local outstr = "`outstr'" + "**"
	}
	else if `pval' <= 0.1 {
		local outstr = "`outstr'" + "*"
	}	
		
	return local coeff = "`outstr'"
		
end

*use "Albouy 2008 data with SJ edits March 19 2011 v10.dta", clear
use "dataset used for final AER rebuttal Sept 11 2011", clear

g rowtitle = ""
for num 1/9: g outcolX = ""
g indexnum = _n

replace rowtitle = "No covariates" if indexnum == 1
replace rowtitle = "(standard error)" if indexnum == 2
replace rowtitle = "(clustered standard error)" if indexnum == 3
replace rowtitle = "Number of clusters" if indexnum == 4
replace rowtitle = "Number of observations" if indexnum == 5


replace rowtitle = "With latitude" if indexnum == 7
replace rowtitle = "(standard error)" if indexnum == 8
replace rowtitle = "(clustered standard error)" if indexnum == 9
replace rowtitle = "Number of clusters" if indexnum == 10
replace rowtitle = "Number of observations" if indexnum == 11


replace rowtitle = "Without neo-Europes" if indexnum == 13
replace rowtitle = "(standard error)" if indexnum == 14
replace rowtitle = "(clustered standard error)" if indexnum == 15
replace rowtitle = "Number of clusters" if indexnum == 16
replace rowtitle = "Number of observations" if indexnum == 17


replace rowtitle = "Without Africa" if indexnum == 19
replace rowtitle = "(standard error)" if indexnum == 20
replace rowtitle = "(clustered standard error)" if indexnum == 21
replace rowtitle = "Number of clusters" if indexnum == 22
replace rowtitle = "Number of observations" if indexnum == 23


replace rowtitle = "With continent dummies" if indexnum == 25
replace rowtitle = "(standard error)" if indexnum == 26
replace rowtitle = "(clustered standard error)" if indexnum == 27
replace rowtitle = "Number of clusters" if indexnum == 28
replace rowtitle = "Number of observations" if indexnum == 29


replace rowtitle = "With continent dummies and latitude" if indexnum == 31
replace rowtitle = "(standard error)" if indexnum == 32
replace rowtitle = "(clustered standard error)" if indexnum == 33
replace rowtitle = "Number of clusters" if indexnum == 34
replace rowtitle = "Number of observations" if indexnum == 35


replace rowtitle = "With percent of European descent in 1975" if indexnum ==37
replace rowtitle = "(standard error)" if indexnum == 38
replace rowtitle = "(clustered standard error)" if indexnum == 39
replace rowtitle = "Number of clusters" if indexnum == 40
replace rowtitle = "Number of observations" if indexnum == 41


replace rowtitle = "With malaria" if indexnum == 43
replace rowtitle = "(standard error)" if indexnum == 44
replace rowtitle = "(clustered standard error)" if indexnum == 45
replace rowtitle = "Number of clusters" if indexnum == 46
replace rowtitle = "Number of observations" if indexnum == 47

replace rowtitle = "" if indexnum == 48 


*COLUMN 1: AJR mortality series, Albouy campaign dummy

local colnum = 1

*column title
replace outcol`colnum' = "AJR mortality series, Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmort0 campaign slave, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmort0 campaign slave latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmort0 campaign slave if neoeuro~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmort0 campaign slave if africa~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmort0 campaign slave asia africa other, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmort0 campaign slave asia africa other latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmort0 campaign slave edes1975, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmort0 campaign slave malfal94, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47

************************************************************************8
*COLUMN 2: AJR mortality series, capped at 250; Albouy campaign dummy

local colnum = 2

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaign slave, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmortcap250 campaign slave latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmortcap250 campaign slave if neoeuro~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmortcap250 campaign slave if africa~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmortcap250 campaign slave asia africa other, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmortcap250 campaign slave asia africa other latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmortcap250 campaign slave edes1975, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmortcap250 campaign slave malfal94, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47


*COLUMN 3: AJR mortality series, minimal correction to Albouy campaign dummy

local colnum = 3

*column title
replace outcol`colnum' = "AJR mortality series, minimal correction to Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmort0 campaignsj slave, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmort0 campaignsj slave latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmort0 campaignsj slave if neoeuro~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmort0 campaignsj slave if africa~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmort0 campaignsj slave asia africa other, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmort0 campaignsj slave asia africa other latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmort0 campaignsj slave edes1975, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmort0 campaignsj slave malfal94, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47

************************************************************************8
*COLUMN 4: AJR mortality series, capped at 250; minimal correction to Albouy campaign dummy

local colnum = 4

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; minimal correction to Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaignsj slave, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmortcap250 campaignsj slave latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmortcap250 campaignsj slave if neoeuro~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmortcap250 campaignsj slave if africa~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmortcap250 campaignsj slave asia africa other, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmortcap250 campaignsj slave asia africa other latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmortcap250 campaignsj slave edes1975, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmortcap250 campaignsj slave malfal94, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47

******************************************************************
*COLUMN 5: AJR mortality series; extended correction to Albouy campaign dummy
*extended recoding


local colnum = 5

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; extended correction to Albouy campaign dummy"  if indexnum == 48


*row 1, Panel B: minimal recoding
reg risk logmort0 campaignsj2 slave, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmort0 campaignsj2 slave latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmort0 campaignsj2 slave if neoeuro~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmort0 campaignsj2 slave if africa~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmort0 campaignsj2 slave asia africa other, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmort0 campaignsj2 slave asia africa other latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmort0 campaignsj2 slave edes1975, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmort0 campaignsj2 slave malfal94, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47



************************************************************************

*COLUMN 6: AJR mortality series, capped at 250; extended correction to Albouy campaign dummy


label var outcol4 "AJR mortality series, capped at 250; extended correction to Albouy campaign dummy"
local colnum = 6


*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; extended correction to Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaignsj2 slave, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmortcap250 campaignsj2 slave latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmortcap250 campaignsj2 slave if neoeuro~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmortcap250 campaignsj2 slave if africa~=1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmortcap250 campaignsj2 slave asia africa other, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmortcap250 campaignsj2 slave asia africa other latitude, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmortcap250 campaignsj2 slave edes1975, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmortcap250 campaignsj2 slave malfal94, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47


*COLUMN 7: AJR mortality series, Albouy preferred sample; campaign dummy

local colnum = 7

*column title
replace outcol`colnum' = "AJR mortality series, Albouy preferred sample; campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmort0 campaign slave  if source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmort0 campaign slave latitude  if source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmort0 campaign slave if neoeuro~=1  & source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmort0 campaign slave if africa~=1  & source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmort0 campaign slave asia africa other  if source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmort0 campaign slave asia africa other latitude  if source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmort0 campaign slave edes1975  if source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmort0 campaign slave malfal94  if source0==1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47



************************************************************************
*COLUMN 8: AJR mortality series, capped at 250; Albouy preferred sample; extended correction to Albouy campaignsj2 dummy; dropping Gambia

local colnum = 8

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaignsj2 slave  if source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 4
replace outcol`colnum' = string(e(N_clust)) if indexnum == 5

*row 2
reg risk logmortcap250 campaignsj2 slave latitude  if source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 10
replace outcol`colnum' = string(e(N_clust)) if indexnum == 11

*row 3
reg risk logmortcap250 campaignsj2 slave if neoeuro~=1  & source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 16
replace outcol`colnum' = string(e(N_clust)) if indexnum == 17


*row 4
reg risk logmortcap250 campaignsj2 slave if africa~=1  & source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 22
replace outcol`colnum' = string(e(N_clust)) if indexnum == 23

*row 5
reg risk logmortcap250 campaignsj2 slave asia africa other  if source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 28
replace outcol`colnum' = string(e(N_clust)) if indexnum == 29

*row 6
reg risk logmortcap250 campaignsj2 slave asia africa other latitude  if source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 34
replace outcol`colnum' = string(e(N_clust)) if indexnum == 35


*row 7
reg risk logmortcap250 campaignsj2 slave edes1975  if source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 40
replace outcol`colnum' = string(e(N_clust)) if indexnum == 41

*row 8
reg risk logmortcap250 campaignsj2 slave malfal94  if source0==1 & shortnam~="GMB", cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 46
replace outcol`colnum' = string(e(N_clust)) if indexnum == 47


************************
**--robust, non-clustered errors
************************


*COLUMN 1: AJR mortality series, Albouy campaign dummy

local colnum = 1

*column title
replace outcol`colnum' = "AJR mortality series, Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmort0 campaign slave, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmort0 campaign slave latitude, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmort0 campaign slave if neoeuro~=1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmort0 campaign slave if africa~=1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmort0 campaign slave asia africa other, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmort0 campaign slave asia africa other latitude, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 32



*row 7
reg risk logmort0 campaign slave edes1975, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmort0 campaign slave malfal94, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 44


************************************************************************8
*COLUMN 2: AJR mortality series, capped at 250; Albouy campaign dummy

local colnum = 2

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaign slave, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortcap250 campaign slave latitude, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortcap250 campaign slave if neoeuro~=1, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmortcap250 campaign slave if africa~=1, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortcap250 campaign slave asia africa other, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortcap250 campaign slave asia africa other latitude, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 32



*row 7
reg risk logmortcap250 campaign slave edes1975, robust


replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmortcap250 campaign slave malfal94, robust


replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 44



*COLUMN 3: AJR mortality series, minimal correction to Albouy campaign dummy

local colnum = 3

*column title
replace outcol`colnum' = "AJR mortality series, minimal correction to Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmort0 campaignsj slave, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmort0 campaignsj slave latitude, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmort0 campaignsj slave if neoeuro~=1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmort0 campaignsj slave if africa~=1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmort0 campaignsj slave asia africa other, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmort0 campaignsj slave asia africa other latitude, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 32



*row 7
reg risk logmort0 campaignsj slave edes1975, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmort0 campaignsj slave malfal94, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 44


************************************************************************8
*COLUMN 4: AJR mortality series, capped at 250; minimal correction to Albouy campaign dummy

local colnum = 4

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; minimal correction to Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaignsj slave, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortcap250 campaignsj slave latitude, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortcap250 campaignsj slave if neoeuro~=1, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmortcap250 campaignsj slave if africa~=1, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortcap250 campaignsj slave asia africa other, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortcap250 campaignsj slave asia africa other latitude, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 32



*row 7
reg risk logmortcap250 campaignsj slave edes1975, robust


replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmortcap250 campaignsj slave malfal94, robust


replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 44


******************************************************************
*COLUMN 5: AJR mortality series; extended correction to Albouy campaign dummy
*extended recoding


local colnum = 5

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; extended correction to Albouy campaign dummy"  if indexnum == 48


*row 1, Panel B: minimal recoding
reg risk logmort0 campaignsj2 slave, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmort0 campaignsj2 slave latitude, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmort0 campaignsj2 slave if neoeuro~=1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmort0 campaignsj2 slave if africa~=1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmort0 campaignsj2 slave asia africa other, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmort0 campaignsj2 slave asia africa other latitude, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 32



*row 7
reg risk logmort0 campaignsj2 slave edes1975, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmort0 campaignsj2 slave malfal94, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 44




************************************************************************

*COLUMN 6: AJR mortality series, capped at 250; extended correction to Albouy campaign dummy


label var outcol4 "AJR mortality series, capped at 250; extended correction to Albouy campaign dummy"
local colnum = 6


*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; extended correction to Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaignsj2 slave, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortcap250 campaignsj2 slave latitude, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortcap250 campaignsj2 slave if neoeuro~=1, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmortcap250 campaignsj2 slave if africa~=1, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortcap250 campaignsj2 slave asia africa other, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortcap250 campaignsj2 slave asia africa other latitude, robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 32



*row 7
reg risk logmortcap250 campaignsj2 slave edes1975, robust


replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmortcap250 campaignsj2 slave malfal94, robust


replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 44



*COLUMN 7: AJR mortality series, Albouy preferred sample; campaign dummy

local colnum = 7

*column title
replace outcol`colnum' = "AJR mortality series, Albouy preferred sample; campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmort0 campaign slave  if source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmort0 campaign slave latitude  if source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmort0 campaign slave if neoeuro~=1  & source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmort0 campaign slave if africa~=1  & source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmort0 campaign slave asia africa other  if source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmort0 campaign slave asia africa other latitude  if source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmort0 campaign slave edes1975  if source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmort0 campaign slave malfal94  if source0==1, robust

replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 44




************************************************************************
*COLUMN 8: AJR mortality series, capped at 250; Albouy preferred sample; extended correction to Albouy campaign dummy; dropping Gambia

local colnum = 8

*column title
replace outcol`colnum' = "AJR mortality series, capped at 250; Albouy campaign dummy"  if indexnum == 48

*row 1, Panel B: minimal recoding
reg risk logmortcap250 campaignsj2 slave  if source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortcap250 campaignsj2 slave latitude  if source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortcap250 campaignsj2 slave if neoeuro~=1  & source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 14


*row 4
reg risk logmortcap250 campaignsj2 slave if africa~=1  & source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortcap250 campaignsj2 slave asia africa other  if source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortcap250 campaignsj2 slave asia africa other latitude  if source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 32


*row 7
reg risk logmortcap250 campaignsj2 slave edes1975  if source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 38


*row 8
reg risk logmortcap250 campaignsj2 slave malfal94  if source0==1 & shortnam~="GMB", robust

replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 44

outsheet rowtitle outcol1 outcol2 outcol3 outcol4 outcol5 outcol6 outcol7 outcol8 using table3a.out, replace noquote 

