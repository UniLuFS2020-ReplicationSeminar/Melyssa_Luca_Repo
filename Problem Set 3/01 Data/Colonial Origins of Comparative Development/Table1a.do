capture log close
clear
clear matrix
clear mata
set more off, permanently
set mem 2000

log using Table1a.scml, replace

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

*COLUMN 1: Original AJR series

local colnum = 1

*column title
replace outcol`colnum' = "Original AJR series"  if indexnum == 48

*row 1
reg risk logmort0, cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmort0, robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmort0 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmort0 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmort0 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmort0 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmort0 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmort0 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmort0 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmort0 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmort0 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmort0 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmort0 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmort0 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmort0 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmort0],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmort0 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmort0],"%4.2f") + ")" if  indexnum == 44




*COLUMN 2: Original AJR series, capped at 250

local colnum = 2

*column title
replace outcol`colnum' = "Original AJR series, capped at 250"  if indexnum == 48

*row 1
reg risk logmortcap250, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortcap250, robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortcap250 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortcap250 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortcap250 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortcap250 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortcap250 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortcap250 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortcap250 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortcap250 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortcap250 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortcap250 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortcap250 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortcap250 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortcap250 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortcap250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortcap250 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortcap250],"%4.2f") + ")" if  indexnum == 44




*COLUMN 3: Benchmarking to Caribbean

local colnum = 3

*column title
replace outcol`colnum' = "Benchmarking to Caribbean"  if indexnum == 48

*row 1
reg risk logmortjam, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortjam, robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortjam latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortjam latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortjam if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortjam if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortjam if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortjam if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortjam asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortjam asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortjam asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortjam asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortjam edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortjam edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortjam malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortjam malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortjam],"%4.2f") + ")" if  indexnum == 44




*COLUMN 4: Benchmarking to Caribbean, capped at 250

local colnum = 4

*column title
replace outcol`colnum' = "Benchmarking to Caribbean, capped at 250"  if indexnum == 48

*row 1
reg risk logmortjam250, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortjam250, robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortjam250 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortjam250 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortjam250 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortjam250 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortjam250 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortjam250 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortjam250 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortjam250 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortjam250 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortjam250 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortjam250 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortjam250 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortjam250 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortjam250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortjam250 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortjam250],"%4.2f") + ")" if  indexnum == 44



*COLUMN 5: Using Naval Stations, Method 1

local colnum = 5

*column title
replace outcol`colnum' = "Using Naval Stations, Method 1"  if indexnum == 48

*row 1
reg risk logmortnaval1, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortnaval1, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortnaval1 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortnaval1 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortnaval1 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortnaval1 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortnaval1 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortnaval1 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortnaval1 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortnaval1 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortnaval1 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortnaval1 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortnaval1 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortnaval1 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortnaval1 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortnaval1 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1],"%4.2f") + ")" if  indexnum == 44


*COLUMN 6: Using Naval Stations, Method 1, capped at 250

drop mortnaval1250
gen mortnaval1250=mortnaval1
replace mortnaval1250=250 if mortnaval1>250 & mortnaval1~=.
*gen logmortnaval1250=log(mortnaval1250)

local colnum = 6

*column title
replace outcol`colnum' = "Using Naval Stations, Method 1, capped at 250"  if indexnum == 48

*row 1
reg risk logmortnaval1250, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortnaval1250, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortnaval1250 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortnaval1250 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortnaval1250 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortnaval1250 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortnaval1250 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortnaval1250 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortnaval1250 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortnaval1250 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortnaval1250 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortnaval1250 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortnaval1250 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortnaval1250 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortnaval1250 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval1250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortnaval1250 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval1250],"%4.2f") + ")" if  indexnum == 44


*COLUMN 7: Using Naval Stations, Method 2

local colnum = 7

*column title
replace outcol`colnum' = "Using Naval Stations, Method 2"  if indexnum == 48

*row 1
reg risk logmortnaval2, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortnaval2, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortnaval2 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortnaval2 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortnaval2 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortnaval2 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortnaval2 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortnaval2 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortnaval2 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortnaval2 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortnaval2 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortnaval2 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortnaval2 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortnaval2 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortnaval2 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortnaval2 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2],"%4.2f") + ")" if  indexnum == 44


*COLUMN 8: Using Naval Stations, Method 2, capped at 250

*gen mortnaval2250=mortnaval2
replace mortnaval2250=250 if mortnaval2>250 & mortnaval2~=.
*gen logmortnaval2250=log(mortnaval2250)


local colnum = 8

*column title
replace outcol`colnum' = "Using Naval Stations, Method 2, capped at 250"  if indexnum == 48

*row 1
reg risk logmortnaval2250, cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 1
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 3
replace outcol`colnum' = string(e(N)) if indexnum == 5
replace outcol`colnum' = string(e(N_clust)) if indexnum == 4

reg risk logmortnaval2250, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 2

*row 2
reg risk logmortnaval2250 latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 7
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 9
replace outcol`colnum' = string(e(N)) if indexnum == 11
replace outcol`colnum' = string(e(N_clust)) if indexnum == 10

reg risk logmortnaval2250 latitude, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 8

*row 3
reg risk logmortnaval2250 if neoeuro~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 13
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 15
replace outcol`colnum' = string(e(N)) if indexnum == 17
replace outcol`colnum' = string(e(N_clust)) if indexnum == 16

reg risk logmortnaval2250 if neoeuro~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 14

*row 4
reg risk logmortnaval2250 if africa~=1 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 19
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 21
replace outcol`colnum' = string(e(N)) if indexnum == 23
replace outcol`colnum' = string(e(N_clust)) if indexnum == 22

reg risk logmortnaval2250 if africa~=1 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 20

*row 5
reg risk logmortnaval2250 asia africa other , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 25
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 27
replace outcol`colnum' = string(e(N)) if indexnum == 29
replace outcol`colnum' = string(e(N_clust)) if indexnum == 28

reg risk logmortnaval2250 asia africa other , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 26

*row 6
reg risk logmortnaval2250 asia africa other latitude , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 31
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 33
replace outcol`colnum' = string(e(N)) if indexnum == 35
replace outcol`colnum' = string(e(N_clust)) if indexnum == 34

reg risk logmortnaval2250 asia africa other latitude , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 32

*row 7
reg risk logmortnaval2250 edes1975 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 37
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 39
replace outcol`colnum' = string(e(N)) if indexnum == 41
replace outcol`colnum' = string(e(N_clust)) if indexnum == 40

reg risk logmortnaval2250 edes1975 , robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 38

*row 8
reg risk logmortnaval2250 malfal94 , cluster(logmort0)

replace outcol`colnum' = string(_b[logmortnaval2250],"%4.2f") if indexnum == 43
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 45
replace outcol`colnum' = string(e(N)) if indexnum == 47
replace outcol`colnum' = string(e(N_clust)) if indexnum == 46

reg risk logmortnaval2250 malfal94, robust
replace outcol`colnum' = "(" + string(_se[logmortnaval2250],"%4.2f") + ")" if  indexnum == 44





outsheet rowtitle outcol1 outcol2 outcol3 outcol4 outcol5 outcol6 outcol7 outcol8 using table1a.out, replace noquote 
