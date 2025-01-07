* This path should have all the .dta files used in this file
* note that this .do file requires the following SOEP files:
*
* ilanguage.dta
* inno.dta
* ppfad.dta
* pgen.dta
* hgen.dta
* p.dta
*
* It does not require county ids -> these are only used in the .R files


** Set path

global path = ""

use "$path/ilanguage.dta", clear 

**language data cleaning:
*keep if mspr==5

**dialect: 
gen dialect=1 if si01==1
replace dialect=0 if si01==2

**importance of dialect:
gen dialect_imp=si02
replace dialect_imp=. if si02<=-2
revrs dialect_imp, replace

**dialect at work: 
gen dialect_work=0 if si05c>=-1
replace dialect_work=1 if si05c==1 

**dialect in comparison to news:
gen dialect_comp=si09 if si09>0 & si09<6

**dialect visible: 
gen dialect_visible=si10a if si10a>0 
revrs dialect_visible, replace

**dialect visible by interviewer:
gen dialect_visible_interviewer=si14 if si14>0 & si14<6

**dialect spoken by classmates 

gen dialect_classmates = si16 if si16>-1 & si16<6

** 

keep pid dialect dialect_imp dialect_work dialect_comp dialect_visible dialect_visible_interviewer dialect_classmates
collapse dialect dialect_imp dialect_work dialect_comp dialect_visible dialect_visible_interviewer dialect_classmates, by(pid)

tempfile dialect
save `dialect'

** Innovation

use "$path/inno.dta", clear 

keep pid syear iss1 iss2

tempfile inno
save `inno'

** PPFAD

use "$path/ppfad.dta", clear 

gen birthyear=gebjahr if gebjahr>0

tempfile pfad
save `pfad'

*other variables at individual level
use "$path/pgen.dta", clear 

gen edu=0
replace edu=1  if pgsbil==6 | pgsbil==8
replace edu=2  if pgsbil==1  
replace edu=3  if pgsbil==2 | pgsbil==3
replace edu=4  if pgsbil==4
label define edu 0 "other" 1 "none" 2 "low" 3 "middle" 4 "high"
label values edu edu

gen edu_time=pgbilzt 
replace edu_time=0 if pgbilzt<0

keep pid syear edu edu_time

tempfile pgen
save `pgen'


*income:
use "$paths/hgen.dta", clear 
xtile income=hghinc, nquantile(5)
replace income=. if hghinc<0 

keep hid syear income

tempfile hgen
save `hgen' 

use "$path/p.dta", clear 

**gender
gen female=0
replace female=1 if pla0009==2

**unemployed 
gen unemployed=0
replace unemployed=1 if plb0021==1

**political interest: 
gen polint=.
replace polint=1 if plh0007==4 
replace polint=2 if plh0007==3 
replace polint=3 if plh0007==2 
replace polint=4 if plh0007==1 

**party identification 
**separate dummies for each party:
gen p_spd=0
replace p_spd=1 if plh0012==1
replace p_spd=. if plh0012<-2
***
gen p_union=0
replace p_union=1 if plh0012==2 | plh0012==3  | plh0012==13
replace p_union=. if plh0012<-2
***
gen p_fdp=0
replace p_fdp=1 if plh0012==4
replace p_fdp=. if plh0012<-2
***
gen p_gru=0
replace p_gru=1 if plh0012==5 | plh0012==39
replace p_gru=. if plh0012<-2
***
gen p_linke=0
replace p_linke=1 if plh0012==6
replace p_linke=. if plh0012<-2
***
gen p_npdrep=0
replace p_npdrep=1 if plh0012==7
replace p_npdrep=. if plh0012<-2
***
gen p_none=0
replace p_none=1 if plh0012==-2 
replace p_none=. if plh0012<-2
***
gen p_mainstream=0 
replace p_mainstream=1 if p_gru==1 ///
| p_fdp==1 | p_union==1 | p_spd==1
replace p_mainstream=. if plh0012<-2
***
gen p_challenger=0 
replace p_challenger=1 if p_linke==1 ///
| p_npdrep==1 | plh0012==27 | plh0012==8 ///
| plh0012==26
replace p_challenger=. if plh0012<-2
***
gen p_afd=0
replace p_afd=1 if plh0012==27
replace p_afd=. if syear<=2013
replace p_afd=. if plh0012<-2

gen lr=plh0004 if plh0004>0

gen pre_vote_afd=0
replace pre_vote_afd=1 if ppol8a==27
replace pre_vote_afd=. if ppol8a==-5

gen post_vote_afd=0
replace post_vote_afd=1 if ppol8b==27
replace post_vote_afd=. if ppol8b==-5

**migration (1999-)
gen worry_mig=0
replace worry_mig=1 if plj0046==1
replace worry_mig=. if syear<=1998
replace worry_mig=. if plj0046<-2

**feel German
gen feel_ger=0 if plj0078==5
replace feel_ger=1 if plj0078==4
replace feel_ger=2 if plj0078==3
replace feel_ger=3 if plj0078==2
replace feel_ger=4 if plj0078==1

tab ppol8a, gen(vote_pre_17)
tab ppol8b, gen(vote_post_17)

keep pid hid syear female-feel_ger vote_*

tempfile voting
save `voting'

merge m:1 pid using "`dialect'"
drop _merge 
merge m:1 pid using "`pfad'"
drop _merge 
merge m:1 hid syear using "`hgen'"
drop _merge 
merge 1:1 pid syear using "`pgen'"
drop _merge 
merge 1:1 pid syear using "`inno'"
drop _merge 

gen age=syear-birthyear
keep if age>=18

drop netto98-casemat17

save "$path/SOEP.dta", replace 