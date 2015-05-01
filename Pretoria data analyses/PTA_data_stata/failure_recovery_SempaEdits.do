version 12.1
clear
set more off
capture log close
set memory 300m

**I have created a subfolder "PTA_data_stata" to sepearte my eidts from the original folder and/or work
**Noted: According to the dofile "failure recovery 2013", analysis starts from "PTA fail wide_panels" and not "Pretoria data in wide"
log using "C:\Users\Joseph\OneDrive\Documents\PhD\Immune response Obj 3\dataset and codebook\Pretoria data analyses\PTA_data_stata\logfile_sempa290415.log", replace
cd "C:\Users\Joseph\OneDrive\Documents\PhD\Immune response Obj 3\dataset and codebook\Pretoria data analyses\PTA_data_stata\

insheet using "C:\Users\Joseph\OneDrive\Documents\PhD\Immune response Obj 3\dataset and codebook\Pretoria data analyses\PTA fail wide_panels.csv", comma

/*
sum  m36vl, d /* What does 'm36vl' standfor?*/
byvar  vl400at6m: sum  m36vl, d
byvar   vlanygt400: sum  m36vl, d
byvar  vlconsec10: sum  m36vl, d
byvar   cd4lt500: sum  m36vl, d
byvar  cd4faila: sum  m36vl, d
byvar  cd4failb: sum  m36vl, d
byvar  cd4failc: sum  m36vl,d
byvar  cd4failabc: sum  m36vl,d
*/
foreach x of varlist maxcd4  vl400m0 vl400m6 vl400m12 vl400m18 vl400m24 vl400m30  vl400m36 vl400m42 vl400m48 vl400m54 vl400m60 vl400any consec400m0 consec400m6 consec400m12 consec400m18 consec400m24 consec400m30 consec400m36 consec400m42 consec400m48 consec400m54 consec400m60 consec400any consec1000m0 consec1000m6 consec1000m12 consec1000m18 consec1000m24 consec1000m30 consec1000m36  consec1000m42 consec1000m48 consec1000m54 consec1000m60 consec1000any consec5000m0 consec5000m6 consec5000m12 consec5000m18 consec5000m24 consec5000m30 consec5000m36 consec5000m42 consec5000m48 consec5000m54 consec5000m60 consec5000any cd4_fail_lt500 afailm0 afailm6 afailm12 afailm18 afailm24 afailm30 afailm36 afailm42 afailm48 afailm54 afailm60 afailany  bfailm6 bfailm18 bfailm12  bfailm24 bfailm30 bfailm36 bfailm42 bfailm48 bfailm54 bfailm60 bfailany  cfailm0 cfailm6 cfailm12 cfailm18 cfailm24 cfailm30 cfailm36 cfailm42 cfailm48 cfailm54 cfailm60 cfailany abcfailm6 abcfailm12 abcfailm18 abcfailm24 abcfailm30 abcfailm36 abcfailm42 abcfailm48 abcfailm54 abcfailm60 orabcany bmi age_cont blipm6 blipm12	blipm18	blipm24	blipm30	blipm36	blipm42	blipm48	blipm54	blipm60 {
replace `x'="" if `x'=="NA"
destring `x',replace
}
replace change="y" if change=="y"
replace  nonadherer="" if  nonadherer=="?"
gen  bl_cd4_strata2=  bl_cd4_strata
replace  bl_cd4_strata2="5: 200+" if  bl_cd4_strata2=="5: 201-250" |  bl_cd4_strata2=="6: 251-300" |  bl_cd4_strata2=="7: 300+"
gen  bl_cd4_bin=.
replace bl_cd4_bin=0 if bl_cd4_strata2=="1: 0-50"|bl_cd4_strata2=="2: 51-100"|bl_cd4_strata2=="3: 101-150"|bl_cd4_strata2=="4: 151-200"
replace  bl_cd4_bin=1 if bl_cd4_strata2=="5: 200+" 

gen blvlstrat=.
replace blvlstrat=1 if  bllogvlstrata=="<4"
replace blvlstrat=2 if  bllogvlstrata=="4-5"
replace blvlstrat=3 if  bllogvlstrata==">5"
label define strat 1 "<4" 2 "4-5" 3 ">5"
label values blvlstrat strat
drop bllogvlstrata
rename blvlstrat bllogvlstrata

gen dist_clinic_cat2=dist_clinic_cat
replace dist_clinic_cat2="60+" if dist_clinic_cat2=="61-80" | dist_clinic_cat2==">80"

gen  sex_bin=. 
replace sex_bin=0 if sex=="m"
replace sex_bin=1 if sex=="f"
drop sex
rename sex_bin sex 
label define sexlab 0 "male" 1 "female" 
label values sex sexlab
gen tb=0
replace tb=1 if comorbs=="tb"

gen age50=0 
replace age50=. if age_cont==.
replace age50=1 if age_cont>=50


*xtset ptnumber month
xi: xtgee cd4failabc  cd4m ,family(binomial) link(logit) corr(ar1)	i(ptnumber) t(month)		
*doesn't converge!!!!! no sense in adding other vars

*****list of vars*****		
	i.agecat i.sex bmi i.whostage i.distclinic i.employed i.change i.interrupte i.stopped i.sideeffect 
                     i.comorbs i.defaulter i.nonadherer  i.transferre i.untraceabl i.blcd4strat	vlm i.bllogvlstr
**********************
 
 xtset ptnumber month
xi: xtlogit  cd4failabc  i.agecat i.sex bmi i.whostage i.distclinic i.employed i.change i.interrupte i.stopped i.sideeffect 
                     i.comorbs i.defaulter i.nonadherer  i.transferre i.untraceabl i.blcd4strat	vlm i.bllogvlstr
 agecat sex bmi whostage distclinic employed change interrupte stopped sideeffect comorbs defaulter nonadherer 
 transferre untraceabl blcd4strat cd4m vlm bllogvlstr  , re
*doesn't converge!!!!! 

*maak al die failures ook panel data

import excel "C:\Users\Joseph\OneDrive\Documents\PhD\Immune response Obj 3\dataset and codebook\Pretoria data analyses\wide_panels.xlsx", sheet("VL integral") firstrow clear

*destring alles wat nie dieselfde is nie




reshape long  CD4m  VLm  logVLm VL400m  consec400m  consec1000m  consec5000m blipm afailm  bfailm  cfailm  abcfailm , i(PT_NUMBER) j(month)

gen sqrtCD4m=sqrt(CD4m)

*vir WHO CD4 failure:
xi: xtgee   abcfailm  logVLm  i.agecat i.sex  BMI i.WHO_stage  i.dist_clinic_cat2 i.employed i.change i.stopped i.side_effects i.tb i.defaulter i.Nonadherer i.Transferred i.Untraceable  i.BL_CD4_strata2  ,family(binomial) link(logit) corr(ar1)i(PT_NUMBER) t(month)
*te groot, konvergeer nie...
*haal uit eers die wat nie significant is nie en klein koeffisiente het.
*voorlopige model: bespreek met Martin
*na praat met martin:
xi: xtgee   abcfailm  logVLm  i.agecat i.sex  i.change  i.defaulter i.Nonadherer   i.BL_CD4_strata2 , family(binomial) link(logit) corr(ar1) i(PT_NUMBER) t(month)
*add suppress VL na 6 maande/12 maande (kyk watter een is cool)

*vir VL >400 na 6 months failure:
xi: xtgee  VL400m  CD4m  age_cont i.sex  BMI i.WHO_stage  i.dist_clinic_cat2 i.employed i.change i.stopped i.side_effects i.tb i.defaulter i.Nonadherer i.Transferred i.Untraceable  i.BL_CD4_strata2 i.BLlogVLstrata ,family(binomial) link(logit) corr(ar1)i(PT_NUMBER) t(month)
*na elimination:


xi: xtgee consecblipm  CD4m  i.agecat i.sex  BMI i.WHO_stage  i.dist_clinic_cat2 i.employed i.change i.stopped i.side_effects i.tb i.defaulter i.Nonadherer i.Transferred i.Untraceable  i.BL_CD4_strata2  ,family(binomial) link(logit) corr(ar1)i(PT_NUMBER) t(month)


***for marconi models:
insheet using "C:\Users\Joseph\OneDrive\Documents\PhD\Immune response Obj 3\dataset and codebook\Pretoria data analyses\wide_panels_martin.csv", comma

destring  maxCD4  VL400m0 VL400m6 VL400m12 VL400m18 VL400m24 VL400m30  VL400m36 VL400m42 VL400m48 VL400m54 VL400m60 VL400ANY consec400m0 consec400m6 consec400m12 consec400m18 consec400m24 consec400m30 consec400m36 consec400m42 consec400m48 consec400m54 consec400m60 consec400ANY consec1000m0 consec1000m6 consec1000m12 consec1000m18 consec1000m24 consec1000m30 consec1000m36  consec1000m42 consec1000m48 consec1000m54 consec1000m60 consec1000ANY consec5000m0 consec5000m6 consec5000m12 consec5000m18 consec5000m24 consec5000m30 consec5000m36 consec5000m42 consec5000m48 consec5000m54 consec5000m60 consec5000ANY CD4_FAIL_lt500 afailm0 afailm6 afailm12 afailm18 afailm24 afailm30 afailm36 afailm42 afailm48 afailm54 afailm60 aFAILany  bfailm6 bfailm18 bfailm12  bfailm24 bfailm30 bfailm36 bfailm42 bfailm48 bfailm54 bfailm60 bFAILany  cfailm0 cfailm6 cfailm12 cfailm18 cfailm24 cfailm30 cfailm36 cfailm42 cfailm48 cfailm54 cfailm60 cFAILany abcfailm6 abcfailm12 abcfailm18 abcfailm24 abcfailm30 abcfailm36 abcfailm42 abcfailm48 abcfailm54 abcfailm60 ORabcANY BMI age_cont blipm6 blipm12	blipm18	blipm24	blipm30	blipm36	blipm42	blipm48	blipm54	blipm60, replace
gen any_blip=0
replace any_blip=1 if  blipm0==1 | blipm6==1 |  blipm12==1 |  blipm18==1 |  blipm24==1 |  blipm30 ==1 |  blipm36==1 |  blipm42 ==1 |  blipm48==1 |  blipm54==1 |  blipm60==1
replace any_blip=. if   blipm6==. &  blipm12==. &  blipm18==. &  blipm24==. &  blipm30 ==. &  blipm36==. &  blipm42 ==. &  blipm48==. &  blipm54==. &  blipm60==.
gen logVLslope6=( log10(VLm6)- log10(VLm0))/(0.5)
gen logVLslope12=( log10(VLm12)- log10(VLm0))

gen sqrtCD4BL=sqrt(CD4mBL)

xi: regress  sqrtCD4_gain  logVLslope12    sqrtCD4BL   i.VL_supp_cat age_cont 
xi: regress  sqrtCD4_gain  logVLslope6    sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  sqrtCD4_gain    totLOGVLpy6   sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  sqrtCD4_gain   totLOGVLpy0   sqrtCD4BL   i.VL_supp_cat age_cont


xi: regress  pyCD4gain2 logVLslope12  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  pyCD4gain2 logVLslope12  sqrtCD4BL   i.VL_supp_cat age_cont if  pyCD4gain2!=0
xi: regress  pyCD4gain2 logVLslope6  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  pyCD4gain2  totLOGVLpy6  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  pyCD4gain2  totLOGVLpy0  sqrtCD4BL   i.VL_supp_cat age_cont

xi: regress  totCD4gain    logVLslope12  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  totCD4gain    logVLslope6  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  totCD4gain   totLOGVLpy6  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  totCD4gain   totLOGVLpy0  sqrtCD4BL   i.VL_supp_cat age_cont

xi: regress  meanCD4after2 logVLslope12  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  meanCD4after2 logVLslope6  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  meanCD4after2  totLOGVLpy6  sqrtCD4BL   i.VL_supp_cat age_cont
xi: regress  meanCD4after2  totLOGVLpy0  sqrtCD4BL   i.VL_supp_cat age_cont
