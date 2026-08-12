**********************************************
**************Dofile - C-QIP Trial************
**Created by - Sheril Rajan, Dr. Meetushi Jain
**Reviewed by - Dr. Dimple Kondal
**********************************************

**Setting the directory
cd "/Users/dr.meetushijain/Library/CloudStorage/OneDrive-CentreForChronicDiseaseControl/PHFI projects/C-QIP/CQIP-Paper"

**Data used 
use "/Users/dr.meetushijain/Library/CloudStorage/OneDrive-CentreForChronicDiseaseControl/PHFI projects/C-QIP/CQIP-Paper/C-QIP_all_visit_merged_Long.dta", clear

**Checking visitwise dropouts
tab VisitName_n dropout,m

**Keeping only Baseline, Annual, and EOS data for analysis purpose
keep if VisitName_n ==1|VisitName_n ==16|VisitName_n ==18
tab VisitName_n
tab VisitName_n TreatmentArm

**Removing the dropouts
drop if VisitName_n ==1& dropout_n==1

recode VisitName_n (1=1 "Baseline") (16=2 "12m") (18=3 "24m"),gen(visit)
tab visit
egen group=group(visit TreatmentArm ),label
tab group

**Public & Private Hospitals
gen hosp=. 
replace hosp=1 if SiteName=="AIIMS" | SiteName=="GB Pant"
replace hosp=2 if SiteName=="Gangaram" | SiteName=="SDM"
tab hosp SiteName, m 
lab def hosp 1 "Public Hospitals" 2 "Private Hospitals", modify
lab val hosp hosp
lab var hosp "Public/Private Hospitals"
tab hosp

**Age Categories
sum AGE
recode AGE (min/35 =1 "<35 years") (36/50 =2 "35 - 49 years") (50/max=4 ">=50 Years") , gen(AGE_CAT2)
tab AGE_CAT2
lab var AGE_CAT2 "Age category"

**Education
recode SCHOOLING_YEARS (0/4=1 "<5") (5/10=2 "5 - 10 years") (11/12=3 "10 -12 years") (13/max=4 ">12 years"), gen(edu_years)
tab edu_years if VisitName_n==1 & dropout!=1, m 
lab var edu_years "Education"

***Income
tab INCOME_CAT if VisitName_n==1 & dropout!=1

**Parameters
sum SBP_AVG DBP_AVG if VisitName_n ==1 & dropout !=1
list pid TOTAL_CHOLESTEROL TOTAL_CHOLESTEROL_NA if VisitName_n==1 & dropout!=1 & (TOTAL_CHOLESTEROL==. & TOTAL_CHOLESTEROL_NA==.)
list pid LDLC LDLC_NA if VisitName_n==1 & dropout!=1 & ( LDLC ==. & LDLC_NA ==.)
list pid FBG FBG_NA if VisitName_n ==1 & dropout !=1 & FBG==. & FBG_NA==.
list pid HBA1C HBA1C_NA if VisitName_n==1 & dropout!=1 & ( HBA1C ==. & HBA1C_NA ==.)

**SINGLE DRUGS**
*Antiplatelet drug 
tab1 DC_01 DC_02 DC_03 DC_04 DC_05 DC_06
edit pid DC_01 DC_02 DC_03 DC_04 DC_05 DC_06 if DC_01==.& DC_02==.& DC_03==.& DC_04==.& DC_05==.& DC_06==.
egen antiplatelet = rownonmiss(DC_01 DC_02 DC_03 DC_04 DC_05 DC_06)
tab antiplatelet, m 
lab var antiplatelet "Taking Antiplatelet medications(As per Medications reported)"
recode antiplatelet (0=0 "No AP drug") (1/max=1 "On AP medication"), gen(antiplatelet_cat)
lab var antiplatelet_cat "Paricipants on Aniplatelet drugs (As per medication reported)"
tab antiplatelet_cat visit
sort pid VisitName_n
edit pid DC_01 DC_02 DC_03 DC_04 DC_05 DC_06 VisitName_n ANTIPLATELET antiplatelet antiplatelet_cat
tab group antiplatelet_cat 

*Statin/Lipid Lowering drugs
tab1 DC_13 DC_14 DC_15 DC_16 DC_17 DC_18 
tab1 GN_13 GN_14 GN_15 GN_16 GN_17 GN_18 
edit pid DC_13 DC_14 DC_15 DC_16 DC_17 DC_18 if DC_13==.& DC_14==.& DC_15==.& DC_16==.& DC_17==.& DC_18==. 
egen lipid_lowering = rownonmiss(DC_13 DC_14 DC_15 DC_16 DC_17 DC_18)
tab lipid_lowering, m 
lab var lipid_lowering "Taking lipid lowering medications(As per Medications reported)"
recode lipid_lowering (0=0 "No Lipid lowering") (1/max=1 "On Lipid lowering medi") , gen(lipid_lowering_cat)
lab var lipid_lowering_cat "Taking lipid lowering medications(As per Medications reported)"
edit pid LIPID_LOWERING  lipid_lowering DC_13 DC_14 DC_15 DC_16 DC_17 DC_18 lipid_lowering_cat Visit
tab lipid_lowering_cat visit,m

*ACEi
gen ACEi=1 if DC_07==1 |  DC_08==1 | DC_09==1| DC_10==1| DC_11 ==1|DC_12==1 
replace ACEi=0 if ACEi==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var ACEi "ACE-inhibitors"
lab val ACEi yesno
tab ACEi
edit pid visit ANTIHYPERTENSIVE ACEi  BR_01 DC_07 GN_07 BR_02  DC_08 GN_08 BR_03  DC_09 GN_09 BR_04 DC_10 GN_10 BR_05 DC_11 GN_11 BR_06 DC_12 GN_12 SAE_ANGINA_MI SAE_STROKE SAE_PCI_CABG SAE_HOSPITILIZATION SAE_DEATH SAE_LAST_VISIT DOA_SAE AGE_SAE if ACEi==.
replace ACEi=0 if ANTIHYPERTENSIVE==0
tab ACEi visit,m

*ARBs
gen ARB=1 if DC_07==2 |  DC_08==2 | DC_09==2 | DC_10==2 | DC_11 ==2 | DC_12==2 
replace ARB=0 if ARB==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var ARB "ARBs"
lab val ARB yesno
tab ARB visit,m
edit pid ANTIHYPERTENSIVE ARB  BR_01 DC_07 GN_07 BR_02  DC_08 GN_08 BR_03  DC_09 GN_09 BR_04 DC_10 GN_10 BR_05 DC_11 GN_11 BR_06 DC_12 GN_12 if ARB==.
replace ARB=0 if ARB==.& ANTIHYPERTENSIVE==0
tab ARB visit,m

*ARNIs 
gen ARNI=1 if DC_07==7 |  DC_08==7 | DC_09==7 | DC_10==7 | DC_11 ==7 |DC_12==7
replace ARNI=0 if ARNI==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
replace ARNI=0  if ANTIHYPERTENSIVE==0
edit pid visit ANTIHYPERTENSIVE ARNI  DC_07 DC_08 DC_09 DC_10 DC_11 DC_12 if DC_07==.&  DC_08==.& DC_09==.&DC_10==.& DC_11==.&DC_12==.
lab var ARNI "ARNIs"
lab val ARNI yesno
tab ARNI visit, m 

*Beta-blockers
gen BB=1 if DC_07==3 |  DC_08==3 | DC_09==3 | DC_10==3| DC_11 ==3|DC_12==3 
replace BB=0 if BB==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var BB "Beta-blockers"
lab val BB yesno
tab BB visit,m 
edit pid ANTIHYPERTENSIVE BB  BR_01 DC_07 GN_07 BR_02  DC_08 GN_08 BR_03  DC_09 GN_09 BR_04 DC_10 GN_10 BR_05 DC_11 GN_11 BR_06 DC_12 GN_12 if BB==.
replace BB=0 if ANTIHYPERTENSIVE==0&BB==.
tab BB visit,m 

*CCBs
gen CCB=1 if DC_07==4 |  DC_08==4 | DC_09==4 | DC_10==4 | DC_11 ==4 | DC_12==4 
replace CCB=0 if CCB==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var CCB "CCBs"
lab val CCB yesno
tab CCB visit, m 
edit pid ANTIHYPERTENSIVE CCB  BR_01 DC_07 GN_07 BR_02  DC_08 GN_08 BR_03  DC_09 GN_09 BR_04 DC_10 GN_10 BR_05 DC_11 GN_11 BR_06 DC_12 GN_12 if CCB==.
replace CCB=0 if ANTIHYPERTENSIVE==0&CCB==.
tab CCB visit, m 

*Diuretic 
gen diuretics=1 if DC_07==5 |  DC_08==5 | DC_09==5 | DC_10==5| DC_11 ==5|DC_12==5 
replace diuretics=0 if diuretics==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var diuretics "Diuretics"
lab val diuretics yesno
tab diuretics visit, m 
edit pid ANTIHYPERTENSIVE diuretics  DC_07 DC_08 DC_09 DC_10 DC_11 DC_12 if DC_07==.& DC_08 ==.&DC_09 ==.&DC_10==.& DC_11 ==.&DC_12==.
replace diuretics=0 if ANTIHYPERTENSIVE==0&diuretics==.
tab diuretics visit, m 

*Loop diuretics 
gen loop_diuretics=1 if DC_07==6 |  DC_08==6 | DC_09==6 | DC_10==6 | DC_11 ==6 |DC_12==6
replace loop_diuretics=0 if loop_diuretics==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var loop_diuretics "Loop Diuretics"
lab val loop_diuretics yesno
tab1 diuretics loop_diuretics , m 
edit pid ANTIHYPERTENSIVE loop_diuretics  BR_01 DC_07 GN_07 BR_02  DC_08 GN_08 BR_03  DC_09 GN_09 BR_04 DC_10 GN_10 BR_05 DC_11 GN_11 BR_06 DC_12 GN_12 if loop_diuretics==.

*MRA 
gen MRA=1 if DC_07==8 |  DC_08==8 | DC_09==8 | DC_10==8| DC_11 ==8|DC_12==8 
replace MRA=0 if MRA==. & (DC_07!=. |  DC_08!=. | DC_09!=.| DC_10!=. | DC_11!=. |DC_12!=.)
lab var MRA "MRAs"
lab val MRA yesno
tab MRA , m
edit pid ANTIHYPERTENSIVE MRA BR_01 DC_07 GN_07 BR_02  DC_08 GN_08 BR_03  DC_09 GN_09 BR_04 DC_10 GN_10 BR_05 DC_11 GN_11 BR_06 DC_12 GN_12 if MRA==.

*SGLT-2 inhibitors
edit pid  DC_19 DC_20 DC_21 DC_22 DC_23 DC_24 if VisitName_n==1 & dropout!=1
gen SGLT=1 if DC_19==7 | DC_20==7 | DC_21==7 | DC_22==7 | DC_23==7 | DC_24==7
replace SGLT=0 if SGLT==. & (DC_19!=. | DC_20!=. | DC_21!=. | DC_22!=. | DC_23!=. | DC_24!=.)
lab val SGLT yesno
lab var SGLT "SGLT-2 inhibitors"
tab SGLT, m 

**DRUG COMBINATIONS**
*ACEi / ARB
tab1 ACEi ARB
egen ACEi_ARB=rowtotal(ACEi ARB)
tab ACEi_ARB
replace ACEi_ARB=. if ACEi==.& ARB==.
recode ACEi_ARB (0=0 "No ACEi/ARB") (1/max=1 "ACEi/ARB"), gen(ACEi_ARB_cat)
tab1 ACEi ARB ACEi_ARB_cat , m 
edit pid ANTIHYPERTENSIVE DC_07 DC_08 DC_09 DC_10 DC_11 DC_12 ACEi ARB ACEi_ARB if ACEi_ARB_cat==.
tab ACEi_ARB_cat  visit,m

*GDMT - CHD - Definition 1 - Antiplatelet + Lipid lowering drug (statin) + ≥ 2 anti-hypertensive drugs
gen diuretics_all=1 if diuretics==1 | MRA==1 | loop_diuretics==1
replace diuretics_all=0 if diuretics_all==.
lab def diuretics_all 0"Not taking diuretics/loop diuretics/MRA" 1"Taking diuretics/loop diuretics/MRA", modify
lab val diuretics_all diuretics_all
tab diuretics_all if VisitName_n==1 & dropout!=1 
lab var diuretics_all "Diuretics + loop diuretics + MRA, combined"

egen htn_drug=rowtotal(ACEi_ARB BB CCB diuretics_all)
recode htn_drug (0/1=0 "<2 HTN drug")(2/max=1 ">=2 HTN drug"), gen(htn_drug_atleast2)
tab htn_drug_atleast2

egen ap_lp_htnat2class=rowtotal(antiplatelet_cat lipid_lowering_cat htn_drug_atleast2)
edit pid antiplatelet_cat lipid_lowering_cat  ACEi_ARB_cat BB CCB diuretics_all htn_drug_atleast2 ap_lp_htnat2class
lab def ap_lp_htnat2class 0 "None" 1 "Any 1" 2 "Any 2 combinations" 3 "All 3 combinations", modify 
lab val ap_lp_htnat2class ap_lp_htnat2class 
tab ap_lp_htnat2class if CHD==1, m  
lab var ap_lp_htnat2class "Antiplatelet + Lipid lowering (statin) + At least 2 Hypertensives"

*GDMT - CHD - Definition 2 - Antiplatelet + Lipid lowering drug (statin) + ACEi or ARB + Beta Blocker
egen ap_lipid_ace_arb_bb=rowtotal(antiplatelet_cat lipid_lowering_cat  ACEi_ARB_cat BB)
lab def condition1 0 "No Combination" 1 "Any One condition" 2 "Any 2 conditions" 3 "Any 3 conditions" 4 "All 4 conditions", modify
lab val ap_lipid_ace_arb_bb condition1
tab ap_lipid_ace_arb_bb if CHD==1, m 
lab var ap_lipid_ace_arb_bb "Antiplatelet + Lipid lowering (statin) + ACEi / ARB + Beta Blocker"
tab ap_lipid_ace_arb_bb if CHD==1 & visit==1, m

*GDMT - STROKE - Definition 1 - Antiplatelet + Lipid lowering drug (statin)
egen aspirin_statin=rowtotal(antiplatelet_cat lipid_lowering_cat)
lab def aspirin_statin 0 "None" 1 "Any 1" 2 "Both", modify
lab val aspirin_statin aspirin_statin 
tab aspirin_statin if STROKE==1, m 
lab var aspirin_statin "Antiplatelet + Lipid lowering (statin)"

*GDMT - STROKE - Definition 2 - Antiplatelet + Lipid lowering drug (statin) + ACEi or ARB / Diuretic
egen ACEi_ARB_diur=rowtotal(ACEi ARB diuretics_all)
recode ACEi_ARB_diur (0=0 "No ACEi/ARB/Diuretics") (1/max=1 "ACEi/ARB/Diuretics"), gen(ACEi_ARB_diur_cat)

egen ap_lipid_ace_arb_diu=rowtotal(antiplatelet_cat lipid_lowering_cat ACEi_ARB_diur_cat)
lab def ap_lipid_ace_arb_diu 0 "None" 1 "Any 1" 2 "Any 2 combinations" 3"All 3 combinations", modify
lab val ap_lipid_ace_arb_diu ap_lipid_ace_arb_diu
tab ap_lipid_ace_arb_diu if STROKE==1, m 
lab var ap_lipid_ace_arb_diu "Antiplatelet + Lipid lowering (statin) + ACEi / ARB / Diuretic"

*GDMT - STROKE - Definition 3 - Antiplatelet + Lipid lowering drug (statin) + ≥ 2 anti-hypertensive drugs
tab ap_lp_htnat2class if STROKE==1, m

*GDMT - HEART FAILURE - Definition 1 - ACEi/ARB/ARNI + Beta blocker + MRA
egen ACEi_ARB_ARNI=rowtotal(ACEi ARB ARNI)
recode ACEi_ARB_ARNI (0=0 "No ACEi/ARB/ARNI") (1/max=1 "ACEi/ARB/ARNI"), gen(ACEi_ARB_ARNI_cat)
tab ACEi_ARB_ARNI_cat

egen ace_arb_arni_bb_mra=rowtotal(ACEi_ARB_ARNI_cat BB MRA)
lab def condition2 0 "None" 1 "Any 1" 2 "Any 2 combinatons" 3 "All 3", modify
lab val ace_arb_arni_bb_mra condition2
tab ace_arb_arni_bb_mra if HF==1, m 
lab var ace_arb_arni_bb_mra "ACEi/ARB/ARNI + Beta blocker + MRA"

*GDMT - HEART FAILURE - Definition 2 - ACEi/ARB/ARNI + Beta blocker + MRA + SGLT2i
egen ace_arb_arni_bb_mra_sglt=rowtotal(ACEi_ARB_ARNI_cat BB MRA SGLT)
lab def ace_arb_arni_bb_mra_sglt 0 "None" 1 "Any 1" 2 "Any 2 combinations" 3 "Any 3 combinations" 4"All 4 combinations", modify
lab val ace_arb_arni_bb_mra_sglt ace_arb_arni_bb_mra_sglt 
tab ace_arb_arni_bb_mra_sglt if HF==1, m 
lab var ace_arb_arni_bb_mra_sglt "ACEi/ARB/ARNI + Beta blockers + MRA + SGLT2i"

*Table 1. Baseline characteristics of study participants
table1 if VisitName_n==1,by(TreatmentArm) vars(AGE contn\GENDER cat\SCHOOLING_YEARS contn\ SCHOOLING_YEARS conts\ edu_years cat\INCOME contn\ INCOME conts\CHD cat\ HF cat\ STROKE cat\ HYPER__PRE cat\ DIABETES cat\ SBP_AVG  contn \DBP_AVG  contn \TOTAL_CHOLESTEROL  contn \LDLC  contn \FBG contn\ HBA1C   contn \   antiplatelet_cat cat\  lipid_lowering_cat cat\ACEi_ARB_cat cat\BB cat \CCB cat\diuretics cat) format(%2.1f) saving(CQIP_Paper.xls, sheet("Table1", modify) ) missing cmissing

table1 if VisitName_n==1 & HF==1,by(TreatmentArm) vars(ARNI cat) format(%2.1f) saving(CQIP_Paper.xls, sheet("Table1_ARNI", modify) ) missing cmissing

save cqip_analysis_data.dta,replace

**GENERATING SAE
gen sae=1 if pid=="AIIMS_017"|pid=="AIIMS_050"|pid=="GB Pant_046"|pid=="GB Pant_049"|pid=="SDM_006"|pid=="SDM_021"|pid=="SDM_024"|pid=="SDM_029"|pid=="SDM_068"|pid=="SDM_070"|pid=="SDM_071"|pid=="SDM_096"|pid=="SDM_104"|pid=="SDM_106"|pid=="SDM_114"|pid=="SDM_115"|pid=="SDM_116"|pid=="SDM_126"|pid=="SDM_137"
drop if sae==1 &  VisitName_n==18


*Generating missing observations at endline for ITT - Run the whole code together
encode pid, gen(id)
drop if visit==2
levelsof id, local(idlist)
levelsof id, local(idlist)
fre visit
levelsof visit, local(timelist)
clear
set obs `: word count `idlist''
gen id = .
local i = 1
foreach val of local idlist {
    replace id = `val' in `i'
    local ++i
}
expand `: word count `timelist''
gen time = .
local i = 1
foreach val of local timelist {
    quietly replace time = `val' in `i'/`=`i' + `: word count `idlist'' - 1'
    local i = `i' + `: word count `idlist''
}
tempfile fullgrid
save `fullgrid'
use "/Users/dr.meetushijain/Library/CloudStorage/OneDrive-CentreForChronicDiseaseControl/PHFI projects/C-QIP/CQIP-Paper/cqip_analysis_data.dta"
encode pid, gen(id)
drop if visit==2
ren visit time
merge 1:1 id time using `fullgrid'

edit
replace pid = "AIIMS_017" in 786
sort pid
edit pid
replace pid = "AIIMS_021" in 1
replace pid = "AIIMS_050" in 2
replace pid = "GB Pant_046" in 3
replace pid = "GB Pant_049" in 4
replace pid = "GB Pant_104" in 5
replace pid = "GB Pant_114" in 6
replace pid = "SDM_021" in 7
replace pid = "SDM_024" in 8
replace pid = "SDM_070" in 9
replace pid = "SDM_071" in 10
replace pid = "SDM_096" in 11
replace pid = "SDM_104" in 12
replace pid = "SDM_106" in 13
replace pid = "SDM_114" in 14
replace pid = "SDM_126" in 15
replace pid = "SDM_137" in 16
sort pid
drop group
replace TreatmentArm=1 if pid=="AIIMS_017" & time==3
edit pid time TreatmentArm
replace TreatmentArm=1 if pid=="AIIMS_021" & time==3
replace TreatmentArm=1 if pid=="AIIMS_050" & time==3
replace TreatmentArm=1 if pid=="GB Pant_046" & time==3
replace TreatmentArm=2 if pid=="GB Pant_049" & time==3
replace TreatmentArm=1 if pid=="GB Pant_104" & time==3
replace TreatmentArm=2 if pid=="GB Pant_114" & time==3
replace TreatmentArm=1 if pid=="SDM_021" & time==3
replace TreatmentArm=2 if pid=="SDM_024" & time==3
replace TreatmentArm=2 if pid=="SDM_070" & time==3
replace TreatmentArm=2 if pid=="SDM_071" & time==3
replace TreatmentArm=2 if pid=="SDM_096" & time==3
replace TreatmentArm=2 if pid=="SDM_104" & time==3
replace TreatmentArm=2 if pid=="SDM_106" & time==3
replace TreatmentArm=1 if pid=="SDM_114" & time==3
replace TreatmentArm=2 if pid=="SDM_126" & time==3
replace TreatmentArm=1 if pid=="SDM_137" & time==3
egen group=group(time TreatmentArm )
lab def group 1"cqip_base" 2"usual_base" 3"cqip_eos" 4"usual_eos", modify
lab val group group
fre group
ren time visit


encode SiteName,gen(site)
tab site
gen SEX= BIO_SEX

**Assigning missing site values at endline
gen Site=.
replace Site = site if visit==1
egen baseline_site=max(Site), by(pid)
replace site=baseline_site if visit==3 & missing(site)
fre site

save cqip_base_eos.dta,replace

clear

use "/Users/dr.meetushijain/Library/CloudStorage/OneDrive-CentreForChronicDiseaseControl/PHFI projects/C-QIP/CQIP-Paper/cqip_base_eos.dta"

encode pid,gen(pid1)


**Creating baseline variables at endline
capture program drop endline
program define endline
egen `1'_all=max(cond(visit==1, `1',.)), by(pid)
tab `1'_all
lab def `1'_all 1"Yes" 0"No", modify
lab val `1'_all `1'_all
fre `1'_all
end
endline CHD
endline STROKE
endline HF

*Table 2. Prescription of medications and adherence to self-care measures at end of study visit
*Medication use
table1 if visit==3,by(TreatmentArm)vars(antiplatelet_cat cat\lipid_lowering_cat cat\ACEi_ARB_cat cat\BB cat\CCB cat\diuretics cat) format(%2.1f) saving(CQIP_Paper.xls,sheet("Table2",replace)) missing

table1 if visit==3 & HF_all==1,by(TreatmentArm) vars(ARNI cat) format(%2.1f) saving(CQIP_Paper.xls, sheet("Table2_ARNI", modify) ) missing cmissing

*CHD
table1 if visit==3 & CHD_all==1,by(TreatmentArm)vars(antiplatelet_cat cat\lipid_lowering_cat cat\ACEi_ARB_cat cat\BB cat\ap_lipid_ace_arb_bb cat) format(%2.1f) saving(CQIP_Paper.xls,sheet("Table2_CHD",replace)) missing

*STROKE
table1 if visit==3 & STROKE_all==1,by(TreatmentArm)vars(antiplatelet_cat cat\lipid_lowering_cat cat\ACEi_ARB_diur_cat cat\ap_lipid_ace_arb_diu cat) format(%2.1f) saving(CQIP_Paper.xls,sheet("Table2_STROKE",replace)) missing 

*Heart Failure
table1 if visit==3 & HF_all==1,by(TreatmentArm)vars(ACEi_ARB_ARNI_cat cat\BB cat\MRA cat\SGLT cat\ace_arb_arni_bb_mra cat\ace_arb_arni_bb_mra_sglt cat) format(%2.1f) saving(CQIP_Paper.xls,sheet("Table2_HF",replace)) missing 

*Patient-level measures
tab1 TAKE_MEDICATIONS FOLLOW_DIET PHY_ACT 
for var TAKE_MEDICATIONS FOLLOW_DIET PHY_ACT :recode X (min/3=0 "< 4days") (4/max=1 ">=4 days"),gen(X_cat2)
tab1 TAKE_MEDICATIONS_cat2 FOLLOW_DIET_cat2 PHY_ACT_cat2 

table1 if visit==3,by(TreatmentArm) vars(TAKE_MEDICATIONS_cat2 cat\FOLLOW_DIET_cat2 cat\ PHY_ACT_cat2 cat) format(%2.1f) saving(CQIP_Paper.xls,sheet("Table2_measures",replace)) missing

****GEE analysis-adjusting for age, sex and site******
*Medication use
capture prog drop adjusted
program define adjusted 
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit,family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted antiplatelet_cat  
adjusted lipid_lowering_cat 
adjusted ACEi_ARB_cat  
adjusted BB 
adjusted CCB 
adjusted diuretics 

xtset pid1 visit
xtgee ARNI ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1,family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

**GDMT
*single drug use - CHD
capture prog drop adjusted_chd
program define adjusted_chd
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit if CHD_all==1,family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted_chd antiplatelet_cat  
adjusted_chd lipid_lowering_cat 
adjusted_chd ACEi_ARB_cat  
adjusted_chd BB 

*single drug use - Stroke
capture prog drop adjusted_stk
program define adjusted_stk
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit if STROKE_all==1,family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted_stk antiplatelet_cat  
adjusted_stk lipid_lowering_cat 
adjusted_stk ACEi_ARB_diur_cat

*single drug use - HF
capture prog drop adjusted_hf
program define adjusted_hf
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1,family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted_hf ACEi_ARB_ARNI_cat  
adjusted_hf BB 
adjusted_hf MRA
adjusted_hf SGLT 

**Recoding GDMT variables for regression
recode ap_lipid_ace_arb_bb (0/3=0 "<4 combinations") (4=1 "All 4 combinations"), gen(ap_lipid_ace_arb_bb_n)
recode ap_lipid_ace_arb_diu (0/2=0 "<3 combinations") (3=1 "All 3 combinations"), gen(ap_lipid_ace_arb_diu_n)
recode ace_arb_arni_bb_mra (0/2=0 "<3 combinations") (3=1 "All 3 combinations"), gen(ace_arb_arni_bb_mra_n)
recode ace_arb_arni_bb_mra_sglt (0/3=0 "<4 combinations") (4=1 "All 4 combinations"), gen(ace_arb_arni_bb_mra_sglt_n)

*GDMT definitions
*CHD 
xtset pid1 visit
xtgee ap_lipid_ace_arb_bb_n ib2.TreatmentArm AGE i.SEX i.site i.visit if CHD_all==1,family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*Stroke 
xtset pid1 visit
xtgee ap_lipid_ace_arb_diu_n ib2.TreatmentArm AGE i.SEX i.site i.visit if STROKE_all==1,family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*HF - Def 1
xtset pid1 visit
xtgee ace_arb_arni_bb_mra_n ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1,family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*HF - Def 2
xtset pid1 visit
xtgee ace_arb_arni_bb_mra_sglt_n ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1,family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*Patient-level measures
adjusted TAKE_MEDICATIONS_cat2
adjusted FOLLOW_DIET_cat2
adjusted PHY_ACT_cat2

*Figure 3 - Processes of care, health status, and treatment satisfaction at EOS
*Global health scale
*Renaming items as per the scale - Ref: https://marcqi.org/dev/wp-content/uploads/2019/07/PROMIS-Global-Health.pdf
gen global01= HEALTH
gen global02= QOL
gen global03= PHY_HEALTH
gen global04= MENTAL_HEALTH
gen global05= SOCOAL_RELATIONSHIPS
gen global06= EVERYDAY_PHY_ACT
gen global07= PAIN
gen global08= FATIGUE
gen global09= SOCIAL_ROLES
gen global10= EMOTIONAL_PROBLEMS_7DAYS

*Generating physical and mental health scores
gen global_physical= global03+ global06+ global07+ global08
gen global_mental= global02+ global04+ global05+ global10
tab1 global_physical global_mental

*Generating tscore value variable for physical health - Ref :https://www.healthmeasures.net/images/PROMIS/manuals/Scoring_Manuals_/PROMIS_Global_Health_Scoring_Manual.pdf
gen t_gph=26.7 if global_physical==7
replace t_gph=29.6 if global_physical==8
replace t_gph=32.4 if global_physical==9
replace t_gph=34.9 if global_physical==10
replace t_gph=37.4 if global_physical==11
replace t_gph=39.8 if global_physical==12
replace t_gph=42.3 if global_physical==13
replace t_gph=44.9 if global_physical==14
replace t_gph=47.7 if global_physical==15
replace t_gph=50.8 if global_physical==16
replace t_gph=54.1 if global_physical==17
replace t_gph=57.7 if global_physical==18
replace t_gph=61.9 if global_physical==19
replace t_gph=67.7 if global_physical==20

*Generating tscore value variable for mental health
gen t_gmh=31.3 if global_mental==7
replace t_gmh=33.8 if global_mental==8
replace t_gmh=36.3 if global_mental==9
replace t_gmh=38.8 if global_mental==10
replace t_gmh=41.1 if global_mental==11
replace t_gmh=43.5 if global_mental==12
replace t_gmh=45.8 if global_mental==13
replace t_gmh=48.3 if global_mental==14
replace t_gmh=50.8 if global_mental==15
replace t_gmh=53.3 if global_mental==16
replace t_gmh=56.0 if global_mental==17
replace t_gmh=59.0 if global_mental==18
replace t_gmh=62.5 if global_mental==19
replace t_gmh=67.6 if global_mental==20

*Average scores
bysort group: sum t_gph
bysort group: sum t_gmh

*Combined mental and physical
gen t_ghs=t_gph + t_gmh
bysort group:sum t_ghs

*Treatment satisfaction score
gen treat_score2=( SAT_CURRENT_TREATMENT + CONTINUE_TREATMENT + UNDERSTAND_HD + RECOMMEND_TREATMENT )/4
sum treat_score2
bysort group: sum treat_score2

table1 if visit==3,by(TreatmentArm) vars(CLINIC_VISITS contn\BLOOD_PRESSURE_CHECKS contn\CHOL_TESTING contn\BG_TESTING contn\ECG contn\t_gph contn\t_gmh contn\t_ghs contn\treat_score2 contn\SAT_CURRENT_TREATMENT contn\CONTINUE_TREATMENT contn\UNDERSTAND_HD contn\RECOMMEND_TREATMENT contn) format(%2.1f) saving(CQIP_Paper.xls,sheet("Figure3",replace)) missing cmissing


capture program drop adjusted2
program define adjusted2
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit, family(gaussian) link(identity) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect
predict mu`1', mu
gen resid`1' = CLINIC_VISITS - mu`1'
scatter resid`1' mu`1', yline(0) name(g1`1', replace)
histogram resid`1', normal name(g2`1', replace)
qnorm resid`1', name(g3`1', replace)
graph box resid`1', over(TreatmentArm) name(g4`1', replace)
scatter resid`1' visit, name(g5`1', replace)
lowess resid`1' visit, name(g6`1', replace)
graph combine g1`1' g2`1' g3`1' g4`1' g5`1' g6`1', cols(2)
end

adjusted2 CLINIC_VISITS 
adjusted2 BLOOD_PRESSURE_CHECKS 
adjusted2 CHOL_TESTING 
adjusted2 BG_TESTING 
adjusted2 ECG 
adjusted2 t_gph
adjusted2 t_gmh
adjusted2 t_ghs
adjusted2 treat_score2
adjusted2 SAT_CURRENT_TREATMENT
adjusted2 CONTINUE_TREATMENT
adjusted2 UNDERSTAND_HD
adjusted2 RECOMMEND_TREATMENT

*Figure 4 - Clinical measures by treatment group at EOS
table1 if visit==3,by(TreatmentArm) vars(SBP_AVG contn\DBP_AVG contn\HEART_RATE contn\TOTAL_CHOLESTEROL contn\HDLC contn\TRIGLYCERIDES contn\LDLC contn\FBG contn\HBA1C contn\SERUM_CREATININE contn\WEIGHT contn\BMI contn) format(%2.1f) saving(CQIP_Paper.xls,sheet("Figure4",replace)) missing cmissing

table1 if visit==3 & HF_all==0,by(TreatmentArm) vars(LDLC contn) format(%2.1f) saving(CQIP_Paper.xls,sheet("Figure4_LDL",replace)) missing cmissing

bysort pid (visit):gen DIAB_all=DIABETES[1] //create baseline variable 
table1 if visit==3 & DIAB_all==1,by(TreatmentArm) vars(HBA1C contn) format(%2.1f) saving(CQIP_Paper.xls,sheet("Figure4_HbA1c",replace)) missing cmissing

adjusted2 SBP_AVG
adjusted2 DBP_AVG 
adjusted2 HEART_RATE
adjusted2 TOTAL_CHOLESTEROL 
adjusted2 HDLC 
adjusted2 TRIGLYCERIDES 
adjusted2 LDLC  

*LDL Without HF patients
xtset pid1 visit
xtgee LDLC  ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==0, family(gaussian) link(identity) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect

adjusted2 FBG  
adjusted2 HBA1C  

*HbA1c with only diabetes patients
xtset pid1 visit
xtgee HBA1C  ib2.TreatmentArm AGE i.SEX i.site i.visit if DIAB_all==1, family(gaussian) link(identity) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect

adjusted2 SERUM_CREATININE  
adjusted2 WEIGHT 
adjusted2 BMI

****************************************
***SUB-GROUP ANALYSIS - EOS (visit==3)*** 
**Age
recode AGE min/49=1 50/64=2 65/max=3, gen(agecat)
lab def agecat 1"≤49" 2"50-64" 3"≥65" 4"71-85", modify
lab val agecat agecat
fre agecat

**Education - creating endline variable
egen edu=max(cond(visit==1, edu_years,.)), by(pid)
lab def edu 1"<5"  2"5-10" 3"11-12" 4">12", modify
lab val edu edu
fre edu

**Income
recode INCOME (min/14999=1 "<15000") (15000/30000=2 "15000-30000") (30001/max=3 ">30000"), gen(inc)
fre inc
egen income=max(cond(visit==1, inc,.)), by(pid)
lab def income 1"<15000" 2"15000-30000" 3">30000"
lab val income income
fre income

**Facility type
gen facility=.
replace facility = hosp if visit==1
egen baseline_hosp=max(facility), by(pid)
replace hosp=baseline_hosp if visit==3 & missing(hosp)
fre hosp

**BMI
recode BMI min/24.99=1 25/max=2, gen(bmicat)
lab def bmicat 1"<25" 2">=25", modify
lab val bmicat bmicat
fre bmicat

**Hypertension
egen hyper=max(cond(visit==1, HYPER__PRE,.)), by(pid)
fre hyper
recode hyper 2=0
lab def hyper 1"Yes" 0"No"
lab val hyper hyper
fre hyper

**Diabetes
egen dm=max(cond(visit==1, DIABETES,.)), by(pid)
fre dm
lab def dm 1"Yes" 0"No"
lab val dm dm
fre dm

**BP - 140/90
gen bp=.
replace bp=1 if (SBP_AVG<140&SBP_AVG~=.) &( DBP_AVG<90 &DBP_AVG~=.)
replace bp=0 if (SBP_AVG>=140&SBP_AVG~=.)|(DBP_AVG>=90&DBP_AVG~=.)&bp==.
label define bp 0">=140/90" 1"<140/90", modify
lab val bp bp
tab bp

fre bp
gen bp_140=1 if bp==0
replace bp_140=0 if bp==1
label def bp_140 0"<140/90 mmHg" 1"≥140/90 mmHg", modify
lab val bp_140 bp_140
fre bp_140

**LDL
recode LDLC (min/99.9=1 "<100") (100/max=0 ">=100"),gen(ldlcat100)
recode ldlcat100 0=1 1=0, gen(ldl100)
lab def ldl100 0"<100" 1">=100"
lab val ldl100 ldl100
fre ldl100

**Adherence to medicines
capture program drop sub1
program define sub1
bysort `1':tab TAKE_MEDICATIONS_cat2 TreatmentArm if visit==3,col
xtset pid1 visit
bysort `1':xtgee TAKE_MEDICATIONS_cat2 ib2.TreatmentArm i.site i.visit,family(poisson) link(log) eform robust
xtgee TAKE_MEDICATIONS_cat2 ib2.TreatmentArm##c.`1' i.site i.visit,family(poisson) link(log) eform robust
testparm ib2.TreatmentArm#c.`1'
end

sub1 agecat
sub1 SEX
sub1 edu
sub1 income
sub1 hosp
sub1 bmicat
sub1 hyper
sub1 dm
sub1 CHD_all
sub1 HF_all
sub1 STROKE_all
sub1 bp_140
sub1 ldl100

**Systolic blood pressure
bysort pid (visit):gen SBP_AVG0=SBP_AVG[1] //create baseline variable 

capture program drop sub2
program define sub2
bysort `1':sum SBP_AVG if visit==3 & TreatmentArm==1
bysort `1':sum SBP_AVG if visit==3 & TreatmentArm==2
bysort `1':regress SBP_AVG ib2.TreatmentArm i.site SBP_AVG0 if visit==3
regress SBP_AVG ib2.TreatmentArm##c.`1' i.site SBP_AVG0 if visit==3
testparm ib2.TreatmentArm#c.`1'
end

sub2 agecat
sub2 SEX
sub2 edu
sub2 income
sub2 hosp
sub2 bmicat
sub2 hyper
sub2 dm
sub2 CHD_all
sub2 HF_all
sub2 STROKE_all
sub2 bp_140
sub2 ldl100

*************************
**IPTW**
use "/Users/dr.meetushijain/Library/CloudStorage/OneDrive-CentreForChronicDiseaseControl/PHFI projects/C-QIP/CQIP-Paper/cqip_analysis_data.dta", clear

bys pid: gen n_obs = _N
gen complete = (n_obs == 2)

keep if visit == 1
for var FOLLOW_DIET PHY_ACT TAKE_MEDICATIONS:recode X (min/3=0 "< 4days") (4/max=1 ">=4 days"),gen(X_cat2)
tab1 FOLLOW_DIET_cat2 PHY_ACT_cat2 TAKE_MEDICATIONS_cat2

encode SiteName, gen (site)
recode site (1/2=1 "Public") (3/4=2 "Private"), gen(fac_type)
logit complete AGE GENDER i.fac_type i.edu_years INCOME  TreatmentArm SBP_AVG LDLC TAKE_MEDICATIONS_cat2
predict prop1
predict p_obs, pr
propwt complete prop1, ipt

graph tw kdensity p_obs if complete == 0 || kdensity p_obs if complete == 1
pbalchk complete AGE GENDER fac_type edu_years INCOME  TreatmentArm SBP_AVG LDLC TAKE_MEDICATIONS_cat2
pbalchk complete AGE GENDER fac_type edu_years INCOME  TreatmentArm SBP_AVG LDLC TAKE_MEDICATIONS_cat2, wt(ipt_wt)

keep pid ipt_wt
merge m:m pid using "/Users/dr.meetushijain/Library/CloudStorage/OneDrive-CentreForChronicDiseaseControl/PHFI projects/C-QIP/CQIP-Paper/cqip_base_eos.dta"


encode pid,gen(pid1)

capture program drop base
program define base
bysort pid (visit):gen `1'0=`1'[1] //create baseline variable 
sort pid visit
edit pid visit `1' `1'0
end
base SBP_AVG
base DBP_AVG
base LDLC
base HBA1C

******Creating baseline variables at endline
capture program drop endline
program define endline
egen `1'_all=max(cond(visit==1, `1',.)), by(pid)
tab `1'_all
lab def `1'_all 1"Yes" 0"No", modify
lab val `1'_all `1'_all
fre `1'_all
end
endline CHD
endline STROKE
endline HF

*Regression
capture prog drop adjusted
program define adjusted 
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit[pweight=ipt_wt],family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end


adjusted antiplatelet_cat  
adjusted lipid_lowering_cat 
adjusted ACEi_ARB_cat  
adjusted BB 
adjusted CCB 
adjusted diuretics 

xtset pid1 visit
xtgee ARNI ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

**GDMT
*single drug use - CHD
capture prog drop adjusted_chd
program define adjusted_chd
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit if CHD_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted_chd antiplatelet_cat  
adjusted_chd lipid_lowering_cat 
adjusted_chd ACEi_ARB_cat  
adjusted_chd BB 

*single drug use - Stroke
capture prog drop adjusted_stk
program define adjusted_stk
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit if STROKE_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted_stk antiplatelet_cat  
adjusted_stk lipid_lowering_cat 
adjusted_stk ACEi_ARB_diur_cat

*single drug use - HF
capture prog drop adjusted_hf
program define adjusted_hf
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins i.TreatmentArm#i.visit
//overall group difference
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform
end

adjusted_hf ACEi_ARB_ARNI_cat  
adjusted_hf BB 
adjusted_hf MRA
adjusted_hf SGLT 

**Recoding GDMT variables for regression
recode ap_lp_htnat2class (0/2=0 "<3 combinations") (3=1 "All 3 combinations"), gen(ap_lp_htnat2class_n)
recode ap_lipid_ace_arb_bb (0/3=0 "<4 combinations") (4=1 "All 4 combinations"), gen(ap_lipid_ace_arb_bb_n)
recode aspirin_statin (0/1=0 "<2 combinations") (2=1 "Both"), gen(aspirin_statin_n)
recode ap_lipid_ace_arb_diu (0/2=0 "<3 combinations") (3=1 "All 3 combinations"), gen(ap_lipid_ace_arb_diu_n)
recode ace_arb_arni_bb_mra (0/2=0 "<3 combinations") (3=1 "All 3 combinations"), gen(ace_arb_arni_bb_mra_n)
recode ace_arb_arni_bb_mra_sglt (0/3=0 "<4 combinations") (4=1 "All 4 combinations"), gen(ace_arb_arni_bb_mra_sglt_n)

**Regression - Marginal estimates adjusted for age, sex and site
*MI - Def 1
xtset pid1 visit
xtgee ap_lp_htnat2class_n ib2.TreatmentArm AGE i.SEX i.site i.visit if CHD_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*MI - Def 2
xtset pid1 visit
xtgee ap_lipid_ace_arb_bb_n ib2.TreatmentArm AGE i.SEX i.site i.visit if CHD_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*Stroke - Def 1
xtset pid1 visit
xtgee aspirin_statin_n ib2.TreatmentArm AGE i.SEX i.site i.visit if STROKE_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*Stroke - Def 2
xtset pid1 visit
xtgee ap_lipid_ace_arb_diu_n ib2.TreatmentArm AGE i.SEX i.site i.visit if STROKE_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*Stroke - Def 3
xtset pid1 visit
xtgee ap_lp_htnat2class_n ib2.TreatmentArm AGE i.SEX i.site i.visit if STROKE_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*HF - Def 1
xtset pid1 visit
xtgee ace_arb_arni_bb_mra_n ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

*HF - Def 2
xtset pid1 visit
xtgee ace_arb_arni_bb_mra_sglt_n ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==1 [pweight=ipt_wt],family(poisson) link(log) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect eform

**Adherence >=4 days
tab1 FOLLOW_DIET PHY_ACT TAKE_MEDICATIONS
for var FOLLOW_DIET PHY_ACT TAKE_MEDICATIONS:recode X (min/3=0 "< 4days") (4/max=1 ">=4 days"),gen(X_cat2)
tab1 FOLLOW_DIET_cat2 PHY_ACT_cat2 TAKE_MEDICATIONS_cat2

****Daily; >3/week; Occasionally; Never (Combine Daily; >3/week as Yes;  and Occasionally and never as No  )
fre FRUITS_FRUITJUICE GREEN_LEAFY_VEG SALTY_DEEPFRIED EGG_MEAT_POULTRY MILK_PRODUCTS
for var  FRUITS_FRUITJUICE GREEN_LEAFY_VEG EGG_MEAT_POULTRY MILK_PRODUCTS:recode X (1/2=1 "Daily or >3 per week") (3/4=0 "Occasionally or never"), gen(X_cat)
tab1 FRUITS_FRUITJUICE_cat GREEN_LEAFY_VEG_cat EGG_MEAT_POULTRY_cat MILK_PRODUCTS_cat

**Note salty/deep fried ocassional and never is good
recode SALTY_DEEPFRIED 3/4=1 1/2=0,gen(SALTY_DEEPFRIED_cat)
tab1 FRUITS_FRUITJUICE_cat GREEN_LEAFY_VEG_cat SALTY_DEEPFRIED_cat EGG_MEAT_POULTRY_cat MILK_PRODUCTS_cat

adjusted TAKE_MEDICATIONS_cat2
adjusted FOLLOW_DIET_cat2
adjusted PHY_ACT_cat2
adjusted FRUITS_FRUITJUICE_cat 
adjusted GREEN_LEAFY_VEG_cat 
adjusted SALTY_DEEPFRIED_cat
adjusted EGG_MEAT_POULTRY_cat
adjusted MILK_PRODUCTS_cat

****Treatment satisfaction score
gen treat_score2=( SAT_CURRENT_TREATMENT + CONTINUE_TREATMENT + UNDERSTAND_HD + RECOMMEND_TREATMENT )/4
sum treat_score2
bysort group: sum treat_score2

capture program drop adjusted2
program define adjusted2
xtset pid1 visit
xtgee `1'  ib2.TreatmentArm AGE i.SEX i.site i.visit [pweight=ipt_wt], family(gaussian) link(identity) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect
end

adjusted2 SAT_CURRENT_TREATMENT
adjusted2 CONTINUE_TREATMENT
adjusted2 UNDERSTAND_HD
adjusted2 RECOMMEND_TREATMENT
adjusted2 treat_score2

******Global health scale
***Renaming items as per the scale - Ref: https://marcqi.org/dev/wp-content/uploads/2019/07/PROMIS-Global-Health.pdf
gen global01= HEALTH
gen global02= QOL
gen global03= PHY_HEALTH
gen global04= MENTAL_HEALTH
gen global05= SOCOAL_RELATIONSHIPS
gen global06= EVERYDAY_PHY_ACT
gen global07= PAIN
gen global08= FATIGUE
gen global09= SOCIAL_ROLES
gen global10= EMOTIONAL_PROBLEMS_7DAYS

**Generating physical and mental health scores
gen global_physical= global03+ global06+ global07+ global08
gen global_mental= global02+ global04+ global05+ global10
tab1 global_physical global_mental

**Generating tscore value variable for physical health - Ref :https://www.healthmeasures.net/images/PROMIS/manuals/Scoring_Manuals_/PROMIS_Global_Health_Scoring_Manual.pdf
gen t_gph=26.7 if global_physical==7
replace t_gph=29.6 if global_physical==8
replace t_gph=32.4 if global_physical==9
replace t_gph=34.9 if global_physical==10
replace t_gph=37.4 if global_physical==11
replace t_gph=39.8 if global_physical==12
replace t_gph=42.3 if global_physical==13
replace t_gph=44.9 if global_physical==14
replace t_gph=47.7 if global_physical==15
replace t_gph=50.8 if global_physical==16
replace t_gph=54.1 if global_physical==17
replace t_gph=57.7 if global_physical==18
replace t_gph=61.9 if global_physical==19
replace t_gph=67.7 if global_physical==20

**Generating tscore value variable for mental health
gen t_gmh=31.3 if global_mental==7
replace t_gmh=33.8 if global_mental==8
replace t_gmh=36.3 if global_mental==9
replace t_gmh=38.8 if global_mental==10
replace t_gmh=41.1 if global_mental==11
replace t_gmh=43.5 if global_mental==12
replace t_gmh=45.8 if global_mental==13
replace t_gmh=48.3 if global_mental==14
replace t_gmh=50.8 if global_mental==15
replace t_gmh=53.3 if global_mental==16
replace t_gmh=56.0 if global_mental==17
replace t_gmh=59.0 if global_mental==18
replace t_gmh=62.5 if global_mental==19
replace t_gmh=67.6 if global_mental==20

****Average scores
bysort group: sum t_gph
bysort group: sum t_gmh

**Combined mental and physical
gen t_ghs=t_gph + t_gmh
bysort group:sum t_ghs

adjusted2 t_gph
adjusted2 t_gmh
adjusted2 t_ghs

****Clinical measures
/*Systolic BP (mmHg)
Diastolic BP (mmHg)
Heart rate
Total cholesterol (mg/dL)
HDL cholesterol (mg/dL)
Triglycerides (mg/dL)
LDLc (mg/dL)
Fasting blood glucose (mg/dl)
HbA1c (%)*/

gen waist=WAIST
replace waist=WAIST*2.52 if waist<50

adjusted2 CLINIC_VISITS  
adjusted2 BLOOD_PRESSURE_CHECKS 
adjusted2 CHOL_TESTING 
adjusted2 BG_TESTING 
adjusted2 ECG 
adjusted2 DIETICIAN_VISIT
adjusted2 SAT_CURRENT_TREATMENT
adjusted2 CONTINUE_TREATMENT
adjusted2 UNDERSTAND_HD
adjusted2 RECOMMEND_TREATMENT

**clinical measures
base HEART_RATE
base TOTAL_CHOLESTEROL
base HDLC
base TRIGLYCERIDES
base FBG
base SERUM_CREATININE
base SODIUM
base POTASSIUM
base ALT_SGPT
base AST_SGOT
base FER_VALUE
base EGRF
base HB_VALUE
base URIC_ACID
base WEIGHT
base BMI
base waist

adjusted2 SBP_AVG 
adjusted2 DBP_AVG 
adjusted2 HEART_RATE  
adjusted2 TOTAL_CHOLESTEROL  
adjusted2 HDLC  
adjusted2 TRIGLYCERIDES  
adjusted2 LDLC 

*Without HF patients
xtset pid1 visit
xtgee LDLC  ib2.TreatmentArm AGE i.SEX i.site i.visit if HF_all==0 [pweight=ipt_wt], family(gaussian) link(identity) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect

adjusted2 FBG  
adjusted2 HBA1C  
*With diabetes
bysort pid (visit):gen diab0=DIABETES[1] //create baseline variable 
edit pid visit DIABETES diab0
xtset pid1 visit
xtgee HBA1C  ib2.TreatmentArm AGE i.SEX i.site i.visit if diab0==1 [pweight=ipt_wt], family(gaussian) link(identity) eform robust
margins TreatmentArm
margins rb2.TreatmentArm
contrast TreatmentArm,effect


adjusted2 SERUM_CREATININE   
adjusted2 SODIUM SODIUM0 
adjusted2 POTASSIUM POTASSIUM0  
adjusted2 ALT_SGPT ALT_SGPT0 
adjusted2 AST_SGOT AST_SGOT0 
adjusted2 FER_VALUE FER_VALUE0  
adjusted2 EGRF EGRF0 
adjusted2 HB_VALUE HB_VALUE0   
adjusted2 URIC_ACID URIC_ACID0  
adjusted2 WEIGHT  
adjusted2 BMI  
adjusted2 waist waist0


**Interaction
xtset pid1 visit
xtgee antiplatelet_cat i.TreatmentArm##i.visit AGE i.SEX i.site ,family(poisson) link(log) eform robust
margins TreatmentArm#visit
marginsplot
margins TreatmentArm#visit, post
contrast r.TreatmentArm@visit, eform





