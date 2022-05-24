/*
MPH Final Project 
Dominic Shehtanian
*/


/*Data Readin and Setup*/
ods graphics on ;

libname mcbs 'C:\Users\dsheh\OneDrive - UCLA IT Services\SASProjects\MPH_Project\Data' ;
filename puf_fall 'C:\Users\dsheh\OneDrive - UCLA IT Services\SASProjects\MPH_Project\Data\puf2019_1_fall.xpt' ;


proc cimport infile=puf_fall library=mcbs ;

run ;

data work.puf_fall ;
	set mcbs.puf_fall ;
	where HLT_OCBETES = 1 and DEM_INCOME = 1 ;
	if ACC_MCQUALTY = 5 then ACC_MCQUALTY = . ;
	if ACC_MCCOSTS = 5 THEN ACC_MCCOSTS = . ;
	if ACC_MCSPECAR = 5 THEN ACC_MCSPECAR = . ;
	ALL_DUAL = 0 ;
		if ADM_OP_MDCD ge 2 then ALL_DUAL = 1 ;
	SATIS_QUAL = 0 ;
		if ACC_MCQUALTY = . then SATIS_QUAL = . ;
		if ACC_MCQUALTY le 2 then SATIS_QUAL = 1 ;
	SATIS_COST = 0 ;
		if ACC_MCCOSTS = . then SATIS_COST = . ;
		if ACC_MCCOSTS le 2 then SATIS_COST = 1 ;
	SATIS_SPEC = 0 ;
		if ACC_MCSPECAR = . then SATIS_SPEC = . ;
		if ACC_MCSPECAR le 2 then SATIS_SPEC = 1 ;
	*where ACC_MCQUALTY ne 5 and ACC_MCCOSTS ne 5 and ACC_MCSPECAR ne 5 ;
	HLT_DIFLIFT_BIN = 0 ;
	if HLT_DIFLIFT ge 2 then HLT_DIFLIFT_BIN = 1 ;
	PHYSVISIT = '0 visits' ;
		if ADM_H_PHYEVT eq 1 or ADM_H_PHYEVT eq 2 then PHYSVISIT = '1-10 visits' ;
		if ADM_H_PHYEVT ge 3 then PHYSVISIT = '11+ visits' ;
	INPATIENT_DAYS = '0 days' ;
		if ADM_H_ACTDAY ge 1 then INPATIENT_DAYS = '1-4 days' ;
		if ADM_H_ACTDAY eq 5 then INPATIENT_DAYS = '5+ days' ;
	CURRENT_SMOKER = . ;
		if RSK_SMKNOWAL = 1 then CURRENT_SMOKER = 1 ;
		if RSK_SMKNOWAL = 2 then CURRENT_SMOKER = 2 ;
		if RSK_EVRSMKAL eq 2 and RSK_SMKNOWAL eq . then CURRENT_SMOKER = 2 ;
run ;

data work.analysis ;
	/*data for export to R*/
	set work.puf_fall ;
	keep SATIS_QUAL SATIS_COST SATIS_SPEC ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCARTERY HLT_OCHBP HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCDEPRSS 
		HLT_OCPSYCHO HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER;
run ;

title1 'Exploratory Analysis' ;

proc freq data=work.puf_fall ;
	tables ALL_DUAL ;
run ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ALL_DUAL / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables SATIS_QUAL SATIS_COST SATIS_SPEC / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

proc surveyfreq data=mcbs.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables HLT_OCBETES / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

/*
proc freq data=work.puf_fall  ;
	tables ACC_MCQUALTY*ADM_OP_MDCD ACC_MCCOSTS*ADM_OP_MDCD ACC_MCSPECAR*ADM_OP_MDCD ;

run ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ACC_MCQUALTY*ADM_OP_MDCD ACC_MCCOSTS*ADM_OP_MDCD ACC_MCSPECAR*ADM_OP_MDCD / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

proc freq data=work.puf_fall ;
	tables ADM_H_PHYEVT ADM_H_ACTDAY PHYSVISIT  INPATIENT_DAYS;
run ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ACC_MCQUALTY*ALL_DUAL ACC_MCCOSTS*ALL_DUAL ACC_MCSPECAR*ALL_DUAL / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

proc freq data=work.puf_fall ;
	tables HLT_OCBETES DEM_INCOME ;
run ;

title2 'Demographics' ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ALL_DUAL*(DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA) / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

title2 'Utilization' ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ALL_DUAL*(PHYSVISIT INPATIENT_DAYS) / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

title2 'Comorbidities' ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ALL_DUAL*(HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH 
		HLT_ALZDEM HLT_OCDEPRSS HLT_OCPSYCHO HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER) / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

title2 'Diabetes Treatments' ;

proc surveyfreq data=work.puf_fall VARMETHOD=brr(fay=0.3) ;
	tables ALL_DUAL*(HLT_DIAINSUL HLT_DIAMEDS HLT_DIATEST HLT_DIATENYR HLT_DIACTRLP HLT_DIAHYPO HLT_DIAMNGE) / chisq ;
	weight PUFFWGT;
	repweight PUFF001 - PUFF100; 
run ;

title2 'Correlations' ;

ods graphics on ;
proc corr data=work.puf_fall spearman outs=work.spearcorr;
	var SATIS_QUAL SATIS_COST SATIS_SPEC ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCARTERY HLT_OCHBP HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCDEPRSS 
		HLT_OCPSYCHO HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER ;
run ;

*/

title1 'Simple Regression' ;
title2 'Logistic Regression' ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL SATIS_QUAL /desc ;
	model SATIS_QUAL = ALL_DUAL ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.puf_fall ;
	class ALL_DUAL SATIS_QUAL /desc ;
	model SATIS_QUAL = ALL_DUAL / RSQUARE lackfit outroc=myroc1a ctable ;
	weight PUFFWGT ;
run ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL SATIS_COST /desc ;
	model SATIS_COST = ALL_DUAL / RSQUARE ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100;
	output out=pred_logitcost p=phat ; 
run ;

proc logistic data=work.puf_fall ;
	class ALL_DUAL SATIS_COST /desc ;
	model SATIS_COST = ALL_DUAL / RSQUARE lackfit outroc=myroc2a ctable ;
	weight PUFFWGT ;
run ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL SATIS_SPEC /desc ;
	model SATIS_SPEC = ALL_DUAL ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.puf_fall ;
	class ALL_DUAL SATIS_SPEC /desc ;
	model SATIS_SPEC = ALL_DUAL / RSQUARE lackfit outroc=myroc3a ctable ;
	weight PUFFWGT ;
run ;
/*
title2 'Ordinal Logistic Regression' ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL  ;
	model ACC_MCQUALTY = ALL_DUAL ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL ;
	model ACC_MCCOSTS = ALL_DUAL ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL ;
	model ACC_MCSPECAR = ALL_DUAL ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;


title2 'Simple Linear Regression' ;

proc surveyreg data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL / ref=FIRST  ;
	model ACC_MCQUALTY = ALL_DUAL / solution ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc surveyreg data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL / ref=FIRST  ;
	model ACC_MCCOSTS = ALL_DUAL / solution ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc surveyreg data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL / ref=FIRST  ;
	model ACC_MCSPECAR = ALL_DUAL / solution ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;
*/
ods graphics  on;
title1 'Full Regression' ;
title2 'Logistic - SATIS_QUAL' ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL (REF = "0") DEM_AGE (REF = "3") DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_QUAL (REF = "0");
	model SATIS_QUAL = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.puf_fall ;
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_QUAL (REF = "0");
	model SATIS_QUAL = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM /lackfit outroc=myroc1b ctable ;
	weight PUFFWGT ; 
run ;

title2 'Logistic - SATIS_COST' ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_COST (REF = "0");
	model SATIS_COST = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.puf_fall ;
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_QUAL (REF = "0");
	model SATIS_COST = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM /lackfit outroc=myroc2b ctable ;
	weight PUFFWGT ; 
run ;

title2 'Logistic - SATIS_SPEC' ;

proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_SPEC (REF = "0");
	model SATIS_SPEC = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.puf_fall ;
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_QUAL (REF = "0");
	model SATIS_SPEC = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM /lackfit outroc=myroc3b ctable ;
	weight PUFFWGT ; 
run ;
/*
title2 'Linear - SATIS_QUAL' ;

proc surveyreg data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_QUAL;
	model SATIS_QUAL = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

title2 'Linear - SATIS_COST' ;

proc surveyreg data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_COST;
	model SATIS_COST = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;

title2 'Linear - SATIS_SPEC' ;

proc surveyreg data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL (REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM SATIS_SPEC;
	model SATIS_SPEC = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;
ods graphics off ;
*/

title1 'Propensity Score' ;
title2 'Satisfaction - Quality' ;
/*
proc surveylogistic data=work.puf_fall varmethod=brr (fay=0.30);
	class ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	model ALL_DUAL =  DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCDEPRSS HLT_ALZDEM ;
	weight PUFFWGT ;
	repweights PUFF001 - PUFF100; 
run ;
*/
ods graphics  on ;

proc PSMATCH data=work.puf_fall region=allobs ;
	class ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_COST SATIS_SPEC ;
	PSMODEL ALL_DUAL(TREATED = "1") = DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_COST SATIS_SPEC PUFFWGT ;
	ASSESS PS VAR = (DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_COST SATIS_SPEC PUFFWGT) /  
		VARINFO PLOTS = ALL ;
	PSWEIGHT WEIGHT = ATEWGT(STABILIZE=YES) NLARGESTWGT=10 ;
	OUTPUT OUT(OBS = ALL) = ATE_QUAL ATEWGT = _ATE_ ;
run ;

ods graphics off ;

data work.PS_QUAL ;
	set ATE_QUAL ;
	if _ATE_ gt 10 then _ATE_ = 10 ;
	PSWGT = _ATE_*PUFFWGT ;
run ;

ods graphics on ;


proc surveylogistic data=work.PS_QUAL varmethod=brr (fay=0.30);
	class ALL_DUAL(REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN 
		HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC(REF = "0") SATIS_COST(REF = "0") SATIS_QUAL(REF = "0");
	model SATIS_QUAL = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC SATIS_COST ;
	weight PSWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.PS_QUAL ;
	class ALL_DUAL(REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN 
		HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC(REF = "0") SATIS_COST(REF = "0") SATIS_QUAL(REF = "0");
	model SATIS_QUAL = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC SATIS_COST 
		/lackfit outroc=myroc1c ctable ;
	weight PSWGT ; 
run ;

title2 'Satisfaction with Costs' ;

ods graphics on ;

proc PSMATCH data=work.puf_fall region=allobs ;
	class ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC ;
	PSMODEL ALL_DUAL(TREATED = "1") = DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_QUAL SATIS_SPEC PUFFWGT ;
	ASSESS ALLCOV / 
		VARINFO PLOTS = ALL ;
	PSWEIGHT WEIGHT = ATEWGT(STABILIZE=YES) NLARGESTWGT=10 ;
	OUTPUT OUT(OBS = ALL) = ATE_COST ATEWGT = _ATE_ ;
run ;

ods graphics off ;

data work.PS_COST ;
	set ATE_COST ;
	PSWGT = _ATE_*PUFFWGT ;
run ;

ods graphics on ;


proc surveylogistic data=work.PS_COST varmethod=brr (fay=0.30);
	class ALL_DUAL(REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN 
		HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC(REF = "0") SATIS_COST(REF = "0") SATIS_QUAL(REF = "0");
	model SATIS_COST = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC SATIS_QUAL ;
	weight PSWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.PS_COST ;
	class ALL_DUAL(REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN 
		HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC(REF = "0") SATIS_COST(REF = "0") SATIS_QUAL(REF = "0");
	model SATIS_COST = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC SATIS_QUAL 
		/lackfit outroc=myroc2c ctable ;
	weight PSWGT ; 
run ;


title2 'Satisfaction with Access to Specialists' ;

ods graphics on ;

proc PSMATCH data=work.puf_fall region=allobs ;
	class ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_COST ;
	PSMODEL ALL_DUAL(TREATED = "1") = DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_QUAL SATIS_COST PUFFWGT ;
	ASSESS ALLCOV / 
		VARINFO PLOTS = ALL ;
	PSWEIGHT WEIGHT = ATEWGT(STABILIZE=YES) NLARGESTWGT=10 ;
	OUTPUT OUT(OBS = ALL) = ATE_SPEC ATEWGT = _ATE_ ;
run ;

ods graphics off ;

data work.PS_SPEC ;
	set ATE_SPEC ;
	if _ATE_ gt 10 then _ATE_ = 10 ;
	PSWGT = _ATE_*PUFFWGT ;
run ;

ods graphics on ;


proc surveylogistic data=work.PS_SPEC varmethod=brr (fay=0.30);
	class ALL_DUAL(REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN 
		HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC(REF = "0") SATIS_COST(REF = "0") SATIS_QUAL(REF = "0");
	model SATIS_SPEC = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_COST SATIS_QUAL ;
	weight PSWGT ;
	repweights PUFF001 - PUFF100; 
run ;

proc logistic data=work.PS_COST ;
	class ALL_DUAL(REF = "0") DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT (REF = "0 visits") INPATIENT_DAYS (REF = "0 days") HLT_DISWALK HLT_DIFLIFT_BIN 
		HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO 
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_SPEC(REF = "0") SATIS_COST(REF = "0") SATIS_QUAL(REF = "0");
	model SATIS_SPEC = ALL_DUAL DEM_AGE DEM_SEX DEM_RACE DEM_EDU DEM_MARSTA DEM_ARMED DEM_CBSA 
		PHYSVISIT INPATIENT_DAYS HLT_DISWALK HLT_DIFLIFT_BIN HLT_OCHBP HLT_OCARTERY HLT_OCMYOCAR HLT_OCCHD HLT_OCSTROKE HLT_OCARTHRH HLT_OCPSYCHO
		HLT_OCDEPRSS HLT_ALZDEM HLT_DISDECSN HLT_DISBATH HLT_EGLAUCOM HLT_ERETINOP CURRENT_SMOKER SATIS_COST SATIS_QUAL 
		/lackfit outroc=myroc3c ctable ;
	weight PSWGT ; 
run ;

ods graphics off ;

ods graphics on ;
title 'ROC Curves - Quality' ;
proc sql ;
	insert into work.myroc1a
		set _1MSPEC_ = 0,
			_SENSIT_ = 0
			;
quit ;

proc sort data=work.myroc1a ;
	by _SENSIT_ ;
run ;
	

data ROC1 ;
	set myroc1a(in=ra) myroc1b(in=rb) myroc1c(in=rc) ;
	if ra then Model=1 ;
	if rb then Model=2 ;
	if rc then Model=3 ;
run ;


proc sgplot data=ROC1 aspect=1 noautolegend;
   step x=_1MSPEC_ y=_SENSIT_ / group=Model;
   where Model ne 1 ;
   lineparm x=0 y=0 slope=1 / lineattrs=(color=gray);
   xaxis grid;   yaxis grid;
   label _1MSPEC_ ="1 - Specificity" _SENSIT_ ="Sensitivity";
      inset ("AUC - Propensity Score Weighting" = "0.76"
				"AUC - with Covariates" = "0.67") ;
run;

title 'ROC Curves - Cost' ;
proc sql ;
	insert into work.myroc2a
		set _1MSPEC_ = 0,
			_SENSIT_ = 0
			;
quit ;

proc sort data=work.myroc2a ;
	by _SENSIT_ ;
run ;
	

data ROC2 ;
	set myroc2a(in=ra) myroc2b(in=rb) myroc2c(in=rc) ;
	if ra then Model=1 ;
	if rb then Model=2 ;
	if rc then Model=3 ;
run ;


proc sgplot data=ROC2 aspect=1 noautolegend;
   step x=_1MSPEC_ y=_SENSIT_ / group=Model;
   where Model ne 1 ;
   lineparm x=0 y=0 slope=1 / lineattrs=(color=gray);
   xaxis grid;   yaxis grid;
   label _1MSPEC_ ="1 - Specificity" _SENSIT_ ="Sensitivity";
      inset ("AUC - Propensity Score Weighting" = "0.70"
				"AUC - with Covariates" = "0.66") ;
run;

title 'ROC Curves - Spec' ;
proc sql ;
	insert into work.myroc3a
		set _1MSPEC_ = 0,
			_SENSIT_ = 0
			;
quit ;

proc sort data=work.myroc3a ;
	by _SENSIT_ ;
run ;
	

data ROC3 ;
	set myroc3a(in=ra) myroc3b(in=rb) myroc3c(in=rc) ;
	if ra then Model=1 ;
	if rb then Model=2 ;
	if rc then Model=3 ;
run ;


proc sgplot data=ROC3 aspect=1 noautolegend;
   step x=_1MSPEC_ y=_SENSIT_ / group=Model;
   where Model ne 1 ;
   lineparm x=0 y=0 slope=1 / lineattrs=(color=gray);
   xaxis grid;   yaxis grid;
   label _1MSPEC_ ="1 - Specificity" _SENSIT_ ="Sensitivity";
   inset ("AUC - Propensity Score Weighting" = "0.79"
			"AUC - with Covariates" = "0.68") ;
run;
ods graphics off ;