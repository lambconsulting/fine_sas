options formdlim = '!' ls = 100 nocenter mprint mlogic;
/* Changing Path for new data location and mapping per Bryan Eason phone call 20200507 */
/*%let path =E:\Lamb_KL_PERS\Consulting\Fine\Fine_SAS_071313\Fine_SAS_Multi_071313;*/
%let path =U:\Consulting\KEL\Fine\Fine_SAS_071313\Fine_SAS_Multi_071313;
libname fine "&path.";



/*proc datasets lib = work kill nolist; quit;*/
* For SAS 9.3;
/*PROC IMPORT OUT= fine.beta */
/*            DATAFILE= "E:\Lamb_KL_PERS\Consulting\Fine\Fine_SAS_071313\F*/
/*ine_SAS_Multi_071313\SAS in SAS format 7-17-13.xlsx" */
/*            DBMS=EXCEL REPLACE;*/
/*     RANGE="Data$"; */
/*     GETNAMES=YES;*/
/*     MIXED=NO;*/
/*     SCANTEXT=YES;*/
/*     USEDATE=YES;*/
/*     SCANTIME=YES;*/
/*RUN;*/
* For SAS 9.2;
/*PROC IMPORT OUT= fine.BETA */
/*            DATAFILE= "E:\Lamb_KL_PERS\Consulting\Fine\Fine_SAS_071313\F*/
/*ine_SAS_Multi_071313\SAS in SAS format 8-1-13.xlsx" */
/*            DBMS=EXCELCS REPLACE;*/
/*     RANGE="Data$"; */
/*     SCANTEXT=YES;*/
/*     USEDATE=YES;*/
/*     SCANTIME=YES;*/
/*RUN;*/


/* Ad Hoc Gender Compare 20200514 */


data gender_chi_20200514; 
set fine.gender_chi_20200514; 
female = sex in ("F", "FS");
run;


PROC FORMAT;		
	VALUE sev_fmt
   		1 = 'Mild' 2 = 'Mod' 3 = 'Severe';
	VALUE sex_exp_fmt
   		1 = 'F' 2 = 'FS' 3 = 'M' 4 = 'MC';
		run;



data beta;
set fine.beta;

/*female = sex in ('F');*/
/* Corrected Gender assignment 20200514 */
female = sex in ('F', 'FS');
/* Sensitivity gender assignment for impact of exclusing FS as female and including as male 20200514 */
female_ns = sex in ('F');

if cond in ('Mild') then Sev = 1;
if cond in ('Moderate') then Sev = 2;
if cond in ('Severe') then Sev = 3;

if cond in ('Mild') then Sever = 1;
if cond in ('Moderate') then Sever = 2;
if cond in ('Severe') then Sever = 3;

if sex_1 in ('F') then sex_exp = 1;
if sex_1 in ('FS') then sex_exp = 2;
if sex_1 in ('M') then sex_exp = 3;
if sex_1 in ('MC') then sex_exp = 4;
if sex_1 in ('MN') then sex_exp = 4;


format sex_exp sex_exp_fmt.;
format sev sev_fmt.;
/*Cond*/ /*Sex_1*/ /*SEX*/ /*Age*/ /*PG*/
/*t*/ /*e*/ /*ce*/

/*if age_y = . then age_0c = 0;*/
/*else age_0c = age_y;*/
/*age_gt4 = (age_0c > 4)*(age_0c - 4);*/
/*age_gt3 = (age_0c > 4)*(age_0c - 3);*/
/**/
/*/*PG_Dx	PG_1Yr	PG_Last*/
/**/
/*if pg = . then pg_0c = 0;*/
/*else pg_0c = pg;*/
/*pg_gt180 = (pg_0c > 180)*(pg_0c - 180);*/
/*pg_gt160 = (pg_0c > 160)*(pg_0c - 160);*/

/*age = age_y;*/
/**/
/**/
/*age_gt_m = (age > 1.7189041);*/
/*pg_gt_m = (pg_0c > 133.64949);*/

;
run;


/* Adding ChiSq 20200514 for follow-up */

/* DMF Eason Full Population */
proc freq data=gender_chi_20200514;
tables female / chisq;
run;

/* Survival Limited Population */
/*proc freq data = fine.beta;*/
proc freq data = beta;
tables sex sex_1 female female_ns / chisq;
run;



proc freq data = fine.beta;
tables (sex sex_1)*(e ce);
run;



%macro uni (var1);
proc phreg data = beta;
model t*e(0) = &var1. / rl;
ods output fitstatistics = aicc&var1.;
run;
data aicc&var1.;
set aicc&var1. ;
aicc = "aicc&var1.";
run;
data uni;
length aicc $20;
set uni aicc&var1.;
run;
%mend;
data uni; set _null_; run;
%uni(sev);
%uni(age);
%uni(pg);
%uni(female);
%uni(female_ns);
/*%uni(sex);*/
/*%uni(sex_1);*/
/*%uni(sex_exp);*/



%macro multi (var2,var3);
proc phreg data = beta;
class sev;
model t*e(0) = sev &var2. &var3.  / rl;
ods output fitstatistics = aiccsev&var2.&var3;
run;
data aiccsev&var2.&var3.;
set aiccsev&var2.&var3.;
aicc = "aiccsev&var2.&var3.";
run;
data multi;
length aicc $30;
set multi aiccsev&var2.&var3.;
run;
%mend;
data multi; set _null_; run;
%multi(sex_exp,);
%multi(age,);
%multi(female,);
%multi(female_ns,);
%multi(pg,);
%multi(pg,female);
%multi(age,female);
%multi(pg,female_ns);
%multi(age,female_ns);
%multi(age,sex_exp);

data fitstat; run;
data fitstat;
set fitstat uni multi;
run;

proc print data = uni; run;
proc print data = multi; run;
proc print data = fitstat; run;
filename excel dde "Excel|[Fine_SevClass_080613.xlsx]AICC!R2C1:R50C3" ;
data _null_ ;
  set fitstat ;
  where aicc not in (' ');
  file excel notab;
  put aicc '09'x criterion '09'x withcovariates '09'x ;
run ;

filename excel dde "Excel|[Fine_SevClass_080613.xlsx]AICC!R2C5:R50C7" ;
data _null_ ;
  set fitstat ;
  where aicc not in (' ') and criterion in ('AIC');
  file excel notab;
  put aicc '09'x criterion '09'x withcovariates '09'x;
run ;

ods trace on;
proc corr data = beta spearman;
var age pg;
ods output SpearmanCorr=corr;
run;
ods trace off;
proc print; run;
filename excel dde "Excel|[Fine_SevClass_080613.xlsx]AICC!R40C1:R45C11" ;
data _null_ ;
  set corr ;
   file excel notab;
  put variable '09'x pg '09'x age '09'x ;
run ;



proc reg data = beta;
model vit_d = alb lactate / vif; run;
/** AdjR2: 0.1977*/
* VIF: 10.2554;


%macro noclassphreg (event,var1);
proc phreg data = beta;
model t*e(0) = &var1. / rl;
ods output parameterestimates=est_&var1._&event.;
run;
data est_&var1._&event.;
length parameter $50.;
set est_&var1._&event.;
length model event $20.;
model = "est_&var1._&event.";
event = "&event.";
run;
data all_uni;
set all_uni est_&var1._&event.;
run;
%mend;
data all_uni; set _null_; run;
%noclassphreg(e,age);
%noclassphreg(e,pg);
%noclassphreg(e,female);
%noclassphreg(e,female_ns);
%noclassphreg(ce,age);
%noclassphreg(ce,pg);
%noclassphreg(ce,female);
%noclassphreg(ce,female_ns);

/* Adding var4 to allow for severity level (sev), age, pg, and female in the same model 20200515 */
%macro classphreg (event,var2,var3,var4);
proc phreg data = beta;
class sev;
model t*&event.(0) = sev &var2. &var3. &var4. / rl;
ods output parameterestimates=est_sev&var2.&var3.&var4._&event.;
run;
data est_sev&var2.&var3.&var4._&event.;
length parameter label $50.;
set est_sev&var2.&var3.&var4._&event.;
length model event $20.;
model = "est_sev&var2.&var3.&var4._&event.";
event = "&event.";
run;
data all_multi;
set all_multi est_sev&var2.&var3.&var4._&event.;
run;
%mend;
data all_multi; set _null_; run;
/* Added female_ns to compare to corrected female category 20200515 */
%classphreg(e,,,);
%classphreg(e,sex_exp,);
%classphreg(e,age,);
%classphreg(e,pg,);
%classphreg(e,female,);
%classphreg(e,female_ns,);
%classphreg(e,age,female);
%classphreg(e,age,female_ns);
%classphreg(e,pg,female);
%classphreg(e,pg,female_ns);
%classphreg(e,age,sex_exp);
%classphreg(e,age,pg);
%classphreg(e,age,pg,female);

%classphreg(ce,,);
%classphreg(ce,sex_exp,);
%classphreg(ce,age,);
%classphreg(ce,pg,);
%classphreg(ce,female,);
%classphreg(ce,female_ns,);
%classphreg(ce,age,female);
%classphreg(ce,age,female_ns);
%classphreg(ce,pg,female);
%classphreg(ce,pg,female_ns);
%classphreg(ce,age,sex_exp);
%classphreg(ce,age,pg);
%classphreg(ce,age,pg,female);

data allphreg; set _null_; run;
data allphreg;
set allphreg all_uni all_multi;
run;

proc print data = all_uni; run;
proc print data = all_multi; run;
proc print data = allphreg; run;


filename excel dde "Excel|[Fine_SevClass_080613.xlsx]MainEffects!R2C1:R40C11" ;
data _null_ ;
  set allphreg ;
  where event in ('e');
  file excel notab;
 put classval0 '09'x event '09'x model '09'x parameter '09'x Estimate '09'x StdErr '09'x ChiSq '09'x ProbChiSq '09'x HazardRatio '09'x HRLowerCL '09'x HRUpperCL '09'x ;
   *put event '09'x model '09'x variable '09'x Estimate '09'x StdErr '09'x ChiSq '09'x ProbChiSq '09'x HazardRatio '09'x HRLowerCL '09'x HRUpperCL '09'x ;
run ;

filename excel dde "Excel|[Fine_SevClass_080613.xlsx]MainEffects!R400C1:R800C11" ;
data _null_ ;
  set allphreg ;
  where event in ('ce');
  file excel notab;
  put classval0 '09'x event '09'x model '09'x parameter '09'x Estimate '09'x StdErr '09'x ChiSq '09'x ProbChiSq '09'x HazardRatio '09'x HRLowerCL '09'x HRUpperCL '09'x ;
  *put event '09'x model '09'x variable '09'x Estimate '09'x StdErr '09'x ChiSq '09'x ProbChiSq '09'x HazardRatio '09'x HRLowerCL '09'x HRUpperCL '09'x ;
run ;

* NS Interactions;
ods trace on;
proc phreg data = beta;
class sev;
model t*e(0) = sev age sev*age / rl;
title 'Cond by Age Interaction';
ods output type3 = t3
run;
proc print data = t3; run;
ods trace off;

proc phreg data = beta;
class sev;
model t*e(0) = pg age pg*age / rl;
title 'PG by Age Interaction';
run;


data all; run;
%macro interevent (event,cont,cont1);
proc phreg data = beta;
class sev;
model t*&event.(0) = sev &cont. &cont1. sev*&cont.  / rl;
ods output parameterestimates=est_sevinter_&event._&cont._&cont1.
type3 = t3_sevinter_&event._&cont._&cont1.;
run;
data est_sevinter_&event._&cont._&cont1.;
set est_sevinter_&event._&cont._&cont1.;
length model $30. event $5.;
model = "est_sevinter_&event._&cont._&cont1.";
event = "&event.";
run;
data t3_sevinter_&event._&cont._&cont1.;
set t3_sevinter_&event._&cont._&cont1. (keep = effect probchisq);
length effect $15.;
model = "t3_sevinter_&event._&cont._&cont1.";
event = "&event.";
run;
data all;
set all est_sevinter_&event._&cont._&cont1. t3_sevinter_&event._&cont._&cont1.;
run;
%mend;
%interevent(e,age,);
%interevent(ce,age,);


proc print data = all; run;


filename excel dde "Excel|[Fine_SevClass_080613.xlsx]Interaction!R2C1:R10C10" ;
data _null_ ;
  set all ;
 where event = "e";
 file excel notab;
  put effect '09'x label '09'x classval0 '09'x event '09'x model '09'x parameter '09'x Estimate '09'x StdErr '09'x ChiSq '09'x ProbChiSq '09'x HazardRatio '09'x HRLowerCL '09'x HRUpperCL '09'x ;
run ;


filename excel dde "Excel|[Fine_SevClass_080613.xlsx]Interaction!R13C1:R75C10" ;
data _null_ ;
  set all ;
  where event = "ce";
  file excel notab;
  put effect '09'x label '09'x classval0 '09'x event '09'x model '09'x parameter '09'x Estimate '09'x StdErr '09'x ChiSq '09'x ProbChiSq '09'x HazardRatio '09'x HRLowerCL '09'x HRUpperCL '09'x ;
run ;

/**/
/** Unadjusted Survival;*/
/**/
/** bb, age_gt_m, pg_gt_m, female;*/
/*proc lifetest data = beta plots=(s) method = km censoredsymbol = none noprint;*/
/*strata female ;*/
/*time t*e(0);*/
/*run;*/
/**/
/*%macro kmbb(iv,e);*/
/*proc lifetest data = beta method = km outsurv=s_&iv._&e. noprint;*/
/*strata &iv. ;*/
/*time t*&e.(0);*/
/*run;*/
/*proc export data=s_&iv._&e.*/
/*   outfile="&path.\survs\s_&iv._&e..csv"*/
/*   dbms=csv replace;*/
/*     run;*/
/*%mend;*/
/**/
/*%kmbb(bb,e);*/
/*%kmbb(bb,ce);*/
/*%kmbb(age_gt_m,e);*/
/*%kmbb(age_gt_m,ce);*/
/*%kmbb(pg_gt_m,e);*/
/*%kmbb(pg_gt_m,ce);*/
/*%kmbb(female,e);*/
/*%kmbb(female,ce);*/


* Adjusted (Cox) Survival;
data beta_cov;
set beta;

sev_mild = (sever = 1);
sev_mod = (sever = 2);
sev_sev = (sever = 3);
run;


proc summary data=beta_cov;
 var  sev_mild sev_mod sev_sev age female pg;
  output out=cov_means mean=;
run;
proc print data = cov_means; run;

%macro adjsev(e);
proc phreg data=beta_cov;
strata sev / missing;
  model t*&e.(0) = sev_mild sev_mod sev_sev age  female pg/ risklimits; 
  baseline out=ADJ_Baseline_&e._20200514
           survival=adj_surv
           stderr=adj_surv_se
           covariates=cov_means / nomean;
		run;
/* Referenced following export for location of adjusted survival curves per Bryan Eason phone call 20200507 */

/* 
Remarking out for now to get output for SGPLOT publication images
based upon U:\Consulting\KEL\Fine\Fine_SAS_071313\Fine_SAS_Multi_071313
Fine_SevClass_wPG_080613 tabs Adj_E and Adj_CE

proc export data=ADJ_Baseline_&e.
   outfile="&path.\survs\Adj_BB_Surv_&e..csv"
   dbms=csv replace;
     run;
*/

/* Placing back in to account for corrected female gender assignment 20200514 */
/*		proc export data=ADJ_Baseline_&e._20200514*/
/*   outfile="&path.\survs\Adj_BB_Surv_&e._20200514.csv"*/
/*   dbms=csv replace;*/
/*     run;*/

%mend;
%adjsev(e);
%adjsev(ce);

/* Get into SGPLOT format for step function */

/* Get E Prepped Plotting Data */
data e_mi (rename=(t=t_mi adj_surv=s_mi)) e_mo (rename=(t=t_mo adj_surv=s_mo)) e_se (rename=(t=t_se adj_surv=s_se));
set adj_baseline_e (keep=sev t adj_surv); 
/*if sev = "Mild" then output e_mi;*/
/*if sev = "Mod" then output e_mo;*/
/*if sev = "Severe" then output e_se;*/
/* Remove format for data processing */
if sev = 1 then output e_mi;
if sev = 2 then output e_mo;
if sev = 3 then output e_se;

run;

/* Get CE Prepped Plotting Data */
data e_mi_mo_se; merge e_mi e_mo e_se; run;
data ce_mi (rename=(t=t_mi adj_surv=s_mi)) ce_mo (rename=(t=t_mo adj_surv=s_mo)) ce_se (rename=(t=t_se adj_surv=s_se));
set adj_baseline_ce (keep=sev t adj_surv); 
if sev = 1 then output ce_mi; if sev = 2 then output ce_mo; if sev = 3 then output ce_se; run;
data ce_mi_mo_se; merge ce_mi ce_mo ce_se; run;


%macro lastme(ds1,ds);
data &ds1._&ds._l; set &ds1._&ds.; where s_&ds. ne .; by sev; if last.sev; call symputx("s_last",s_&ds.); run;
/*data &ds1._&ds._r; set &ds1._&ds.; if s_&ds. = . then s_&ds. = &s_last.; run;*/
/*data &ds1._&ds.; set &ds1._&ds. e_&ds._r; run;*/
%put &s_last;
/*%symdel s_last;*/
%mend;
%lastme(e,mi);
%lastme(e,mo);
%lastme(e,se);
%lastme(ce,mi);
%lastme(ce,mo);
%lastme(ce,se);

data e_mi_mo_se_l; merge e_mi_l e_mo_l e_se_l; lastone = 1; run;
data ce_mi_mo_se_l; merge ce_mi_l ce_mo_l ce_se_l; lastone = 1; run;

data e_mi_mo_se; set e_mi_mo_se e_mi_mo_se_l; 
if lastone = 1 then do;
t_mi = max(t_mi, t_mo, t_se);
t_mo = max(t_mi, t_mo, t_se);
t_se = max(t_mi, t_mo, t_se);
end;
run;

data ce_mi_mo_se; set ce_mi_mo_se ce_mi_mo_se_l; 
if lastone = 1 then do;
t_mi = max(t_mi, t_mo, t_se);
t_mo = max(t_mi, t_mo, t_se);
t_se = max(t_mi, t_mo, t_se);
end;
run;




proc sgplot data= e_mi_mo_se;
ODS GRAPHICS / RESET IMAGENAME = "e_mi_mo_se_20200514" IMAGEFMT =PNG ;
ODS LISTING IMAGE_DPI= 300 GPATH = 'C:\Junk' ; 
step x= t_mi y=s_mi / LEGENDLABEL = "Mild" LINEATTRS = (color = red THICKNESS = 2 pattern = solid);
/*step x= &comorb2._&cohort2._t y=&comorb2._&cohort2._s / LEGENDLABEL = "&comorb2. &cohort2." LINEATTRS = (color = red THICKNESS = 2 pattern = dash);*/
step x= t_mo y=s_mo / LEGENDLABEL = "Moderate" LINEATTRS = (color = blue THICKNESS = 2 pattern = solid);
/*step x= &comorb4._&cohort4._t y=&comorb4._&cohort4._s / LEGENDLABEL = "&comorb4. &cohort4." LINEATTRS = (color = blue THICKNESS = 2 pattern = dash);*/
step x= t_se y=s_se / LEGENDLABEL = "Severe" LINEATTRS = (color = green THICKNESS = 2 pattern = solid);
/*step x= &comorb6._&cohort6._t y=&comorb6._&cohort6._s / LEGENDLABEL = "&comorb6. &cohort6." LINEATTRS = (color = green THICKNESS = 2 pattern = dash);*/
/*step x= &comorb7._&cohort7._t y=&comorb7._&cohort7._s / LEGENDLABEL = "&comorb7. &cohort7." LINEATTRS = (color = black THICKNESS = 2 pattern = dash);*/
/*XAXIS Label= "Years";*/
/* YAXIS LABEL = 'Percent Survival' GRID ; */
run;


proc sgplot data= ce_mi_mo_se;
ODS GRAPHICS / RESET IMAGENAME = "ce_mi_mo_se_20200514" IMAGEFMT =PNG ;
ODS LISTING IMAGE_DPI= 300 GPATH = 'C:\Junk' ; 
step x= t_mi y=s_mi / LEGENDLABEL = "Mild" LINEATTRS = (color = red THICKNESS = 2 pattern = solid);
/*step x= &comorb2._&cohort2._t y=&comorb2._&cohort2._s / LEGENDLABEL = "&comorb2. &cohort2." LINEATTRS = (color = red THICKNESS = 2 pattern = dash);*/
step x= t_mo y=s_mo / LEGENDLABEL = "Moderate" LINEATTRS = (color = blue THICKNESS = 2 pattern = solid);
/*step x= &comorb4._&cohort4._t y=&comorb4._&cohort4._s / LEGENDLABEL = "&comorb4. &cohort4." LINEATTRS = (color = blue THICKNESS = 2 pattern = dash);*/
step x= t_se y=s_se / LEGENDLABEL = "Severe" LINEATTRS = (color = green THICKNESS = 2 pattern = solid);
/*step x= &comorb6._&cohort6._t y=&comorb6._&cohort6._s / LEGENDLABEL = "&comorb6. &cohort6." LINEATTRS = (color = green THICKNESS = 2 pattern = dash);*/
/*step x= &comorb7._&cohort7._t y=&comorb7._&cohort7._s / LEGENDLABEL = "&comorb7. &cohort7." LINEATTRS = (color = black THICKNESS = 2 pattern = dash);*/
/*XAXIS Label= "Years";*/
/* YAXIS LABEL = 'Percent Survival' GRID ; */
run;



/*
Adding Lifetests for pairwise comparisons per Bryan Eason phone call 20200507
Not perfect since actual analyses were based off of adjusted survival curves
with strata statement carried out seven years ago, but curves are very similar so will use unadjusted 
survival pairwise comparisons.
*/
%macro unadjsev(e,s1,s2);
ods trace off;
proc lifetest data = beta_cov plots=(s) method = km censoredsymbol = none ;
where sev in (&s1. &s2.);
strata sev ;
time t*&e.(0);
ods output HomTests=h;
run;
data h (keep=grps e probchisq); set h; where test = "Log-Rank"; 
grps = put(&s1.,6.)||" " ||put(&s2.,6.);

e = "&e.";
run;
data log_all; set log_all h; run;
%mend;
data log_all; set _null_; run;
%unadjsev(e,1,2);
%unadjsev(e,1,3);
%unadjsev(e,2,3);
%unadjsev(ce,1,2);
%unadjsev(ce,1,3);
%unadjsev(ce,2,3);

/* Adding back unadjusted KM curves for tabs KM_E and KM_CE 20200515 */
proc lifetest data = beta plots=(s) method = km censoredsymbol = none ;
strata sev ;
time t*e(0);
run;

proc lifetest data = beta plots=(s) method = km censoredsymbol = none ;
strata sev ;
time t*ce(0);
run;

/**/
/*ProbChiSq	grps	e*/
/*0.0007	     1      2	e*/
/*0.0000	     1      3	e*/
/*0.0000	     2      3	e*/
/*0.0002	     1      2	c*/
/*0.0000	     1      3	c*/
/*0.0000	     2      3	c*/


*** Test for proportionality;
data allprop; run;
%macro prop (e);
proc phreg data = beta;
class sev;
model t*&e.(0) = age sev female aget sevt femalet pg/ rl;
  aget = age*log(t);
  sevt = sev*log(t);
  femalet = female*log(t);
  proportionality_test: test aget, sevt, femalet;
  ods output TestStmts=prop_&e.;
data prop_&e.;
set prop_&e.;
length event $5.;
event = "&e.";
run;
data allprop;
set allprop prop_&e.;
run; 
%mend;
%prop(e);
%prop(ce);

proc print data = allprop; run;
filename excel dde "Excel|[Fine_SevClass_080613.xlsx]Proportionality!R2C1:R5C5" ;
data _null_ ;
  set allprop ;
  where event not in (" ");
  file excel notab;
  put label '09'x waldchisq '09'x df '09'x probchisq '09'x event '09'x ;
run ;



* Graphically;

%include "E:\Lamb_KL_PERS\Consulting\Fine\Fine_SAS_071313\Fine_SAS_Multi_071313\Schoen_Macro_Proportionality_Mayo_071311.sas";


%schoen  (time= t,
  event=e,
  xvars=  age sev_mild sev_mod sev_sev female pg,
   data=beta_cov,
   cen_vl=0,
  strata= ,
  outsch=schr,
  outbt=schbt,
  plot=t,   
  vref=yes,
  points=yes,
  pvars= ,
   method=spline,
  df=4,
  smooth= ,
  alpha=.05,
  rug=no,
  ties=efron);

filename grafout "&path.\survs";  
   goptions reset=all device=PNG gsfname=grafout gsfmode=replace; 
   proc greplay igout=work.gschbt nofs;  *Not GSEG library;
        replay _all_;  
   run;
   quit; 








* ADR Panhlmacro;

	proc delete data=covmean_sql; run;
	proc sql noprint;
	select count(*) into :nobs
		from beta;

	create table covmean_sql as
			select age, pg,
			count(*)/&nobs. as percent
		from beta
			group by
				age, pg;
				quit;
	data covmean_sql;
		set covmean_sql;
		group=_n_;
	run;

	proc print data = covmean_sql; run;
	proc sort data=beta; by age pg; run;
	proc sort data=covmean_sql; by age pg; run;

	proc delete data=beta2; run;
	data beta2;
		merge beta covmean_sql;
		by age pg;
	run;
	proc print data = beta2; run;
	proc phreg data = beta2 noprint ;
	    class female pg ;
	    model t*e(0) = age pg;
		strata bb;
	    baseline out=e_adjust (keep = group bb t s) covariates=covmean_sql survival=s /nomean;
		id group;
run;

		proc print data = e_adjust; run;
