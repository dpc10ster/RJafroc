/*-----------------------------------------------------------------------------
  SAS Version:           9.4
  Operating System:      Windows
-------------------------------------------------------------------------------

  Author:                Jason Cai
  Creation Date:         21Aug2019

  Program Name:          StSignificanceTesting.sas

  Program Purpose:       Implement Obuchowski-Rockette model for the MRMC study design, including both fully crossed and split-plot designs (cases nested within reader)

  Macro Parameters       dataset: the dataset to be analyzed, assign file path and name.
                         FOM: the figure of merit, can be AFROC, AFROC1, wAFROC (the default), wAFROC1, HrAuc, MaxLLF, MaxNLF, MaxNLFAllCases, HrSe, HrSp, ROC,
                              LLF@#, NLF@#, NLFAllCases@#, Se@#, Sp@#. # is the cutpoint. Ratings >= # are considered as abnormal in the calculation of LLF, NLF,
                              NLFAllCases, Se and Sp.
                         nboots: the number of bootstraps (default is 200).
                         seed: seed number used in resampling (bootstrapping). The default uses the time of day from the computer's clock as the initial seed.
                         alpha: the significance level of the test of the null hypothesis that all treatment effects are zero. the default alpha is 0.05.
                         option: Determines which factors are regarded as random vs. fixed: "RRRC" = random-reader random case, "FRRC" = fixed-reader random case, 
                                 "RRFC" = random-reader fixed case, "ALL" outputs the results of "RRRC", "FRRC" and "RRFC" analyses

  Notes	                 High ratings indicate high confidence in abnormality 				

-------------------------------------------------------------------------------
MODIFICATION HISTORY:

Version/   | Programmer    |
Date       | Name          | Description of change
-------------------------------------------------------------------------------
-----------------------------------------------------------------------------*/

%macro StSignificanceTesting(dataset=, FOM=wAFROC, nboots=200, seed=0, alpha=0.05, option=All);
ods graphics off;
ods listing close;
ods html close;

/*Datasets in the current work lib*/
%let workdata=;
data _null_;
	set sashelp.vmember end=_end;
	where upcase(libname)="WORK" and upcase(memtype)="DATA";
	length workdata $32767.;
	retain workdata;
	workdata=catx(" ", workdata, memname);
	if _end then call symput("workdata", strip(workdata));
run;

/*Import data*/
%macro import(data);
proc import out= &data 
            datafile= "&dataset" 
            DBMS=EXCEL replace;
     range="&data$"; 
     getnames=YES;
     mixed=YES;
     scantext=YES;
     usedate=YES;
     scantime=YES;
run;
%mend import;

%import(TP)
%import(FP)
%import(Truth)

/*Check variables*/
%let dataerr=N; %let reader_truth=N; %let test_truth=N;
data _data_vars;
	set sashelp.vcolumn;
	length newname $100.;
	where upcase(libname)="WORK" and upcase(memtype)="DATA" and upcase(memname) in ("TP" "FP" "TRUTH");
	by memname;
	if first.memname then count_var=0;
	if upcase(memname)="FP" then do;
	   if lowcase(name) in ("readerid" "modalityid" "caseid" "fp_rating" "nl_rating") then count_var+1;
	   if last.memname and count_var<4 then call symput('dataerr', 'Y');
	end;

	if upcase(memname)="TP" then do;
	   if lowcase(name) in ("readerid" "modalityid" "caseid" "lesionid" "tp_rating" "ll_rating") then count_var+1;
	   if last.memname and count_var<5 then call symput('dataerr', 'Y');
	end;

	if upcase(memname)="TRUTH" then do;
	   if lowcase(name) in ("caseid" "lesionid" "weight") then count_var+1;
	   if last.memname and count_var<3 then call symput('dataerr', 'Y');
	   if lowcase(name)="readerid" then call symput('reader_truth', "Y");
	   if lowcase(name)="modalityid" then call symput('test_truth', "Y");
	end;

	if lowcase(name)="nl_rating" then newname="FP_Rating";
	else if lowcase(name)="ll_rating" then newname="TP_Rating";
	else newname=name;
run;

/*if the data structure does not fit for the analysis, the program will be ended*/
%if &dataerr=Y %then %do;
proc datasets lib=work memtype=data nolist;
	delete TP FP Truth _data_vars;
run; quit;

%window info color=white #5 @28 'Variable names were incorrect. Please check. Press ENTER to continue.' attr=highlight;
%display info;

%goto exit;
%end;

/*Renaming variables*/
data _null_;
	set _data_vars;
	by memname;
	if first.memname then call execute("Data "||strip(memname)||"; set "||strip(memname)||";");
	call execute("rename "||strip(name)||"="||strip(newname)||";");
	if last.memname then call execute("if missing(CaseID) then delete; run;");
run;

/*Data processing for the Truth data*/
%let design=999;
%if &reader_truth=N and &test_truth=N %then %let design=1;
%else %if &reader_truth=Y and &test_truth=N %then %do;
	proc sql;
		create table _reader_truth1 as select distinct ReaderID, count(distinct CaseID) as sum1 from Truth group by ReaderID;
		create table _reader_truth2 as select count(distinct CaseID) as sum2 from Truth;
		create table _reader_truth3 as select * from _reader_truth1, _reader_truth2 order by ReaderID;
	quit;

	proc transpose data=_reader_truth3 out=_reader_truth4(drop=_name_) prefix=rdr;
		by sum2;
		id ReaderID;
		var sum1;
	run;

	data _null_;
		set _reader_truth4;
		if min(of rdr:)=max(of rdr:) and sum2=sum(of rdr:) then call symput('design', '2');
	run;
%end;
%else %do;

%window info color=white #5 @28 'Not supported. Press ENTER to continue.' attr=highlight;
%display info;

%goto exit;

%end;

%if &design ne 1 and &design ne 2 %then %do;

%window info color=white #5 @28 'Not supported. Press ENTER to continue.' attr=highlight;
%display info;

%goto exit;

%end;

/*the weights*/
data Truth;
	set Truth;
	if missing(LesionID) then LesionID=0;
run;

proc sql;
	create table _Truth_w1 as select distinct CaseID, 1/count(distinct LesionID) as Weight2 from Truth group by CaseID order by CaseID;
	create table _Truth_w2 as select *, max(Weight) as Max_Weight from Truth group by CaseID order by CaseID, LesionID;
quit;

data _null_;
	set _Truth_w2;
	if LesionID ne 0 and Weight=0 and Max_Weight ne 0 then put "War" "ning: Please check weight variable in Truth tab. " CaseID=;
run;

data Truth;
	merge _Truth_w2 _Truth_w1;
	by CaseID;
	if LesionID ne 0 and Max_Weight=0 then Weight=Weight2;
	drop Max_Weight Weight2;
run;

/*merge to find the number of readers and test modalities*/
data _TPFP;
	set TP FP;
run;

proc sql;
	/*Readers*/
	create table _Reader as select distinct ReaderID from _TPFP;
	/*Tests*/
	create table _Test as select distinct ModalityID from _TPFP;
	/*Cases*/
	create table _Case as select distinct CaseID, %if &design=2 %then ReaderID,; case when max(Weight)>0 then 1 else 0 end as LesionYN from Truth group by CaseID order by CaseID;
quit;

proc sql noprint;
	/*Number of cases*/
	select count(distinct CaseID) into :ncase from Truth;
	/*Number of tests*/
	select count(distinct ModalityID) into :ntest from _TPFP;
	/*Number of readers*/
	select count(distinct ReaderID) into :nreader from _TPFP;
	/*Number of normal cases*/
	select count(distinct CaseID) into :nnorm from _case where LesionYN=0;
	/*Number of lesions*/
	select count(LesionID) into :nlesion from Truth where Weight>0;
	/*Number of abnormal cases*/
	select count(distinct CaseID) into :nabnorm from _case where LesionYN=1;
	/*Smallest rating*/
	select min(min(TP_Rating), min(FP_Rating)) into :smallest_r from _TPFP;
quit;

%put test #=%sysfunc(strip(&ntest)) reader #=%sysfunc(strip(&nreader)) case #=%sysfunc(strip(&ncase))
	 normal case #=%sysfunc(strip(&nnorm)) abnormal case #=%sysfunc(strip(&nabnorm)) Lesion #=%sysfunc(strip(&nlesion)) Smallest rating=%sysfunc(strip(&smallest_r));

/*Number of cases/normal cases/abnormal cases by reader*/
%if &design=1 %then %do;
proc sql;
	create table _Case_by_Reader_ as select distinct
		   count(distinct CaseID) as N_AllCase, count(case when LesionID ne 0 then cats(CaseID) else '' end) as N_Lesion,
		   count(distinct case when LesionID=0 then cats(CaseID) else '' end) as N_Norm, 
		   count(distinct case when LesionID ne 0 then cats(CaseID) else '' end) as N_Abnorm
		   from Truth;
	create table _Case_by_Reader as select distinct * from _Reader, _Case_by_Reader_ order by ReaderID;
quit;	
%end;

%if &design=2 %then %do;
proc sql;
	create table _Case_by_Reader as select distinct
		   ReaderID, count(distinct CaseID) as N_AllCase, count(case when LesionID ne 0 then cats(CaseID) else '' end) as N_Lesion,
		   count(distinct case when LesionID=0 then cats(CaseID) else '' end) as N_Norm, 
		   count(distinct case when LesionID ne 0 then cats(CaseID) else '' end) as N_Abnorm
		   from Truth group by ReaderID order by ReaderID;
quit;	
%end;

/*Dummy data - reader by modality*/
%macro merged(_data, _val);
proc sql;
	create table _rm as select * from _reader, _test order by ReaderID, ModalityID;
quit;

data &_data;
	merge _rm &_data;
	by ReaderID ModalityID;
	if missing(FOM) then FOM=&_val;
run;
%mend merged;

%if %upcase(&FOM)=MAXNLFALLCASES %then %do;
/*MaxNLFAllCases*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(CaseID)/N_AllCase as FOM 
		   from FP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %length(&FOM)>12 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 12)=NLFALLCASES@ %then %do;
%let cutpoint=%substr(%upcase(&FOM), 13);
/*NLFAllCases by Cut Point*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(case when FP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_AllCase as FOM 
		   from FP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %upcase(&FOM)=MAXNLF %then %do;
/*MaxNLF*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(CaseID)/N_Norm as FOM from FP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where CaseID in (select CaseID from Truth where Weight=0) group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %length(&FOM)>4 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 4)=NLF@ %then %do;
%let cutpoint=%substr(%upcase(&FOM), 5);
/*NLF by Cut Point*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(case when FP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_Norm as FOM 
		   from FP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID where CaseID in (select CaseID from Truth where Weight=0) group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %upcase(&FOM)=MAXLLF %then %do;
/*MaxLLF*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(CaseID)/N_Lesion as FOM from TP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %length(&FOM)>4 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 4)=LLF@ %then %do;
%let cutpoint=%substr(%upcase(&FOM), 5);
/*LLF by Cut Point*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(case when TP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_Lesion as FOM 
		   from TP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %upcase(&FOM)=HRSE %then %do;
/*HrSe*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(distinct CaseID)/N_Abnorm as FOM from _TPFP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID
		   where CaseID in (select CaseID from Truth where Weight>0) group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %length(&FOM)>3 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 3)=SE@ %then %do;
%let cutpoint=%substr(%upcase(&FOM), 4);
/*SE by Cut Point*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, count(distinct case when TP_Rating>=&cutpoint or FP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_Abnorm as FOM 
		   from _TPFP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID where CaseID in (select CaseID from Truth where Weight>0) group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 0)
%end;

%if %upcase(&FOM)=HRSP %then %do;
/*HrSp*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, 1-count(distinct CaseID)/N_Norm as FOM from _TPFP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID
		   where CaseID in (select CaseID from Truth where Weight=0) group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 1)
%end;

%if %length(&FOM)>3 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 3)=SP@ %then %do;
%let cutpoint=%substr(%upcase(&FOM), 4);
/*SP by Cut Point*/
proc sql;
	create table _FOM as select distinct a.ReaderID, ModalityID, 1-count(distinct case when TP_Rating>=&cutpoint or FP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_Norm as FOM 
		   from _TPFP a left join _Case_by_Reader b on a.ReaderID=b.ReaderID where CaseID in (select CaseID from Truth where Weight=0) group by a.ReaderID, ModalityID order by ReaderID, ModalityID;
quit;

%merged(_FOM, 1)
%end;

%if %upcase(&FOM)=HRAUC or %upcase(&FOM)=ROC %then %do;
/*data processing form the ROC analysis*/
%if &design=1 %then %do;
proc sql;
	create table _crossed_tr_case as select distinct ReaderID, ModalityID, CaseID, (Weight>0) as Truth from Truth, _Reader, _Test order by ReaderID, ModalityID, CaseID;
quit; 
%end;
%if &design=2 %then %do;
proc sql;
	create table _crossed_tr_case as select distinct ReaderID, ModalityID, CaseID, (Weight>0) as Truth from Truth, _Test order by ReaderID, ModalityID, CaseID;
quit; 
%end;

/*TP - using the highest rating by case*/
proc sql;
	create table _TP1 as select distinct ReaderID, ModalityID, CaseID, max(max(TP_Rating), max(FP_Rating)) as Rating from _TPFP 
		   group by ReaderID, ModalityID, CaseID order by ReaderID, ModalityID, CaseID;
quit;

data _TP2;
	merge _TP1 _crossed_tr_case;
	by ReaderID ModalityID CaseID;
	if missing(Rating) then Rating=&smallest_r-0.01;
	if Truth=1;
run;

/*FP - using the highest rating by case*/
proc sql;
	create table _FP1 as select distinct ReaderID, ModalityID, CaseID, max(FP_Rating) as Rating from FP 
		   group by ReaderID, ModalityID, CaseID order by ReaderID, ModalityID, CaseID;
quit;

data _FP2;
	merge _FP1 _crossed_tr_case;
	by ReaderID ModalityID CaseID;
	if missing(Rating) then Rating=&smallest_r-0.01;
	if Truth=0; 
run;

/*Set together*/
data _all;
	set _TP2 _FP2;
	by ReaderID ModalityID;
run;

/*To exclude perfect separation in logistic model*/
proc sql;
	create table _freq1 as select distinct ReaderID, ModalityID, Truth, min(Rating) as min, max(Rating) as max from _all 
		   group by ReaderID, ModalityID, Truth order by ReaderID, ModalityID, Truth;
quit;

proc transpose data=_freq1 out=_freq2;
	by ReaderID ModalityID Truth;
	var min max;
run;

proc transpose data=_freq2 out=_freq3(drop=_name_);
	by ReaderID ModalityID;
	id _name_ Truth;
	var col1;
run;

data _freq4;
	set _freq3;
	if max0<min1 then separate=1;
	if max1<min0 then separate=2;
	keep ReaderID ModalityID separate;
run;

data _all;
	merge _all _freq4;
	by ReaderID ModalityID;
run;
	
/*ROC-AUC*/
proc logistic data=_all descending;
	where missing(separate);
	by ReaderID ModalityID;
	model Truth=Rating/firth gconv=1e-5 maxiter=100;
	roc "ROC-AUC" Rating;
	ods output rocassociation=_FOM(where=(propcase(ROCModel)="Model"));
run;

data _FOM;
	set _FOM _freq4(where=(separate>.) in=here);
	by ReaderID ModalityID;
	if here then area=1;
	keep ReaderID ModalityID area;
	rename area=FOM;
run;
%end;

%if %upcase(&FOM)=AFROC or %upcase(&FOM)=WAFROC or %upcase(&FOM)=AFROC1 or %upcase(&FOM)=WAFROC1 %then %do;
/*data processing for the AFROC analysis*/
%if &design=1 %then %do;
proc sql;
	create table _crossed_tr as select *, (Weight>0) as Truth from Truth, _Reader, _Test order by ReaderID, ModalityID, CaseID, LesionID;
	create table _crossed_tr_case as select distinct ReaderID, ModalityID, CaseID, (Weight>0) as Truth from Truth, _Reader, _Test order by ReaderID, ModalityID, CaseID;
quit; 
%end;
%if &design=2 %then %do;
proc sql;
	create table _crossed_tr as select *, (Weight>0) as Truth from Truth, _Test order by ReaderID, ModalityID, CaseID, LesionID;
	create table _crossed_tr_case as select distinct ReaderID, ModalityID, CaseID, (Weight>0) as Truth from Truth, _Test order by ReaderID, ModalityID, CaseID;
quit;
%end;

/*TP*/
proc sort data=TP;
	by ReaderID ModalityID CaseID LesionID;
run;

data _TP1;
	merge TP _crossed_tr(where=(Truth));
	by ReaderID ModalityID CaseID LesionID;
	if missing(TP_Rating) then TP_Rating=&smallest_r-0.01;
	rename TP_Rating=Rating;
run;

/*FP - using the highest rating by case*/
proc sql;
	create table _FP1 as select distinct ReaderID, ModalityID, CaseID, max(FP_Rating) as Rating from FP 
		   group by ReaderID, ModalityID, CaseID order by ReaderID, ModalityID, CaseID;
quit;

data _FP2;
	merge _FP1 _crossed_tr_case;
	by ReaderID ModalityID CaseID;
	if missing(Rating) then Rating=&smallest_r-0.01;
	%if %upcase(&FOM)=AFROC or %upcase(&FOM)=WAFROC %then %do;
		if Truth=0; /*AFROC and wAFROC were averged over normal cases*/
	%end;
	%if %upcase(&FOM)=AFROC1 or %upcase(&FOM)=WAFROC1 %then %do;
		Truth=0; /*AFROC1 and wAFROC1 include FPs on abnormal cases*/
	%end;
run;

/*Set together*/
data _all;
	set _TP1 _FP2(in=here);
	by ReaderID ModalityID;
	if here then weight=1;
run;

/*To exclude perfect separation in logistic model*/
proc sql;
	create table _freq1 as select distinct ReaderID, ModalityID, Truth, min(Rating) as min, max(Rating) as max from _all 
		   group by ReaderID, ModalityID, Truth order by ReaderID, ModalityID, Truth;
quit;

proc transpose data=_freq1 out=_freq2;
	by ReaderID ModalityID Truth;
	var min max;
run;

proc transpose data=_freq2 out=_freq3(drop=_name_);
	by ReaderID ModalityID;
	id _name_ Truth;
	var col1;
run;

data _freq4;
	set _freq3;
	if max0<min1 then separate=1;
	if max1<min0 then separate=2;
	keep ReaderID ModalityID separate;
run;

data _all;
	merge _all _freq4;
	by ReaderID ModalityID;
run;

%if %upcase(&FOM)=AFROC or %upcase(&FOM)=AFROC1 %then %do;
/*AFROC-AUC or AFROC1-AUC*/
proc logistic data=_all descending;
	where missing(separate);
	by ReaderID ModalityID;
	model Truth=Rating/firth gconv=1e-5 maxiter=100;
	%if %upcase(&FOM)=AFROC %then roc "AFROC-AUC" Rating;;
	%if %upcase(&FOM)=AFROC1 %then roc "AFROC1-AUC" Rating;;
	ods output rocassociation=_FOM(where=(propcase(ROCModel)="Model"));
run;

data _FOM;
	set _FOM _freq4(where=(separate>.) in=here);
	by ReaderID ModalityID;
	if here then area=1;
	keep ReaderID ModalityID area;
	rename area=FOM;
run;
%end;

%if %upcase(&FOM)=WAFROC or %upcase(&FOM)=WAFROC1 %then %do;
/*AFROC-AUC or AFROC1-AUC*/
proc logistic data=_all descending rocoptions(weighted);
	where missing(separate);
	by ReaderID ModalityID;
	weight weight;
	model Truth=Rating/firth gconv=1e-5 maxiter=100;
	%if %upcase(&FOM)=WAFROC %then roc "wAFROC-AUC" Rating;;
	%if %upcase(&FOM)=WAFROC1 %then roc "wAFROC1-AUC" Rating;;
	ods output rocassociation=_FOM(where=(propcase(ROCModel)="Model"));
run;

data _FOM;
	set _FOM _freq4(where=(separate>.) in=here);
	by ReaderID ModalityID;
	if here then area=1;
	keep ReaderID ModalityID area;
	rename area=FOM;
run;
%end;
%end;

/*MS(T) MS(R) MS(TR)*/
proc anova data=_FOM noprint outstat=_anova;
	class ModalityID ReaderID;
	model FOM=ModalityID|ReaderID;
run; quit;

data _null_;
	set _anova;
	if upcase(_SOURCE_)="MODALITYID" then call symput("ms_t", put(SS/DF, 32.16));
	if upcase(_SOURCE_)="READERID" then call symput("ms_r", put(SS/DF, 32.16));
	if upcase(_SOURCE_) in ("MODALITYID*READERID" "READERID*MODALITYID") then call symput("ms_tr", put(SS/DF, 32.16));
run;
%put &ms_t &ms_r &ms_tr;

****************************************************************************************************************************
								    	Bootstrapping for the covariances
****************************************************************************************************************************;
/*Sampling with replacement*/
data _case_boots;
	set _case;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_case_boots;
	by _SampleID LesionYN %if &design=2 %then ReaderID; CaseID;
run;

proc surveyselect data=_case_boots method=urs out=_resamp(drop=ExpectedHits SamplingWeight LesionYN %if &design=2 %then ReaderID;) noprint seed=&seed rate=1;
	strata _SampleID LesionYN %if &design=2 %then ReaderID;;
run;

proc sort data=_resamp;
	by CaseID _SampleID;
run;

/*Dummy data - reader by modality by resampling*/
%macro merged_sim(_data, _val);
data _rm_sim;
	set _rm;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_rm_sim;
	by ReaderID _SampleID ModalityID;
run;

data &_data;
	merge _rm_sim &_data;
	by ReaderID _SampleID ModalityID;
	if missing(FOM) then FOM=&_val;
run;
%mend merged_sim;

%if %upcase(&FOM)=MAXNLFALLCASES or (%length(&FOM)>12 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 12)=NLFALLCASES@) %then %do;
data _FP_sim;
	set FP;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_FP_sim;
	by CaseID _SampleID;
run;

data _FP_sim1;
	merge _FP_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _FP_sim2;
	set _FP_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

%if %upcase(&FOM)=MAXNLFALLCASES %then %do;
proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(CaseID)/N_AllCase as FOM 
		   from _FP_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(FP_Rating) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;

%if %length(&FOM)>12 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 12)=NLFALLCASES@ %then %do;
proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(case when FP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_AllCase as FOM 
		   from _FP_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(FP_Rating) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;
%end;

%if %upcase(&FOM)=MAXNLF or (%length(&FOM)>4 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 4)=NLF@) %then %do;
proc sql;
	create table _FP_norm as select * from FP where CaseID in (select CaseID from Truth where Weight=0);
quit;

data _FP_sim;
	set _FP_norm;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_FP_sim;
	by CaseID _SampleID;
run;

data _FP_sim1;
	merge _FP_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _FP_sim2;
	set _FP_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

%if %upcase(&FOM)=MAXNLF %then %do;
proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(CaseID)/N_Norm as FOM 
		   from _FP_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(FP_Rating) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;

%if %length(&FOM)>4 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 4)=NLF@ %then %do;
proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(case when FP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_Norm as FOM 
		   from _FP_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(FP_Rating) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;
%end;

%if %upcase(&FOM)=MAXLLF or (%length(&FOM)>4 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 4)=LLF@) %then %do;
data _TP_sim;
	set TP;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_TP_sim;
	by CaseID _SampleID LesionID;
run;

data _TP_sim1;
	merge _TP_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _TP_sim2;
	set _TP_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

%if %upcase(&FOM)=MAXLLF %then %do;
proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(CaseID)/N_Lesion as FOM 
		   from _TP_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(TP_Rating) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;

%if %length(&FOM)>4 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 4)=LLF@ %then %do;
proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(case when TP_Rating>=&cutpoint then cats(CaseID) else "" end)/N_Lesion as FOM 
		   from _TP_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(TP_Rating) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;
%end;

%if %upcase(&FOM)=HRSE or (%length(&FOM)>3 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 3)=SE@) %then %do;
%if %upcase(&FOM)=HRSE %then %do;
proc sql;
	create table _P_abnorm as select distinct CaseID, ReaderID, ModalityID from _TPFP where CaseID in (select CaseID from Truth where Weight>0);
quit;
%end;

%if %length(&FOM)>3 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 3)=SE@ %then %do;
proc sql;
	create table _P_abnorm as select distinct CaseID, ReaderID, ModalityID from _TPFP 
		   where CaseID in (select CaseID from Truth where Weight>0) and (TP_Rating>=&cutpoint or FP_Rating>=&cutpoint);
quit;
%end;

data _P_sim;
	set _P_abnorm;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_P_sim;
	by CaseID _SampleID;
run;

data _P_sim1;
	merge _P_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _P_sim2;
	set _P_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, count(CaseID)/N_Abnorm as FOM 
		   from _P_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(a.ReaderID) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 0)
%end;

%if %upcase(&FOM)=HRSP or (%upcase(&FOM)>3 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 3)=SP@) %then %do;
%if %upcase(&FOM)=HRSP %then %do;
proc sql;
	create table _P_norm as select distinct CaseID, ReaderID, ModalityID from _TPFP where CaseID in (select CaseID from Truth where Weight=0);
quit;
%end;

%if %upcase(&FOM)>3 and %substr(%upcase(&FOM)xxxxxxxxxxxx, 1, 3)=SP@ %then %do;
proc sql;
	create table _P_norm as select distinct CaseID, ReaderID, ModalityID from _TPFP
		   where CaseID in (select CaseID from Truth where Weight=0) and (TP_Rating>=&cutpoint or FP_Rating>=&cutpoint);
quit;
%end;

data _P_sim;
	set _P_norm;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;

proc sort data=_P_sim;
	by CaseID _SampleID;
run;

data _P_sim1;
	merge _P_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _P_sim2;
	set _P_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

proc sql;
	create table _FOM_sim as select distinct _SampleID, a.ReaderID, ModalityID, 1-count(CaseID)/N_Norm as FOM 
		   from _P_sim2 a left join _Case_by_Reader b on a.ReaderID=b.ReaderID 
		   where ^missing(a.ReaderID) group by _SampleID, a.ReaderID, ModalityID order by ReaderID, _SampleID, ModalityID;
quit;

%merged_sim(_FOM_sim, 1)
%end;

%if %upcase(&FOM)=HRAUC or %upcase(&FOM)=ROC %then %do;
data _all_sim;
	set _all;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;	 

proc sort data=_all_sim;
	by CaseID _SampleID;
run;

data _all_sim1;
	merge _all_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _all_sim2;
	set _all_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

proc sort data=_all_sim2;
	by ReaderID _SampleID ModalityID;
run;

/*To exclude perfect separation in logistic model*/
proc sql;
	create table _freq1 as select distinct _SampleID, ReaderID, ModalityID, Truth, min(Rating) as min, max(Rating) as max from _all_sim2
		   group by _SampleID, ReaderID, ModalityID, Truth order by ReaderID, _SampleID, ModalityID, Truth;
quit;

proc transpose data=_freq1 out=_freq2;
	by ReaderID _SampleID ModalityID Truth;
	var min max;
run;

proc transpose data=_freq2 out=_freq3(drop=_name_);
	by ReaderID _SampleID ModalityID;
	id _name_ Truth;
	var col1;
run;

data _freq4;
	set _freq3;
	if max0<min1 then separate=1;
	if max1<min0 then separate=2;
	keep ReaderID _SampleID ModalityID separate;
run;

data _all_sim2;
	merge _all_sim2(drop=separate) _freq4;
	by ReaderID _SampleID ModalityID;
run;

/*HrROC-AUC*/
proc logistic data=_all_sim2 descending;
	where missing(separate);
	by ReaderID _SampleID ModalityID;
	model Truth=Rating/firth gconv=1e-5 maxiter=100;
    roc "HrROC-AUC" Rating;;
	ods output rocassociation=_FOM_sim(where=(propcase(ROCModel)="Model"));
run;

data _FOM_sim;
	set _FOM_sim _freq4(where=(separate>.) in=here);
	by ReaderID _SampleID ModalityID;
	if here then area=1;
	keep _SampleID ReaderID ModalityID area;
	rename area=FOM;
run;
%end;

%if %upcase(&FOM)=AFROC or %upcase(&FOM)=WAFROC or %upcase(&FOM)=AFROC1 or %upcase(&FOM)=WAFROC1 %then %do;
data _all_sim;
	set _all;
	do _SampleID=1 to &nboots;
	   output;
	end;
run;	 

proc sort data=_all_sim;
	by CaseID _SampleID;
run;

data _all_sim1;
	merge _all_sim _resamp(in=here);
	by CaseID _SampleID;
	if here;
run;

data _all_sim2;
	set _all_sim1;
	do _i=1 to NumberHits;
	   output;
	end;
	drop _i NumberHits;
run;

proc sort data=_all_sim2;
	by ReaderID _SampleID ModalityID;
run;

/*To exclude perfect separation in logistic model*/
proc sql;
	create table _freq1 as select distinct _SampleID, ReaderID, ModalityID, Truth, min(Rating) as min, max(Rating) as max from _all_sim2
		   group by _SampleID, ReaderID, ModalityID, Truth order by ReaderID, _SampleID, ModalityID, Truth;
quit;

proc transpose data=_freq1 out=_freq2;
	by ReaderID _SampleID ModalityID Truth;
	var min max;
run;

proc transpose data=_freq2 out=_freq3(drop=_name_);
	by ReaderID _SampleID ModalityID;
	id _name_ Truth;
	var col1;
run;

data _freq4;
	set _freq3;
	if max0<min1 then separate=1;
	if max1<min0 then separate=2;
	keep ReaderID _SampleID ModalityID separate;
run;

data _all_sim2;
	merge _all_sim2(drop=separate) _freq4;
	by ReaderID _SampleID ModalityID;
run;

%if %upcase(&FOM)=AFROC or %upcase(&FOM)=AFROC1 %then %do;
/*AFROC-AUC or AFROC1-AUC*/
proc logistic data=_all_sim2 descending;
	where missing(separate);
	by ReaderID _SampleID ModalityID;
	model Truth=Rating;
	%if %upcase(&FOM)=AFROC %then roc "AFROC-AUC" Rating;;
	%if %upcase(&FOM)=AFROC1 %then roc "AFROC1-AUC" Rating;;
	ods output rocassociation=_FOM_sim(where=(propcase(ROCModel)="Model"));
run;

data _FOM_sim;
	set _FOM_sim _freq4(where=(separate>.) in=here);
	by ReaderID _SampleID ModalityID;
	if here then area=1;
	keep _SampleID ReaderID ModalityID area;
	rename area=FOM;
run;
%end;

%if %upcase(&FOM)=WAFROC or %upcase(&FOM)=WAFROC1 %then %do;
/*AFROC-AUC or AFROC1-AUC*/
proc logistic data=_all_sim2 descending rocoptions(weighted);
	where missing(separate);
	by ReaderID _SampleID ModalityID;
	weight weight;
	model Truth=Rating;
	%if %upcase(&FOM)=WAFROC %then roc "wAFROC-AUC" Rating;;
	%if %upcase(&FOM)=WAFROC1 %then roc "wAFROC1-AUC" Rating;;
	ods output rocassociation=_FOM_sim(where=(propcase(ROCModel)="Model"));
run;

data _FOM_sim;
	set _FOM_sim _freq4(where=(separate>.) in=here);
	by ReaderID _SampleID ModalityID;
	if here then area=1;
	keep _SampleID ReaderID ModalityID area;
	rename area=FOM;
run;
%end;
%end;

/*Cov1 - same reader, different tests*/
proc transpose data=_FOM_sim out=_cov1a(drop=_name_) Prefix=FOM;
	by ReaderID _SampleID;
	id ModalityID;
	var FOM;
run;

proc corr data=_cov1a cov noprint out=_cov1b(where=(upcase(_TYPE_)="COV"));
	by ReaderID;
	var FOM:;
run;

proc transpose data=_cov1b(rename=(_name_=Variable)) out=_cov1c(where=(upcase(Variable)<upcase(_NAME_)));
	by ReaderID Variable;
	var FOM:;
run;

proc sql noprint;
	select put(mean(col1), 32.16) into :cov1 from _cov1c;
quit;
%put &cov1;

/*Cov2 - same test, different readers*/
proc sort data=_FOM_sim;
	by ModalityID _SampleID ReaderID;
run;

proc transpose data=_FOM_sim out=_cov2a(drop=_name_) Prefix=FOM;
	by ModalityID _SampleID;
	id ReaderID;
	var FOM;
run;

proc corr data=_cov2a cov noprint out=_cov2b(where=(upcase(_TYPE_)="COV"));
	by ModalityID;
	var FOM:;
run;

proc transpose data=_cov2b(rename=(_name_=Variable)) out=_cov2c(where=(upcase(Variable)<upcase(_NAME_)));
	by ModalityID Variable;
	var FOM:;
run;

proc sql noprint;
	select put(mean(col1), 32.16) into :cov2 from _cov2c;
quit;
%put &cov2;

/*Cov3 - different tests, different readers*/
proc sql;
	create table _cov3a as select a.*, b.ReaderID as ReaderID2, b.ModalityID as ModalityID2, b.FOM as FOM2
		   from _FOM_sim a, _FOM_sim b where a._SampleID=b._SampleID and a.ReaderID<b.ReaderID and a.ModalityID<b.ModalityID 
		   order by ReaderID, ReaderID2, ModalityID, ModalityID2, _SampleID;
quit;

proc corr data=_cov3a cov noprint out=_cov3b(where=(upcase(_TYPE_)="COV"));
	by ReaderID ReaderID2 ModalityID ModalityID2;
	var FOM:;
run;

proc transpose data=_cov3b(rename=(_name_=Variable)) out=_cov3c(where=(upcase(Variable)<upcase(_NAME_)));
	by ReaderID ReaderID2 ModalityID ModalityID2 Variable;
	var FOM:;
run;

proc sql noprint;
	select put(mean(col1), 32.16) into :cov3 from _cov3c;
quit;
%put &cov3;

%if &design=2 %then %do;
	data _cov2c;
		set _cov2c;
		col1=0;
	run;
	%let cov2=0;

	data _cov3c;
		set _cov3c;
		col1=0;
	run;
	%let cov3=0;
%end;

/*Var(error)*/
proc sort data=_FOM_sim;
	by ModalityID ReaderID _SampleID;
run;

proc means data=_FOM_sim noprint;
	by ModalityID ReaderID;
	var FOM;
	output out=_var var=var;
run;

proc sql noprint;
	select put(mean(var), 32.16) into :var from _var;
quit;
%put &var;

/*Compute statistics*/
proc format;
	value inf 1e9="Inf";
run;

/*1 - Treatment x Reader Estimates of FOM*/
data _FOM; set _FOM; label FOM=; run;

proc transpose data=_FOM out=_stat1(drop=_name_) prefix=Modality_;
	by ReaderID;
	var FOM;
	id ModalityID;
run;

/*2 - ANOVA*/
data _stat2;
	set _anova;
	where upcase(_SOURCE_) ne "ERROR";
	MS=SS/DF;
	keep _SOURCE_ DF SS MS;
	rename _SOURCE_=Source;
run;

/*3 - Variance component and error-covariance estimates*/
data _stat3;
	length col1 $15. col2 8.;
	col1="Var(R)";
	col2=(&ms_r-&ms_tr)/&ntest-&cov1+&cov3;
	output;
	col1="Var(T*R)";
	col2=&ms_tr-&var+&cov1+max(&cov2-&cov3,0);
	output;
	col1="Cov1";
	col2=&cov1;
	output;
	col1="Cov2";
	col2=&cov2;
	output;
	col1="Cov3";
	col2=&cov3;
	output;
	col1="Var(Error)";
	col2=&var;
	output;
	label col2="varcov" col1="varcomp";
run;

/*4 - Test for H0: Treatments have the same FOM*/
data _stat4;
	%if %upcase(&option)=ALL or %upcase(&option)=RRRC %then %do;
		Type="RRRC";
		%if %sysevalf(&ms_tr+%sysfunc(max(&nreader*(&cov2-&cov3),0))) ne 0 %then %do;
		   F_OR=&ms_t/(&ms_tr+max(&nreader*(&cov2-&cov3),0));
		%end;
		%else %do;
		   call missing(F_OR);
		%end;
		%if %sysevalf(&ms_tr+0) ne 0 %then %do;
		   ddf=((&ms_tr+max(&nreader*(&cov2-&cov3),0))**2)/(((&ms_tr)**2)/((&nreader-1)*(&ntest-1)));
		%end;
		%else %do;
		   call missing(ddf);
		%end;
		ndf=&ntest-1;
		if ^missing(F_OR) and ^missing(ndf) and ^missing(ddf) then P=1-probf(F_OR, ndf, ddf);
		output;
	%end;

	%if %upcase(&option)=ALL or %upcase(&option)=FRRC %then %do;
		Type="FRRC";
		%if %sysevalf(&var-&cov1+%sysfunc(max((&nreader-1)*(&cov2-&cov3),0))) ne 0 %then %do;
		   F_OR=&ms_t/(&var-&cov1+max((&nreader-1)*(&cov2-&cov3),0));
		%end;
		%else %do;
		   call missing(F_OR);
		%end;
		ddf=1e9;
		ndf=&ntest-1;
		if ^missing(F_OR) and ^missing(ndf) and ^missing(ddf) then P=1-probf(F_OR, ndf, ddf);
		output;
	%end;

	%if %upcase(&option)=ALL or %upcase(&option)=RRFC %then %do;
		Type="RRFC";
		%if %sysevalf(&ms_tr+0) ne 0 %then %do;
		   F_OR=&ms_t/&ms_tr;
		%end;
		%else %do;
		   call missing(F_OR);
		%end;
		ddf=(&nreader-1)*(&ntest-1);
		ndf=&ntest-1;
		if ^missing(F_OR) and ^missing(ndf) and ^missing(ddf) then P=1-probf(F_OR, ndf, ddf);
		output;
	%end;

	format P Pvalue6.4 ddf Inf10.;
	label F_OR="F value" P="Pr>F";
run;

/*5 - confidence intervals and hypothesis tests for treatment differences*/
proc sql;
	create table _FOM1 as select distinct ModalityID, mean(FOM) as FOM from _FOM group by ModalityID order by ModalityID;
	create table _FOM2 as select a.*, b.ModalityID as ModalityID2, b.FOM as FOM2 from _FOM1 a, _FOM1 b where a.ModalityID<b.ModalityID;
quit;

data _stat5;
	set _FOM2;
	if _n_>1 then put "War" "ning: more than two tests.";
	%if %upcase(&option)=ALL or %upcase(&option)=RRRC %then %do;
	    Type="RRRC";
    	length col1 $10.;
    	col1=catx(" - ", ModalityID, ModalityID2);
    	col2=FOM-FOM2;
    	col3=sqrt(2/&nreader*(&ms_tr+&nreader*max(&cov2-&cov3,0)));
    	%if %sysevalf(&ms_tr+0) ne 0 %then %do;
		   col4=((&ms_tr+max(&nreader*(&cov2-&cov3),0))**2)/(((&ms_tr)**2)/((&nreader-1)*(&ntest-1)));
		%end;
    	%else %do;
		   call missing(col4);
		%end;
    	if col3 ne 0 then col5=col2/col3;
    	else call missing(col5);
    	if cmiss(col4, col5)=0 then col6=probt(abs(col5)*(-1), col4)*2;
    	if cmiss(col3, col4)=0 then col7=col2-tinv(1-&alpha/2, col4)*col3;
    	if cmiss(col3, col4)=0 then col8=col2+tinv(1-&alpha/2, col4)*col3;
	    output;
	%end;

	%if %upcase(&option)=ALL or %upcase(&option)=FRRC %then %do;
    	Type="FRRC";
    	col1=catx(" - ", ModalityID, ModalityID2);
    	col2=FOM-FOM2;
    	col3=sqrt(2/&nreader*(&var-&cov1+(&nreader-1)*max(&cov2-&cov3,0)));
    	col4=1e9;
    	if col3 ne 0 then col5=col2/col3;
    	else call missing(col5);
    	if cmiss(col4, col5)=0 then col6=probt(abs(col5)*(-1), col4)*2;
    	if cmiss(col3, col4)=0 then col7=col2-tinv(1-&alpha/2, col4)*col3;
    	if cmiss(col3, col4)=0 then col8=col2+tinv(1-&alpha/2, col4)*col3;
    	output;
	%end;

	%if %upcase(&option)=ALL or %upcase(&option)=RRFC %then %do;
    	Type="RRFC";
    	col1=catx(" - ", ModalityID, ModalityID2);
    	col2=FOM-FOM2;
    	col3=sqrt(2/&nreader*&ms_tr);
    	col4=(&nreader-1)*(&ntest-1);
    	if col3 ne 0 then col5=col2/col3;
    	else call missing(col5);
    	if cmiss(col4, col5)=0 then col6=probt(abs(col5)*(-1), col4)*2;
    	if cmiss(col3, col4)=0 then col7=col2-tinv(1-&alpha/2, col4)*col3;
    	if cmiss(col3, col4)=0 then col8=col2+tinv(1-&alpha/2, col4)*col3;
    	output;
	%end;

	format col6 Pvalue6.4 col4 Inf10.;
	keep Type col1-col8;
	label col1="Treatment Comparison" col2="Difference" col3="StdErr" col4="df" col5="t" col6="Pr>|t|" col7="CI Lower" col8="CI Upper";
run;

%if %upcase(&option)=ALL or %upcase(&option)=FRRC %then %do;
/*6 - Treatment Difference by Reader*/
proc sql;
	create table _FOM_r1 as select a.*, b.ModalityID as ModalityID2, b.FOM as FOM2 from _FOM a, _FOM b 
		   where a.ReaderID=b.ReaderID and a.ModalityID<b.ModalityID order by ReaderID, ModalityID;
	create table _var_r as select distinct ReaderID, mean(var) as var from _var group by ReaderID order by ReaderID;
quit;

data _stat6;
	merge _FOM_r1 _var_r _cov1c(rename=(COL1=cov1) drop=Variable _name_);
	by ReaderID;
	length Type Comp $10.;
	Type="FRRC";
	Comp=catx(" - ", ModalityID, ModalityID2);
	Diff=FOM-FOM2;
	StdErr=sqrt(2*(var-cov1));
	df=1e9;
	if StdErr ne 0 then t=Diff/StdErr;
	if cmiss(t, df)=0 then p=probt(abs(t)*(-1), df)*2;
	low=Diff-tinv(1-&alpha/2, df)*StdErr;
	up=Diff+tinv(1-&alpha/2, df)*StdErr;
	format df Inf10.;
run;

data _stat6;
	retain Type ReaderID Comp Diff StdErr df t p low up;
	set _stat6;
	keep Type ReaderID Comp Diff StdErr df t p low up;
	label Comp="Treatment Comparison" Diff="Difference" p="Pr>|t|" low="CI Lower" up="CI Upper";
run;
%end;

/*7 - Single-treatment confidence intervals*/
/*Calculating MS(R) in each treatment*/
proc sort data=_FOM;
	by ModalityID;
run;

proc anova data=_FOM noprint outstat=_anova_t;
	by ModalityID;
	class ReaderID;
	model FOM=ReaderID;
run; quit;

data _anova_t1;
	set _anova_t;
	where upcase(_SOURCE_) ne "ERROR";
	MS=SS/DF;
	keep ModalityID MS;
run;

proc sql;
	create table _cov2d as select distinct ModalityID, mean(col1) as cov2 from _cov2c group by ModalityID order by ModalityID;
	create table _var_t as select distinct ModalityID, mean(var) as var from _var group by ModalityID order by ModalityID;
quit;

data _stat7;
	length Type $5.;
	merge _FOM1 _anova_t1 _cov2d _var_t;
	by ModalityID;
	Type="RRRC";
	StdErr=sqrt((MS+&nreader*max(cov2,0))/&nreader);
	if MS ne 0 then do;
	   df=((MS+&nreader*max(cov2,0))**2)/((MS**2)/(&nreader-1));
	   low_CI=FOM-tinv(1-&alpha/2, df)*StdErr;
	   up_CI=FOM+tinv(1-&alpha/2, df)*StdErr;
	end;
	order=1;
	output;

	Type="FRRC";
	StdErr=sqrt((var+(&nreader-1)*max(cov2,0))/&nreader);
	df=1e9;
	low_CI=FOM-tinv(1-&alpha/2, df)*StdErr;
	up_CI=FOM+tinv(1-&alpha/2, df)*StdErr;
	order=2;
	output;

	Type="RRFC";
	StdErr=sqrt(MS/&nreader);
	df=&nreader-1;
	low_CI=FOM-tinv(1-&alpha/2, df)*StdErr;
	up_CI=FOM+tinv(1-&alpha/2, df)*StdErr;
	order=3;
	output;

	format df Inf10.;
	label MS="MS(R)" cov2="Cov2" low_CI="CI Lower" up_CI="CI Upper";
	drop MS cov2 var;
run;

proc sort data=_stat7 out=_stat7(drop=order);
	by order;
run;

********************************************************************************************************
										Study info
********************************************************************************************************;
data _info;
	length col1 col2 $100.;
	col1="Study Design";
	if "&design"="1" then col2="Factorial design";
	else if "&design"="2" then col2="Case-nested-within-reader split-plot design";
	output;
	col1="Number of readers";
	col2="&nreader";
	output;
	col1="Number of treaments";
	col2="&ntest";
	output;
	col1="Number of cases";
	col2="&ncase (&nnorm normal, &nabnorm abnormal)";
	output;
	col1="Analysis method";
	col2="Obuchowski-Rockette model";
	output;
	col1="Covariance estimation method";
	col2="Bootstrapping";
	output;
	col1="Dependent variable";
	col2="&FOM";
	output;
	label col1="Study information" col2="Study information";
run;

ods graphics on;
ods html file="&FOM..html";
options nonumber nodate;
title "Study Information";
proc print data=_info label noobs;
run;

title "Treatment x Reader Estimates of FOM";
proc print data=_stat1 label noobs;
run;

title "ANOVA Table";
proc print data=_stat2 label noobs;
run;

title "Variance Component and Error-covariance Estimates";
proc print data=_stat3 label noobs;
run;

title "Test for H0: Treatments have the same FOM";
proc print data=_stat4 label noobs;
run;

title "Confidence Intervals and Hypothesis Tests for Treatment Differences";
proc print data=_stat5 label noobs;
run;

%if %upcase(&option)=ALL or %upcase(&option)=FRRC %then %do;

title "Treatment Differences by Reader";
proc print data=_stat6 label noobs;
run;

%end;

title "Single-treatment Confidence Intervals";
proc print data=_stat7 label noobs;
run;

proc datasets lib=work memtype=data nolist;
	save _info &workdata _stat1-_stat5 _stat7 %if %upcase(&option)=ALL or %upcase(&option)=FRRC %then _stat6;;
run; quit;

%exit: 
ods listing;
%mend StSignificanceTesting;

/*Examples*/
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=AFROC)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=AFROC1)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=wAFROC)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=wAFROC1)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=MaxNLFAllCases)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=MaxNLF)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=MaxLLF)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=HrAuc)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=HrSe)
%StSignificanceTesting(dataset=E:\MRMC - Documents\DataFile.xls, FOM=HrSp)


%StSignificanceTesting(dataset=E:\MRMC - Documents\SplitPlotData.xlsx, FOM=ROC)
%StSignificanceTesting(dataset=E:\MRMC - Documents\SplitPlotData.xlsx, FOM=Se@4)
%StSignificanceTesting(dataset=E:\MRMC - Documents\SplitPlotData.xlsx, FOM=Sp@4)

