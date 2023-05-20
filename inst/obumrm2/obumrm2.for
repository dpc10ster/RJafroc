c https://www.lerner.ccf.org/quantitative-health/documents/OBUMRM2.for
c	                     OBUMRM2.FOR
c
c	Revised on April 7, 2005 to incorporate the changes to the
c	denominator degrees of freedom proposed and presented by
c	Steve Hillis at ENAR March 22, 2005 in Austin, TX.
c
c	Revised on 10/08/03 to accommodate a fixed-reader analysis and
c		also to improve on the outputted estimates of parameters
c		needed for sample size determination.
c	Previously revised on 01/15/03 to correct the reader-specific p-values.
c
c	A FORTRAN program to implement the Obuchowski-Rockette 
c	method for analyzing Multi-Reader and Multi-Modality ROC data. 
c	The method is described in the following reference:
c
c	Obuchowski NA, Rockette HE. Hypothesis testing of diagnostic
c	accuracy for multiple readers and multiple tests: an ANOVA
c	approach with dependent observations. Commun Stat - Simul  1995; 
c	24: 285-308.
c
c	written by Nancy A Obuchowski, PhD     October, 2001
c	*******************************************************************
c
c	PURPOSE: To perform the computations of the Obuchowski-Rockette
c	method for an inputted dataset.  The inputted dataset should have
c	the following specifications: N subjects without the condition 
c	("normals") and M subjects with the condition ("diseased") each
c	underwent I diagnostic tests.  The results of the I diagnostic tests
c	were interpreted by the same sample of J readers.  The objective
c	of the study is to compare the mean accuracies of readers in the I
c	tests to determine if the accuracy of the I tests differ.
c
c	NOTES:
c	(1) We assume there is a gold standard that provides each patients'
c	true diagnosis, i.e. distinguishes normal from abnormal patients without
c	error.
c	(2) We assume that the readers interpreted the test results blinded
c	to other test results and the gold standard result.
c	(3) We assume there is only one test result per patient, per reader,
c	per diagnostic test.  The following scenarios are not appropriate 
c	for this program: analysis of the test results of the two eyes of
c	a patient, analysis of the test results of the two kidneys of a patient.
c	(4) We assume there is no missing datapoints.
c	(5) The program can accomodate either ordinal or continuous-scale
c	test results.
c	(6) Under FORMAT A, the program estimates the area under the empirical \
c	ROC curve (so called "nonparametric estimate") and performs the necessary
c	calculations using these estimates of accuracy.  
c	***To use other estimates of the ROC area (e.g. binormal MLEs) or to use 
c	other measures of accuracy (e.g. partial area under the ROC curve), 
c	the data must be inputted using FORMAT B.  
c	(7) The program treats the diagnostic tests as fixed effects and the 
c	patient sample as random effects.  The program analyzes the data first
c	treating the readers as a random effect and secondly treating the readers
c	as a fixed effect.  The interpretations of these two analyses are
c	different.  Do not just use the analysis with the more favorable p-value.
c	Rather, consider the appropriate interpretation for the readers and
c	then use the corresponding analysis.  
c	(8) The program is currently dimensioned for either 2 or 3 modalities,
c	up to 15 readers, and up to 500 patients with and 500 without the condition.
c	These dimensions can be changed.
c	(9) The current name of the input file is 'obumrm_in.dat'.
c	The current name of the output file is 'obumrm_out.dat'.
c	(10) There are example input and output files that can be used as
c	templates for data entry and as verification that the program is
c	performing properly.
c	*********************************************************************
c
c	For FORMAT A, the inputted dataset must be structured as follows:
c         NOTE_1: The user must specify the format statement for reading in
c	the data.  The format statement that the user must modify is clearly
c	labeled in the beginning of the main body of the program.  We hope that
c	this approach to data entry will make this program easier to use.
c	  NOTE_2: All input should be right-justified.
c
c	Line 1, column 2: 0 or 1
c		0 if high values of the confidence score indicate higher suspicion
c		for the condition; 1 if low values of the confidence score indicate
c		higher suspicion for the condition.
c	Line 1, column 4: 0 or 1
c		0 if the confidence scores are integer values; 1 if the confidence
c		scores are continuous values.
c	Line 2, columns 2-4: integer value (current maximum is 500)
c		number of patients without the condition
c	Line 3, column 2: 2 or 3
c		number of diagnostic tests (or modalities)
c	Line 4, column 1-2: integer value (current maximum is 15)
c		number of readers
c	Line 5, columns 2-4: integer value (current maximum is 500)
c		number of patients with the condition 
c	Line 6, first field
c		confidence score for reader 1, diagnostic test 1, normal pt 1
c	Line 6, second field
c		confidence score for reader 1, diagnostic test 2, normal pt 1
c	Line 6, third field (if applicable)
c		confidence score for reader 1, diagnostic test 3, normal pt 1
c	Line 7, first field
c		confidence score for reader 1, diagnostic test 1, normal pt 2
c	Line 7, second field
c		confidence score for reader 1, diagnostic test 2, normal pt 2
c	Line 7, third field (if applicable)
c		confidence score for reader 1, diagnostic test 3, normal pt 2
c
c	Continue with all normal patients (i.e. pts without the condition),
c	followed by all abnormal patients (i.e. pts with the condition).  
c	Then use this same format for all readers.  
c
c	See the two example datasets named formAordin.dat and formAcont.dat.
c	**********************************************************************
c
c	For FORMAT B, the inputted data must be structured as follows:
c	   NOTE: Be sure to right-justify the inputted data
c
c	Line 1, column 2: 2 or 3
c		number of diagnostic tests
c	Line 1, columns 4-5: maximum is 15
c		number of readers
c
c	{The next J lines give the J x I matrix of estimated accuracies
c	where J is the number of rows, aka readers, and I is the number
c	of columns, aka diagnostic tests.  Note that these estimated
c	accuracies (or a suitable transformation of them) should take on 
c	values between zero and one.}
c	The fixed format for these data must be specified by the user in
c	the main body of the program.
c		estimated accuracy for reader 1 for each of I tests
c		estimated accuracy for reader 2 for each of I tests
c		etc.
c		estimated accuracy for reader J for each of I tests
c
c	{The next J lines give the J x J matrix of estimated 
c	variances and covariances corresponding to modality 1.  So, the first
c	entry is the variance of the estimated accuracy of reader 1 in 
c	modality 1.  The second entry in that same row is the covariance of
c	the estimated accuracies of readers 1 and 2 for modality 1.  etc.  
c	In the J-th row, the first entry is the covariance of the estimated
c	accuracies of readers 1 and J for modality 1.} 
c		estimated variance and covariances for reader 1 in test 1
c		estimated variance and covariances for reader 2 in test 1
c		etc.
c		estimated variances and covariances for reader J in test 1
c
c	{The next J lines give the J x J matrix of estimated 
c	variances and covariances corresponding to modality 2.  So, the first
c	entry is the variance of the estimated accuracy of reader 1 in 
c	modality 2.  The second entry in that same row is the covariance of
c	the estimated accuracies of readers 1 and 2 for modality 2.  etc.  
c	In the J-th row, the first entry is the covariance of the estimated
c	accuracies of readers 1 and J for modality 2.} 
c		estimated variance and covariances for reader 1 in test 2
c		estimated variance and covariances for reader 2 in test 2
c		etc.
c		estimated variances and covariances for reader J in test 2
c
c	{The next J lines give the J x J matrix of estimated 
c	variances and covariances corresponding to modality 3 (if there is
c	a third modality.  Otherwise, skip to the next set of instructions.)}   
c		estimated variance and covariances for reader 1 in test 3
c		estimated variance and covariances for reader 2 in test 3
c		etc.
c		estimated variances and covariances for reader J in test 3
c
c	{The next J lines give the J x J matrix of covariances
c	corresponding to having a reader in modality 1 and a reader in
c	modality 2.   So, the first entry is the covariance between
c	the estimated accuracies of reader 1 in modality 1 and reader 1 in
c	modality 2.  The second entry in that same row is the covariance of
c	the estimated accuracies of reader 1 in modality 1 and reader 2 in
c	modality 2, etc.  
c	In the J-th row, the first entry is the covariance of the estimated
c	accuracies of reader J in modality 1 and reader 1 in modality 2.} 
c		cov for reader 1 in modality 1 with other readers in modality 2.
c		cov for reader 2 in modality 1 with other readers in modality 2.
c		etc.
c		cov for reader J in modality 1 with other readers in modality 2.
c
c	{The next J lines give the J x J matrix of covariances
c	corresponding to having a reader in modality 1 and a reader in
c	modality 3 (If a third modality exists). So, the first entry is the 
c	covariance between the estimated accuracies of reader 1 in modality 1 
c	and reader 1 in modality 3.  The second entry in that same row is the 
c	covariance of the estimated accuracies of reader 1 in modality 1 and 
c	reader 2 in modality 3, etc.}  
c		cov for reader 1 in modality 1 with other readers in modality 3.
c		cov for reader 2 in modality 1 with other readers in modality 3.
c		etc.
c		cov for reader J in modality 1 with other readers in modality 3.
c
c	{The next J lines give the J x J matrix of covariances
c	corresponding to having one reader in modality 2 and one reader in
c	modality 3 (If a third modality exists). So, the first entry is the 
c	covariance between the estimated accuracies of reader 1 in modality 2 
c	and reader 1 in modality 3.  The second entry in that same row is the 
c	covariance of the estimated accuracies of reader 1 in modality 2 and 
c	reader 2 in modality 3, etc.}  
c		cov for reader 1 in modality 2 with other readers in modality 3.
c		cov for reader 2 in modality 2 with other readers in modality 3.
c		etc.
c		cov for reader J in modality 2 with other readers in modality 3.
c
c	See the example dataset named formB.dat.
c	**********************************************************************	
c
c	DEFINE VARIABLES AND CONSTANTS
c
	implicit real (a-z)
	integer inormal(3,17,500),iabnrmal(3,17,500)
	integer method,i,j,l,m,n,a,direct,scale,tnor,tabn,trts,readers
	dimension VAB(3,17,500),VNO(3,17,500),mean_t(3),mean_r(17)
	dimension accur(3,17),varcov(3,3,17,17),S01M(3,3,17,17),
     + S10M(3,3,17,17),p_read(3,3,17),normal(3,17,500),
     + abnormal(3,17,500),readersum(17),trtsum(3)
c
c
c	***********************************************************************
c
c	SPECIFY FILE NAMES
	open(unit=1,
     + file='input.dat',
     + status='old')
	open(unit=2,
     + file='output.dat',
     + status='new')
c	***********************************************************************
c
c	READ IN INPUT DATA, ONCE DETERMINE WHETHER FORMAT A OR B:
c	print *,'Data Input Format A (enter 0) or B (enter 1)?'
	method=1
	IF(method .EQ. 1)GOTO 50
c
c	READ DATA IN USING FORMAT A
	read(1,5)direct,scale
	read(1,6)tnor
	read(1,7)trts
	read(1,8)readers
	read(1,6)tabn
c
 5	format(1x,i1,1x,i1)
 6	format(i4)
 7	format(1x,i1)
 8	format(i2)
 9	format(1x,i1,1x,i2)
c
c
c	************ATTENTION USERS!!!!!!************************************
c	******FOR FORMAT A, IF YOU HAVE CONTINUOUS CONFIDENCE SCORES, HERE IS 
C	******WHERE YOU MUST SPECIFY THE INPUT STATEMENT FOR READING IN THE
c	******CONFIDENCE SCORES.
 10	format(t33,f3.0,t42,f3.0)
c
c
c	************ATTENTION USERS!!!!!!************************************
c	******FOR FORMAT A, IF YOU HAVE DISCRETE CONFIDENCE SCORES, HERE IS 
C	******WHERE YOU MUST SPECIFY THE INPUT STATEMENT FOR READING IN THE 
C	******CONFIDENCE SCORES.
 11	format(t7,i2,1x,i2)
c
c
c
	do 30, j=1,readers
	DO 20,n=1,tnor
	if(scale .eq. 1)then
	READ(1,10)(normal(i,j,n),i=1,trts)
	endif
	if(scale .eq. 0)then
	READ(1,11)(inormal(i,j,n),i=1,trts)
	endif
 20	CONTINUE
c
	DO 25,a=1,tabn
	if(scale .eq. 1)then
	READ(1,10)(abnormal(i,j,a),i=1,trts)
	endif
	if(scale .eq. 0)then
	READ(1,11)(iabnrmal(i,j,a),i=1,trts)
	endif
 25	CONTINUE
c
 30	continue
c
c	Convert integer to real
	if(scale .eq. 0)then
	do 40, i=1,trts
	do 39, j=1,readers
	do 37, n=1,tnor
	normal(i,j,n)=real(inormal(i,j,n))
 37	continue
 	do 38, a=1,tabn
 	abnormal(i,j,a)=real(iabnrmal(i,j,a))
 38	continue
 39	continue
 40 	continue
 	endif
c
 	goto 75
c
 50	CONTINUE
c
c 	READ DATA IN USING FORMAT B
 	trts=2
 	readers=17
c
 	do 51, j=1,readers
 	read(1,73)(accur(i,j),i=1,trts)
 51	continue
c
	do 53, i=1,trts
	do 52, j=1,readers
	read(1,74)(varcov(i,i,j,l),l=1,readers)
 52	continue
 53	continue
c
	do 57, i=1,trts
	do 56, m=(i+1),trts
	do 55, j=1,readers
	read(1,74)(varcov(i,m,j,l),l=1,readers)
 55	continue
 56	continue
 57	continue
c		
	do 69, i=1,trts
	do 68, m=1,trts
	do 67, j=1,readers
	do 66, l=1,readers
	varcov(m,i,l,j)=varcov(i,m,j,l)
 66	continue
 67	continue
 68	continue
 69	continue
c
 	goto 250
c
C	************ATTENTION USERS!!!!!!************************************
c	******FOR FORMAT B, HERE IS WHERE YOU MUST SPECIFY THE INPUT STATEMENT 
C	******FOR READING IN THE ACCURACY ESTIMATES.
 73	format(2(f8.6,1x))
C	******AND HERE IS WHERE YOU MUST SPECIFY THE INPUT STATEMENT FOR 
C	******READING IN THE VARIANCE-COVARIANCE MATRICES.  
 74	format(17(f10.8,1x))
c
c	********************************************************************
c
c	IF FORMAT A WAS USED, ESTIMATE THE ROC AREAS AND THE VAR-COV MATRIX
 75	continue
c
C	COMPUTING THE STRUCTURAL COMPONENTS
	DO 100,i=1,trts
	do 95, j=1,readers
	DO 80,a=1,tabn
	SUM=0.0
c
	DO 79,n=1,tnor
	if(direct .eq. 0)then
	IF(abnormal(i,j,a) .GT. normal(i,j,n))SUM=SUM+1.0
	IF(abnormal(i,j,a) .EQ. normal(i,j,n))SUM=SUM+0.5
	endif
	if(direct .eq. 1)then
	IF(abnormal(i,j,a) .LT. normal(i,j,n))SUM=SUM+1.0
	IF(abnormal(i,j,a) .EQ. normal(i,j,n))SUM=SUM+0.5
	endif
 79	CONTINUE
c
	VAB(i,j,a)=SUM/REAL(tnor)
 80	CONTINUE
	DO 90,n=1,tnor
	SUM=0.0
	DO 85,a=1,tabn
	if(direct .eq. 0)then
	IF(normal(i,j,n) .LT. abnormal(i,j,a))SUM=SUM+1.0
	IF(normal(i,j,n) .EQ. abnormal(i,j,a))SUM=SUM+0.5
	endif
	if(direct .eq. 1)then
	IF(normal(i,j,n) .GT. abnormal(i,j,a))SUM=SUM+1.0
	IF(normal(i,j,n) .EQ. abnormal(i,j,a))SUM=SUM+0.5
	endif
 85	CONTINUE
c
	VNO(i,j,n)=SUM/REAL(tabn)
 90	CONTINUE
 95	continue
 100	CONTINUE
c
C	COMPUTE THE ESTIMATES OF THE AREAS
	DO 110,i=1,trts
	do 109, j=1,readers
	SUM=0.0
	DO 105,a=1,tabn
	SUM=SUM+VAB(i,j,a)
 105	CONTINUE
	accur(i,j)=SUM/REAL(tabn)
 109	continue
 110	CONTINUE
c
C	COMPUTE THE ESTIMATED COVARIANCE MATRIX OF THE AREA VECTOR
c
	do 160, j=1,readers
	do 155, l=1,readers
	DO 150,i=1,trts
	DO 145,m=1,trts
	SUM=0.0
	DO 135,a=1,tabn
	SUM=SUM+((VAB(i,j,a)-accur(i,j))*(VAB(m,l,a)-accur(m,l)))
 135	CONTINUE
	S10M(i,m,j,l)=SUM/REAL(tabn-1)
 145	CONTINUE
 150	CONTINUE
 155	continue
 160	continue
c
	DO 190,i=1,trts
	DO 185,m=1,trts
	do 170, j=1,readers
	do 165, l=1,readers
	SUM=0.0
	DO 164,n=1,tnor
	SUM=SUM+((VNO(i,j,n)-accur(i,j))*(VNO(m,l,n)-accur(m,l)))
 164	CONTINUE
	S01M(i,m,j,l)=SUM/REAL(tnor-1)
 165	CONTINUE
 170	CONTINUE
 185	continue
 190	continue
c
	do 210, j=1,readers
	do 209, l=1,readers
	DO 200,i=1,trts
	DO 195,m=1,trts
	varcov(i,m,j,l)=(S10M(i,m,j,l)/REAL(tabn))+(S01M(i,m,j,l)/REAL(tnor))
 195	CONTINUE
 200	CONTINUE
 209	continue
 210	continue
c
c	**********************************************************************
c
c	PERFORM THE READER-SPECIFIC ANALYSES 
 250	continue
c
c	COMPUTE THE P-VALUES FOR EACH READER FOR EACH MODALITY PAIR
	special=0.0
	do 280, j=1,readers
	do 275, i=1,trts
	special=special+varcov(i,i,j,j)
	do 270, m=(i+1),trts
	num_zz=accur(i,j)-accur(m,j)
	den_zz=sqrt(varcov(i,i,j,j)+varcov(m,m,j,j)-2.0*varcov(i,m,j,j))
	zz=num_zz/den_zz
	p_read(i,m,j)=2.0*pnorm(zz)
	print *,j,varcov(i,i,j,j),varcov(m,m,j,j),varcov(i,m,j,j)
 270	continue
 275	continue
 280	continue
c
	special=special/real(readers*trts)
c
c	OUTPUT THE READER-SPECIFIC ANALYSES
	write(2,399)
	write(2,400)
	write(2,401)
	do 300, j=1,readers
	write(2,402)j,accur(1,j),sqrt(varcov(1,1,j,j)),accur(2,j),
     + sqrt(varcov(2,2,j,j)),p_read(1,2,j)
 300	continue
c
 	if(trts .eq. 3)then
 	write(2,403)
	write(2,401)
	do 305, j=1,readers
	write(2,402)j,accur(1,j),sqrt(varcov(1,1,j,j)),accur(3,j),
     + sqrt(varcov(3,3,j,j)),p_read(1,3,j)
 305	continue
c
 	write(2,404)
	write(2,401)
	do 310, j=1,readers
	write(2,402)j,accur(2,j),sqrt(varcov(2,2,j,j)),accur(3,j),
     + sqrt(varcov(3,3,j,j)),p_read(2,3,j)
 310	continue
 	endif
c
 399	format('                 Reader-Specific Analyses')
 400	format('             Modality 1         Modality 2')
 401	format('            Estimate (SE)      Estimate (SE)       p-value')
 402	format('Reader ',i2,3x,f6.4,' (',f6.4,')    ',f6.4,' (',f6.4,')     ',f6.4)
 403	format('             Modality 1         Modality 3')
 404	format('             Modality 2         Modality 3')
c
c	************************************************************************
c	PERFORM THE MULTI-READER ANALYSIS 
c
c	ESTIMATE THE MEAN FOR EACH MODALITY
	do 450, i=1,trts
	sum=0.0
	do 449, j=1,readers
	sum=sum+accur(i,j)
 449	continue
 	mean_t(i)=sum/real(readers)
 450	continue
c
c	ESTIMATE THE MEAN FOR EACH READER
	do 460, j=1,readers
	sum=0.0
	do 459, i=1,trts
	sum=sum+accur(i,j)
 459	continue
 	mean_r(j)=sum/real(trts)
 460	continue
c
c	ESTIMATE THE OVERALL MEAN
	sum=0.0
	do 470, i=1,trts
	sum=sum+mean_t(i)
 470	continue
 	mean_o=sum/real(trts)
c
c	ESTIMATE R_1
	sum=0.0
	sum2=0.0
	minus=0
	do 480, i=1,trts
	do 479, m=(i+1),trts
	do 478, j=1,readers
	if(varcov(i,i,j,j) .eq. 0.0 .or. varcov(m,m,j,j) .eq. 0.0)then
	minus=minus+1
	goto 478
	endif
	sum=sum+(varcov(i,m,j,j)/sqrt(varcov(i,i,j,j)*varcov(m,m,j,j)))
	sum2=sum2+varcov(i,m,j,j)
 478	continue
 479	continue
 480	continue
 	r1=sum/(real(readers*trts*(trts-1))/2.0)
 	covr1=sum2/(real(readers*trts*(trts-1)-minus)/2.0)
c
c	ESTIMATE R_2
	sum=0.0
	sum2=0.0
	do 500, i=1,trts
	do 499, j=1,readers
	do 498, l=(j+1),readers
	sum=sum+(varcov(i,i,j,l)/sqrt(varcov(i,i,j,j)*varcov(i,i,l,l)))
	sum2=sum2+varcov(i,i,j,l)
 498	continue
 499	continue
 500	continue
 	r2=sum/(real(trts*readers*(readers-1))/2.0)
 	covr2=sum2/(real(trts*readers*(readers-1))/2.0)
c
c	ESTIMATE R_3
	sum=0.0
	sum2=0.0
	do 520, i=1,trts
	do 519, m=1,trts
	do 518, j=1,readers
	do 517, l=1,readers
	if(i .eq. m)goto 519
	if(j .eq. l)goto 517
	sum=sum+(varcov(i,m,j,l)/sqrt(varcov(i,i,j,j)*varcov(m,m,l,l)))
	sum2=sum2+varcov(i,m,j,l)
 517	continue
 518	continue
 519	continue
 520	continue
 	r3=sum/((real(trts*(trts-1)))*
     + (real(readers*(readers-1))))
 	covr3=sum2/((real(trts*(trts-1)))*
     + (real(readers*(readers-1))))
c
c	COMPARE R_2 AND R_3
	if(r3 .gt. r2)then
	tempm=(r2+r3)/2.0
	r2=tempm
	r3=tempm
	endif
c
	if(covr3 .gt. covr2)then
	tempm=(covr2+covr3)/2.0
	covr2=tempm
	covr3=tempm
	endif
c
c	ESTIMATE SIGMAB and RB
	sum1=0.0
	sum2=0.0
	sum3=0.0
	sum12=0.0
	sum13=0.0
	sum23=0.0
c
	do 523, j=1,readers
	sum1=sum1+(accur(1,j)-mean_t(1))**2
	sum2=sum2+(accur(2,j)-mean_t(2))**2
	sum12=sum12+(accur(1,j)-mean_t(1))*(accur(2,j)-mean_t(2))
c
	if(trts .eq. 3)then
	sum3=sum3+(accur(3,j)-mean_t(3))**2
	sum13=sum13+(accur(1,j)-mean_t(1))*(accur(3,j)-mean_t(3))
	sum23=sum23+(accur(2,j)-mean_t(2))*(accur(3,j)-mean_t(3))
	endif
c
 523	continue
 	rb=sum12/sqrt(sum1*sum2)
 	if(trts .eq. 3)then
 	rb13=sum13/sqrt(sum1*sum3)
 	rb23=sum23/sqrt(sum2*sum3)
 	rb=(rb+rb13+rb23)/3.0
 	endif
c
	if(trts .eq. 2)then
	sigmab=(sum1+sum2)/(real(readers-1)*2.0)
	endif
	if(trts .eq. 3)then
	sigmab=(sum1+sum2+sum3)/(real(readers-1)*3.0)
	endif	
c
c	CALCULATE THE DENOMINATOR OF FSTAR (equation 3.13) (Random-Reader)
	sum=0.0
	do 540, i=1,trts
	do 539, j=1,readers
	sum=sum+(accur(i,j)-mean_t(i)-mean_r(j)+mean_o)**2
 539	continue
 540	continue
 	sum=sum/real((trts-1)*(readers-1))
 	den_f=sum+real(readers)*(covr2-covr3)
c
c	CALCULATE THE NUMERATOR OF FSTAR (Random-Reader)
	sum=0.0
	do 560, i=1,trts
	sum=sum+(mean_t(i)-mean_o)**2
 560	continue
 	num_f=(real(readers)*sum)/real(trts-1)
c
c	CALCULATE FSTAR 
	fstar=num_f/den_f
c
c	GET THE P-VALUE OF FSTAR (Random-Reader)
	dfn=real(trts-1)
	dfd=real((trts-1)*(readers-1))
	XX=dfd/(dfd+fstar*dfn)
	call BDTR(XX,dfd/2.0,dfn/2.0,p_fstar,dd,ier)
c
c
c	CALCULATE THE VAR OF THE DIFF ASSUMING FIXED READERS
 	den_bzz=sqrt((2.0/real(readers))*(special-covr1+
     + real(readers-1)*(covr2-covr3)))
c
c	CALCULATE THE STATISTIC FOR FIXED READERS
	if(trts .eq. 2)then
	zz=(mean_t(1)-mean_t(2))/den_bzz
	p_fixed=2.0*pnorm(zz)
	endif
c
	if(trts .eq. 3)then
	zz1=(mean_t(1)-mean_t(2))/den_bzz
	zz2=(mean_t(1)-mean_t(3))/den_bzz
	zz3=(mean_t(2)-mean_t(3))/den_bzz
	p_fixed1=2.0*(pnorm(zz1))
	p_fixed2=2.0*(pnorm(zz2))
	p_fixed3=2.0*(pnorm(zz3))
	endif
c
c
c	WRITE OUT RESULTS OF STATISTICAL TESTING - 1st Random-reader
	write(2,647)
	write(2,647)
	write(2,568)
	write(2,569)
	write(2,570)
	write(2,571)fstar,(trts-1),(trts-1)*(readers-1)
	write(2,572)p_fstar
 568	format('Results of Overall Test that Accuracies of Diagnostic Modalities
     + are equivalent')
 569	format('USING DFs ORIGINALLY PROPOSED BY OBUCHOWSKI AND ROCKETTE')
 570	format('RANDOM-READER effect MODEL')
 571	format('F*=',F7.3,' with ',i1,' and ',i2,' dfs') 
 572	format('corresponding p-value is ',f6.4)
c
c	***********************************************************************
c
c	CONSTRUCT 95% CIs FOR DIFFERENCES IN MEAN ACCURACIES OF MODALITIES
c
c	FIRST, GET THE PIVOTAL t-STATISTIC
c
	tt=TP(readers-1)
c	
c	CONSTRUCT THE CIS
	write(2,648)
	write(2,610)
c
	do 620, i=1,trts
	do 619, m=(i+1),trts
	sum=0.0
	do 590, j=1,readers
	sum=sum+((accur(i,j)-accur(m,j))-(mean_t(i)-mean_t(m)))**2
 590	continue
c	
	denom_t=sqrt(sum/(real(readers*(readers-1)))+2.0*(covr2-covr3))
	pivot=tt*sqrt(sum/(real(readers*(readers-1)))+2.0*(covr2-covr3))
	lower=(mean_t(i)-mean_t(m))-pivot
	upper=(mean_t(i)-mean_t(m))+pivot
c
c	OUTPUT THE LOWER AND UPPER BOUNDS
	write(2,611)i,m,lower,upper
 610	format('95% CIs for Difference in Mean Accuracy Between Modalities')
 611	format('For Modalities ',i1,' vs ',i1,' : [',f6.4,', ',f6.4,']')
c
c	If appropriate, perform pairwise comparisons of modalities
	if(p_fstar .lt. 0.05 .and. trts .eq. 3)then
	tstar=(mean_t(i)-mean_t(m))/denom_t
c
c	GET THE P-VALUE OF tSTAR
	dfn=1.0
	dfd=real(readers-1)
	ffstar=tstar**2
	XX=dfd/(dfd+ffstar*dfn)
	call BDTR(XX,dfd/2.0,dfn/2.0,p_tstar,dd,ier)
c
	write(2,612)p_tstar
	endif
 612	format('(The p-value for the comparison of these 2 modalities is ',
     + f6.4,')')
c
 619	continue
 620	continue
c
c
c	***********************************************************************
c
c	CONSTRUCT 95% CIs FOR MEAN ACCURACIES OF EACH MODALITY
c
c	
c	CONSTRUCT THE CIS
	write(2,648)
	write(2,625)
c
	do 639, i=1,trts
	sum=0.0
	do 621, j=1,readers
	sum=sum+(accur(i,j)-mean_t(i))**2
 621	continue
c	
	pivot=tt*sqrt(sum/(real(readers*(readers-1)))+covr2)
	lower=(mean_t(i))-pivot
	upper=(mean_t(i))+pivot
c
c	OUTPUT THE LOWER AND UPPER BOUNDS
	write(2,626)i,lower,upper
 625	format('95% CIs for Mean Accuracy of Each Modality')
 626	format('For Modality ',i1,' : [',f6.4,', ',f6.4,']')
c
c
 639	continue
 647 	format('_____________________________________________________________________________')
 648	format('-----------------------------------------------------------------------------')
c
c	*******************************************************************
c	WRITE OUT RESULTS OF STATISTICAL TESTING - now fixed reader
	write(2,647)
	write(2,647)
	write(2,670)
	if(trts .eq. 2)then
	write(2,671)zz,p_fixed
	endif
	if(trts .eq. 3)then
	write(2,672)p_fixed1,p_fixed2,p_fixed3
	endif
 670	format('Results of Overall Test that Accuracies of Diagnostic Modalities
     + are equivalent',/' - FIXED READER EFFECT')
 671	format('z=',F7.3,' with p-value of ',f6.4) 
 672	format('P-values for Modalities 1vs2, 1vs3, 2vs3 are ',3(f6.4,1x)) 
c
c	***********************************************************************
c
c	CONSTRUCT 95% CIs FOR DIFFERENCES IN MEAN ACCURACIES OF MODALITIES -fixed reader
c	
c	CONSTRUCT THE CIS
	write(2,648)
	write(2,610)
c
	do 720, i=1,trts
	do 719, m=(i+1),trts
	pivotz=1.96*(den_bzz)
	lowerz=(mean_t(i)-mean_t(m))-pivotz
	upperz=(mean_t(i)-mean_t(m))+pivotz
c
c	OUTPUT THE LOWER AND UPPER BOUNDS
	write(2,611)i,m,lowerz,upperz
 719	continue
 720	continue
c
c
c	***********************************************************************
c
c	CONSTRUCT 95% CIs FOR MEAN ACCURACIES OF EACH MODALITY
c
c	
c	CONSTRUCT THE CIS
	write(2,648)
	write(2,625)
c
	do 739, i=1,trts
c	sum=0.0
c	do 721, j=1,readers
c	sum=sum+(accur(i,j)-mean_t(i))**2
c 721	continue
c	
	pivotz=1.96*sqrt((1.0/real(readers))*(special+
     + (real(readers-1))*covr2))
	lowerz=(mean_t(i))-pivotz
	upperz=(mean_t(i))+pivotz
c
c	OUTPUT THE LOWER AND UPPER BOUNDS
	write(2,626)i,lowerz,upperz
c
c
 739	continue
c
c	********************************************************************
c	CALCULATE SOME OF THE PARAMETERS NEEDED FOR SAMPLE SIZE ESTIMATION
c
	correct=0.0
	totalss=0.0
	do 750, i=1,trts
	do 749, j=1,readers
	correct=correct+accur(i,j)
	totalss=totalss+accur(i,j)*accur(i,j)
 749	continue
 750	continue
c
	correct=(correct*correct)/real(readers*trts)
	totalss=(totalss)-correct
c
	do 755, i=1,trts
	trtsum(i)=0.0
 755	continue
c
	do 765, j=1,readers
	readersum(j)=0.0
 765	continue
c
	do 770, i=1,trts
	do 769, j=1,readers
	trtsum(i)=trtsum(i)+accur(i,j)
	readersum(j)=readersum(j)+accur(i,j)
 769	continue
 770	continue
c
	trtsss=0.0
	do 775, i=1,trts
	trtsss=trtsss+trtsum(i)*trtsum(i)
 775	continue
 	trtsss=(trtsss/real(readers))-correct
c
	readss=0.0
	do 780, j=1,readers
	readss=readss+readersum(j)*readersum(j)
 780	continue
 	readss=(readss/real(trts))-correct
c
	ss_inter=totalss-trtsss-readss
	ms_inter=ss_inter/real((trts-1)*(readers-1))
c
c	Estimate below due to Steve Hillis 09/05/2003
	v_inter=ms_inter-(special-covr1-covr2+covr3)
c
c	**********************************************************************
c	OUTPUT THE ESTIMATES OF R1, R2, R3, and the reader-modality interaction
	write(2,647)
	write(2,647)
	write(2,840)
	write(2,841)
	write(2,842)r1,covr1
	write(2,843)r2,covr2
	write(2,844)r3,covr3
	write(2,845)special
	if(v_inter .gt. 0.0)write(2,846)v_inter
	if(v_inter .le. 0.0)then
	write(2,847)rb
	write(2,848)sigmab
	endif
c
 840	format('For Calculating the Required Sample Size for Future 
     + Studies,')
 841	format('the following values, estimated from these data, 
     + are useful.')
 842	format('Estimated value of r_1 is ',f6.4,
     + ' and the estimated covariance is ',f9.6)
 843	format('Estimated value of r_2 is ',f6.4,
     + ' and the estimated covariance is ',f9.6)
 844	format('Estimated value of r_3 is ',f6.4,
     + ' and the estimated covariance is ',f9.6)
 845	format('Estimated value of the sum of variance components for
     + patient samples and within-reader error is ',f9.6)
 846	format('Estimated value of the variance component for the
     + interaction of reader and modality is ',f9.6)
 847	format('Estimated value of r_b is ',f6.4)
 848	format('Estimated value of variance between readers is ',f9.6)
c
c
c	**********************************************************************
c	Revisions made on April 7, 2005 to accommodate the DFs proposed
c	by Steve Hillis at ENAR 2005.
c
c
c	GET THE P-VALUE OF FSTAR (Random-Reader)
	dfn=real(trts-1)
	if((covr2-covr3) .gt. 0.0)then
	dfd=((ms_inter+readers*(covr2-covr3))**2)/
     + ((ms_inter**2)/real((trts-1)*(readers-1)))
	endif
	if((covr2-covr3) .le. 0.0)then
	dfd=((ms_inter)**2)/
     + ((ms_inter**2)/real((trts-1)*(readers-1)))
	endif
	XX=dfd/(dfd+fstar*dfn)
	call BDTR(XX,dfd/2.0,dfn/2.0,p_fstar,dd,ier)
c
c
c	WRITE OUT RESULTS OF STATISTICAL TESTING - Random-reader
c	(Note that the Fixed-reader results are not affected by Hillis' proposal)
	write(2,647)
	write(2,647)
	write(2,568)
	write(2,900)
	write(2,901)
	write(2,902)fstar,(trts-1),dfd
	write(2,572)p_fstar
 900	format('USING DFs PROPOSED BY HILLIS, 2005')
 901	format('RANDOM-READER effect MODEL (fixed reader model not affected)')
 902	format('F*=',F7.3,' with ',i1,' and ',f7.3,' dfs') 
c
c
c	**********************************************************************
	end
c
c	************************************************
	function pnorm (zscore)
c	************************************************
	implicit real (a-z)
	r=0.2316419
	pi=3.14159268
	b1=0.31938153
	b2=-0.356563782
	b3=1.781477937
	b4=-1.821255978
	b5=1.330274429
	sign=1.0
	ref=0.0
	if(zscore .ge. 0.0)goto 50
c	sign=-1.0
c	ref=1.0
	zscore=-1.0*zscore
 50	continue
	x=zscore
	t=1/(1+r*x)
	poly=b1*t+b2*t**2+b3*t**3+b4*t**4+b5*t**5
	func=(1/sqrt(2*pi))*exp(-1.0*((x**2)/2))
	pnorm=sign*(func*poly)+ref
	return
	end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE BDTR (X,A,B,P,D,IER)
C
C     X : INPUT FOR WHICH P(X) IS COMPUTED
C     A : DETA DIST. PARAMETER
C     B : BETA DIST. PARAMETER
C     P : OUTPUT PROBABLILTY
C     D : OUTPUT DENSITY
C
      REAL*8 XX,DLXX,DLIX,AA,BB,G1,G2,G3,G4,DD,PP,
     + X0,FF,FN,XI,SS,CC,RR,DLBETA
C
C     TEST FOR VALID DATA
C
      IF (A-(.5D0-1.D-5)) 640,10,10
  10  IF (B-(.5D0-1.D-5)) 640,20,20
  20  IF (A-1.D+5) 30,30,640
  30  IF (B-1.D+5) 40,40,640
  40  IF (X) 640,50,50
  50  IF (1.0D0-X) 460,60,60
C
C     COMPUTE LOG(BETA(A,B))
C
  60  AA=A
      BB=B
      CALL DLGAM(AA,G1,IOK)
      CALL DLGAM(BB,G2,IOK)
      CALL DLGAM(AA+BB,G3,IOK)
      DLBETA = G1+G2-G3
C
C     TEST FOR X NEAR 0.0 OR 1.0
C
      IF (X-1.D-8) 80,80,70
  70  IF ((1.0D0-X)-1.D-8) 130,130,140
  80  P=0.0
      IF (A-1.0D0) 90,100,120
  90  D=1.D+38
      GO TO 660
  100 DD=-DLBETA
      IF(DD+1.068D02) 120,120,110
  110 DD=DEXP(DD)
      D=SNGL(DD)
      GO TO 660
  120 D=0.0
      GO TO 660
  130 P=1.0
      IF(B-1.0D0) 90,100,120
C
C     SET PROGRAM ORDINATE
C
  140 XX=X
      DLXX=DLOG(XX)
      DLIX=DLOG(1.0d0-XX)
      X0=XX/(1.0D0-XX)
      ID=0
C
C     COMPUTE ORDINATE
C
      DD=(AA-1.0D0)*DLXX+(BB-1.0D0)*DLIX-DLBETA
      IF(DD-1.68D02) 150,150,160
  150 IF(DD+1.68D02) 170,170,180
  160 D=1.D38
      GO TO 190
  170 D=0.0
      GO TO 190
  180 DD=DEXP(DD)
      D=SNGL(DD)
C
C     A OR B BOTH WITHIN 1.E-8 OF 1.0
C
  190 IF(ABS(A-1.0D0)-1.D-8) 200,200,210
  200 IF(ABS(B-1.0D0)-1.D-8) 220,220,230
  210 IF(ABS(B-1.0D0)-1.D-8) 260,260,290
  220 P=X
      GO TO 660
  230 PP=BB*DLIX
      IF(PP+1.68D02) 240,240,250
  240 P=1.0D0
      GO TO 660
  250 PP=DEXP(PP)
      PP=1.D0-PP
      P=SNGL(PP)
      GO TO 600
  260 PP=AA*DLXX
      IF(PP+1.68D02) 270,270,280
  270 P=0.0
      GO TO 660
  280 PP=DEXP(PP)
      P=SNGL(PP)
      GO TO 600
C
C     TEST FOR A OR B .GT. 1000
C
  290 IF (A-1000.0) 300,300,310
  300 IF (B-1000.0) 330,330,320
  310 XX=2.D0*AA/X0
      XS=SNGL(XX)
      AA=2.0D0*BB
      DF=SNGL(AA)
      CALL CDTR(XS,DF,P,DUMMY,IER)
      P=1.0-P
      GO TO 670
  320 XX=2.0D0*BB*X0
      XS=SNGL(XX)
      AA=2.D0*AA
      DF=SNGL(AA)
      CALL CDTR(XS,DF,P,DUMMY,IER)
      GO TO 670
C
C     SELECT PARAMETER FOR CONTINUED FRACTION COMPUTATION
C
  330 IF (X-.5D0) 340,340,380
  340 IF (AA-1.0D0) 350,350,360
  350 RR=AA+1.0D0
      GO TO 370
  360 RR=AA
  370 DD=DLXX/5.0D0
      DD=DEXP(DD)
      DD=(RR-1.0D0)-(RR+BB-1.0D0)*XX*DD +2.D0
      IF (DD) 420,420,430
  380 IF (BB-1.D0) 390,390,400
  390 RR=BB+1.D0
      GO TO 410
  400 RR=BB
  410 DD=DLIX/5.D0
      DD=DEXP(DD)
      DD=(RR-1.0)-(AA+RR-1.0D0)*(1.0D0-XX)*DD+2.D0
      IF(DD) 430,430,420
  420 ID=1
      FF=DLIX
      DLIX=DLXX
      DLXX=FF
      X0=1.0D0/X0
      FF=AA
      AA=BB
      BB=FF
      G2=G1
C
C     TEST FOR A LESS THAN 1.0
C
  430 FF=0.D0
      IF (AA-1.D0) 440,440,470
  440 CALL DLGAM(AA+1.D0,G4,IOK)
      DD=AA*DLXX+BB*DLIX+G3-G2-G4
      IF (DD+1.68D02) 460,460,450
  450 FF=FF+DEXP(DD)
  460 AA=AA+1.D0
C
C     COMPUTE P USING CONTINUED FRACTION EXPANSION
C
  470 FN=AA+BB-1.D0
      RR=AA-1.D0
      II=80
      XI=DFLOAT(II)
      SS=((BB-XI)*(RR+XI))/((RR+2.D0*XI-1.D0)*(RR+
     + 2.D0*XI))
      SS=SS*X0
      DO 480 I=1,79
      II=80-I
      XI=DFLOAT(II)
      DD=(XI*(FN+XI))/((RR+2.D0*XI+1.D0)*(RR+
     + 2.D0*XI))
      DD=DD*X0
      CC=((BB-XI)*(RR+XI))/((RR+2.D0*XI-1.D0)*
     + (RR+2.D0*XI))
      CC=CC*X0
      SS=CC/(1.D0+DD/(1.D0-SS))
  480 CONTINUE
      SS=1.D0/(1.D0-SS)
      IF (SS) 650,650,490
  490 CALL DLGAM(AA+BB,G1,IOK)
      CALL DLGAM(AA+1.0D0,G4,IOK)
      CC=G1-G2-G4+AA*DLXX+(BB-1.D0)*DLIX
      PP=CC+DLOG(SS)
      IF(PP+1.98D02) 500,500,510
  500 PP=FF
      GO TO 520
  510 PP=DEXP(PP)+FF
  520 IF (ID) 540,540,530
  530 PP=1.D0-PP
  540 P=SNGL(PP)
C
C     SET ERROR
C
      IF (P) 550,570,570
  550 IF(ABS(P)-1.E-7) 560,560,650
  560 P=0.0
      GO TO 660
  570 IF(1.-P) 580,600,600
  580 IF(ABS(1.-P)-1.E-7) 590,590,650
  590 P= 1.0
      GO TO 660
  600 IF(P-1.E-6) 610,610,620
  610 P=0.0
      GO TO 660
  620 IF ((1.0-P)-1.E-8) 630,630,660
  630 P=1.0
      GO TO 660
  640 IER =-2
      D=-1.E38
      P=-1.E38
      GO TO 670
  650 IER=+2
      P= 1.E38
      GO TO 670
  660 IER=0
  670 RETURN
      END
CC
      SUBROUTINE DLGAM(XX,DLNG,IER)
      REAL*8 XX,ZZ,TERM,RZ2,DLNG
      IER=0
      ZZ=XX
      IF(XX-1.D10) 2,2,1
   1  IF(XX-1.D70) 8,9,9
C
C     SEE IF XX IS NEAR 0 OR NEGATIVE
C
   2  IF(XX-1.D-9) 3,3,4
   3  IER=-1
      DLNG=-1.D75
      GO TO 10
C
C     XX .GT. 0 AND .LE. 1.0D10
C
   4  TERM=1.D0
   5  IF(ZZ-18.D0) 6,6,7
   6  TERM=TERM*ZZ
      ZZ=ZZ+1.D0
      GO TO 5
   7  RZ2=1.D0/ZZ**2
      DLNG=(ZZ-0.5D0)*DLOG(ZZ)-ZZ+0.9189385332046727D0 
     + -DLOG(TERM)+(1.D0/ZZ)*(.833333333333333D-1 -
     + (RZ2*(.277777777777777D-2 +(RZ2*
     + (.7936507936507936D-3 -(RZ2*
     + (.5952380952380952D-3)))))))
      GO TO 10
C
C     XX .GT. 1.D10 AND .LT. 1.D70
C
   8  DLNG= ZZ*(DLOG(ZZ)-1.D0)
      GO TO 10
C
C     XX .GE. 1.OD70
C
   9  IER=+1
      DLNG=1.075
   10 RETURN
      END
C
C     THIS SUBROUTINE COMPUTES P(X) = PROBABILTY THAT THE RANDOM
C     VARIABLE U, DISTRIBUTED ACCRODING TO THE CHI-SQUARE DISTRIBUTION
C     WITH PARAMETER G DEGREE OF FREEDOM, IS LESS THAN OR EQUAL TO X,
C     F(G,X).
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CDTR (X,G,P,D,IER)
C
C     X : INPUT FOR WHICH P(X) IS COMPUTED
C     G : DEGREE OF FREEDOM
C     P : OUTPUT PROBABLILTY
C     D : OUTPUT DENSITY
C
      REAL*8 XX, DLXX, X2, DLX2, GG, G2, DLT3, THETA, 
     + THP1, GLG2, DD,T11,SER,CC,XI,FAC,TLOG,TERM,GTH,
     + A2,A,B,C,DT2,DT3,THPI

C
C     TEST FOR VALID INPUT DATA
C
      IF(G-(.5-1.0E-5)) 590,10,10
  10  IF(G-2.0E+5) 20,20,590
  20  IF (X) 590,30,30
  
C
C     TEST FOR X NEAR 0.0
C
  30  IF (X-1.0E-8) 40,40,80
  40  P=0.0
      IF(G-2.0) 50,60,70
  50  D=1.0E38
      GO TO 610
  60  D=0.5
      GO TO 610
  70  D=0.0
      GO TO 610
C
C     TEST FOR X GREATER THAN 1.E+6
C
  80  IF (X-1.E+6) 100,100,90
  90  D=0.0
      P=1.0
      GO TO 610
C
C     SET PROGRAM PARAMETERS
C
  100 XX=DBLE(X)
      DLXX=DLOG(XX)
      X2=XX/2.D0
      DLX2=DLOG(X2)
      GG=DBLE(G)
      G2=GG/2.0D0
C
C     COMPUTE ORDINATE
C
      CALL DLGAM(G2,GLG2,ICK)
      DD=(G2-1.D0)*DLXX-X2-G2*.6931471805599453D0 
     + -GLG2
      IF (DD-1.68D02) 110,110,120
 110  IF (DD+1.68D02) 130,130,140
 120  D=1.E38
      GO TO 150
 130  D=0.0
      GO TO 150
 140  DD=DEXP(DD)
      D=SNGL(DD)
C
C     TEST FOR G .GT. 1000.0
C     TEST FOT X .GT. 2000.0
 150  IF(G-1000.) 160,160,180
 160  IF(X-2000.) 190,190,170
 170  P=1.0
      GO TO 610
 180  A=DLOG(XX/GG)/3.D0
      A=DEXP(A)
      B=2.D0/(9.D0*GG)
      C=(A-1.D0+8)/DSQRT(B)
      SC=SNGL(C)
      CALL NDTR (SC,P,DUMMY)
      GO TO 490
C
C     COMPUTE THETA
C
  190 K= IDINT(G2)
      THETA=G2-DFLOAT(K)
      IF(THETA-1.D-8) 200,200,210
  200 THETA=0.D0
  210 THP1=THETA+1.D0
C
C     SELECT METHHOD OF COMPUTING
C
      IF (THETA) 230,230,220
 220  IF(XX-10.D0) 260,260,320
C
C     COMPUTE T1 FOR THETA EQUAL 0.0
C
  230 IF(X2-1.68D02) 250,240,240
  240 T1=1.0
      GO TO 400
  250 T11=1.D0-DEXP(-X2)
      T1=SNGL(T11)
      GO TO 400
C
C     COMPUTE T1 FOR THETA .GT. 0 AND
C     X .LE. 10.0
C
 260  SER=X2*(1.D0/THP1 -X2/(THP1+1.0D0))
      J=+1
      CC=DFLOAT(J)
      DO 270 IT1=3, 30
      XI=DFLOAT(IT1)
      CALL DLGAM(XI,FAC,IOK)
      TLOG= XI*DLX2-FAC-DLOG(XI+THETA)
      TERM=DEXP(TLOG)
      TERM=DSIGN(TERM,CC)
      SER=SER+TERM
      CC=-CC
      IF (DABS(TERM)-1.D-9) 280,270,270
 270  CONTINUE
      GO TO 600
 280  IF (SER) 600,600,290
 290  CALL DLGAM(THP1,GTH,IOK)
      TLOG= THETA* DLX2+DLOG(SER)- GTH
      IF (TLOG+1.68D02) 300,300,310
 300  T1=0.0
      GO TO 400
 310  T11=DEXP(TLOG)
      T1=SNGL(T11)
      GO TO 400
C
C     COMPUTE T1 FOR THETA GRATER THAN 0.0 AND
C     X GREATER THAN 10.0 AND LESS THAN 2000.0
C
  320 A2=0.D0
      DO 340 I=1,25
      XI=DFLOAT(I)
      CALL DLGAM(THP1,GTH,IOK)
      T11=-(13.D0*XX)/XI +THP1*DLOG(13.D0*XX/XI)-GTH-
     + DLOG(XI)
      IF(T11+1.68D02) 340,340,330
  330 T11=DEXP(T11)
      A2=A2+T11
  340 CONTINUE
      A=1.01282051D0+ THETA/156.D0-XX/312.D0
      B=DABS(A)
      C= -X2+THP1**DLX2+DLOG(B)-GTH-3.951243718581427D0
      IF (C+1.68D02) 370,370,350
  350 IF (A) 360,370,380
  360 C=-DEXP(C)
      GO TO 390
  370 C=0.D0
      GO TO 390
  380 C=DEXP(C)
  390 C=C+A2
      T11=1.D0-C
      T1=SNGL(T11)
C
C     SELECT PROPER EXPRESSION FOR P
C
  400 IF (G-2.) 420,410,410
  410 IF (G-4.) 450,460,460
C
C     COMPUTE P FOR G .GT. 0 AND .LE. 2
C
  420 CALL DLGAM(THP1,GTH,IOK)
      DT2=THETA*DLXX-X2-THP1*.6931471805599453D0 -GTH
      IF(DT2+1.68D02) 430,430,440
  430 P=T1
      GO TO 490
  440 DT2=DEXP(DT2)
      T2=SNGL(DT2)
      P=T1+T2+T2
      GO TO 490
C
C     COMPUTE P FOR G GRATER THAN OR EQUAL TO 2.0
C     AND LESS THAN OR EQUAL TO 4.0
C
  450 P=T1
      GO TO 490
C
C     COMPUTE P FOR G GRATER THAN OR EQUAL TO 4.0
C     AND LESS THAN OR EQUAL TO 1000.0
C
 460  DT3=0.D0
      DO 480 I3=2,K
      THPI=DFLOAT(I3)+THETA
      CALL DLGAM(THPI,GTH,IOK)
      DLT3=THPI*DLX2-DLXX-X2-GTH
      IF(DLT3+1.68D02) 480,480,470
  470 DT3=DT3+DEXP(DLT3)
  480 CONTINUE
      T3=SNGL(DT3)
      P=T1-T3-T3
C
C     SET THE ERROR
C
  490 IF (P) 500,520,520
  500 IF(ABS(P)-1.E-7) 510,510,600
  510 P=0.0
      GO TO 610
  520 IF(1.-P) 530,550,550
  530 IF(ABS(1.-P)-1.E-7) 540,540,600
  540 P=1.0
      GO TO 610
  550 IF(P-1.E-8) 560,560,570
  560 P=0.0
      GO TO 610
  570 IF((1.0-P)-1.E-8) 580,580,610
  580 P=1.0
      GO TO 610
  590 IER=-1
      D=-1.E38
      P=-1.E38
      GO TO 620
  600 IER=+1
      P=1.0E38
      GO TO 620
  610 IER=0
  620 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C t DISTRIBUTION FUNCTION t(0.025,DF=V) WHERE                       C	
C V IS THE INPUT DEGREE OF FREEDOM				    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION TP(V)
      REAL TP,XP
      INTEGER V
      XP=1.96
	VI=V
      G1=.25*(XP**3+XP)
      G2=(1./96.)*(5.*XP**5+16.*XP**3+3.*XP)
      G3=(1./384.)*(3.*XP**7+19.*XP**5+17.*XP**3-15.*XP)
      G4=(1./92160)*(79.*XP**9+776.*XP**7+1482.*XP**5-1920.*XP**3-
     -945.*XP)
      TP=XP+(G1/VI)+(G2/VI**2)+(G3/VI**3)+(G4/VI**4)
      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE NDTR(X,P,D)                                                    
C------------------------------------
C
C     PURPOSE                                                                   
C       COMPUTES Y=P(X)=PROBABILITY THAT THE RANDOM VARIABLE                    
C       DISTRIBUTED NORMALLY IN (0,1), IS LESS THAN OR EQUAL                    
C       TO X.  F(X), THE ORDINATE OF THE NORMAL DENSITY AT X,                   
C       IS ALSO COMPUTED.                                                       
C                                                                               
C     DESCRIPTION OF PARAMETERS                                                 
C       X     - INPUT SCALAR FOR WHICH P(X) IS COMPUTED.                        
C       P     - OUTPUT PROBABILITY.                                             
C       D     - OUTPUT DENSITY.                                                 
C                                                                               
C     REMARKS                                                                   
C       MAXIMUM ERROR IS .0000007.                                              
C                                                                               
C     SUBROUTINE AND SUBPROGRAM REQUIRED                                        
C       NONE                                                                    
C                                                                               
C     METHOD                                                                    
C       BASED ON APPROXIMATION IN C. HASTINGS, APPROXIMATIONS                   
C       FOR DIGITAL COMPUTERS, PRINCETON UNIV. PRESS, PRINCETON,                
C       N.J., 1955.  SEE EQUATION 26.2.17, HANDBOOK OF MATHEMATICAL             
C       FUNCTIONS, ABRAMOWITZ AND STEGUN, DOVER PUBLICATION, INC.,              
C       NEW YORK.                                                               
C                                                                               
      AX=ABS(X)                                                                 
      T=1.0/(1.0+0.2316419*AX)                                                  
      IF (AX.GT.18) GO TO 5                                                     
      D=0.3989423*EXP(-X*X/2.0)                                                 
      GO TO 6                                                                   
5     D=0.0                                                                     
6     P=1.0-D*T*((((1.330274*T-1.821256)*T+1.781478)*T-0.3565638)*T+            
     10.3193815)                                                                
      IF(X)1,2,2                                                                
1     P=1.0-P                                                                   
2     RETURN                                                                    
      END                                       


