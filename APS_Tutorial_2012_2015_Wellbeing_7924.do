/***************************************************************************************************/
/** Standard Syntax for Analysing the ONS Annual Population Survey Subjective Wellbeing Data      **/
/** For use with 3 year 2012-2015 APS wellbeing data UKDA 7924								     **/
/***************************************************************************************************/

// Setup paths - This tells stata where to find data files and save outputs 
global PROJECT "$DROPBOX/Simetrica Data\APS/tutorial_temp" // Dropbox project folder
global OUTPUT  "$PROJECT/Output"                                   // .. Output folder
global INPUT   "$PROJECT/Input"                                    // .. Input folder

/* Open the APS dataset 																			*/
use "$DROPBOX\Simetrica Data\APS\UKDA-7924-stata11\stata11\a12m15_wellbeing_eul.dta", clear

/***************************************************************************************************/
/** Creation of standard variables                                                             **/
/***************************************************************************************************/

* The desribe command will list the variable name, variable label, type of variable etc
describe

* First, lets convert all variable names to lower case. This ensures consistency across the dataset and is helpful later if you need to merge with other waves
*The code below is a loop. More on this later (line 176). For now, just run.
set more off
foreach var of varlist _all {
    	
		local new_var = lower("`var'") 
		display "`new_var'"
		display "`var'"
			
		
	if  "`var'" != "`new_var'" {	
	
	rename `var' `new_var'
	
	}
}

describe // notice any difference in variable names?

*All variables have two levels of naming. The variable name you generated, and a label for the variable. 
*This information allows you to identify the varible and can be displayed in table outputs (=more user friendly tables)	

/***************************************************************************************************/
/*** First example of creation of standard dummy variable 										  **/
/** Subjective wellbeing varies by gender. The code below generate a dummy variable for       **/
/** gender called Female which  = 1 for females and  = 0 for males.                                   **/
/***************************************************************************************************/
codebook sex // Displays information on variable
describe sex	// Displays information on variable
des sex // Note that stata has shorter versions of commands. eg describe = des ; generate = gen ; tabulate = ta

*Longhand creation of dummy variables
gen female=. // Create a new variable where all observations are missing (. = missing)
replace female = 1 if sex==2 // replace the missing observation with 1 if the original variable == 2
replace female = 0 if sex== 1 // replace the missing observation with 0 if the original variable == 1
label variable female "Female" // Give the new variable a label so others can identify it
* Note the use of "=" One"=" before the "if" two"==" after the "if"

tabulate female sex // After creating a new variable you should crosstabulate the old and new variable. Do they match? Are all the 1's where you want them to be? What happens to the missing variables, or those coded 99?
  									  
/***************************************************************************************************/
/** Create 4 new variables for the wellbeing questions                                        **/
/** LS= Life Satisfaction, WW= Worthwhile, HA= Happiness Yesterday and AN= Anxiety Yesterday      **/
/** PAB= Positive Affect Balance HA minus (10 minus AN)                                           **/    
/***************************************************************************************************/
* Here we recode the existing variable excluding observations outside of the 0-10 scale, and generate a new set of SWB variables

gen LS = satis
replace LS =. if (satis ==. | satis<0 | satis >10)
gen WW = worth
replace WW =. if (worth ==.  | worth<0 | worth >10)
gen HA = happy
replace HA =. if (happy ==.  | happy<0 | happy >10)
gen AN = anxious
replace AN =. if (anxious ==.  | anxious<0 | anxious >10)
* Positive Affect Balance (PAB) = happiness / anxiety
gen PAB=.
replace PAB = (HA - AN)/2
replace PAB =. if (HA ==. | AN ==.)

label variable LS "Life Satisfaction"
label variable WW "Worthwhile"
label variable HA "Happiness"
label variable AN "Anxiety"
label variable PAB "Positive Affect Balance"

/***************************************************************************************************/
/** Subjective wellbeing varies by age. The relationship is not linear and the code below     **/
/** generates an Age Squared Variable to use in regression models.                                **/
/***************************************************************************************************/

gen age=.
replace age=dvage if (dvage>0 & dvage!=.) // 

gen agesq = age * age
label variable agesq "Age Squared"
label variable age "Age"

*Alternatviely we could create a log age variable
gen lage = ln(dvage) 
replace lage = . if if dvage < 0 | dvage > . 
label var lage "Log age"

/***************************************************************************************************/
/** Subjective wellbeing varies by marital status.  											  **/
/** We may want to create dummy variables for use in regression models               		      **/
/***************************************************************************************************/

* First, explore the variable
ta marsta // Are there any categories that we want to exclude? In this case no. But look out for -99 etc
ta marsta, nol // Tabulates the underlying coding of the variable. ie 1 ="Single, never married"
ta marsta, missing // Tabulates any missing variables present in the variable

/***************************************************************************************************/
/** An easy way to make dummies from one categorical variable						 	  **/
/** tabulate variable, then generate a new set of variables for each of the categories            **/
/** Note that each of the new variables will be names after the original variable, with a number suffix	  **/
/** However, they will have labels to identify which category they refer to	  **/
/** Be careful to exclude missing: In this case )  marsta <10 & marsta >0   **/
/***************************************************************************************************/

ta marsta, gen (marital) // tabulate variable, then generate a new set of variables based on each category
// Check that these variables have been created by typing "marital" into the Variable search window
tab1 marital* // tab1 allows you to tabulate multiple variables at the same time // The asterix * tabulates all variables with a sifficx after marital 
* The global comand collects all your variables in one place. This allows you to tabulate/include in regression using $global
* In the example below, we collect all marital dummies excluding marital1 (single) into a global $marital. Easiest way to do this is to search marital in Variable finder, select marital2 to marital9, click on the left arrow (pulls them to command line) and copy from the command line 
global marital marital2 marital3 marital4 marital5 marital6 marital7 marital8 marital9 // 

/***************************************************************************************************/
/** An alternative approach is to use a single categorical variable with the i.variable factor function (ie i.marital).  **/
/** This will drop the first categorical as a reference group. 										*/
/** Be careful to exclude missing categories. IT IS ALWAYS SAFER TO CREATE A NEW VARIABLE **/
/***************************************************************************************************/

gen maritals = marsta 
replace maritals = . if marsta > 10 | marsta < 0 // To drop any potential missing observation categories
label var maritals "Marital status"
* You can now use the variable as i.maritals in regressions

/***************************************************************************************************/
/** Combine multiple categories into one Dummy               		      						  **/
/** We may want to combine categories togethereg single; married/couple; divorced/seperated; widowed/surviving		**/
/** This may be necessary for regressions on small sample sizes (you may also want to use a simple single dummy for small sample regressions) */
/** This requires some manual coding. Be sure to reguarly crosstab your new dummies against the original variable **/
/***************************************************************************************************/
* refer back to raw variable
ta marsta
ta marsta, nol

gen m_single = . // It will be easier to find the variables together if they have a shared prefic eg m_
replace m_single = 1 if marsta == 1 
replace m_single = 0 if marsta != 1 & (marsta > 0 & marsta < 10) // Define your dummy  0 reference group. All categories not equal to 1 (using exclamation for not equals (! = 1), AND (&) excluding potential missings above 9 and below 0
label variable m_single "single" // label the new variable
ta marsta m_single // Cross tab to check you got it right

gen m_married = .
replace m_married = 1 if marsta == 2 | marsta == 6 // Vertical line (|) denotes OR. Dummy = 1 if married OR (|) Civil Partner
replace m_married = 0 if marsta != 2 & marsta != 6 & (marsta > 0 & marsta < 10) // Note that the defining the dummy 0, you will usually use AND(&) to clearly exclude 
label variable m_married "Married or Civil Partner" // Clearly label new combined dummy
ta marsta m_married // Cross tab to check you got it right

gen m_seperated = .
replace m_seperated = 1 if marsta == 3 | marsta == 4 | marsta == 7 | marsta == 8 // all divorced or seperated categories
replace m_seperated = 0 if marsta != 3 & marsta != 4 & marsta != 7 & marsta != 8 & (marsta > 0 & marsta < 10) // Note use of & and != to exclude from dummy 0
label variable m_seperated "seperated"
* Cross tab to check you got it right!! (this was a toughy)
ta marsta m_seperated

gen m_widowed = .
replace m_widowed = 1 if marsta == 5 | marsta == 9
replace m_widowed = 0 if marsta != 5 & marsta != 9 & (marsta > 0 & marsta < 10)
label variable m_widowed "widowed"
* Cross tab to check you got it right
/* Global maritals Reference single */
global maritals m_married m_seperated m_widowed
*Now lets check you have coded up all the categories
ta marsta m_single
ta marsta m_married 
ta marsta m_seperated 
ta marsta m_widowed
* All present and accounted for?

/***************************************************************************************************/
/** 		Loops!!! Yay! 																		 ***/
/***************************************************************************************************/
*There is a smarter way to run multiple commands at the same time using the "foreach" command */

foreach var of global maritals { // Tells stata, for each variable in the global list maritals... THis is followed by an open brackt { The loop happens within the brackets
ta marsta `var' // crosstab marsta with each of the variables in the global (`var', this time in inverted commas to denote its role in the loop 
} // Always close the loop when you are done, or stata will try to run it until the end of the do file
* This will produce three crosstabs (note that m_single is the dropped reference variables in the global so will not be tabulated - You can redefine the global if you like, but define it back before running regressions

/***************************************************************************************************/
/** Subjective wellbeing varies by ethnicity. The code below generates a binary ethnicity     **/
/***************************************************************************************************/
* Tabulate the variable ethuk11. What do you notice?
* What to do with the "No Answer" category? The code below excludes it. Think of other ways you could write the code to exclude ethuk11 == -8?
gen BMEA=.
replace BMEA = 1 if ethuk11 > 1 & ethuk11 <12
replace BMEA= 0 if ethuk11 == 1
label variable BMEA "BME"

* If you wanted to include all categories in a regression/tabulation, what would you do? Remember to exclude ethuk11 == -8

/***************************************************************************************************/
/** Subjective wellbeing varies by health. The code below generates a categorical health      **/
/** variable (General health). Note the need to invert (recode) the scale to make 5 good                   **/
/***************************************************************************************************/

recode qhealth1 (1=5 VeryGood) (2=4 Good) (3=3) (4=2 Bad) (5 = 1 Very_Bad), gen (ghealth)
replace ghealth=. if qhealth1 <1 | qhealth1 > 5 // Recode as missing to exclude possible missing observations
label variable qhealth1 "General Health"

/***************************************************************************************************/
/** Subjective wellbeing varies by health. The code below generates a binary health variable  **/
/** variable(Poor Health) corresponding to if people feel their poor health limits their          **/
/** activities. This is a more objective measure of health that pure self reporting health.       **/
/***************************************************************************************************/

gen limitinghealth=.
replace limitinghealth = 1 if limita == 1
replace limitinghealth = 0 if limita ==2 
label variable limitinghealth "LimitingHealth"

/** There is an alternative way to code dummy variables, which uses fewer lines of code  			**/
/** Note you have to be VERY CAREFUL to exclude all missing 										**/
gen limitinghealth2 = limita == 1 
replace limitinghealth2 = . if limita <1 | limita > 2 // replace all missing (below 1) with .

/***************************************************************************************************/
/** Smokers tend to have lower subjective wellbeing. Smoking Variable created below.          **/
/***************************************************************************************************/

gen smoker =.
replace smoker = 1 if cignow == 1
replace smoker = 0 if cignow == 2
label variable smoker "Smoker (currently)"

/***************************************************************************************************/
/** We will continue with data cleaning shortly, but now that we have a number of variables coded we can look at...	    **/
/** Desciptive statistics! Your Stata bread and butter **/
/***************************************************************************************************/

/***************************************************************************************************/
/* 						Tabulate ta																*/
*You are already familiar with tabulating and cross-tabulating to double check your created variables are coded correctly
* Lets tabulate life satisfaction
ta LS
* If we are interested in the distribution, we may want a graph

/***************************************************************************************************/
/* 						Histogram hist																*/
hist LS
* Is life sat normally distributed? Any skew?

* How does life satisfaction vary with gender? We could crosstabulate:
ta LS female
*but this doesnt tell us very much. Averages would be much better:

/***************************************************************************************************/
/* 						Summarise su 																*/
su LS // gives mean life sat across whole sample. Also standard deviation, min and max
su LS if female == 1 // if function gives mean life sat for females only
su LS if female == 0 // if function gives mean life sat for males only
*is this difference significant? Lets do a ttest to find out

/***************************************************************************************************/
/* 						ttest 																*/
ttest LS, by (female) // This gives the mean life sat for each gender. The middle p value (diff != 0 ) gives the significance of the association
* In this case we see that life sat is significantly different between males and females. Is it higher or lower for females?

* What about other descriptive statistics? Median, deciles, confidence intervals

/***************************************************************************************************/
/* 						Confidence intervals ci 													*/
ci LS // Gives mean, standard error, and 95% confidence intervals

/***************************************************************************************************/
/* 						Detailed summary (su var, detail) percentiles, variance, skew etc				*/
su LS, detail

/***************************************************************************************************/
/* 						More stats are available in tabstat eg median (p50) (see stata hel)	     	*/
tabstat LS, stats (mean p50)

* These are the main descriptive stats commands. You can use if functions with all of them. 
*Take some time to explore the data you have coded so far
*Now back on with the data cleaning...

/***************************************************************************************************/
/** Data cleaning... continued						 											 **/

/***************************************************************************************************/
/** Subjective wellbeing varies by education. The code below generates an education variable  **/
/** variable (Education) and 6 corresponding dummy variables. This is highest education acheived. **/
/***************************************************************************************************/

* One option is to usethe factor variable i.var. Remember to create a new variable to exclude all negative, missing observation etc
gen education = hiqul11d 
replace education = . if hiqul11d <1 | hiqul11d > 7

* To manually code the dummies 
gen ed_degree=.
replace ed_degree = 1 if hiqul11d == 1
replace ed_degree = 0 if hiqul11d != 1 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_degree "Degree"

gen ed_higherdegree=.
replace ed_higherdegree = 1 if hiqul11d == 2
replace ed_higherdegree = 0 if hiqul11d != 2 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_higherdegree "Higher Degree"

gen ed_alevel=.
replace ed_alevel = 1 if hiqul11d == 3
replace ed_alevel = 0 if hiqul11d != 3 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_alevel "Alevel"

gen ed_gcse=.
replace ed_gcse = 1 if hiqul11d == 4
replace ed_gcse = 0 if hiqul11d != 4 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_gcse "GCSE"

gen ed_otherqual=.
replace ed_otherqual = 1 if hiqul11d == 5
replace ed_otherqual = 0 if hiqul11d != 5 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_otherqual "Other qualification"

gen ed_noqual=.
replace ed_noqual = 1 if hiqul11d == 6
replace ed_noqual = 0 if hiqul11d != 6 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_noqual "No qualifications"

gen ed_unknown=.
replace ed_unknown = 1 if hiqul11d == 7
replace ed_unknown = 0 if hiqul11d != 7 & (hiqul11d > 0 & hiqul11d < 8)
label variable ed_unknown "Qualifications unknown"

/* Global education: Reference=degree */
global education ed_higherdegree ed_alevel ed_gcse ed_otherqual ed_noqual ed_unknown

* Most commonly, we simply create a single dummy for higher education (degree or higher ed)
gen degreeorhigher=.
replace degreeorhigher = 1 if hiqul11d == 1 | hiqul11d == 2
replace degreeorhigher = 0 if hiqul11d > 2 & hiqul11d < 8

/***************************************************************************************************/
/**  Subjective wellbeing varies by religion. The code below generates a binary religion      **/
/** variable (Religionb) and 8 corresponding dummy variables of respondent's specified religion.  **/
/***************************************************************************************************/
gen religious = relig11 > 1 & relig11 < .
replace religious = . if relig11 < 1
label variable religious "Religious"

/***************************************************************************************************/
/**  Housing status is a storng indicator of socioeconomic status      **/
/**  Note that we have to combine two variables (llord and ten1) to create the housing status variable in the APS  **/
/***************************************************************************************************/

/* Llord: Just for coding purposes. Not for running in regression */
recode llord (1/2 = 1 Social) (3/7=2 Private) (8/99=.) (-99/0=.), gen(landlord)
replace landlord =. if llord ==.
label variable landlord "Landlord"

/* Housing status */
gen home_owner = ten1 == 1 
replace home_owner =. if ten1==. | ten1 < 0
label variable home_owner "Homeowner Paid Mortgage"

gen home_mortgage= ten1 ==2 | ten1 ==3
replace home_mortgage=. if ten1 ==. | ten1 < 0
label variable home_mortgage "Mortgagee"

gen home_privaterent= (ten1 == 4 & landlord == 2) | (ten1 == 5 & landlord == 2) // compound
replace home_privaterent=. if ten1==. | ten1< 0
label variable home_privaterent "Private renter"

gen home_socialrent= (ten1 == 4 & landlord == 1) | (ten1 == 5 & landlord == 1)
replace home_socialrent=. if ten1 ==. | ten1 < 0
label variable home_socialrent "Social renter"

gen home_norentsquat = ten1 == 5
replace home_norentsquat =. if ten1 == . | ten1 < 0
label variable home_norentsquat "Squat/ no rent"

*reference group = homeowner and home mortgage
global housing home_privaterent home_socialrent home_norentsquat

*Check coded correctly (check out the double loop)
global housing2 home_owner home_mortgage home_privaterent home_socialrent home_norentsquat
global original ten1 landlord
foreach var of global housing2 { 
	foreach grp of global original { 
ta `grp' `var' 
	} 
}
/***************************************************************************************************/
/**  Subjective wellbeing varies by household income, but we do not have such a variable in   **/
/** the APS. An indicator of lower income is worth including. The code below is a broad benefits  **/
/** indicator, based on whether the respondent is claiming Other State Benefits                   **/
/***************************************************************************************************/

gen benefits = benfts == 1
replace benefits = . if benefits <1 | benefits > 2
label var benefits "Claiming benefits currently"

/***************************************************************************************************/
/** 1.15 Subjective wellbeing varies by employment and Income. The syntax below generates a       **/
/** compound income quintile and economic activity variable(DVemp4) with dummies.                 **/
/***************************************************************************************************/

gen grosspay=.
replace grosspay= grsswk if grsswk>=0 & (ilodefr ==1 & ftptw ==6 & inecac05 !=2) 
xtile grsswk_quin = grosspay, nq(5)
gen FTpaygrp=.
replace FTpaygrp=1 if grsswk_quin==1
replace FTpaygrp=2 if grsswk_quin==2
replace FTpaygrp=3 if grsswk_quin==3
replace FTpaygrp=4 if grsswk_quin==4
replace FTpaygrp=5 if grsswk_quin==5

gen DVemp4=.
replace DVemp4=1 if ilodefr ==3 & (inecac05 >=6 & inecac05 <=11)
replace DVemp4=2 if ilodefr ==3 & ((inecac05 ==12) |(inecac05 >=14 & inecac05 <=19) | (inecac05 >=21 & inecac05 <=22)) 
replace DVemp4=3 if ilodefr ==3 & ((inecac05 ==23) |(inecac05 >=25 & inecac05 <=30) | (inecac05 >=32 & inecac05 <=33))
replace DVemp4=4 if ilodefr ==3 & (inecac05 ==20 | inecac05 ==31)
replace DVemp4=5 if ilodefr ==2
replace DVemp4=6 if ilodefr ==3 & (inecac05 ==13 | inecac05 ==24)
replace DVemp4=7 if ilodefr ==1 & inecac05 == 4
replace DVemp4=8 if ilodefr ==1 & ftptw ==3
replace DVemp4=9 if ilodefr ==1 & (ftptw==1 | ftptw ==2 | ftptw ==4 | ftptw ==5)
replace DVemp4=10 if ilodefr ==1 & ftptw ==6 & inecac05 == 2
replace DVemp4=11 if ilodefr ==1 & ftptw ==6 & FTpaygrp==1
replace DVemp4=12 if ilodefr ==1 & ftptw ==6 & FTpaygrp==2
replace DVemp4=13 if ilodefr ==1 & ftptw ==6 & FTpaygrp==3
replace DVemp4=14 if ilodefr ==1 & ftptw ==6 & FTpaygrp==4
replace DVemp4=15 if ilodefr ==1 & ftptw ==6 & FTpaygrp==5
quietly tabulate DVemp4, generate(DVemp4_)
label variable DVemp4_1 "Inactive - seeking"
label variable DVemp4_2 "Inactive - not seeking but wants work"
label variable DVemp4_3 "Inactive - not seeking not want work"
label variable DVemp4_4 "Inactive - retired"
label variable DVemp4_5 "Unemployed"
label variable DVemp4_6 "Student"
label variable DVemp4_7 "Unpaid Family Worker"
label variable DVemp4_8 "Underemployed"
label variable DVemp4_9 "Part-Time"
label variable DVemp4_10 "Full Time Self-Employed"
label variable DVemp4_11 "Full Time lowest pay quintile"
label variable DVemp4_12 "Full Time 2nd pay quintile"
label variable DVemp4_13 "Full Time 3rd pay quintile"
label variable DVemp4_14 "Full Time 4th pay quintile"
label variable DVemp4_15 "Full Time highest pay quintile"
recode DVemp4 (1=1 InactiveSeekUnavail) (2=2 InactiveNotSeekWant) (3=3 InactiveNotSeekNotWant) (4=4 Retired) (5=5 Unemployed) (6=6 Student)(7=7 UnpaidFamilyWorker)(8=8 Underemployed) (9=9 Part-Time) (10=10 FullTimeSelf) (11=11 FullTimeQ1) (12=12 FullTimeQ2) (13=13 FullTimeQ3) (14=14  FullTimeQ4) (15=15 FullTimeQ5), gen(EmployStatus)
replace EmployStatus =. if DVemp4 ==.

gen DV_Inactive_seeking= DVemp4_1 
gen DV_Inactive_wants= DVemp4_2 
gen DV_Inactive_notseeking = DVemp4_3 
gen DV_retired= DVemp4_4 
gen DV_unemployed = DVemp4_5 
gen DV_student =  DVemp4_6 
gen DV_unpaidfam =  DVemp4_7 
gen DV_underemployed =  DVemp4_8 
gen DV_ptemployed =  DVemp4_9 
gen DV_selfemployed =  DVemp4_10 
gen DV_ftquin1 =  DVemp4_11 
gen DV_ftquin2 =  DVemp4_12 
gen DV_ftquin3 =  DVemp4_13 
gen DV_ftquin4 =  DVemp4_14 
gen DV_ftquin5 =  DVemp4_15 
/* Ref unemployed & underemployed */
global empincome DV_Inactive_seeking DV_Inactive_wants DV_Inactive_notseeking DV_retired  DV_student DV_unpaidfam  DV_ptemployed DV_selfemployed DV_ftquin1 DV_ftquin2 DV_ftquin3 DV_ftquin4 DV_ftquin5

* Dummy variable for employed
gen employed = econ_ftime ==1 | econ_pttime==1 | econ_selfemployed==1
replace employed=. if econ_ftime==. & econ_pttime==. & econ_selfemployed==.

/* Pay quintile for full time workers only (note, this will drop sample size by exlcuding non-FT workers */

/* First, full time pay quintiles */

xtile grssftwk_quin = grosspay, nq(5)

gen ftpay_quint1= grssftwk_quin==1
replace ftpay_quint1=. if  grssftwk_quin==.
label variable ftpay_quint1 "1st income quintile_FT pay"
gen ftpay_quint2= grssftwk_quin==2
replace ftpay_quint2=. if  grssftwk_quin==.
label variable ftpay_quint2 "2nd income quintile_FT pay"
gen ftpay_quint3= grssftwk_quin==3
replace ftpay_quint3=. if  grssftwk_quin==.
label variable ftpay_quint3 "3rd income quintile_FT pay"
gen ftpay_quint4= grssftwk_quin==4
replace ftpay_quint4=. if  grssftwk_quin==.
label variable ftpay_quint4 "4th income quintile_FT pay"
gen ftpay_quint5= grssftwk_quin==5
replace ftpay_quint5=. if  grssftwk_quin==.
label variable ftpay_quint5 "5th income quintile_FT pay"

global Quintile ftpay_quint2 ftpay_quint3 ftpay_quint4 ftpay_quint5

/* Second, full time pay deciles */
xtile grssftwk_dec = grosspay, nq(10)

gen ftpay_dec1= grssftwk_dec==1
replace ftpay_dec1=. if  grssftwk_dec==.
label variable ftpay_dec1 "1st income decile pay"

gen ftpay_dec2= grssftwk_dec==2
replace ftpay_dec2=. if  grssftwk_dec==.
label variable ftpay_dec2 "2nd income decile pay"

gen ftpay_dec3= grssftwk_dec==3
replace ftpay_dec3=. if  grssftwk_dec==.
label variable ftpay_dec3 "3rd income decile pay"

gen ftpay_dec4= grssftwk_dec==4
replace ftpay_dec4=. if  grssftwk_dec==.
label variable ftpay_dec4 "4th income decile pay"
gen ftpay_dec5= grssftwk_dec==5
replace ftpay_dec5=. if  grssftwk_dec==.
label variable ftpay_dec5 "5th income decile pay"
gen ftpay_dec6= grssftwk_dec==6
replace ftpay_dec6=. if  grssftwk_dec==.
label variable ftpay_dec6 "6th income decile pay"
gen ftpay_dec7= grssftwk_dec==7
replace ftpay_dec7=. if  grssftwk_dec==.
label variable ftpay_dec7 "7th income decile pay"
gen ftpay_dec8= grssftwk_dec==8
replace ftpay_dec8=. if  grssftwk_dec==.
label variable ftpay_dec8 "8th income decile pay"
gen ftpay_dec9= grssftwk_dec==9
replace ftpay_dec9=. if  grssftwk_dec==.
label variable ftpay_dec9 "9th income decile pay"
gen ftpay_dec10= grssftwk_dec==10
replace ftpay_dec10=. if  grssftwk_dec==.
label variable ftpay_dec10 "10th income decile pay"

global Decile ftpay_dec2 ftpay_dec3 ftpay_dec4 ftpay_dec5 ftpay_dec6 ftpay_dec7 ftpay_dec8 ftpay_dec9 ftpay_dec10

gen lgrosspay=log((grosspay+1))
replace lgrosspay=. if grosspay==.

/***************************************************************************************************/
/**  Wellbeing varies by place. The code below generates dummy variables for each region      **/
/** This dataset does not have a regional vairable - use the code below on other APS datasets     **/
/***************************************************************************************************/
/* No variables code as missing. But be careful, gora is string variable. Note the use of inverted commas for strong categories*/

gen region_NE = gor9d== "E12000001"
label var region_NE "North-east"

gen region_NW = gor9d== "E12000002"
label var region_NW "North-west"

gen region_YH = gor9d== "E12000003"
label var region_YH "Yorkshire & Humber"

gen region_EM = gor9d== "E12000004"
label var region_EM "East Midlands"

gen region_WM = gor9d== "E12000005"
label var region_WM "West Midlands"

gen region_EE = gor9d== "E12000006"
label var region_EE "East of England"

gen region_LN = gor9d== "E12000007"
label var region_LN "London"

gen region_SE = gor9d== "E12000008"
label var region_SE "South East"

gen region_SW = gor9d== "E12000009"
label var region_SW "South West"

gen region_NI = gor9d == "N99999999"
label var region_NI "N. Ireland"

gen region_SL = gor9d == "S99999999"
label var region_SL "Scotland"

gen region_WA = gor9d == "W99999999"
label var region_WA "Wales"

/* Global region Reference = London */
global region region_NE region_NW region_YH region_EM region_WM region_EE region_SE region_SW region_NI region_SL 

/***************************************************************************************************/
/**  There are survey mode effects with subjective wellbeing data. Respondents answer         **/
/** differently depending on whether they are asked online, via telephone or face to face         **/
/** The code below creates a binary variable which  = 1 for Face to Face and  = 0 for Telephone       **/
/***************************************************************************************************/	

gen facetoface=.
replace facetoface = 1 if intrtype==2
replace facetoface = 0 if intrtype== 1


/***************************************************************************************************/
/**  There are seasonal effects with subjective wellbeing data and it is good to control for  **/
/** month of interview (i.refwkm) - Note, I checked there are no missing observations etc before using the raw variable in the factor i.var      **/
/** Also have day of week (i.refwkd)    **/
/***************************************************************************************************/


/***************************************************************************************************/
/**  For modelling work it is helpful to clust variables together into themes (global lists).  **/
/** For ease, I have collected all of the global commands from the clean up code above  **/
/***************************************************************************************************/										
 
/* Global maritals Reference married */
global maritals m_married m_seperated m_widowed
global education ed_higherdegree ed_alevel ed_gcse ed_otherqual ed_noqual ed_unknown
global empincome DV_Inactive_seeking DV_Inactive_wants DV_Inactive_notseeking DV_retired  DV_student DV_unpaidfam  DV_ptemployed DV_selfemployed DV_ftquin1 DV_ftquin2 DV_ftquin3 DV_ftquin4 DV_ftquin5
global housing home_privaterent home_socialrent home_norentsquat
global region region_NE region_NW region_YH region_EM region_WM region_EE region_SE region_SW region_NI region_SL 
global survey facetoface i.refwkm i.refwkd

/***************************************************************************************************/
/**  Regression analysis																		  **/
/**  One way to check that all variables are coded without error is to include them in a regression and check the sample size - Errors will lead to loss of model sample size		  **/

/* Standard reg variables. Including weight																		 */
reg LS female age agesq BME religious ghealth smoker  $maritals $education $empincome $housing $region $survey [pweight = np122r14], r
* Interpretation questions
*Do females have higher life satisfaction than males?
* What factors are associated with lower life satisfaction?
*Whats going on with education?
*How do you interpret the housing variable? What is the reference group in this case?

* Summary stats for all variables in regression model
su LS female age agesq BME religious ghealth smoker  $maritals $education $empincome $housing $region $survey [aweight = np122r14]

* Regression with factor variables
reg LS female age agesq i.maritals i.education $empincome $housing $region $survey [pweight = np122r14], r

* Regression with if function: Scotland only
reg LS female age agesq BME religious ghealth smoker  $maritals degreeorhigher $empincome $housing $region $survey if region_SL==1 [pweight = np122r14], r

/***************************************************************************************************/
/**  pweight and aweight																		  **/
/** See APS guidance on weighting. Regressions should be weighted using pweight = np122r14; Descriptive stats with aweight = np122r14   **/

* Regression loop: 5 SWB and general health dependent variables
* Remember the loop function (line 176)
global SWB LS HA AN WW PAB ghealth
foreach var of global SWB {
reg `var' female age agesq BME religious ghealth smoker $maritals degreeorhigher employed $housing $region $survey [pweight = np122r14], r
}

/***************************************************************************************************/
/**  Export stata output to csv																		  **/
* Finally you may want to actually use your output tables in reports, presentations etc
* The last thing you want to do is manually copy and paste (if you do this be sure to select the whole, table, then right click > Copy Table)
* Stata has a number of options for outputting tables in neater (and retraceable) ways

/**  estso (to store) and esttab (to export stata output to csv)															  **/

eststo clear // clears any previousl stored results
eststo: reg `var' female age agesq BME religious ghealth smoker $maritals degreeorhigher employed $housing $region $survey [pweight = np122r14], r // stores the regression output
esttab using "$OUTPUT/LS_reg.csv", /// name and location of csv output
	wide b(%10.3f) star(* 0.1 ** 0.05 *** 0.01) /// report coefficient (b)and pvalue (asterix for levels of significance (standard for SWB analysis is minimum 90% confidence, given that around 30-40% of SWB is thought to be driven by genetic factors); you can also output standard error with the se command
	not replace plain // not=no t statistic; replace = write over any previously saved csv of same name; plain = produce a minimally formatted table. For more table formatting commands see: http://repec.org/bocode/e/estout/hlp_esttab.html
eststo clear
* Note, if you still have the csv file open and try to run the esttab command, stata will report an error. So close your csv files 

* You can now find the output csv in the Output folder

* You can also esttab summary outputs, tables etc
eststo clear
eststo: estpost sum LS [aweight = np122r14]
	esttab ///
		using "$OUTPUT/LS_sum.csv", /// output loication (change name, or it will overwrite you regression csv)
		cells("count mean") label wide plain /// stats to report (observations and mean)
		replace 
eststo clear

* Esttab wih loop through each of the SWB variables (regression)
eststo clear
foreach var of global SWB { 
eststo: reg `var' female age agesq BME religious ghealth smoker $maritals degreeorhigher employed $housing $region $survey [pweight = np122r14], r // stores the regression output
} // close loop. When you esttab the 6 regression models will be output side by side
esttab using "$OUTPUT/SWB_reg.csv", ///
	wide b(%10.3f) star(* 0.1 ** 0.05 *** 0.01) ///
	not mtitles replace plain
eststo clear

* Esttab LS reg wih loop through each of the regional variables (regression)
eststo clear
global region2 region_NE region_NW region_YH region_EM region_WM region_EE region_LN region_SE region_SW region_SL region_WA
foreach var of global region2 {
eststo: reg LS female age agesq BME religious ghealth smoker $maritals degreeorhigher employed $housing $survey if `var'==1 [pweight = np122r14], r // stores the regression output
} // close loop. When you esttab the 6 regression models will be output side by side
esttab using "$OUTPUT/SWB_region.csv", ///
	wide b(%10.3f) star(* 0.1 ** 0.05 *** 0.01) ///
	not mtitles replace plain
eststo clear

* Esttab wih loop (summarise)
eststo clear
foreach var of global SWB {
eststo: estpost sum LS [aweight = np122r14]
} // Close loop Summary stats for each var in global are reported in sequence as est1 est2 est3 etc
	esttab using "$OUTPUT/SWB_sum.csv", /// output loication (change name, or it will overwrite you previous summary csv)
		cells("count mean") label wide plain  ///
		replace 
eststo clear

* esttab can also be used with ci, tabulate and other commands

*In later tutorials we will look at putexcel and matrix table outputs


