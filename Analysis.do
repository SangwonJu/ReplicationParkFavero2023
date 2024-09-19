*****************************************************************************
*****************************************************************************
//Project: Community Racial Bias
//Last updated date: 10.21.2022 
*****************************************************************************
*****************************************************************************
cd "\\tsclient\D\Database\CommunityRacialBias"
clear

ssc install estout, replace
ssc install reghdfe, replace

// Gifted Programs - Explicit Bias (Table 1)
use Gifted_Program

egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg gift_black_p wexpwhite_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18643
drop if cooksd > 0.00021456

egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

tabstat gift_black_p wexp_white wexpwhite_ct wiat_white tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, s(n mean sd min max) format(%9.3fc) c(v)

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}


// Explicit - Teacher
clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg gift_black_p wexpwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/7431
drop if cooksd > 0.00053829

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

tabstat gift_black_p wexp_white wexpwhite_ct wiat_white tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south tblack thispanic, s(n mean sd min max) format(%9.3fc) c(v)

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wexpwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

label variable wexpwhite_ct "Explicit Bias"
label variable tblack_cc "Black teachers (%)"
label variable tot_black "Black students (%)"
label variable thispanic "Hispanic teachers (%)"
label variable tot_latino "Hispanic students (%)"
label variable gift_total "Gifted students (%)"
label variable frp "Free or reduced lunch (%)"
label variable tot_enr "Total students"
label variable highschool_ccd "High schools"
label variable urban "Urban schools"
label variable south "South"
esttab using Gift_explicit.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model1.0 Model1.1 Model1.2 Model1.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The explicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 

esttab using Gift_explicit_se.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) se(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model1.0 Model1.1 Model1.2 Model1.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. Two-way clustered robust standard errors are reported in parentheses. The explicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear

summ
margins, at(tblack_cc==(-10.86005(5)89.13995) wexpwhite_ct==(-0.0538682 0.0538682)) vsquish 
marginsplot, x(tblack_cc) xlabel(-10.86005 "0"  -0.86005  "10"  9.13995 "20" 19.13995 "30" 29.13995 "40" 39.13995 "50" 49.13995 "60" 59.13995 "70" 69.13995 "80" 79.13995 "90" 89.13995 "100") /// 
graphregion(color(white)) bgcolor(white) title("") ytitle("Black gifted students(%)") xtitle("Black teachers(%)") ylab(,nogrid) /// 
addplot(histogram tblack_cc, width(5) yaxis(2) yscale(alt axis(2) range(0 -0.5)) xlabel(-10.86005 "0"  -0.86005  "10"  9.13995 "20" 19.13995 "30" 29.13995 "40" 39.13995 "50" 49.13995 "60" 59.13995 "70" 69.13995 "80" 79.13995 "90" 89.13995 "100") ///
bcolor(*.6) fcolor(*.6) ytitle(" ", axis(2)) ylab(0 " ", axis(2))) ///
plot1opts(msymbol(O)) plot2opts(msymbol(T)) plot(, label( "Low Explicit Bias(-1SD)" "High Explicit Bias(+1SD)" "Density(Black teachers)"))

graph export figure1a.tif, width(2000)

// Semiparametric regression (Figure A3.1)
ssc install semipar, replace
semipar gift_black_p wexpwhite_ct tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, nonpar(tblack) robust cluster(leaid) ci


// Gifted Programs - Implicit Bias (Table 2)
clear
use Gifted_Program

egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg gift_black_p wiatwhite_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18643
drop if cooksd > 0.00021456

egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}


// Implicit - Teacher
clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg gift_black_p wiatwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/7431
drop if cooksd > 0.00053829

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wiatwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

label variable wiatwhite_ct "Implicit Bias"
label variable tblack_cc "Black teachers (%)"
label variable tot_black "Black students (%)"
label variable thispanic "Hispanic teachers (%)"
label variable tot_latino "Hispanic students (%)"
label variable gift_total "Gifted students (%)"
label variable frp "Free or reduced lunch (%)"
label variable tot_enr "Total students"
label variable highschool_ccd "High schools"
label variable urban "Urban schools"
label variable south "South"
esttab using Gift_implicit.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model2.0 Model2.1 Model2.2 Model2.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 

esttab using Gift_implicit_se.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) se(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model2.0 Model2.1 Model2.2 Model2.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. Two-way clustered robust standard errors are reported in parentheses. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Explicit (Table 3)
clear
use Suspension

egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg sus_black_p wexpwhite_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18895
drop if cooksd > 0.0002117

egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

tabstat sus_black_p wexp_white wiat_white tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, s(n mean sd min max) format(%9.3fc) c(v)

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}


// Explicit - Teacher
clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg sus_black_p wexpwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6677
drop if cooksd > 0.00059907

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wexpwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

label variable wexpwhite_ct "Explicit Bias"
label variable tblack_cc "Black teachers (%)"
label variable tot_black "Black students (%)"
label variable thispanic "Hispanic teachers (%)"
label variable tot_latino "Hispanic students (%)"
label variable sus_total "Suspended students (%)"
label variable frp "Free or reduced lunch (%)"
label variable tot_enr "Total students"
label variable highschool_ccd "High schools"
label variable urban "Urban schools"
label variable south "South"
esttab using Sus_explicit.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model3.0 Model3.1 Model3.2 Model3.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 

esttab using Sus_explicit_se.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) se(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model3.0 Model3.1 Model3.2 Model3.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. Two-way clustered robust standard errors are reported in parentheses. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear

summ
margins, at(tblack_cc==(-14.18238(5)85.81762) wexpwhite_ct==(-0.0557089 0.0557089)) vsquish 
marginsplot, x(tblack_cc) xlabel(-14.18238 "0"  -4.18238  "10"  5.81762 "20" 15.81762 "30" 25.81762 "40" 35.81762 "50" 45.81762 "60" 55.81762 "70" 65.81762 "80" 75.81762 "90" 85.81762 "100") /// 
graphregion(color(white)) bgcolor(white) title("") ytitle("Black suspended students(%)") xtitle("Black teachers(%)") ylab(,nogrid) /// 
addplot(histogram tblack_cc, width(5) yaxis(2) yscale(alt axis(2) range(0 0.5)) xlabel(-14.18238 "0"  -4.18238  "10"  5.81762 "20" 15.81762 "30" 25.81762 "40" 35.81762 "50" 45.81762 "60" 55.81762 "70" 65.81762 "80" 75.81762 "90" 85.81762 "100") ///
bcolor(*.6) fcolor(*.6) ytitle(" ", axis(2)) ylab(0 " ", axis(2))) ///
plot1opts(msymbol(O)) plot2opts(msymbol(T)) plot(, label( "Low Explicit Bias(-1SD)" "High Explicit Bias(+1SD)" "Density(Black teachers)"))

graph export figure1b.tif, width(2000)

// Semiparametric regression (Figure A3.2)
semipar sus_black_p wexpwhite_ct tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, nonpar(tblack) robust cluster(leaid) ci


// Suspension - Implicit (Table 4)
clear
use Suspension

egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg sus_black_p wiatwhite_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18895
drop if cooksd > 0.0002117

egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}


// Implicit - Teacher
clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg sus_black_p wiatwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6677
drop if cooksd > 0.00059907

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wiatwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

label variable wiatwhite_ct "Implicit Bias"
label variable tblack_cc "Black teachers (%)"
label variable tot_black "Black students (%)"
label variable thispanic "Hispanic teachers (%)"
label variable tot_latino "Hispanic students (%)"
label variable sus_total "Suspended students (%)"
label variable frp "Free or reduced lunch (%)"
label variable tot_enr "Total students"
label variable highschool_ccd "High schools"
label variable urban "Urban schools"
label variable south "South"
esttab using Sus_implicit.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model4.0 Model4.1 Model4.2 Model4.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 

esttab using Sus_implicit_se.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) se(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model4.0 Model4.1 Model4.2 Model4.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Gifted Programs - Explicit w/ outliers (Talbe A3.1)
clear
use Gifted_Program

egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m

foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wexpwhite_c##c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Gift_explicit_raw.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model1.1 Model1.2 Model1.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear

// Gifted Programs - Implicit w/ outliers (Talbe A3.2)
clear
use Gifted_Program

egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m

foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wiatwhite_c##c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Gift_implicit_raw.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model2.1 Model2.2 Model2.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Explicit w/ outliers (Talbe A3.3)
clear
use Suspension

egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m

foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wexpwhite_c##c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Sus_explicit_raw.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model3.1 Model3.2 Model3.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear

// Suspension - Implicit w/ outliers (Talbe A3.4)
clear
use Suspension

egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m

foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wiatwhite_c##c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Sus_implicit_raw.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model4.1 Model4.2 Model4.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear



// Gifted Programs - Explicit Bias w/ state fixed effects (Table A4.1)
clear
use Gifted_Program

egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg gift_black_p wexpwhite_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18643
drop if cooksd > 0.00021456

egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

gen byte constant = 1 
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}


clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg gift_black_p wexpwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/7431
drop if cooksd > 0.00053829

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wexpwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

esttab using Gift_explicit_sfixed.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model1.1 Model1.2 Model1.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The explicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Gifted Programs - Implicit Bias w/ state fixed effects (Table A4.2)
clear
use Gifted_Program

egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg gift_black_p wiatwhite_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18643
drop if cooksd > 0.00021456

egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1 
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}


clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg gift_black_p wiatwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/7431
drop if cooksd > 0.00053829

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wiatwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

esttab using Gift_implicit_sfixed.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model2.1 Model2.2 Model2.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The explicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Explicit w/ state fixed effects (Table A4.3)
clear
use Suspension

egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg sus_black_p wexpwhite_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18895
drop if cooksd > 0.0002117

egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

gen byte constant = 1 
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}


clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpwhite_m = mean(wexp_white)
gen wexpwhite_c = wexp_white - wexpwhite_m

reg sus_black_p wexpwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6677
drop if cooksd > 0.00059907

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpwhite_mt = mean(wexp_white)
gen wexpwhite_ct = wexp_white - wexpwhite_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wexpwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

esttab using Sus_explicit_sfixed.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model3.1 Model3.2 Model3.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The explicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Implicit w/ state fixed effects (Table A4.4)
clear
use Suspension

egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg sus_black_p wiatwhite_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18895
drop if cooksd > 0.0002117

egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1 
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}


clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatwhite_m = mean(wiat_white)
gen wiatwhite_c = wiat_white - wiatwhite_m

reg sus_black_p wiatwhite_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6677
drop if cooksd > 0.00059907

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatwhite_mt = mean(wiat_white)
gen wiatwhite_ct = wiat_white - wiatwhite_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wiatwhite_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban, absorb(i.sfips) vce(cluster leaid fips)
}

esttab using Sus_implicit_sfixed.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model4.1 Model4.2 Model4.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The explicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear



// Gifted Programs - Explicit Bias (Table A5.1)
clear
use Gifted_Program

egen wexpall0716_m = mean(wexp_all)
gen wexpall0716_c = wexp_all - wexpall0716_m

reg gift_black_p wexpall0716_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18643
drop if cooksd > 0.00021456

egen wexpall0716_mt = mean(wexp_all)
gen wexpall0716_ct = wexp_all - wexpall0716_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpall0716_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpall0716_m = mean(wexp_all)
gen wexpall0716_c = wexp_all - wexpall0716_m

reg gift_black_p wexpall0716_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/7431
drop if cooksd > 0.00053829

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpall0716_mt = mean(wexp_all)
gen wexpall0716_ct = wexp_all - wexpall0716_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpall0716_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wexpall0716_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Gift_explicit_all0716.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model1.1 Model1.2 Model1.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Gifted Programs - Implicit Bias (Table A5.2)
clear
use Gifted_Program

egen wiatall0716_m = mean(wiat_all)
gen wiatall0716_c = wiat_all - wiatall0716_m

reg gift_black_p wiatall0716_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18643
drop if cooksd > 0.00021456

egen wiatall0716_mt = mean(wiat_all)
gen wiatall0716_ct = wiat_all - wiatall0716_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatall0716_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatall0716_m = mean(wiat_all)
gen wiatall0716_c = wiat_all - wiatall0716_m

reg gift_black_p wiatall0716_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/7431
drop if cooksd > 0.00053829

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatall0716_mt = mean(wiat_all)
gen wiatall0716_ct = wiat_all - wiatall0716_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatall0716_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wiatall0716_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Gift_implicit_all0716.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model2.1 Model2.2 Model2.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Explicit (Table A5.3)
clear
use Suspension

egen wexpall0716_m = mean(wexp_all)
gen wexpall0716_c = wexp_all - wexpall0716_m

reg sus_black_p wexpall0716_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18895
drop if cooksd > 0.0002117

egen wexpall0716_mt = mean(wexp_all)
gen wexpall0716_ct = wexp_all - wexpall0716_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpall0716_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpall0716_m = mean(wexp_all)
gen wexpall0716_c = wexp_all - wexpall0716_m

reg sus_black_p wexpall0716_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6677
drop if cooksd > 0.00059907

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpall0716_mt = mean(wexp_all)
gen wexpall0716_ct = wexp_all - wexpall0716_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpall0716_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wexpall0716_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Sus_explicit_all0716.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model3.1 Model3.2 Model3.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Implicit (Table A5.4)
clear
use Suspension

egen wiatall0716_m = mean(wiat_all)
gen wiatall0716_c = wiat_all - wiatall0716_m

reg sus_black_p wiatall0716_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/18895
drop if cooksd > 0.0002117

egen wiatall0716_mt = mean(wiat_all)
gen wiatall0716_ct = wiat_all - wiatall0716_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatall0716_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatall0716_m = mean(wiat_all)
gen wiatall0716_c = wiat_all - wiatall0716_m

reg sus_black_p wiatall0716_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6677
drop if cooksd > 0.00059907

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatall0716_mt = mean(wiat_all)
gen wiatall0716_ct = wiat_all - wiatall0716_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatall0716_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wiatall0716_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Sus_implicit_all0716.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model4.1 Model4.2 Model4.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear



// Gifted Programs - Explicit Bias (Table A6.1)
clear
use Gifted_Program

egen wexpwhite1216_m = mean(wexp_1216)
gen wexpwhite1216_c = wexp_1216 - wexpwhite1216_m

reg gift_black_p wexpwhite1216_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/17247
drop if cooksd > 0.00023192

egen wexpwhite1216_mt = mean(wexp_1216)
gen wexpwhite1216_ct = wexp_1216 - wexpwhite1216_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite1216_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpwhite1216_m = mean(wexp_1216)
gen wexpwhite1216_c = wexp_1216 - wexpwhite1216_m

reg gift_black_p wexpwhite1216_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6862
drop if cooksd > 0.00058292

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpwhite1216_mt = mean(wexp_1216)
gen wexpwhite1216_ct = wexp_1216 - wexpwhite1216_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wexpwhite1216_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wexpwhite1216_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Gift_explicit_1216.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model1.1 Model1.2 Model1.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Gifted Programs - Implicit Bias (Table A6.2)
clear
use Gifted_Program

egen wiatwhite1216_m = mean(wiat_1216)
gen wiatwhite1216_c = wiat_1216 - wiatwhite1216_m

reg gift_black_p wiatwhite1216_c tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/17247
drop if cooksd > 0.00023192

egen wiatwhite1216_mt = mean(wiat_1216)
gen wiatwhite1216_ct = wiat_1216 - wiatwhite1216_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite1216_ct tot_black tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Gifted_Program

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatwhite1216_m = mean(wiat_1216)
gen wiatwhite1216_c = wiat_1216 - wiatwhite1216_m

reg gift_black_p wiatwhite1216_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6862
drop if cooksd > 0.00058292

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatwhite1216_mt = mean(wiat_1216)
gen wiatwhite1216_ct = wiat_1216 - wiatwhite1216_mt

gen byte constant = 1
foreach i in gift_black_p {
eststo: reghdfe  `i' wiatwhite1216_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in gift_black_p {
eststo: reghdfe  `i' c.wiatwhite1216_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino gift_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Gift_implicit_1216.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model2.1 Model2.2 Model2.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Explicit (Table A6.3)
clear
use Suspension

egen wexpwhite1216_m = mean(wexp_1216)
gen wexpwhite1216_c = wexp_1216 - wexpwhite1216_m

reg sus_black_p wexpwhite1216_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/17524
drop if cooksd > 0.00022826

egen wexpwhite1216_mt = mean(wexp_1216)
gen wexpwhite1216_ct = wexp_1216 - wexpwhite1216_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite1216_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wexpwhite1216_m = mean(wexp_1216)
gen wexpwhite1216_c = wexp_1216 - wexpwhite1216_m

reg sus_black_p wexpwhite1216_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6283
drop if cooksd > 0.00063664

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wexpwhite1216_mt = mean(wexp_1216)
gen wexpwhite1216_ct = wexp_1216 - wexpwhite1216_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wexpwhite1216_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wexpwhite1216_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Sus_explicit_1216.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model3.1 Model3.2 Model3.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear


// Suspension - Implicit (Table A6.4)
clear
use Suspension

egen wiatwhite1216_m = mean(wiat_1216)
gen wiatwhite1216_c = wiat_1216 - wiatwhite1216_m

reg sus_black_p wiatwhite1216_c tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/17524
drop if cooksd > 0.00022826

egen wiatwhite1216_mt = mean(wiat_1216)
gen wiatwhite1216_ct = wiat_1216 - wiatwhite1216_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite1216_ct tot_black tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

clear
use Suspension

drop if tblack==.

egen tblack_m = mean(tblack)
gen tblack_c = tblack - tblack_m
egen wiatwhite1216_m = mean(wiat_1216)
gen wiatwhite1216_c = wiat_1216 - wiatwhite1216_m

reg sus_black_p wiatwhite1216_c c.tblack_c##c.tblack_c tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south

predict r, rstudent
twoway (scatter r tot_black)
predict cooksd, cooksd
twoway (scatter cooksd tot_black)
drop if r > 3 
drop if r < -3
dis 4/6283
drop if cooksd > 0.00063664

egen tblack_cm = mean(tblack)
gen tblack_cc = tblack - tblack_cm
egen wiatwhite1216_mt = mean(wiat_1216)
gen wiatwhite1216_ct = wiat_1216 - wiatwhite1216_mt

gen byte constant = 1
foreach i in sus_black_p {
eststo: reghdfe  `i' wiatwhite1216_ct c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

foreach i in sus_black_p {
eststo: reghdfe  `i' c.wiatwhite1216_ct##c.tblack_cc##c.tblack_cc tot_black thispanic tot_latino sus_total frp tot_enr highschool_ccd urban south, absorb(constant) vce(cluster leaid fips)
}

esttab using Sus_implicit_1216.rtf, varwidth(21) modelwidth(8) label cells(b(star fmt(%9.3f)) p(par)) stats(r2 N, fmt(%9.3f %9.0g) labels(R-squared "N")) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) nonumbers mlabels(Model4.1 Model4.2 Model4.3) interaction(" X ") collabels(none) addnote("Note: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001. P values are reported in parentheses. Two-way clustered robust standard errors are used. The implicit bias and the percentage of Black teachers variables are mean-centered.") alignment(l) nobaselevels 
est clear

