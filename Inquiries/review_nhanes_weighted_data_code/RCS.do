clear
import delimited "C:\Users\lenovo\Desktop\RCS\Analysis_mort.csv"
keep ridageyr  riagendr  ridreth1 wtmec4yr seqn sdmvstra lbxtc mort_all peryear_exm sdmvpsu
mkspline doses  = lbxtc, nknots(3) knots(141,200,275) cubic displayknots
//Cox regression without weight
stset peryear_exm, failure(mort_all==1)
stcox doses*  ridageyr  b1.riagendr  b1.ridreth1

//weighted cox regression
svyset sdmvpsu [pweight=wtmec4yr], strata(sdmvstra) singleunit(centered) 
svy: stcox doses*  ridageyr  riagendr  ridreth1

cap drop loghrs lo hi hrs lbs ubs se2
cap drop lo hi 
//reference is 200, and calculate hr and ci.
predictnl loghrs = _b[doses1]*(doses1-200) + _b[doses2]*(doses2-11.44),ci(lo hi) se(se2)
gen hrs = exp(loghrs)
gen lbs=exp(lo)
gen ubs=exp(hi)

//Draw the spline plot
twoway (line  hrs lbs ubs lbxtc, sort lc(blue blue blue blue) lp(l - - dot))    ///
   if inrange(lbxtc, 100, 400),  ///
   ylab(0(0.5) 3, angle(horiz) format(%3.2fc))  xlab(100 (100) 400)  ///
   legend(off) ///
   scheme(s1mono) plotregion(style(none))
  
twoway (line  hrs lbs ubs lbxtc, sort lc(red red red red) lp(l - - dot))    ///
   if inrange(lbxtc, 100, 400),  ///
   ylab(0(0.5) 3, angle(horiz) format(%3.2fc))  xlab(100 (100) 400)  ///
   legend(off) ///
   scheme(s1mono) plotregion(style(none))