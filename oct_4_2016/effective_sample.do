// LPO Quantitative Methods Workshop

// ===============================================
// SETUP
// ===============================================

// read in data
insheet using "./data/online.csv", clear

// new variables
gen lefdesom = log(efdesom + 1)
gen lefteug = log(efteug)
gen lupgrntn = log(upgrntn + 1)
gen lpttot = log(pttot + 1)
gen lage25o = log(age25o + 1)

// ===============================================
// MAIN MODEL
// ===============================================

// terms
global y lefdesom
global d wmdown
global x twoyr privnp privfp openadmp lefteug lupgrntn lpttot lage25o ///
         ctbapct ctpopdens ctmedianinc i.rucc i.stfips i.year

// estimate
reg $y $d $x

// ===============================================
// COMPUTE WEIGHTS: w = (D_i - Vw)^2
// ===============================================

// estimate D ~ X
reg $d $x

// (D_i - E[D_i | X_i])
predict r, residuals

// D_tilde
gen w = r^2

// proportion of (unexplaned) variance
egen sumw = sum(w)
gen pv = w / sumw

// ===============================================
// STATE-LEVEL EFFECTIVE SAMPLE
// ===============================================

preserve

collapse (sum) pv, by (stname)
egen rank = rank(-pv)
sort rank

// list top 10
list in 1/10

// list bottom 10
list in -10/l

restore

// ===============================================
// DESCRIPTIVES
// ===============================================

// selected untransformed covariates
global xx twoyr privnp privfp openadmp efteug upgrntn pttot age25o ctbapct ///
          ctpopdens ctmedianinc

local n : list sizeof global(xx)
matrix sumstat = J(`n', 4, .)

local i = 1
foreach var in $xx {

    qui summarize `var'
    matrix sumstat[`i',1] = round(r(mean), .01)
    matrix sumstat[`i',2] = round(r(sd), .001)

    qui summarize `var' [aw = w]
    matrix sumstat[`i',3] = round(r(mean), .01)
    matrix sumstat[`i',4] = round(r(sd), .001)

    local i = `i' + 1
}

// print
matrix rownames sumstat = $xx
matrix colnames sumstat = mean sd wmean wsd
matrix list sumstat

// END FILE
exit
