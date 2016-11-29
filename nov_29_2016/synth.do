// synth.do

// read data
insheet using "dropout.csv", clear

// drop DC b/c missing data
drop if stfips == 11

// =====================================
// gen some new variables
// =====================================

// log population
gen lpop2000 = log(pop2000)
gen lpop2010 = log(pop2010)

// log school age (6 - 17)
gen lpop2000sch = log(pop2000sch)
gen lpop2010sch = log(pop2010sch)

// log race/ethnic populations
gen lpop2000_amerind = log(pop2000_amerind)
gen lpop2000_asian = log(pop2000_asian)
gen lpop2000_black = log(pop2000_black)
gen lpop2000_hisp = log(pop2000_hisp)
gen lpop2000_multrace = log(pop2000_multrace)
gen lpop2000_nhpi = log(pop2000_nhpi)
gen lpop2010_amerind = log(pop2010_amerind)
gen lpop2010_asian = log(pop2010_asian)
gen lpop2010_black = log(pop2010_black)
gen lpop2010_hisp = log(pop2010_hisp)
gen lpop2010_multrace = log(pop2010_multrace)
gen lpop2010_nhpi = log(pop2010_nhpi)

// $ vars in $10k
gen medinc_current_10k = medinc_current / 10000
gen medinc_2015_10k = medinc_2015 / 10000

// factor out census region
tab cenreg, gen(cenreg_)

// =====================================
// OLS
// =====================================

global y_1 br1517
global y_2 acgr
global x above16
global w unemrate state_pov medinc_current_10k
global pop lpop2000sch lpop2000 lpop2000_*
global ideo citi6013 inst6014_nom

// teen births
reg $y_1 $x $w $pop $ideo
reg $y_1 $x $w $pop $ideo i.stfips

// average cohort graduation rate
reg $y_2 $x $w $pop $ideo
reg $y_2 $x $w $pop $ideo i.stfips

// =====================================
// synth
// =====================================

// Colorado, which changed in 2008, is the treatment unit (8)

// set up as panel
tsset stfips year

// need to be explicit about varnames (no wildcards)
global pop_1 lpop2000sch lpop2000 lpop2000_amerind lpop2000_asian
global pop_2 lpop2000_black lpop2000_hisp lpop2000_multrace lpop2000_nhpi
global cenreg cenreg_1 cenreg_2 cenreg_3

// get comparison groups (only want those where Freq == 11)
tab stfips if above16 == 0

// 2,4,5,10,12,13,16,19,21,24,25,27,30,34,36,37,38,50,56
global ctrl 2 4 5 10 12 13 16 19 21 24 25 27 30 34 36 37 38 50 56

// teen birth
synth $y_1 $x $w $pop_1 $pop_2 $ideo $cenreg, trunit(8) trperiod(2010) fig
synth $y_1 $x $w $pop_1 $pop_2 $ideo $cenreg, counit($ctrl) trunit(8) trperiod(2010) fig

// change comparison group slightly to account for missing acgr in some states/years
global ctrl_mod 2 4 5 10 12 13 19 24 25 27 30 34 37 38 50 56

// acgr
synth $y_2 $x $w $pop_1 $pop_2 $ideo $cenreg, counit($ctrl_mod) trunit(8) trperiod(2010) fig

exit

// end file
