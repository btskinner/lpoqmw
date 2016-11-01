***Balance check for quant workshop/Hansen & Bowers applied to ALEKS RCT
***Brent Evans
***10/18/16

use analytic_sample, clear

**unit id = id
	**n = 1085
**cluster = treattestdate
	**63 clusters ranging in size from 2 to 35
**blocking variables = _b*
	**blocked by day of the week: monday - friday
**treatment assignment = t_assign
	**three treatments: ALEKS-2, ALEKS-5, Compass (BAS/control)
	**could recode into ALEKS vs Compass T/C for simplicity
	gen treat=t_assign=="2"|t_assign=="5"
**covariates include age, female, pell, firstgen, hsgpa


***Wrong way #1
ttest age, by(treat)
ttest pell, by(treat)
ttest female, by(treat)
reg treat female
reg treat female _b*
reg treat female _b*, cluster(treattestdate)

***Wrong way #2
logit treat _b*
est sto m1
logit treat female age pell _b*
est sto m2
lrtest m1 m2
