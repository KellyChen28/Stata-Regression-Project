cls
use "D:\各类\毕业论文\4正式答辩\定稿数据重做\cfps2014adult_201906.dta", clear

vlookup pid, gen(feduc) key(pid_10) value(feduc_10)
vlookup pid, gen(meduc) key(pid_10) value(meduc_10)
drop feduc_10 meduc_10

vlookup pid, gen(qz206_fix) key(pid_10) value(qz206_10)
vlookup pid, gen(qz208_fix) key(pid_10) value(qz208_10)
vlookup pid, gen(qz209_fix) key(pid_10) value(qz209_10)
vlookup pid, gen(qz210_fix) key(pid_10) value(qz210_10)
vlookup pid, gen(qz211_fix) key(pid_10) value(qz211_10)
vlookup pid, gen(qz212_fix) key(pid_10) value(qz212_10)
vlookup pid, gen(mathtest_fix) key(pid_10) value(mathtest_10)
vlookup pid, gen(wordtest_fix) key(pid_10) value(wordtest_10)
drop qz206_10 qz208_10 qz209_10 qz210_10 qz211_10 qz212_10 mathtest_10 wordtest_10 pid_10

keep pid urban14 cfps_gender cfps2014_age qea0 cfps2014edu cfps2014eduy qz202 ///
mathtest14 wordtest14 ///
qz5 qz201 qz202 qz203 qz204 qz205 qz206 qz207 qz208 qz209 qz210 qz211 qz212 ///
employ2014 qgb4 qg302code qc201 qm2011 qm2013 ///
qq601 qq602 qq603 qq604 qq605 qq606 income qg1303 ///
qz206_fix qz208_fix qz209_fix qz210_fix qz211_fix qz212_fix mathtest_fix wordtest_fix feduc meduc

//基础回归处理
drop if cfps2014_age <16
drop if cfps2014_age <=60 & income <0
drop if qea0 <0
drop if qea0 ==2 & cfps2014_age <18
drop if qgb4 == 1
drop qgb4
drop if mathtest14 <0
drop if qz202 <0
drop if employ2014 == 3
drop employ
drop if qz209 <0
drop if cfps2014edu <0
drop if income <=0 & cfps2014_age >60
drop if qg302code <0

//生成基础回归变量
//控制变量
gen urban = urban14
drop urban14
gen age = cfps2014_age
drop cfps2014_age
gen gender = cfps_gender
drop cfps_gender
gen married = qea0
drop qea0
replace married =0 if married != 2
replace married =1 if married == 2
gen indus_code = qg302code
drop qg302code
gen edu_level = cfps2014edu
drop cfps2014edu
gen edu_year = cfps2014eduy
drop cfps2014eduy
gen health = qz202
drop qz202

gen exper = age - edu_year -8
drop if exper <0
gen exper2 = exper^2

//因变量
gen lnincome = ln(income)

//解释变量
gen cog = (wordtest14/34 + mathtest14/24)/2
gen noncog = ((qz206 + qz208 + qz209 +qz211 +qz212)/7 + 1/qz210 + 1/qz5)/7

drop if missing(lnincome,cog,noncog,gender,urban,health,married,feduc,meduc,exper,exper2,edu_level)

summarize

//分解解释变量
bysort indus_code: egen cog_mean = mean(cog)
bysort indus_code: egen noncog_mean = mean(noncog)
bysort indus_code: egen cog_sd = sd(cog)
bysort indus_code: egen noncog_sd = sd(noncog)

gen cog_cha1 = cog - cog_mean - cog_sd
gen cog_cha2 = cog - cog_mean + cog_sd
gen cog_more = cog_cha1 if cog_cha1 >=0
gen cog_less = cog_cha2 if cog_cha2 <0
replace cog_more = 0 if cog_more == .
replace cog_less = 0 if cog_less == .

gen noncog_cha1 = noncog - noncog_mean - noncog_sd
gen noncog_cha2 = noncog - noncog_mean + noncog_sd
gen noncog_more = noncog_cha1 if noncog_cha1 >=0
gen noncog_less = noncog_cha2 if noncog_cha2 <0
replace noncog_more = 0 if noncog_more == .
replace noncog_less = 0 if noncog_less == .

//缩尾
winsor2 cog_mean cog_more cog_less noncog_mean noncog_more noncog_less, replace cuts(1 99)
drop if missing(cog_sd)

//基础回归
reg lnincome cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
urban married health gender feduc meduc edu_level exper exper2, robust
est sto m1

reg lnincome urban gender married health edu_level feduc meduc exper exper2, robust
est sto m2

esttab m1 m2 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2014基本回归结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//处理效应模型
//D1
drop if qz201 <0
gen D1 = 0
replace D1 = 1 if qz201 >3
gen intel = qz207
drop qz207

etregress lnincome edu_level feduc meduc urban gender health married exper exper2, ///
treat(D1 = cog) twostep hazard(imr1)
est sto m3

//D2
drop if qm2011 <0
drop if qm2013 <0
gen D2 = 0
replace D2 =1 if qm2011 >5
drop qm2011
gen relation  = qm2013
drop qm2013

etregress lnincome edu_level married exper exper2 urban gender health feduc meduc, ///
treat(D2 = noncog) twostep hazard(imr2)
est sto m4

esttab m3 m4 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2014处理效应模型结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//稳健性检验1
//更换noncog指标
drop if qq601 <0
drop if qq604 <0
drop if qq605 <0
drop if qq606 <0

gen noncog2 = ((qq601 + qq602 + qq603 + qq604 + qq605 + qq606)/5)/6

bysort indus_code: egen noncog2_mean = mean(noncog2)
bysort indus_code: egen noncog2_sd = sd(noncog2)
gen noncog2_cha1 = noncog2 - noncog2_mean - noncog2_sd
gen noncog2_cha2 = noncog2 - noncog2_mean + noncog2_sd
gen noncog2_more = noncog2_cha1 if noncog2_cha1 >=0
gen noncog2_less = noncog2_cha2 if noncog2_cha2 <0
replace noncog2_more = 0 if noncog2_more == .
replace noncog2_less = 0 if noncog2_less == .

replace noncog2_more = noncog2_more * 8

reg lnincome cog_mean cog_more cog_less noncog2_mean noncog2_more noncog2_less ///
urban married health edu_level feduc meduc gender exper exper2, robust
est sto m5

//都更换
reg cog urban health,robust
predict e,r

gen cog2 = e
drop e

bysort indus_code: egen cog2_mean = mean(cog2)
bysort indus_code: egen cog2_sd = sd(cog2)
gen cog2_cha1 = cog2 - cog2_mean - cog2_sd
gen cog2_cha2 = cog2 - cog2_mean + cog2_sd
gen cog2_more = cog2_cha1 if cog2_cha1 >=0
gen cog2_less = cog2_cha2 if cog2_cha2 <0
replace cog2_more = 0 if cog2_more == .
replace cog2_less = 0 if cog2_less == .

reg lnincome cog2_mean cog2_more cog2_less noncog2_mean noncog2_more noncog2_less ///
urban married edu_level health exper exper2 feduc meduc gender,robust
est sto m6

esttab m5 m6 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2014稳健性检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//反向因果检验
drop if qz206_fix <0
drop if mathtest_fix <0
gen cog_fix = (mathtest_fix/24 + wordtest_fix/34)/2
gen noncog_fix = ((qz206_fix + qz208_fix + qz209_fix + qz211_fix + qz212_fix)/7 + 1/qz210)/6

bysort indus_code: egen cog_fix_mean = mean(cog_fix)
bysort indus_code: egen noncog_fix_mean = mean(noncog_fix)
bysort indus_code: egen cog_fix_sd = sd(cog_fix)
bysort indus_code: egen noncog_fix_sd = sd(noncog_fix)

gen cog_fix_cha1 = cog_fix - cog_fix_mean - cog_fix_sd
gen cog_fix_cha2 = cog_fix - cog_fix_mean + cog_fix_sd
gen cog_fix_more = cog_fix_cha1 if cog_fix_cha1 >=0
gen cog_fix_less = cog_fix_cha2 if cog_fix_cha2 <0
replace cog_fix_more = 0 if cog_fix_more == .
replace cog_fix_less = 0 if cog_fix_less == .

gen noncog_fix_cha1 = noncog_fix - noncog_fix_mean - noncog_fix_sd
gen noncog_fix_cha2 = noncog_fix - noncog_fix_mean + noncog_fix_sd
gen noncog_fix_more = noncog_fix_cha1 if noncog_fix_cha1 >=0
gen noncog_fix_less = noncog_fix_cha2 if noncog_fix_cha2 <0
replace noncog_fix_more = 0 if noncog_fix_more == .
replace noncog_fix_less = 0 if noncog_fix_less == .

reg lnincome cog_fix_mean cog_fix_more cog_fix_less noncog_fix_mean noncog_fix_more noncog_fix_less ///
urban gender married health edu_level exper exper2 feduc meduc, robust
est sto m7

esttab m7 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2014反向因果检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//机制检验
drop if qg1303 <0
gen help = qg1303
drop qg1303

reg help cog noncog urban married edu_level health exper exper2 feduc meduc gender, robust
est sto m10

reg help cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
urban married edu_level health exper exper2 feduc meduc gender,robust
est sto m8

reg lnincome cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
help urban married health edu_level exper exper2 feduc meduc, robust
est sto m9

esttab m8 m9 m10 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2014机制检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace
