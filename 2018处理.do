cls
use "D:\各类\毕业论文\4正式答辩\定稿数据重做\cfps2018person_202012.dta",clear

vlookup pid, gen(feduc) key(pid_10) value(feduc_10)
vlookup pid, gen(meduc) key(pid_10) value(meduc_10)
drop feduc_10 meduc_10 pid_10

vlookup pid, gen(qz206_fix) key(pid_14) value(qz206_14)
vlookup pid, gen(qz208_fix) key(pid_14) value(qz208_14)
vlookup pid, gen(qz209_fix) key(pid_14) value(qz209_14)
vlookup pid, gen(qz210_fix) key(pid_14) value(qz210_14)
vlookup pid, gen(qz211_fix) key(pid_14) value(qz211_14)
vlookup pid, gen(qz212_fix) key(pid_14) value(qz212_14)
vlookup pid, gen(mathtest_fix) key(pid_14) value(mathtest_14)
vlookup pid, gen(wordtest_fix) key(pid_14) value(wordtest_14)
drop qz206_14 qz208_14 qz209_14 qz210_14 qz211_14 qz212_14 mathtest_14 wordtest_14 pid_14

keep pid urban18 gender age qea0 income kz202 cfps2018edu cfps2018eduy qg302code qgb4 employ ///
qm201 qm202 qm203 qm204 qm205 qm206 qm207 qm208 qm209 qm210 qm211 qm212 qm213 qm214 qm215 ///
mathtest18 wordtest18 ///
qc201 kz207 qm2011 qm6 qg1303 ///
qn406 qn407 qn411 qn412 qn414 qn416 qn418 qn420 ///
feduc meduc qz206_fix qz208_fix qz209_fix qz210_fix qz211_fix qz212_fix mathtest_fix wordtest_fix

//基础回归处理
drop if age <=60 & income <=0
drop if employ == 3
drop if age <16
drop if qea0 == 2 & age <18
drop if qgb4 == 1
drop qgb4
drop if kz202 <0
drop if wordtest18 <0
drop if mathtest18 <0
drop if qm201 <0
drop if qm202 <0
drop if qm203 <0
drop if qm204 <0
drop if qm205 <0
drop if qm206 <0
drop if qm207 <0
drop if qm208 <0
drop if qm209 <0
drop if qm210 <0
drop if qm212 <0
drop if qm214 <0
drop if qm215 <0
drop if urban18 <0
drop if age >60 &income <=0

//生成变量
//控制变量
gen urban = urban18
drop urban18
gen married = qea0 
replace married = 0 if married != 2
replace married = 1 if married == 2
gen indus_code = qg302code
drop qg302code
gen health = kz202
drop kz202
gen edu_level = cfps2018edu
drop cfps2018edu
gen edu_year = cfps2018eduy
drop cfps2018eduy

gen exper = age - edu_year - 8
drop if exper <0
gen exper2 = exper^2

//因变量
gen lnincome = ln(income)

//解释变量
gen cog = (mathtest18/24 + wordtest18/34)/2
gen noncog = ((qm201+qm202+qm204+qm206+qm208+qm209+qm211+qm213+qm214+qm215)/5+1/qm203+1/qm205+1/qm207+1/qm210+1/qm212)/15

drop if missing(lnincome,cog,noncog,urban,gender,married,health,exper,exper2,feduc,meduc,edu_level)

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
winsor2 cog_mean cog_more cog_less noncog_mean noncog_more noncog_less lnincome, replace cuts(1 99)
drop if missing(cog_sd)

//基础回归
reg lnincome cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
urban health edu_level gender married exper exper2 feduc meduc, robust
est sto m1

reg lnincome urban gender married health edu_level feduc meduc exper exper2, robust
est sto m2

esttab m1 m2 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2018基本回归结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//处理效应模型
//D1
drop if age <45 & qc201 <0
replace qc201 = edu_level if qc201 <0
replace qc201 = 0 if qc201 == 11
gen D1 = 0
replace D1 = 1 if qc201 >3
drop qc201
gen intel = kz207
drop kz207

etregress lnincome urban health edu_level gender married exper exper2 feduc meduc, ///
treat(D1 = urban feduc meduc intel) twostep hazard(imr1)
est sto m3

drop if qm6 <0
gen D2 = qm6
drop qm6
replace D2 = 0 if D2 == 5
gen relation = qm2011
drop qm2011

etregress lnincome urban health edu_level gender married exper exper2 feduc meduc, ///
treat(D2 = relation) twostep hazard(imr2)
est sto m4

esttab m3 m4 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2018处理效应模型结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//稳健性检验1
//更换noncog指标
drop if qn406 <0
drop if qn412 <0
gen noncog2 = (1/qn406 + 1/qn407 + 1/qn411 +qn412/4 +1/qn414 +qn416/4 +1/qn418 +1/qn420)/8
bysort indus_code: egen noncog2_mean = mean(noncog2)
bysort indus_code: egen noncog2_sd = sd(noncog2)
gen noncog2_cha1 = noncog2 - noncog2_mean - noncog2_sd
gen noncog2_cha2 = noncog2 - noncog2_mean + noncog2_sd
gen noncog2_more = noncog2_cha1 if noncog2_cha1 >=0
gen noncog2_less = noncog2_cha2 if noncog2_cha2 <0
replace noncog2_more = 0 if noncog2_more == .
replace noncog2_less = 0 if noncog2_less == .

reg lnincome cog_mean cog_more cog_less noncog2_mean noncog2_more noncog2_less ///
urban health edu_level gender married exper exper2 feduc meduc, robust
est sto m5

//全部更换
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
urban health edu_level gender married exper exper2 feduc meduc,robust
est sto m6

esttab m5 m6 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2018稳健性检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//反向因果检验
drop if qz206_fix <0
drop if missing(qz206_fix,qz208_fix,qz209_fix,qz210_fix,qz211_fix,qz212_fix)
summarize
gen cog_fix = (mathtest_fix/24 + wordtest_fix/34)/2
gen noncog_fix = ((qz206_fix + qz208_fix + qz209_fix + qz211_fix + qz212_fix)/7 + 1/qz210_fix)/6

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
urban health edu_level married exper exper2 feduc meduc, robust
est sto m7

esttab m7 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2018反向因果检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//机制检验
drop if qg1303 <0
gen help = qg1303
drop qg1303

reg help cog noncog urban health edu_level gender married exper exper2 feduc meduc,robust
est sto m10

reg help cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
urban health edu_level gender married exper exper2 feduc meduc,robust
est sto m8

reg lnincome cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
help urban health edu_level gender married exper exper2 feduc meduc, robust
est sto m9

esttab m8 m9 m10 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2018机制检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

