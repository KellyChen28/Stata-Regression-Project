cls
use "D:\各类\毕业论文\4正式答辩\定稿数据重做\cfps2012adult_201906.dta", clear

keep pid income urban12 cfps2012_age qe104 edu2012 eduy2012 qz202 qv102 qv202 cfps2012_gender ///
employ qg401 ///
qq6011 qq6012 qq6013 qq6014 qq6015 qq6016 qq6017 qq6018 qq6019 qq60110 qq60111 qq60112 qq60113 qq60114 ///
qq60115 qq60116 qq60117 qq60118 qq60120 ///
qz206 qz208 qz210 qz5 qz209 qz211 qz212 ///
qz207 qc201 qn1001 qq60119 ///
iwr ns_g qg410code_a_1 qz201

drop if income <=0
drop if employ == 3
drop employ
drop if qz206 <0
drop if cfps2012_gender <0
drop if qe104 <0
drop if cfps2012_age <18 & qe104 == 2
drop if qv102 <0
drop if urban12 <0
drop qg401
drop if qv202 <0
drop if iwr <0
drop if qg410code_a_1 <0

gen married = qe104
drop qe104
replace married = 0 if married != 2
replace married = 1 if married == 2
gen age = cfps2012_age
drop cfps2012_age
gen urban = urban12
drop urban12
gen edu_level = edu2012
drop edu2012
gen edu_year = eduy2012
drop eduy2012
gen health = qz202
drop qz202
gen feduc = qv102
drop qv102
gen meduc = qv202
drop qv202
gen gender = cfps2012_gender
drop cfps2012_gender
gen indus_code = qg410code_a_1
drop qg410code_a_1

gen exper = age - edu_year - 8
drop if exper <0
gen exper2 = exper^2

gen lnincome = ln(income)

//解释变量
gen cog = (iwr/10 + ns_g/15)/2
gen noncog = ((qz206 + qz208 + qz209 + qz211 + qz212)/7 + 1/qz210 + 1/qz5)/7

drop if missing(lnincome,cog,noncog,gender,urban,health,married,exper,exper2,feduc,meduc,edu_level)

summarize

//分解
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
urban gender married health edu_level feduc meduc exper exper2, robust
est sto m1

reg lnincome urban gender married health edu_level feduc meduc exper exper2, robust
est sto m2

esttab m1 m2 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2012基本回归结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//处理效应模型
drop if qz201 <0
gen D1 = 0
replace D1 = 1 if qz201 >3 //生成期望认知能力水平虚拟变量
gen intel = qz207
drop qz207 //生成选择方程的工具变量：智力水平

etregress lnincome urban gender edu_level health married feduc meduc exper exper2, ///
treat(D1 = cog) twostep hazard(imr1)
est sto m3

drop if qn1001 <0
replace qn1001 = 0 if qn1001 == 5
gen D2 = qn1001
drop qn1001
drop if qq60119 <0
gen relation = qq60119
drop qq60119 //生成选择方程的工具变量：与人相处评分

etregress lnincome gender edu_level health married feduc meduc urban exper exper2, ///
treat(D2 = noncog) twostep hazard(imr2)
est sto m4

esttab m3 m4 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2012处理效应模型结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//稳健性检验1：替换非认知能力变量
//替换变量
drop if qq6011 <0
drop if qq6012 <0
drop if qq6013 <0
drop if qq6014 <0
drop if qq6015 <0
drop if qq6016 <0
gen noncog2 = (1/qq60115 + 1/qq60120 + 1/qq6017 + 1/qq6014 + qq6015 + 1/qq6019)/6
bysort indus_code: egen noncog2_mean = mean(noncog2)
bysort indus_code: egen noncog2_sd = sd(noncog2)
gen noncog2_cha1 = noncog2 - noncog2_mean - noncog2_sd
gen noncog2_cha2 = noncog2 - noncog2_mean + noncog2_sd
gen noncog2_more = noncog2_cha1 if noncog2_cha1 >=0
gen noncog2_less = noncog2_cha2 if noncog2_cha2 <0
replace noncog2_more = 0 if noncog2_more == .
replace noncog2_less = 0 if noncog2_less == .

replace cog_more = cog_more * 6

reg lnincome cog_mean cog_more cog_less noncog2_mean noncog2_more noncog2_less ///
urban gender married health feduc meduc edu_level exper exper2, robust
est sto m5

//稳健性检验2：全部替换
//替换变量
reg cog edu_level age,robust
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
urban gender married edu_level health feduc meduc exper exper2,robust
est sto m6

esttab m5 m6 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2012稳健性检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//机制检验
//没有帮助统计
