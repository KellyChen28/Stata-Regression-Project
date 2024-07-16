cls
use "D:\各类\毕业论文\4正式答辩\定稿数据重做\cfps2016adult_201906.dta",clear
vlookup pid, gen(feduc) key(pid_12) value(feduc_12)
vlookup pid, gen(meduc) key(pid_12) value(meduc_12)
drop feduc_12 meduc_12

keep pid urban16 cfps_gender qea0 qz202 qz206 qz207 qz208 qz209 qz210 qz211 qz212 qz5 cfps_age ///
cfps2016edu cfps2016eduy iwr ns_g pn401 pn402 pn403 pn404 pn405 pn406 pn407 pn408 pn409 pn410 ///
pn411 pn412 pn413 pn414 pn415 pn416 pn417 pn418 pn419 pn420 ///
qm2011 pn1001 qc201 qg302code qgb4 employ income feduc meduc qz201

drop if qgb4 == 1
drop qgb4
drop if employ == 3
drop employ
drop if cfps_age <16
drop if income <= 0
drop if qea0 ==2 & cfps_age <18
drop if qz206 <0
drop if iwr <0
drop if ns_g <0
drop if urban16 <0
drop if missing(cfps2016edu,cfps2016eduy)
drop if feduc <0
drop if meduc <0

//生成变量
gen age = cfps_age
drop cfps_age
gen married = qea0
replace married = 0 if married != 2
replace married = 1 if married == 2
drop qea0
gen indus_code = qg302code
drop qg302code
gen health = qz202
drop qz202
gen edu_level = cfps2016edu
drop cfps2016edu
gen edu_year = cfps2016eduy
drop cfps2016eduy
gen gender = cfps_gender
drop cfps_gender
gen urban = urban16
drop urban16

gen exper = age - edu_year -8
drop if exper <0
gen exper2 = exper^2

//因变量
gen lnincome = ln(income)

//解释变量
gen cog = (iwr/10 + ns_g/15)/2
gen noncog = ((qz206 + qz208 + qz209 + qz211 + qz212)/7 + 1/qz210 + 1/qz5)/7

drop if missing(lnincome,cog,noncog,gender,urban,health,married,exper,exper2,edu_level)

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
urban gender married health edu_level feduc meduc exper exper2,robust
est sto m1

reg lnincome urban gender married health edu_level feduc meduc exper exper2, robust
est sto m2

esttab m1 m2 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2016基本回归结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//稳健性检验1：替换非认知能力变量
//替换变量
drop if pn401 <0
drop if pn404 <0
drop if pn405 <0
drop if pn407 <0
drop if pn408 <0
drop if pn412 <0
drop if pn415 <0
drop if pn419 <0
drop if pn420 <0
gen noncog2 = (1/pn401 + 1/pn403 + pn404/4 + 1/pn405 + 1/pn407 + 1/pn409 + pn412/4 + 1/pn413 + 1/pn415 + 1/pn419)/10 
bysort indus_code: egen noncog2_mean = mean(noncog2)
bysort indus_code: egen noncog2_sd = sd(noncog2)
gen noncog2_cha1 = noncog2 - noncog2_mean - noncog2_sd
gen noncog2_cha2 = noncog2 - noncog2_mean + noncog2_sd
gen noncog2_more = noncog2_cha1 if noncog2_cha1 >=0
gen noncog2_less = noncog2_cha2 if noncog2_cha2 <0
replace noncog2_more = 0 if noncog2_more == .
replace noncog2_less = 0 if noncog2_less == .

reg lnincome cog_mean cog_more cog_less noncog2_mean noncog2_more noncog2_less ///
urban gender married health edu_level feduc meduc exper exper2, robust
est sto m5

//稳健性检验2：全部替换
//替换变量
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
urban gender married edu_year health feduc meduc exper exper2,robust
est sto m6

esttab m5 m6 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2016稳健性检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//处理效应模型
//认知能力水平
drop if qz201 <0
gen D1 = 0
replace D1 = 1 if qz201 >3 //生成期望认知能力水平虚拟变量
gen intel = qz207
drop qz207 //生成选择方程的工具变量：智力水平

etregress lnincome gender married urban edu_level health feduc meduc exper exper2, ///
treat(D1 = cog) twostep hazard(imr1)
est sto m3

drop if pn1001 <0
replace pn1001 = 0 if pn1001 == 5
gen D2 = pn1001
drop pn1001
drop if qm2011 <0
gen relation = qm2011
drop qm2011

etregress lnincome edu_level health married urban feduc meduc exper exper2 gender, ///
treat(D2 = noncog) twostep hazard(imr2)
est sto m4

esttab m3 m4 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2016处理效应模型结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace
