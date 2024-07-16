cls
use "D:\各类\毕业论文\4正式答辩\定稿数据重做\cfps2010adult_202008.dta",clear

keep pid gender urban qa1age qc8 qd3 qe1 qg3 qg308code qg4 qk801 qk804 qm101_a_4 feduc meduc ///
qz201 qz202 qz203 qz204 qz205 qz206 qz207 qz208 qz209 qz210 qz211 qz212 income mathtest wordtest ///
cfps2010edu_best cfps2010eduy_best qq601 qq602 qq603 qq604 qq605 qq606 depression

//基本回归的处理（还有期望教育程度、人缘、是否获得帮助未处理）
drop if qa1age <60 & income <0
drop if qd3 == 1 //去除还在上学的
drop qd3
drop if qe1 == 2 & qa1age <18 //去除未成年结婚的
drop if qg4 == 1  //去除从事农业工作的
drop qg4
drop if qe1 <0 //去除婚姻状况不适用的
drop if qz202 <0
drop if mathtest <0
drop if qz205 <0
drop if qa1age >60

//基本回归变量
//控制变量
gen age = qa1age
drop qa1age
gen married = qe1
replace married = 0 if married != 2
replace married = 1 if married == 2
gen indus_code = qg308code
drop qg308code
gen health = qz202
drop qz202
gen edu_level = cfps2010edu_best
drop cfps2010edu_best
gen edu_year = cfps2010eduy_best
drop cfps2010eduy_best

gen exper = age - edu_year -8
drop if exper <0
gen exper2 = exper^2

//因变量
gen lnincome = ln(income)

//解释变量
gen cog = (mathtest/24 + wordtest/34)/2
gen noncog = ((qz206 + qz208 + qz209 + qz211 + qz212)/7 + 1/qz210)/6

drop if missing(lnincome,cog,noncog,gender,urban,health,married,exper,exper2,feduc,meduc,edu_level)

summarize

//分解解释变量
drop if indus_code <0
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
winsor2 cog_mean cog_more cog_less noncog_mean noncog_more noncog_less lnincome, replace cuts(2 98)

//基础回归
replace cog_more = cog_more * 6
reg lnincome cog_mean cog_more cog_less noncog_mean noncog_more noncog_less ///
urban gender married health feduc meduc edu_level exper exper2, robust
est sto m1

reg lnincome urban gender married health feduc meduc edu_level exper exper2, robust
est sto m2

esttab m1 m2 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2010基本回归结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//处理效应模型
//认知能力水平
drop if qz201 <0
gen D1 = 0
replace D1 = 1 if qz201 >3 //生成期望认知能力水平虚拟变量

etregress lnincome urban gender edu_level health married feduc meduc exper exper2, ///
treat(D1 = cog_more cog_mean cog_less) twostep hazard(imr1)
est sto m3

drop if qk801 <0
gen D2 = 0
replace D2 = 1 if qk801 >2 //生成人缘关系的虚拟变量
drop qk801
drop if qk804 <0
gen relation = qk804
drop qk804 //生成选择方程的工具变量：与人相处评分

etregress lnincome edu_level health married feduc meduc urban gender exper exper2, ///
treat(D2 = noncog_mean noncog_more noncog_less) twostep hazard(imr2)
est sto m4

esttab m3 m4 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2010处理效应模型结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//稳健性检验1：替换非认知能力变量
//替换变量
drop if qq601 <0
drop if qq602 <0
drop if qq603 <0
drop if qq604 <0
drop if qq605 <0
drop if qq606 <0
gen noncog2 = depression/30
bysort indus_code: egen noncog2_mean = mean(noncog2)
gen noncog2_cha1 = noncog2 - noncog2_mean
gen noncog2_cha2 = noncog2 - noncog2_mean
gen noncog2_more = noncog2_cha1 if noncog2_cha1 >=0
gen noncog2_less = noncog2_cha2 if noncog2_cha2 <0
replace noncog2_more = 0 if noncog2_more == .
replace noncog2_less = 0 if noncog2_less == .

replace cog_more = cog_more * 2

reg lnincome cog_mean cog_more cog_less noncog2_mean noncog2_more noncog2_less ///
urban health feduc meduc edu_level, robust
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

replace cog2_more = cog2_more * 4

reg lnincome cog2_mean cog2_more cog2_less noncog2_mean noncog2_more noncog2_less ///
urban edu_level health feduc,robust
est sto m6

esttab m5 m6 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2010稳健性检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

//机制检验
drop if qm101_a_4 <0
gen help = qm101_a_4
drop qm101_a_4

reg help cog_mean cog_more cog_less noncog_mean noncog_more noncog_less,robust
est sto m7

reg lnincome cog_mean cog_more cog_less noncog_mean noncog_more noncog_less help ///
urban gender married health edu_level feduc meduc exper exper2,robust
est sto m8

esttab m7 m8 using D:\各类\毕业论文\4正式答辩\定稿数据重做\2010稳健性检验结果.rtf, ///
se star(* 0.1 ** 0.05 *** 0.01) nolabel r2 replace

