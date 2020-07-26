#### t-test ####
# 정의 : 모집단의 표준편차가 알려지지 않았을 때, 정규분포의 모집단에서 모은 샘플의 평균값에 대한 가설 검정 방법
# 목적 : 두 개의 집단이 같은지 다른지를 비교하기 위해 사용



#### Power Analysis : 적정한 표본의 개수 산출 ####
# 유효한 개수 (Effective Size)
# Cohens's D : 최소한 표본이 몇 개 있어야 하는가를 알려주는 공식

#                | 평균1   -    평균 2 |               <- 평균 간의 차
#             -----------------------------
#              /-------------------------
#             / 표준편차 1 + 표준편차 2
#            /   ----------------------
#          |/            2




#### 실습 1) Power Analysis (KY) ####
ky  <- read.csv("../data/KY.csv", header = TRUE)
View(ky)

# 개수 확인 (1 순위 확인 사항)
table(ky$group)

# 각각 레벨 별 평균
mean.1 <- mean(ky$score[ky$group==1])
mean.2 <- mean(ky$score[ky$group==2])
cat(mean1, mean.2)

# 각각 표본별 표준편차
sd.1 <- sd(ky$score[ky$group==1])
sd.2 <- sd(ky$score[ky$group==2])
cat(sd.1, sd.2)

# cohen's D
eff_size <- abs(mean.1 - mean.2) / sqrt((sd.1^2 + sd.2^2)/2)
eff_size

# pwr 설치
install.packages("pwr")
library(pwr)

# power 검정 -> n의 값 확인.
pwr.t.test(d=eff_size, 
           type="two.sample", 
           alternative = "two.sided", 
           power=.8, 
           sig.level = .05)



#### 두 그룹의 평균 비교 ####
# 사용 조건 : 결과값이 연속변수여야 한다 / 정규분포여야 한다. / 등분산이어야 한다.
# 위의 조건들을 모두 만족 할 때, t-test 를 사용(student t-test, two sample t-test,...)

# 만약, 결과가 연속변수가 아닐 경우 : Mann-Whitney U-test, Wilcoxen rank-sum test, Mann-Whitney-Wilcoxen test, MWW
# 만약, 정규분포가 아닐 경우 : MWW
# paired t-test일 경우 wilcoxen signed rank test
# 만약, 등분산이 아닐 경우 : Welch's t-test 

#install.packages("moonBook")
library("moonBook")

# 데이터 load : ACS 환자
# 급성 관동맥 증후군(ACS)은 관상동맥의 혈관 내에 급성으로 생긴 크고 작은 혈전으로 인해 순간적으로 혈관이 폐쇄되거나, 혈전에서 분비되는 여러 혈관 수축성 물질이 혈관을 더욱 심하게 수축시킴으로써 관상동맥이 심하게 폐쇄되어 심장에 혈류 공급이 부족해지는 질환을 의미.
# Dx : 진단결과 , LDLC : 저혈당치, HDLC : 고혈당치
# 진단결과에 영향을 끼치는 요인들에 뭐가 있을까?
?acs



# 1) 가설 수립
# 귀무가설 : 남성과 여성의 평균나이 차이가 ACS 발생에 영향이 없다.
# 대립가설 : 남성과 여성의 평균나이 차이는 ACS 발생에 영향을 미친다. (남성이 많아야하나? 여성이 많아야하나? 둘 다 보는 것이므로 양측검정이다.)

head(acs)
str(acs)



# 2) 평균 나이를 구해서 비교해보기
mean.man <- mean(acs$age[acs$sex=="Male"])
mean.woman <- mean(acs$age[acs$sex=="Female"])
cat(mean.man, mean.woman)

# 8살 정도의 차이가 나는데 -> 이것이 과연 유의미한 차이인가?
moonBook::densityplot(age ~ sex, data = acs)



# 3) 정규분포 테스트 : Shapiro-Wilk test (https://m.blog.naver.com/PostView.nhn?blogId=jjy0501&logNo=221016092102&proxyReferer=https:%2F%2Fwww.google.com%2F)
# 귀무가설 : 정규분포가 맞다
# 대립가설 : 정규분포가 아니다.
shapiro.test(acs$age[acs$sex == "Male"])    # p-value = 0.2098 : .05 이상이므로 귀무가설 O
shapiro.test(acs$age[acs$sex == "Female"])  # p-value = 6.34e-07 : .05 이하이므로 대립가설 O



# 4) 등분산 테스트 ()
# 귀무가설 : 등분산이 맞다
# 대립가설 : 등분산이 아니다.
var.test(age ~ sex, data=acs)               # p-value = 0.3866 : .05 이상이므로 귀무가설 O



# 5) MWW사용
# 등분산이지만 정규분포가 아니다 -> t-test X -> MWW 사용
wilcox.test(age ~ sex, data=acs)            # p-value < 2.2e-16 : .05 이하이므로 대립가설 O

# 따라서, "대립가설 : 남성과 여성의 평균나이 차이는 ACS 발생에 영향을 미친다."을 채택.


# t-test 해 보면?
t.test(age ~ sex, data=acs, var.test=TRUE, alt="less")         # p-value = 1 : .05 이상이므로 귀무가설 O. MWW와 다른 결과가 나왔다.
t.test(age ~ sex, data=acs, var.test=TRUE, alt="greater")      # p-value < 2.2e-16 : .05 이하이므로 대립가설 O.
t.test(age ~ sex, data=acs, var.test=TRUE, alt="two.sided")    # alt = two.sided: 양측검정. p-value < 2.2e-16 : .05 이하이므로 대립가설 O.
t.test(age ~ sex, data=acs, var.test=FALSE)                    # var.test=FALSE : 등분산이 아닐 경우에 사용하는 Welch's t-test로 검정하겠다.      




#### 집단이 한 개인 경우 : one sample t-test ####
# 주제 : A회사의 건전지 수명이 1,000시간일 때, 무작위로 뽑은 10개의 건전지 수명에 대해, 샘플이 모집단과 다르다고 할 수 있는가?
# 귀무가설 : 모집단의 평균과 같다.
# 대립가설 : 모집단의 평균과 다르다.

# 1) 무작위로 뽑은 건전지 수명이 다음과 같다.
a <- c(980, 1008, 968, 1032, 1012, 1002 , 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

# 2) 정규분포인가? -> shapiro test
shapiro.test(a)

# 3) 등분산인가? -> 집단이 두 개가 아니므로 테스트 할 필요 없음.

# 4) t-test
t.test(a, mu=1000)    # mu : 모집단의 평균값을 넣어준다.
                      # p-value = 0.602 : .05보다 크다. 귀무가설 O -> 모집단의 평균과 같다.




# 어떤 학급의 수학 평균 성적이 55점이었다. 0교시 수업을 하고 다시 성적을 살펴보았다.
# 귀무가설 : 성적에 변화가 없었다
# 대립가설 : 성적이 나아졌다.

b <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)

# 0교시 수업 하기 전의 평균보다 지금의 평균이 더 나은가? 를 알아보는 것 -> 단측검정. alt에 less 혹은 greater를 넣어서 알아본다. 
# 만약, '차이가 있는가? (나아졌든 나빠졌든 우선 변화의 유무를 우선으로)'를 알아본다면? 양측검정

mean.b <- mean(b)
mean.b

shapiro.test(b)
t.test(b, mu=55, alt = "greater")      # p-value = 0.4046 : .05보다 크다. 귀무가설 O -> 성적에 변화가 없었다.
t.test(b, mu=55, alt = "less")         # 응용 : p-value = 0.5954 : .05보다 크다. 귀무가설 O


install.packages('PairedData')
library('PairedData')

mydata <- read.csv("../data/independent.csv")
View(mydata)

#Dummy라는 컬럼에서 0은 군을 나타내고 1은 시를 나타낸다.
#시와 군에 따라 합계 출산율의 차이가 있는지 알아보려고 한다.
# 귀무가설 : 차이가 없다.
# 대립가설 : 차이가 있다.

gun.mean <- mean(mydata$birth_rate[mydata$dummy==0])
si.mean <- mean(mydata$birth_rate[mydata$dummy==1])

cat(gun.mean, si.mean)
shapiro.test(mydata$birth_rate[mydata$dummy==0])
shapiro.test(mydata$birth_rate[mydata$dummy==1])

wilcox.test(mydata$birth_rate ~mydata$dummy, data=mydata)

#---------------------------
str(mtcars)
head(mtcars)

# am:0은 오토, 1은 수동, 연비는 mpg
# 오토나 수동에 따라 연비가 같을까? 다를까?


a_mpg <- mean(mtcars$mpg[mtcars$am ==0])
m_mpg <- mean(mtcars$mpg[mtcars$am ==1])
cat(a_mpg, m_mpg)
#정규분포 확인
shapiro.test(mtcars$mpg[mtcars$am ==0])
shapiro.test(mtcars$mpg[mtcars$am ==1])

#등분산 확인
var.test(mtcars[mtcars$am ==1, 1], mtcars[mtcars$am ==0,1])

t.test(mpg ~am, data =mtcars, var.test=T)
#----------------------------------

pd <- read.csv("../data/pairedData.csv")
pd
#쥐의 몸무게가 전과 후의 변화가 있는지 없는지 판단.

# install.packages("tidyr")
library(tidyr)

?gather
#reslut를 뺀 나머지는 group에서id를 뺴라
pd2 <- gather(pd, key = "GROUP", value = "RESLUT", -ID)
pd2
# melt를 써도됨

pd3 <-paired(pd$before, pd$After)

shapiro.test(pd2$RESLUT[pd2$GROUP=="before"])
shapiro.test(pd2$RESLUT[pd2$GROUP=="After"])

var.test(pd2$RESLUT[pd2$GROUP=="before"],
         pd2$RESLUT[pd2$GROUP=="After"] )

t.test(pd2$RESLUT ~ pd2$GROUP, data=pd2, paired = T)


before <- subset(pd2, GROUP=="before", RESLUT)
before

after <- subset(pd2, GROUP=="After", RESLUT)
after

pd3<-paired(before, after)
plot(pd3, type="profile") +theme_bw()

#---------------------------
pd <- read.csv("../data/paired.csv")
View(pd)
# long형으로 바꾸기
pd2 <- gather(pd, key = "GROUP", value = "RESLUT", -c(ID,cities))
pd2

with(pd2, shapiro.test(RESULT[GROUP=="birth_rate_2015"]))

pd3 <-paired(pd$birth_rate_2015, pd$birth_rate_2010)
# 정규분포 확인 결과 아니라서 윌콕슨테스트 함수 써야됨
shapiro.test(pd2$RESLUT[pd2$GROUP=="birth_rate_2015"])
shapiro.test(pd2$RESLUT[pd2$GROUP=="birth_rate_2010"])

var.test(pd2$RESLUT[pd2$GROUP=="birth_rate_2015"],
         pd2$RESLUT[pd2$GROUP=="birth_rate_2010"] )

wilcox.test(pd2$RESLUT ~pd2$GROUP, data=pd2, paired=T)

#### kaggle ####
# https://www.kaggle.com/kappernielsen/independent-t-test-example


mat <- read.csv("../data/student-mat.csv",header = T)
View(mat)
str(mat)
summary(mat$G1)
summary(mat$G2)
summary(mat$G3)

table(mat$sex)

# 남 여 학생별로 3번의 시험에 대한 평균값 조회
#### 정규성 검정 ####
# https://bioinformaticsandme.tistory.com/37

library(dplyr)
mat %>% select(sex, G1, G2, G3) %>% group_by(sex) %>%
  summarise(mean_g1=mean(G1), mean_g2=mean(G2),mean_g3=mean(G3))
# 만족함(남자)
shapiro.test(mat$G1[mat$sex=="M"])
shapiro.test(mat$G1[mat$sex=="F"])

shapiro.test(mat$G2[mat$sex=="M"])
shapiro.test(mat$G2[mat$sex=="F"])

shapiro.test(mat$G3[mat$sex=="M"])
shapiro.test(mat$G3[mat$sex=="F"])

wilcox.test(G1 ~ sex, data = mat)
wilcox.test(G2 ~ sex, data = mat)
wilcox.test(G3 ~ sex, data = mat)

wilcox.test(G1 ~ sex, data = mat, alt="less")
wilcox.test(G2 ~ sex, data = mat, alt="less")
wilcox.test(G3 ~ sex, data = mat, alt="less")

#G1 과 G3에 대해서 변화가 있었는지 확인
mat %>% select(G1, G3) %>% summarise(mean_g1=mean(G1),mean_g3=mean(G3))

mydata <- gather(mat, key = "GROUP", value="RESULT", "G1","G3")
wilcox.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T)

wilcox.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T,alt=
              "greater")





#=================================

sleep
# before <- subset(sleep, group == 1, extra)
# after <- subset(sleep, group == 2, extra)
# library(PairedData)
# sleep2 <- paired(before, after)
# sleep2
with(sleep, shapiro.test(sleep$extra[sleep$group == 1]))
with(sleep, shapiro.test(sleep$extra[sleep$group == 2]))
with(sleep, var.test(extra[group == 1], extra[group == 2]))
with(sleep, t.test(extra ~ group, data = sleep, paired = T))

































