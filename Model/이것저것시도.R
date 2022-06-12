setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\휴게소")

#대출 가능여부 파악>>화이트박스 알고리즘을 사용해야함

#stringsAsFactors 디폴트값이 False
Sys.setlocale("LC_ALL","C")
rest=read.csv("RestingPlace.csv", stringsAsFactors = TRUE,encoding = 'UTF-8')
Sys.setlocale("LC_ALL","Korean") 
head(rest)


summary(rest)
#y는 class
table(rest$class) #빈도수를 제대로 보려고
summary(rest$class) 
rest$class=factor(rest$class)#class 명목형을 변환
summary(rest$class) #확인


##########
#변수 생성
##########
#sale의 분포 확인
summary(rest$sale.1.000.)
rest_n=rest[,c(-1)] #휴게소 이름 필요 없음

#minmax 정규화-매출액 주차장/매출액
normalize =function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
summary(rest_n)
rest_n["sale.1.000."]=lapply(rest_n["sale.1.000."],normalize)
summary(rest_n) #확인



rest_n$parking_n=rest_n$parking/rest_n$sale.1.000.
rest_n$toilet_n=rest_n$toilet/rest_n$sale.1.000.
summary(rest_n)

#z점수 표준화
rest_z=rest[,c(-1)] 
temp=scale(rest["sale.1.000."])
rest_z$sale.1.000.=temp
rest_z
str(rest_z)
summary(rest_z)

#변수 생성
rest_z$parking_n=rest_z$parking/rest_z$sale.1.000.
rest_z$toilet_n=rest_z$toilet/rest_z$sale.1.000.
summary(rest_z)
rest_z$parking_n



#데이터의 행순서를 한번 섞어서 데이터 쪼개기
set.seed(12345)
rest_rand_z=rest_z[order(runif(193)),]
str(rest_rand_z)

#확인
head(rest_rand_z$parking)
head(rest_rand_z$class)


#데이터 쪼개기- 총193개 row, test를 40개 나머지 train
rest_train=rest_rand_z[1:153,]
rest_test=rest_rand_z[154:193,] 
table(rest_train$class)
table(rest_test$class)
rest_test

#DT 적용 - C5.0 모델 활용
#install.packages("C50")
library("C50")

length(rest_train)
length(rest_test)
#모델 생성
m=C5.0(rest_train[,c(-1)],rest_train$class)#name,class 빼고 train/ y에는 class
m
summary(m)

#모델 테스트
p=predict(m,rest_test) #-17안해도 되는 이유는 모델이 알아서 변수 찾아서 하기 때문
p
library(gmodels)
rest_test$class
CrossTable(rest_test$class, p)
acc1=0
rest_test[1,"class"]==p[1]

for (i in 1:40){
  acc1=acc1+ifelse(p[i]==rest_test$class[i],1,0)}

acc1/length(rest_test)
#test data에 대한 정확도 0.37 >> y의 범주가 너무 많나?



###############
#y의 범주를 3개 class로 
###############
summary(rest_z$class)
rest_z_3=rest_z
for(i in 1:193){
  rest_z_3[i,"class"]=ifelse((rest_z[i,"class"]==1)|(rest_z[i,"class"]==2),1,ifelse((rest_z[i,"class"]==3),2,3))
}
table(rest_z_3$class)
rest_z_3$class=factor(rest_z_3$class)
summary(rest_z_3$class)
######################
#아까랑 똑같은 과정

#데이터의 행순서를 한번 섞어서 데이터 쪼개기
set.seed(12345)
rest_rand_3=rest_z_3[order(runif(193)),]
str(rest_rand_3)

#데이터 쪼개기- 총193개 row, test를 40개 나머지 train
rest_train=rest_rand_3[1:163,]
rest_test=rest_rand_3[164:193,] 
table(rest_train$class)
table(rest_test$class)
rest_test






#모델 생성
m3=C5.0(rest_train[,c(-1)],rest_train$class)#name,class 빼고 train/ y에는 class
m3
summary(m3)

#모델 테스트
p3=predict(m3,rest_test) #-17안해도 되는 이유는 모델이 알아서 변수 찾아서 하기 때문
p3
rest_test$class
CrossTable(rest_test$class, p3)
acc3=0
for (i in 1:30){
  acc3=acc3+ifelse(p3[i]==rest_test$class[i],1,0)}

acc3/30



#tree 패키지 활용하기
library(tree)
rest_tree=tree(rest_train$class~.,data=rest_train)
summary(rest_tree)
#그림 그리기
plot(rest_tree)
text(rest_tree)
summary(rest_train)
#prunning 
cv.trees= cv.tree(rest_tree, FUN=prune.misclass)
plot(cv.trees) #5일 때 missclass 가자 ㅇ적음

rest_tree_prun=prune.misclass(rest_tree, best=15)
plot(rest_tree_prun)
text(rest_tree_prun)

#예측 
rest_tree_pred= predict(rest_tree_prun, rest_test, type="class") #타입 설정해줘야됨
CrossTable( rest_tree_pred,rest_test$class)
acc4=0
for (i in 1:30){
  acc4=acc4+ifelse(rest_tree_pred[i]==rest_test$class[i],1,0)}

acc4/length(rest_test)


