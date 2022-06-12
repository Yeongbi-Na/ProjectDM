setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\휴게소")
library("C50")

#stringsAsFactors 디폴트값이 False
Sys.setlocale("LC_ALL","C")
rest=read.csv("RestingPlace.csv", stringsAsFactors = TRUE,encoding = 'UTF-8')
Sys.setlocale("LC_ALL","Korean") 
head(rest)


summary(rest)
#y는 class
table(rest$class)
summary(rest$class) 
rest$class=factor(rest$class)#class 명목형으로 변환
summary(rest$class) #확인




##########
#변수 생성
##########3
rest_z=rest

str(rest_z)
summary(rest_z)

#변수 생성
#rest_z$parking_n=scale(rest_z$parking/rest_z$sale.1.000.)
#rest_z$toilet_n=scale(rest_z$toilet/rest_z$sale.1.000.)
#rest_z$sales_p=rest_z$sale.1.000./rest_z$parking
#rest_z$sales_t=rest_z$sale.1.000./rest_z$toilet
rest_z=rest_z[,c(-23,-39,-40)] #음식점 뺀거
length(rest_z)


str(rest_z)
rest_z_3=rest_z
for(i in 1:193){
  rest_z_3[i,"class"]=ifelse((rest_z_3[i,"class"]==1)|(rest_z_3[i,"class"]==2),1,ifelse((rest_z_3[i,"class"]==3),2,3))
}
table(rest_z_3$class)
rest_z_3$class=factor(rest_z_3$class)
str(rest_z_3)




rest_z_3$cafe_n=0
cafe=c("dunkind","tomandtoms","angelinus","hollys","paba","pascucci","droptop","natuur","baskin","chrispy","tousles","cafebene","dalcomm","starbucks","rotvun","mangosix")
cafe[1]
rest_z_3[1,cafe[1]]
for(i in 1:length(rest_z_3$cafe_n)){
  for(j in 1:length(cafe)){
    rest_z_3[i,"cafe_n"]=rest_z_3[i,"cafe_n"]+ifelse(rest_z_3[i,cafe[j]]=="Yes",1,0)
    
  }}
rest_z_3
rest_z_3=rest_z_3[,c(-1,-3)]
rest_z_3$class
######################
#아까랑 똑같은 과정

#데이터의 행순서를 한번 섞어서 데이터 쪼개기
set.seed(12345)
rest_rand_3=rest_z_3[order(runif(193)),]
rest_rand_3
str(rest_rand_3)
#데이터 쪼개기- 총193개 row, test를 30개 나머지 train
rest_train=rest_rand_3[1:163,]
rest_test=rest_rand_3[164:193,] 
table(rest_train$class)
table(rest_test$class)
rest_test

rest_train
str(rest_z_3)
table(rest_z_3$class)
#모델 생성
m3=C5.0(rest_train[,c(-1)],rest_train$class)#name,class 빼고 train/ y에는 class
m3
summary(m3)

#모델 테스트
p3=predict(m3,rest_test) #-17안해도 되는 이유는 모델이 알아서 변수 찾아서 하기 때문
p3
rest_test$pascucci
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

acc4/30

str(rest)




#부스팅 >> 모델의 성능을 개선시키는 >> 성능 향상 안됨
rest_boosting=C5.0(rest_train[,-1], rest_train$class,trials=5) #16,18,19
rest_boosting
summary(rest_boosting)

boost_predict=predict(rest_boosting, rest_test)
boost_predict

CrossTable(rest_test$class, boost_predict)
acc_b=0
for (i in 1:30){
  acc_b=acc_b+ifelse(boost_predict[i]==rest_test$class[i],1,0)}

acc_b/30









library(rpart)
model_rpart=rpart(class~., data=rest_train)
rest_rpart_pred= predict(model_rpart, rest_test,type="class")
rest_rpart_pred


acc5=0
for (i in 1:30){
  acc5=acc5+ifelse(rest_rpart_pred[i]==rest_test$class[i],1,0)}
acc5
acc5/30








