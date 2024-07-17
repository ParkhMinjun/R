#계층적 군집화 데이터 선택
#계층적 군집화 데이터는 관측치가 많은것은 적당하지 않다고 판단(많은것은 비계층적 적합)
#다른 변수들로 내가 원하는 변수 계층적 군집화 시켜 상관관계 파악하고자 함
#kaggle과 UCI에서 데이터셋 찾은 결과 rainfall(kaggle)데이터셋 선택
#데이터셋 다운로드 후
#데이터셋 불러오기
rain=read.csv("C:/Users/user/Desktop/rainfall.csv")
#데이터 확인
head(rain)
str(rain)

#온도,습도,풍속을 계산해 강우량 군집화 시행
#내가 분석하려는 데이터 추출
rain2=rain[,3:5]
head(rain2)
#덴드로그램 표현 위해 군집화 함수 선택 R 기본함수-hclust()
#군집화 위해 적절히 데이터 변경
#거리기반 표준화 진행
rain3=scale(rain2)
#거리 행렬 계산 (유클리드 거리 사용)
rain_d=dist(rain3,method = "euclidean")


# 계층적 군집화 수행 (평균 연결법 사용)
rain_hc=hclust(rain_d, method = "average")

#덴드로그램 시각화
plot(rain_hc,hang=-1,labels=rain$rainfall,main="rainfall 덴드로그램",xlab="rainfall",ylab="")
#덴드로그램에서 시각적으로 3개의 군집이 보임
#R을 이용해서 군집 추가 판단
#군집수 결정을 위해 NbClust()함수 사용
#NbClust 패키지 설치
install.packages("NbClust")
library(NbClust)
nc=NbClust(rain3, distance="euclidean", min.nc=2, max.nc=15, method="average")
#앞 숫자는 NB 클러스터 수행 시 분산, 표준편차 등 여러가지 통계적 수치들이 나오는데, 이러한 측정 지표들의 개수를 의미
#결과(10개의 지표에서 3개가 최적)=>군집화 최적 개수:3 (참고만 할것 절대적X) 
#Elbow point: 기울기가 급변하는 지점이 좋은 지점

par(mfrow=c(1,1))
# 추천군집수로 군집 시각화
plot(rain_hc,hang=-1,labels=rain$rainfall,main="rainfall 덴드로그램",xlab="rainfall",ylab="")
rect.hclust(rain_hc,k=3)
#덴드로그램시각화에서도 3개의 군집 추정 가능하고 추천군집수=3 또한 덴드로그램에서 알맞게 나뉘는 걸로 판별됨=>적합

#클러스터링 결과 확인(3개의 클러스터)
clusters=cutree(rain_hc, k = 3)
# 클러스터링 결과 출력
print(clusters)
#군집별 데이터 수 확인
table(clusters)
#강우량 군집 확인
table(clusters,rain$rainfall)
# 시각적으로 1번 군집의의 강우량 많고 2번 보통 3번 군집은 적은 편
#R로 통계분석 후 처음 목표였던 온도, 습도, 풍속이 강우량 군집화에 연관성 있는지 파악

#각 군집별로 강우량의 평균 계산
mean_rainfall=aggregate(rain$rainfall, by = list(clusters), FUN = mean)
#결과 데이터프레임 컬럼명 수정
colnames(mean_rainfall)=c("Cluster", "Mean_rainfall")

#각 군집별로 온도의 평균 계산
mean_temp=aggregate(rain$temperature, by = list(clusters), FUN = mean)
#데이터프레임 컬럼명 수정
colnames(mean_temp)=c("Cluster", "Mean_Temperature")

#각 군집별로 습도의 평균 계산
mean_humid=aggregate(rain$humidity, by = list(clusters), FUN = mean)
#결과 데이터프레임 컬럼명 수정
colnames(mean_humid)=c("Cluster", "Mean_Humidity")

#각 군집별로 풍속의 평균 계산
mean_wind=aggregate(rain$wind_speed, by = list(clusters), FUN = mean)
#결과 데이터프레임 컬럼명 수정
colnames(mean_wind)=c("Cluster", "Mean_Wind Speed")

#데이터프레임으로 병합후 결과 확인
df_clu=data.frame(mean_rainfall,mean_temp[,2],mean_humid[,2],mean_wind[,2])
df_clu
#군집화에 따른 최종 결과 해석
#평균적으로 온도가 높을수록 강우량이 적어지고 낮을수록 강우량은 많아진다.
#평균적으로 습도가 높을수록 강우량이 많아지고 낮을수록 강우량은 적어진다.
#평균적으로 풍속이 빠를수록 강우량이 많아지고 느릴수록 강우량은 적어진다.
#강우량과 온도는 반비례관계, 습도와 풍속은 강우량과 정비례관계로 판단된다.

#보완점
#내가 놓친 오류나 잘못 판단한 게 없는지 검증이 필요함.
#계층적 군집분석만 진행한 것이기 때문에 상관관계가 있다고 판단하기 어려움
#추가 분석기법으로 정밀한 분석 후 판단

