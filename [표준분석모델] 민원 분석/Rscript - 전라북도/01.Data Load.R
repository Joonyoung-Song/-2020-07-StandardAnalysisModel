Sys.getenv('JAVA_HOME')
install.packages('https://cran.r-project.org/src/contrib/00Archive/KoNLP/KoNLP_0.80.2.tar.gz',repos = NULL,
                 type = 'source',INSTALL_opts = c('--no-lock'))
install.packages('backports')
install.packages(c('rJava', 'stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'))
library(KoNLP)

useNIADic()

list.files()
setwd('C:/Users/Admin/Desktop/민원분석/Rscript')
source('load_library.R',encoding = 'UTF-8')
source('make_dtm.R',encoding = 'UTF-8')

#.csv file 불러오기
setwd('C:/Users/Admin/Desktop/민원분석/data')
list.files()
files <- list.files(pattern = "*.csv") #확장자 csv파일의 리스트를 files에 저장

#install.packages("data.table")
require(data.table)
DT <- rbindlist(lapply(files,fread)) #각 열을 리스트로 가지는 데이터 프레임의 형태로 로드
colnames(DT)


#Transpose Data Type

#날짜타입 변환
DT$민원등록일<-as.POSIXct(DT$민원등록일,format='%y-%m-%d',origin='1970-01-01',tz='Asia/Seoul')


re_DT<-subset(DT,민원등록일>='2017-01-01') #2017년부터 2018년까지의 데이터를 분석대상으로 선정
re_DT<-subset(re_DT,nchar(민원내용)>=15 & nchar(민원내용)<=100) #민원내용이 15글자인 행 제거
re_DT<-re_DT[!duplicated(re_DT[,c('민원내용','주소','민원등록일')])]#중복 및 악성민원 제거
#duplicated() 함수는 중복데이터일경우 TRUE를 아니면 FALSE를 반환 하는 함수
rm(files); rm(DT) #불필요 객체 제거


#전체 테이블의 주소에서 시도 가져오기/ 시도가 전라북도인 데이터 테이블 생성
re_DT_add_1<-sapply(re_DT$주소,function(x) stri_split_fixed(x, pattern = " ", omit_empty = T)[[1]][1],USE.NAMES = F)
re_DT$민원인주소시도<-re_DT_add_1
re_DT$민원인주소시도<-gsub('전북','전라북도',re_DT$민원인주소시도)
re_DT<-re_DT[grep('전라북도',re_DT$민원인주소시도),] #시도가 전라북도인 데이터 테이블 생성
head(re_DT_add_1)

#전라북도 테이블의 주소에서 시군구 가져오기
re_DT_add_2<-sapply(re_DT$주소,function(x) stri_split_fixed(x, pattern = " ", omit_empty = T)[[1]][2],USE.NAMES = F)
re_DT$민원인주소시군구<-re_DT_add_2
head(re_DT_add_2)

#전라북도 테이블의 주소에서 읍면동 가져오기
re_DT_add_3<-sapply(re_DT$주소,function(x) stri_split_fixed(x, pattern = " ", omit_empty = T)[[1]][3],USE.NAMES = F)
re_DT$민원인주소법정동<-re_DT_add_3
head(re_DT_add_3)

#도로명코드 데이터 로드하기
road_code<-read.table('C:/Users/Admin/Desktop/민원분석/preprocessing/road_code_total.txt',sep='|')
road_code_jeon<-subset(road_code,V5=='전라북도')
road_code_jeon<-subset(road_code_jeon,V4=='1')
aa<-unique(data.table(road_code_jeon$V9))
aa<-aa[-1,]

#민원내용에서 전북 법정동이 존재하는 경우 주소로 가져오기
re_DT$민원내용주소법정동<-NA
for (i in 1:dim(aa)[1]){
  dong <- grep(aa[i],re_DT$민원내용)
  for (j in dong){
    re_DT$민원내용주소법정동[j]<-aa[i]
  }
}
re_DT$민원내용주소법정동<-unlist(re_DT$민원내용주소법정동)


#민원 내용에서 전북 법정동이 없는 경우 민원인 주소 법정동으로 대체
re_DT$최종민원주소<-NA
for (i in 1:dim(re_DT)[1]){
  if(is.na(re_DT$민원내용주소법정동)[i]=='FALSE'){
    re_DT$최종민원주소[i]<-re_DT$민원내용주소법정동[i]
  }else{
    re_DT$최종민원주소[i]<-re_DT$민원인주소법정동[i]
  }
}

#최종 민원주소에서 도로명 주소를 법정동으로 변환
#도로명주소
pattern<-as.character(road_code_jeon$V2)
#법정동 주소
replacement<-as.character(road_code_jeon$V9)
for (i in seq_along(pattern)){
  re_DT$최종민원주소<-gsub(pattern[i],replacement[i],re_DT$최종민원주소,fixed = T )
}

re_DT$주소<-NULL
re_DT$민원인주소시도<-NULL
re_DT$민원인주소시군구<-NULL
re_DT$민원인주소법정동<-NULL
re_DT$민원내용주소법정동<-NULL


# 9. 읍,면,동,가+α->읍,면,동,가로 변경
# 동+α 를 동 으로 변경
# 최종민원주소에서 [동][0-9]로 시작하는 객체 추출
pattern_1 <- grep('[동][0-9]*[번길]', re_DT$최종민원주소, value=T)
replacement_1 <- gsub('[동][0-9]*[번길]*', '동', pattern_1)
for(i in seq_along(pattern_1)){
  re_DT$최종민원주소 <- gsub(pattern_1[i], replacement_1[i], re_DT$최종민원주소, fixed = TRUE)
}

# 면+α 를 면 으로 변경
# 최종민원주소에서 [면][0-9]로 시작하는 객체 추출
pattern_2 <- grep('[면][0-9]*[번길]', re_DT$최종민원주소, value=T)
replacement_2 <- gsub('[면][0-9]*[번길]*', '면', pattern_2)
for(i in seq_along(pattern_2)){
  re_DT$최종민원주소 <- gsub(pattern_2[i], replacement_2[i], re_DT$최종민원주소, fixed = TRUE)
}

# 읍+α 를 읍 으로 변경
# 최종민원주소에서 [읍][0-9]로 시작하는 객체 추출
pattern_3 <- grep('[읍][0-9]*[번길]', re_DT$최종민원주소, value=T) 
replacement_3 <- gsub('[읍][0-9]*[번길]*', '읍', pattern_3)
for(i in seq_along(pattern_3)){
  re_DT$최종민원주소 <- gsub(pattern_3[i], replacement_3[i], re_DT$최종민원주소, fixed = TRUE)
}

# 가+α 를 가 로 변경
pattern_4 <- grep('[가][0-9]*[번길]', re_DT$최종민원주소, value=T)
replacement_4 <- gsub('[가][0-9]*[번길]*', '가', pattern_4)
for(i in seq_along(pattern_4)){
  re_DT$최종민원주소 <- gsub(pattern_4[i], replacement_4[i], re_DT$최종민원주소, fixed = TRUE)
}

# 최종민원주소 재검수
for (i in 1:dim(aa)[1]) {
  dong <- grep(aa[i], re_DT$최종민원주소)
  for (j in dong){
    re_DT$최종민원주소[j] <- aa[i]
  }
}
re_DT$최종민원주소<-unlist(re_DT$최종민원주소)
unique(re_DT$최종민원주소)

# 11. 1000개 샘플 추출 및 연번 매기기
re_DT<-re_DT[sample(nrow(re_DT), 1000)]
re_DT$민원접수번호 <- seq(1:nrow(re_DT))

# 불필요한 객체 제거
rm(aa); rm(pattern); rm(pattern_1); rm(pattern_2); rm(pattern_3); rm(pattern_4);
rm(replacement); rm(replacement_1); rm(replacement_2); rm(replacement_3); rm(replacement_4);
rm(road_code); rm(re_DT_add_1); rm(re_DT_add_2); rm(i); 
rm(re_DT_add_3); rm(road_code_jeon); rm(dong); rm(j);

