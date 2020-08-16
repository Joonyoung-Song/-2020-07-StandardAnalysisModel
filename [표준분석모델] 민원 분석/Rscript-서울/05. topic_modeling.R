

#############################################################
######################          #############################
###################### 55page   #############################
######################          #############################
#############################################################


setwd('C:/Users/Admin/Desktop/민원분석/Rscript') #워킹 디렉토리 설정

# EDA
source('05-1-1. Keyword_chart.R', encoding = "UTF-8")
source('05-1-2. Keyword_wordcloud.R', encoding = "UTF-8")
# 연관성 분석 (Association Rules)
source('05-2. keyword_arules.R', encoding = "UTF-8")
# 토픽 분석 (Topic Modeling)
source('05-3-1. find_topic_N.R', encoding = "UTF-8")
source('05-3-2. keyword_topic.R', encoding = "UTF-8")

# 1) 교통
re_DT_교통 <- keyword_chart(keyword = '교통')
dtm_교통 <- keyword_wordcloud(data = re_DT_교통, keyword = '교통')
keyword_arules(dtm = dtm_교통, keyword = '교통', supp = 0.03, conf = 0.15)
#결과값이 나오지 않을 경우 지지도 낮추기
find_topic_N(dtm = dtm_교통)
k <- 23 # 최적의 토픽 수 결정 후 확인, 수정
keyword_topic(data = re_DT_교통, dtm = dtm_교통, k = k, keyword = '교통')

# 2) 안전
re_DT_안전 <- keyword_chart(keyword = '안전')
dtm_안전 <- keyword_wordcloud(data = re_DT_안전, keyword = '안전')
keyword_arules(dtm = dtm_안전, keyword = '안전', supp = 0.2, conf = 0.15)
find_topic_N(dtm = dtm_안전)
k <- 8 # 최적의 토픽 수 결정 후 확인, 수정
keyword_topic(data = re_DT_안전, dtm = dtm_안전, k = k, keyword = '안전')
Q
