## 혹시나 모를 마우스오버 사태에 대비해 이렇게 미리 대피준비합니다.
## 하... 이거 처음부터 다시 어떻게 하냐...
## 멜론 플레이리스트 // 제목에 따른 노래 가사 // 제목과 가사 상관관계 분석

library(RSelenium)
library(dplyr)
library(ggplot2)
library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(htmlwidgets)
library(tm)
library(qgraph)
library(proxy)

# 데이터 스크래핑 순서 
# 좋아요 개수 1000개 이상 클릭 -> 플레이리스트 제목 수집 -> 아래 가사 버튼 클릭 -> 가사 수집 -> csv 파일 저장 
# 참고사항 : 전체 페이지는 1~35페이지 


remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
site <- "https://www.melon.com/genre/recmconts_list.htm?gnrCode=GN0100&tabType=DJ#params%5BgnrCode%5D=GN0100&params%5BtabType%5D=DJ&po=pageObj&startIndex=1"
remDr$navigate(site)


# 10개씩 3번 넘어가기
i <- 0
while (i < 4){
  # 첫페이지가 1이니까 먼저 2페이지로 넘어가서 세팅하기
  settingpage <- remDr$findElement(using='css', '#pageObjNavgation > div > span > a:nth-child(2)')
  settingpage$clickElement()
  Sys.sleep(1)
  
  # 페이지 1~10, 11~20, 21~30, 31~35 넘어가기
  for (x in 1:10) {  
    nextpage <- remDr$findElement(using='css', paste0('#pageObjNavgation > div > span > a:nth-child(',x,')'))
    nextpage$clickElement() 
    Sys.sleep(1)
    
    
    # 각 페이지 내 20개의 플레이리스트 훑rl
    # 좋아요 1000개 이상 확인하기
    for (b in 1:20){ 
      likebtn<-remDr$findElements(using='css', paste0('#djPlylstList > div > ul > li:nth-child(', b,') > div.entry > div.meta > button > span.cnt'))
      likebtn1 <- sapply(likebtn, function(x) {x$getElementText()})
      likebtn2 <- gsub("[[:punct:]]", "", likebtn1)
      likebtn2 <- as.numeric(likebtn2)
      print(paste0('좋아요 :',likebtn2))
      
      # 수록곡 개수 추출하기
      songcntnodes <- remDr$findElement(using='css', paste0('#djPlylstList > div > ul > li:nth-child(', b,') > div.entry > div.meta > span'))
      songcnt <- songcntnodes$getElementText()
      songcnt <- gsub("[[:punct:][:alpha:]]", "", songcnt)
      songcnt <- as.numeric(songcnt)
      print(paste0('수록곡 :',songcnt))
      Sys.sleep(1)
      
    # 좋아요 1000개 이상, 수록곡 40개 미만 페이지 들어가기
      if (likebtn2 > 1000 & songcnt <= 40){
         like1000btn <- remDr$findElement(using='css', paste0('#djPlylstList > div > ul > li:nth-child(', b,') > div.entry > div.info > a.ellipsis.album_name')) # 이거 되는 지 확인해볼 것
         like1000btn_title<- like1000btn$getElementText()
         like1000btn$clickElement()
         Sys.sleep(1)

         # 수록곡 가사 가져오기 // 뒤로 가기
         songlyricsum <-c()
         for (y in 1:songcnt){
           songclickbtn <- remDr$findElement(using='css', paste0('#frm > div > table > tbody > tr:nth-child(', y,') > td:nth-child(4) > div > a'))
           songclickbtn$clickElement()
           Sys.sleep(1)
           
           songlyricnodes <- remDr$findElement(using='css', '#d_video_summary')
           songlyric <- songlyricnodes$getElementText()
           songlyric <- gsub("[[:punct:]]", "", songlyric)
           songlyric <- gsub("\n", " ", songlyric)
           print(songlyric)
           songlyricsum <- c(songlyricsum, songlyric)
           remDr$goBack()
         }
         
         # 가사 저장하기
         playlistsum <- data.frame(songlyricsum)
         like1000btn_title <- gsub("[[:punct:]]", "", like1000btn_title)
         write.csv(playlistsum, file=paste0('C:/Rexam/songlyric/', like1000btn_title, '.csv'))
         
         # 플레이리스트 페이지로 돌아가기
         remDr$goBack()
         Sys.sleep(1)
      }
    }
  }
  # 다음 10페이지 넘어가기
  i <- i + 1
  beyondpage <- remDr$findElement(using='css', '#pageObjNavgation > div > a.next')
  beyondpage$clickElement()
  Sys.sleep(2)
}




# =========================================================================================================
# =========================================================================================================

# 가사분석

useSejongDic() 

# 자소서 항목 분석

word_title <- read.csv("C:/Rexam/가을 아침, 조용히 귀 기울이는 국내 포크&어쿠스틱.csv")
#mode(word_title)
#head(word_title)
#str(word_title)
#names(word_title)
word_title1 <- word_title %>% select(songlyricsum)

word_title1 <- gsub("[()*'-,./?1234567890\"]", "", word_title1)
word_title1 <- gsub("[[:cntrl:][:lower:][:upper:]]", "", word_title1)
word_title1 <- gsub("  ", "", word_title1)

word_title1
word_title2 <- extractNoun(word_title1)  ## 경고 질문
word_title2
unlist1 <- unlist(word_title2)
re_unlist <- Filter(function(x) {nchar(x) >= 2 & nchar(x) < 4}, unlist1)
title_table <- table(re_unlist)
final <- sort(title_table, decreasing = T)
final %>% head(10) -> represent1
represent1

represent1 <- as.data.frame(represent1)

result1 <- wordcloud2(final, fontFamily = "휴먼옛체")
saveWidget(result1,"analy.html",selfcontained = F)

ggplot(data=w, aes(x=year, y=weight)) + geom_bar(aes(fill=year), stat="identity") + coord_cartesian(ylim=c(0, 60))

