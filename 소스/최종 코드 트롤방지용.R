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
library(arules)
library(tm)
library(qgraph)
library(proxy)
library(webshot)
webshot::install_phantomjs()
library(showtext) # 폰트추가
showtext_auto() # 호출해야만 폰트 사용 가능
font_add(family = "cat", regular = "fonts/HoonWhitecatR.ttf")
font_add(family = "dog", regular = "fonts/THEdog.ttf")
font_add(family = "maple", regular = "fonts/MaplestoryBold.ttf")

useSejongDic() 


# 데이터 스크래핑 순서 
# 좋아요 개수 1000개 이상 클릭 -> 플레이리스트 제목 수집 -> 아래 가사 버튼 클릭 -> 가사 수집 -> csv 파일 저장 
# 참고사항 : 전체 페이지는 1~35페이지 / 명사는 2만자 이상 추출불가 / 노래 40개로 잘라야 함
# 지뢰찾기 : 11, 12, 15, 30, 33 페이지에 지뢰 있음 / 우회해야 함


remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
site <- "https://www.melon.com/genre/recmconts_list.htm?gnrCode=GN0100&tabType=DJ#params%5BgnrCode%5D=GN0100&params%5BtabType%5D=DJ&po=pageObj&startIndex=1"
remDr$navigate(site)


# 10개씩 3번 넘어가기
i <- 0
like1000btn_titleSum <- c()
while (i < 4){
  # 첫페이지가 1이니까 먼저 2페이지로 넘어가서 세팅하기
  settingpage <- remDr$findElement(using='css', '#pageObjNavgation > div > span > a:nth-child(2)')
  settingpage$clickElement()
  Sys.sleep(1)
  
  # 페이지 1~10, 11~20, 21~30, 31~35 넘어가기
  for (x in 4:5) {  
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
         print("기준 합격")
         like1000btn <- remDr$findElement(using='css', paste0('#djPlylstList > div > ul > li:nth-child(', b,') > div.entry > div.info > a.ellipsis.album_name')) # 이거 되는 지 확인해볼 것
         like1000btn_title<- like1000btn$getElementText()
         like1000btn_titleSum <- c(like1000btn_titleSum, like1000btn_title)
         like1000btn$clickElement()
         Sys.sleep(1)

         # 수록곡 정보 가져오기 // 뒤로 가기
         songlyricsum <-c()
         playlistlikesum <- c() #conts > div.section_info.d_djcol_list > div > div.entry > div.button > button > span.cnt
         songtitlesum <- c() #downloadfrm > div > div > div.entry > div.info > div.song_name
         songlikesum <- c() #d_like_count
         singersum <- c() #downloadfrm > div > div > div.entry > div.info > div.artist > a > span:nth-child(1)
         for (y in 1:songcnt){
           # 플레이리스트 좋아요 개수
           playlistlike <- remDr$findElement(using='css', '#conts > div.section_info.d_djcol_list > div > div.entry > div.button > button > span.cnt')
           playlistlike <- playlistlike$getElementText()
           playlistlike <- gsub("[[:punct:]]", "", playlistlike)
           playlistlikesum <- c(playlistlikesum, playlistlike)
           Sys.sleep(1)
           
           # 플레이리스트 내 곡 정보 들어가기
           songclickbtn <- remDr$findElement(using='css', paste0('#frm > div > table > tbody > tr:nth-child(', y,') > td:nth-child(4) > div > a'))
           songclickbtn$clickElement()
           Sys.sleep(1)
           
           # 노래 제목 추출
           songtitle <- remDr$findElement(using='css', '#downloadfrm > div > div > div.entry > div.info > div.song_name')
           songtitle <- songtitle$getElementText()
           songtitle <- gsub("[[:punct:]]", "", songtitle)
           print(songtitle)
           songtitlesum <- c(songtitlesum, songtitle)
           Sys.sleep(1)
           
           # 노래 좋아요 갯수 추출
           songlike <- remDr$findElement(using='css', '#d_like_count')
           songlike <- songlike$getElementText()
           songlike <- gsub("[[:punct:]]", "", songlike)
           print(songlike)
           songlikesum <- c(songlikesum, songlike)
           Sys.sleep(1)
           
           # 가수 이름 추출
           singer <- remDr$findElement(using='css', '#downloadfrm > div > div > div.entry > div.info > div.artist > a > span:nth-child(1)')
           singer <- singer$getElementText()
           singer <- gsub("[[:punct:]]", "", singer)
           print(singer)
           singersum <- c(singersum, singer)
           Sys.sleep(1)
           
           # 가사추출 
           songlyricnodes <- remDr$findElement(using='css', '#d_video_summary')
           songlyric <- songlyricnodes$getElementText()
           songlyric <- gsub("[[:punct:]]", "", songlyric)
           songlyric <- gsub("\n", " ", songlyric)
           print(songlyric)
           songlyricsum <- c(songlyricsum, songlyric)
           remDr$goBack()
         }
         
         # 가사 저장하기
         playlistsum <- data.frame(playlistlikesum, songtitlesum, songlikesum, singersum, songlyricsum)
         like1000btn_title <- gsub("[[:punct:]]", "", like1000btn_title)
         write.csv(playlistsum, file=paste0('C:/Rexam/allsong/', like1000btn_title, '.csv'))
         
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
# =========================================================================================================

# 가사분석



## 분석 주제 6개!!
# 1. 한국인이 가장 좋아하는 가수는?
# 2. 노래 제목 길이와 노래 좋아요 수의 상관관계는?
# 3. 플레이리스트 제목 키워드와 가사 내용 중 관련있는 단어는?
# 4. 플레이리스트 좋아요와 플레이리스트 내 노래 좋아요와의 상관관계는?
# 5. 노래 가사 전체에서 가장 많이 등장하는 단어는?
# 6. 플레이리스트 가사 단어 사이의 연관성은?


# 데이터 열 : # "X" / "playlistlikesum" / "songtitlesum" / "songlikesum" / "singersum" / "songlyricsum" 

# =====================================================================================================
# =====================================================================================================

## 1. 한국인이 가장 좋아하는 가수는?
# 분석 순서 : 각 플레이리스트 내 가수 이름 추출 / rbind 함수로 묶기 / 가장 많이 등장한 이름순 정리
# 필요한 자료 : 총 몇곡인지 / rbind로 묶은 데이터 / table함수로 묶은 뒤 head() 함수를 사용해 정리한 데이터
# 시각화 : 바플롯 사용


# 플레이리스트 제목 벡터 생성
titleSum <- list("This Weeks 취향저격 발라드 매주 업데이트","폭풍눈물주의 절절한 이별 노래","새벽감성 100% 충전 잠 못 이루는 밤에 듣는 발라드","당신에게 따뜻한 위로가 되어줄 발라드곡 모음","아련한 감성 발라드로 가창력 뽐낸 아이돌","뻔하지 않은 위로곡들과 함께 오늘 하루도 고생했어요","알앤비송","가을바람 생각나는 7080 추억의노래","담백한 멜로디와 창법 1980년대 AC 발라드","우울한 하루에 위로가 되어주는 음악들 part1","여친이 불러주면 참 좋겠다하는 노래들")
titleSum <- unlist(titleSum)
titleSum <- gsub("[()*'-,./※?\"]", "",titleSum)
titleSum

# 플레이리스트 내 가수 이름 추출 및 rbind로 묶기
singerinfoSum <- data.frame()
for (x in titleSum){
  songinfo <- read.csv(paste0("C:/Rexam/allsong/", x,".csv"))
  singerinfo <- songinfo %>% select(singersum)
  singerinfo <- as.data.frame(singerinfo)
  singerinfoSum <- rbind(singerinfoSum, singerinfo)
}

# 가수 이름 추출 데이터에서 가장 많이 등장한 가수 찾기
singerinfoSum
singerinfoSum %>% select(singersum) -> singdercol  
table(singercol) %>% sort(decreasing = T) %>% head(5) -> singerlike
singerlike
# 바플롯으로 시각화하기
coldens = seq(100, 50, -10)
barplot(singerlike, main="332곡 중 등장 빈도", las=1, xlab="선호가수",
        ylab="빈도", cex.main=1.5, font.main=2, col=rainbow(5), border="red", density=coldens, family="dog") # 322개 곡
text(0.7,5, font=2, col="black", cex=1.5, family="dog", labels="Winner!!")

singerlike1 <- as.data.frame(singerlike)
ggplot(singerlike1, aes(x=singercol, y=Freq)) + geom_bar() # ??

# savePlot(filename = "가장 인기있는 가수11.png", type="png")
# =====================================================================================================
# =====================================================================================================


# 2. 노래 제목 길이와 노래 좋아요 수의 상관관계는?
# 분석 순서 : 노래의 제목과 좋아요 추출 / 노래 제목길이를 mutate 함수로 생성 / 상관관계 분석
# 필요한 자료 : 노래제목과 좋아요 
# 시각화 : lm함수 사용

# 플레이리스트 제목 벡터 생성
titleSum <- list("This Weeks 취향저격 발라드 매주 업데이트","폭풍눈물주의 절절한 이별 노래","새벽감성 100% 충전 잠 못 이루는 밤에 듣는 발라드","당신에게 따뜻한 위로가 되어줄 발라드곡 모음","아련한 감성 발라드로 가창력 뽐낸 아이돌","뻔하지 않은 위로곡들과 함께 오늘 하루도 고생했어요","알앤비송","가을바람 생각나는 7080 추억의노래","담백한 멜로디와 창법 1980년대 AC 발라드","우울한 하루에 위로가 되어주는 음악들 part1","여친이 불러주면 참 좋겠다하는 노래들")
titleSum <- unlist(titleSum)
titleSum <- gsub("[()*'-,./※?\"]", "",titleSum)
titleSum

# 플레이리스트 내 노래제목 및 좋아요 추출, rbind로 묶기 // songtitlesum  songlikesum
rm(songinfo)
songlikeinfoSum <- data.frame()
for (x in titleSum){
  songinfo <- read.csv(paste0("C:/Rexam/allsong/", x,".csv"))
  songlikeinfo <- songinfo %>% select(songtitlesum, songlikesum)
  songlikeinfo <- as.data.frame(songlikeinfo)
  songlikeinfoSum <- rbind(songlikeinfoSum, songlikeinfo)
}

# 노래 제목글자를 세서 새로운 열로 만들기
songlikeinfoSum
songlikeinfoSum %>% mutate(songtitlenum = nchar(songtitlesum)) -> titlenum
titlenum
titlenum <-as.data.frame(titlenum)


# 상관관계를 갖는 지 먼저 그래프로 확인하기  // 수평선 -> 관계없음

x <- titlenum$songlikesum
mode(x)
y <- titlenum$songtitlenum
mode(y)
options(scipen=999)  
plot(x, y, xlab="노래좋아요", ylab="노래제목글자",xlim=c(1000,200000),ylim=c(10,30), family="dog") # 노래제목 5글자 이상 / 좋아요 1000개 이상으로 기준 제한을 두자

# 고밀도 산점도로 어디에 가장 많이 분포해있는지 파악한다.
smoothScatter(titlenum$songlikesum, titlenum$songtitlenum, xlab="노래좋아요", ylab="노래제목글자", main="고밀도 산점도", las=1)

# ggplot으로 주자
ggplot(data=titlenum, aes(x = songlikesum, y =songtitlenum)) + geom_point(shape="+" , size=3, colour="red") + xlim(5000,200000) + ylim(5,30) + geom_smooth(linetype=3, size=1) + labs(title="제목 길이와 노래 좋아요 수의 상관관계", x = "노래 좋아요 수", y="제목 글자 수") #+ geom_text(hjust=0, vjust=0, nudge_y=0.7, size=2)

# 전혀 상관이 없어보이지만 그래도 회귀분석을 한다.
sing_lm <- lm(songlikesum~songtitlenum,data=titlenum)
summary(sing_lm) # 저어ㅓ어어어ㅓㄴ혀 상관이 없다. // p-value봐라


# 박스플롯으로 노래제목 글자 수가 대체로 어느 정도의 범위를 갖는지
boxplot(titlenum$songtitlenum, las=2, col="blue", ylim=c(0,30), xlab="노래제목 글자 수", family="dog")
ggplot(data=titlenum, aes(x = songlikesum, y =songtitlenum)) + geom_boxplot(colour="red") + geom_point(shape="+" , size=3, colour="black") + geom_rug(sides = "l")+ labs(title="제목 길이와 노래 좋아요 수의 상관관계", x = "노래 좋아요 수", y="제목 글자 수")
# 고밀도 산점도로 어디에 가장 많이 분포해있는지 파악한다.


# =====================================================================================================
# =====================================================================================================



# 3. 플레이리스트 제목 키워드와 가사 내용 중 관련있는 단어는? 
# 분석 순서 : 플레이리스트 제목과 가사 내 명사 추출 / lm을 통한 분석
# 필요한 자료 : 
# 시각화 :

# 플레이리스트 제목 벡터 생성
titleSum <- list("This Weeks 취향저격 발라드 매주 업데이트","폭풍눈물주의 절절한 이별 노래","새벽감성 100% 충전 잠 못 이루는 밤에 듣는 발라드","당신에게 따뜻한 위로가 되어줄 발라드곡 모음","아련한 감성 발라드로 가창력 뽐낸 아이돌","뻔하지 않은 위로곡들과 함께 오늘 하루도 고생했어요","알앤비송","가을바람 생각나는 7080 추억의노래","담백한 멜로디와 창법 1980년대 AC 발라드","우울한 하루에 위로가 되어주는 음악들 part1","여친이 불러주면 참 좋겠다하는 노래들")
titleSum <- unlist(titleSum)
titleSum <- gsub("[()*'-,./※?\"]", "",titleSum)
titleSum

# 플레이리스트 별 가사 추출 //  songlyricsum
rm(songinfo)
lyricSum <- c()
y <- 1
for (x in titleSum){
  songinfo <- read.csv(paste0("C:/Rexam/allsong/", x,".csv"))
  lyricinfo <- songinfo %>% select(songlyricsum)
  lyricinfo <- as.data.frame(lyricinfo)
  lyricinfo <- gsub("[()*'-,./?1234567890\"]", "",lyricinfo)
  lyricinfo <- gsub("[[:cntrl:][:lower:][:upper:]]", "",lyricinfo)
  lyricinfo <- gsub("  ", "",lyricinfo)
  
  lyric <- extractNoun(lyricinfo)
  Sys.sleep(1)
  lyric_unlist <- unlist(lyric)
  lyric_filter <- Filter(function(x) {nchar(x) >= 2 & nchar(x) < 4}, lyric_unlist)
  lyricSum <- c(lyricSum, lyric_filter)
  lyric_table <- table(lyric_filter)
  final_lyric <- sort(lyric_table, decreasing = T)
  final_lyric %>% head(10) -> represent_lyric
  
  wordcloud2(final_lyric, fontFamily = "휴먼옛체", col="random-dark")
  result <- wordcloud2(final_lyric, fontFamily = "휴먼옛체", col="random-dark")
  htmltools::save_html(result,paste0("C:/Rexam/wordcloud/",y,".html"))
  webshot(paste0("C:/Rexam/wordcloud/", y,".html"), paste0("C:/Rexam/wordcloud/", y,".png"), delay = 5, vwidth = 500, vheight = 500)
  print(paste0(y," : 저장 완료"))
  y <- y + 1
  # webshot(paste0("C:/Rexam/wordcloud//",x,".html"), paste0("C:/Rexam/wordcloud/",x,".png"), delay = 5, vwidth = 500, vheight = 500)
  # saveWidget(result,paste0(x,".html"),selfcontained = F) --> html 문제로 실행 불가
}

# =====================================================================================================
# =====================================================================================================



# 4. 플레이리스트 좋아요와 플레이리스트 내 노래 좋아요와의 상관관계는?
# 분석 순서 : 플레이리스트 좋아요 추출, 노래 좋아요 추출 / 상관관계 분석
# 필요한 자료 : 플레이리스트 좋아요, 노래 좋아요
# 시각화 : lm 함수 이용
# 참고할 함수 : g + geom_count(col="tomato3", show.legend=F) + # geom_count는 해당 지점 포인트 개수에 따라 포인트 크기가 달라짐


# 플레이리스트 제목 벡터 생성
titleSum <- list("This Weeks 취향저격 발라드 매주 업데이트","폭풍눈물주의 절절한 이별 노래","새벽감성 100% 충전 잠 못 이루는 밤에 듣는 발라드","당신에게 따뜻한 위로가 되어줄 발라드곡 모음","아련한 감성 발라드로 가창력 뽐낸 아이돌","뻔하지 않은 위로곡들과 함께 오늘 하루도 고생했어요","알앤비송","가을바람 생각나는 7080 추억의노래","담백한 멜로디와 창법 1980년대 AC 발라드","우울한 하루에 위로가 되어주는 음악들 part1","여친이 불러주면 참 좋겠다하는 노래들")
titleSum <- unlist(titleSum)
titleSum <- gsub("[()*'-,./※?\"]", "",titleSum)
titleSum

# 플레이리스트 좋아요 및  노래제목 좋아요 추출 // playlistlikesum  songlikesum
rm(songinfo)
playlikeinfoSum <- data.frame()
for (x in titleSum){
  songinfo <- read.csv(paste0("C:/Rexam/allsong/", x,".csv"))
  playlikeinfo <- songinfo %>% select(playlistlikesum, songlikesum)
  playlikeinfo <- as.data.frame(playlikeinfo)
  playlikeinfoSum <- rbind(playlikeinfoSum, playlikeinfo)
}
playlikeinfoSum <- as.data.frame(playlikeinfoSum)
playlikeinfoSum %>% group_by(playlistlikesum) %>% mutate(songlikemean = mean(songlikesum)) -> finallike
head(finallike)
tail(finallike)
dim(finallike)

infoSum <- as.data.frame(playlikeinfoSum)

# 상관관계를 갖는 지 먼저 그래프로 확인하기  // 오히려 노래 좋아요가 적을수록 플레이리스트 좋아요가 높다?
x <- infoSum$playlistlikesum
#mode(x)
y <- infoSum$songlikesum
#mode(y)
options(scipen=999)
plot(x, y, xlab="플레이리스트좋아요", ylab="노래좋아요") # 노래제목 5글자 이상 / 좋아요 1000개 이상으로 기준 제한을 두자
ggplot(data=infoSum, aes(x = playlistlikesum, y =songlikesum)) + geom_point(shape="+" , size=3, colour="red")+ geom_smooth(linetype=3, size=1) + labs(title="플레이리스트 좋아요와 노래 좋아요 수의 상관관계", x = "플레이리스트 좋아요 수", y="노래 좋아요 수")
# legend를 이용해 집단 구분을 범례로 형성 // 집단별로 다른 pch를 줘보자.


like_lm <- lm(songlikesum~playlistlikesum, data=infoSum)
summary(like_lm) # 별로 크게 상관없다




# =====================================================================================================
# =====================================================================================================


# 5. 노래 가사 전체에서 가장 많이 등장하는 단어는?
# 분석 순서 : 각 플레이리스트별 가장 많이 등장한 명사 추출 / 통합 후 다시 table 함수로 계산
# 필요한 자료 : 
# 시각화 :

# 플레이리스트 제목 벡터 생성
titleSum <- list("This Weeks 취향저격 발라드 매주 업데이트","폭풍눈물주의 절절한 이별 노래","새벽감성 100% 충전 잠 못 이루는 밤에 듣는 발라드","당신에게 따뜻한 위로가 되어줄 발라드곡 모음","아련한 감성 발라드로 가창력 뽐낸 아이돌","뻔하지 않은 위로곡들과 함께 오늘 하루도 고생했어요","알앤비송","가을바람 생각나는 7080 추억의노래","담백한 멜로디와 창법 1980년대 AC 발라드","우울한 하루에 위로가 되어주는 음악들 part1","여친이 불러주면 참 좋겠다하는 노래들")
titleSum <- unlist(titleSum)
titleSum <- gsub("[()*'-,./※?\"]", "",titleSum)
titleSum

# 플레이리스트 별 가사 추출 //  songlyricsum
rm(songinfo)
lyricSum <- c()
for (x in titleSum){
  songinfo <- read.csv(paste0("C:/Rexam/allsong/", x,".csv"))
  lyricinfo <- songinfo %>% select(songlyricsum)
  lyricinfo <- as.data.frame(lyricinfo)
  lyricinfo <- gsub("[()*'-,./?1234567890\"]", "",lyricinfo)
  lyricinfo <- gsub("[[:cntrl:][:lower:][:upper:]]", "",lyricinfo)
  lyricinfo <- gsub("  ", "",lyricinfo)
  
  lyric <- extractNoun(lyricinfo)
  Sys.sleep(1)
  lyric_unlist <- unlist(lyric)
  lyric_filter <- Filter(function(x) {nchar(x) >= 2 & nchar(x) < 4}, lyric_unlist)
  lyricSum <- c(lyricSum, lyric_filter)
  lyric_table <- table(lyric_filter)
  final_lyric <- sort(lyric_table, decreasing = T)
  final_lyric %>% head(10) -> represent_lyric
  
}

# 가사 내 명사 총합 정리
table(lyricSum) -> lyricSum_table
final_lyricSum <- sort(lyricSum_table, decreasing = T)
final_lyricSum %>% head(10) -> represent_lyricSum
represent_lyricSum <- sort(represent_lyricSum)

coldens = seq(0, 100, 10)
barplot(represent_lyricSum, main="단어 순위", las=1, xlab="단어 목록",
        ylab="빈도", cex.lab=1.5, cex.main=3, font.main=2, col=rainbow(10), border="black", density=coldens, family="cat")


# 가사 간 연관성 분석
lyricSum <- unique(lyricSum)
length(lyricSum)
lyric_filter <- Filter(function(x) {nchar(x) >= 2 & nchar(x) < 4}, lyricSum)
lyric_filter

# 트랜잭션 생성
wordtran <- as(strsplit(lyric_filter, " "), "transactions")
wordtran

# apriori 함수를 통한 지지도와 신뢰도 지정, 연관규칙 발견
tranrules <- apriori(wordtran, parameter = list(supp =0.01, conf = 0.01))  
# 연관 규칙 없음 // 최대한으로 지지도와 신뢰도를 낮춰도 없음...

# 연관규칙 생성 결과 보기
inspect(tranrules) # 아무 것도 안나온다....



# =====================================================================================================
# =====================================================================================================

## 6. 플레이리스트 내 가사들 사이의 연관어 분석
# 분석 순서 : 각 플레이리스트별 \n으로 나눠진 가사 추출 / 플레이리스트 내 비교 / proxy 이용
# 필요한 자료 : 문장 단위 가사 데이터 / 
# 시각화 : 해야하나...?

# 플레이리스트 제목 벡터 생성
titleSum <- list("This Weeks 취향저격 발라드 매주 업데이트","폭풍눈물주의 절절한 이별 노래","새벽감성 100% 충전 잠 못 이루는 밤에 듣는 발라드","당신에게 따뜻한 위로가 되어줄 발라드곡 모음")
titleSum <- unlist(titleSum)
titleSum <- gsub("[()*'-,./※?\"]", "",titleSum)
titleSum

# 가사 추출 후 문장 유사도 분석 // proxy 이용
#for (x in titleSum){
#songinfo <- read.csv(paste0("C:/Rexam/songlyric/", x,".csv"))

# 가사 추출
songinfo <- read.csv("C:/Rexam/songlyric/폭풍눈물주의 절절한 이별 노래.csv")
lyricinfo <- songinfo %>% select(songlyricsum)
lyricinfo <- as.data.frame(lyricinfo)
lyricinfo <- gsub("[()*'-,./?1234567890\"]", "",lyricinfo)
lyricinfo <- gsub("[[:punct:][:lower:][:upper:]]", "",lyricinfo)
lyricinfo <- gsub("  ", "",lyricinfo)

# 가사 전처리
lyric <- extractNoun(lyricinfo)
lyric <- unique(lyric)
length(lyric)
lyric_filter <- Filter(function(x) {nchar(x) >= 2 & nchar(x) < 4}, lyric)
lyric_filter

# 트랜잭션 생성
wordtran <- as(strsplit(lyric_filter, " "), "transactions")
wordtran

# apriori 함수를 통한 지지도와 신뢰도 지정, 연관규칙 발견
tranrules <- apriori(wordtran, parameter = list(supp =0.25, conf = 0.05)) 
# 연관 규칙 없음

# 연관규칙 생성 결과 보기
inspect(tranrules) # 아무것도 안나온다....

#}







# =====================================================================================================
# =====================================================================================================

# 중간에 자른 코드들 

# 3번 워드클라우드 코드 
# wordcloud(final_lyric,freq = final_lyric, scale = c(5.1), 
#          rot.per = 0.35, min.freq = 2, random.order = F, 
#          random.color = T, colors = rainbow(10),fontfamily="휴먼옛체")

# 여기서 x는 제목을 출력이 담겨진 반복문 인수
# par(mar=c(1,1,1,1))
# wordcloud2(final_lyric, fontFamily = "맑은고딕", size=0.5,
# color="random-light", backgroundColor="pink", figpath = paste0("C:/Rexam/wordcloud/",x,".png"))
# figpath -> 트위터 이미지 모양으로 워드클라우드  // 대소문자 구분해야 함 // 

# ===================
# 끝