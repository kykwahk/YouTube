
###############################################################
## 셀레니움(Selenium)을 이용하여 유튜브 동영상 정보 추출하고  
## 서클패킹(circle packing) 도표 그리기                     
##                                                       
## 곽기영 교수 kykwahk@kookmin.ac.kr                      
##   국민대학교 경영대학 / 비즈니스IT 전문대학원            
##   유튜브: https://www.youtube.com/곽기영                  
###############################################################

# 필요 패키지 로딩
library(RSelenium)
library(rvest)
library(tidyverse)

# 셀레니움과 연결하고 유튜브 '곽기영' 채널로 이동
remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.youtube.com/곽기영")

# 재생목록 탭으로 이동 후 재생목록 이름과 각 재생목록에 속한 동영상 제목 수집
btn.playlist <- remDr$findElements(using="xpath", 
                                   value="//div[@id='tabsContent']/tp-yt-paper-tab/div")
btn.playlist[[3]]$clickElement()

btn.viewmore <- remDr$findElements(using="xpath", 
                                   value="//yt-formatted-string[@id='view-more']/a")
playlist.data <- vector("list", length(btn.viewmore))
for (i in seq_along(btn.viewmore)) {
  btn.viewmore[[i]]$clickElement()
  body <- remDr$findElement(using="xpath", value="//body")
  flag <- TRUE
  j <- 0
  while (flag) {
    j <- j + 1
    body$sendKeysToElement(list(key="page_down"))
    Sys.sleep(1)
    if (exists("pagesource")) {
      if (pagesource == remDr$getPageSource()[[1]]) {
        flag <- FALSE
      } else {
        pagesource <- remDr$getPageSource()[[1]]
      }
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  }
  
  html <- remDr$getPageSource()[[1]]
  
  playlist <- read_html(html) %>% 
    html_elements("h1#title") %>% 
    html_text()
  video <- read_html(html) %>% 
    html_elements("#meta #video-title") %>% 
    html_text() %>% 
    str_trim()
  playlist.data[[i]] <- tibble(playlist=playlist, video=video)
  remDr$goBack()
  Sys.sleep(3)
  btn.viewmore <- remDr$findElements(using="xpath", 
                                     value="//yt-formatted-string[@id='view-more']/a")
}

playlist.video <- reduce(playlist.data, bind_rows)
playlist.video

# 동영상 탭으로 이동 후 더 이상 새로운 콘텐츠가 없을 때까지 모든 웹페이지 로딩
remDr$navigate("https://www.youtube.com/곽기영")
btn.playlist <- remDr$findElements(using="xpath", 
                                   value="//div[@id='tabsContent']/tp-yt-paper-tab/div")
btn.playlist[[2]]$clickElement()

body <- remDr$findElement(using="css selector", value="body")
flag <- TRUE
i <- 0
while (flag) {
  i <- i + 1
  body$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  if (exists("pagesource")) {
    if (pagesource == remDr$getPageSource()[[1]]) {
      flag <- FALSE
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  } else {
    pagesource <- remDr$getPageSource()[[1]]
  }
}

# 동영상 콘텐츠 관련 데이터 수집
html <- remDr$getPageSource()[[1]]

video <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']") %>% 
  html_attr("title") %>% 
  str_trim()
view <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']") %>% 
  html_attr("aria-label") %>% 
  str_extract("조회수.*") %>% 
  parse_number()
url <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']") %>% 
  html_attr("href") %>% 
  str_c("https://www.youtube.com", .)
video.all <- tibble(video=video, view=view, url=url)

youtube.playlist <- full_join(playlist.video, video.all, by="video")

youtube.playlist$title <- str_split(youtube.playlist$video, "🔑") %>% 
  map_chr(~.x[1]) %>% 
  str_split("-", n=2) %>% 
  map_chr(~.x[2]) %>% 
  str_trim()
youtube.playlist$keyword <- str_split(youtube.playlist$video, "🔑") %>% 
  map_chr(~.x[2]) %>% 
  str_trim()  
youtube.playlist

# 서클패킹 도표 그리기
edges <- youtube.playlist %>% 
  drop_na(playlist) %>% 
  distinct(video, .keep_all=TRUE) %>% 
  select(from=playlist, to=title)
vertices <- youtube.playlist %>% 
  drop_na(playlist) %>% 
  distinct(video, .keep_all=TRUE) %>% 
  select(name=title, keyword, view)

view.playlist <- youtube.playlist %>% 
  drop_na(playlist) %>% 
  group_by(playlist) %>% 
  summarise(view=sum(view)) %>% 
  rename(name=playlist)

vertices <- vertices %>% 
  bind_rows(view.playlist) %>% 
  replace_na(list(keyword="Playlist"))

library(igraph)
mygraph <- graph_from_data_frame(d=edges, vertices=vertices)

library(ggraph)
windows(width=7.0, height=7.0)
set.seed(123)
ggraph(mygraph, layout="circlepack", weight=view) + 
  geom_node_circle(aes(fill=depth)) +
  theme_void() +
  geom_node_label(aes(label=str_c(name, ":", format(view, big.mark=",")), 
                      filter=(keyword=="Playlist")), repel=TRUE) +
  geom_node_text(aes(label=str_c(name, ":", format(view, big.mark=",")), 
                     filter=(view > quantile(view[keyword!="Playlist"], 0.95)) 
                     & (keyword!="Playlist")), color="orangered") +
  theme(legend.position="FALSE") +
  scale_fill_distiller(palette="GnBu") +
  labs(title="곽기영 교수 유튜브 동영상 강의",
       subtitle="조회수 상위 동영상",
       caption="출처: YouTube") +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=16),
        plot.caption=element_text(size=13))
