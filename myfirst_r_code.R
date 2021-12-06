# test code

a = c(1:10)
b = a*2
a
b

c = a + b

install_github()


install.packages("devtools")
install.packages("RNeo4j")
install.packages("visNetwork")

devtools::install_github("nicolewhite/RNeo4j")

devtools::install_github("dataknowledge/visNetwork")
library(devtools)
library(RNeo4j)
library(visNetwork)

library(tidyverse)
library(plotly)

load("../../R/old.rda")

library(RColorBrewer)
RColorBrewer::display.brewer.all(type = "div")
RColorBrewer::display.brewer.all(type = "qual")
RColorBrewer::display.brewer.all(type = "seq")
RColorBrewer::brewer.pal.info




# 1. 참여유형별 노인 일자리 성별에 따른 참여자 수
# = 할머니, 할아버지들 중에서 노인일자리를 참여하는 수가 어떻게 되는가? / 지속적으로 그 일을 하시는가?
(
    old_new %>% group_by(sex,participation) %>% 
        summarise(participation_count = n()) %>% 
        print() %>% 
        ggplot(aes(x = sex, y = participation_count, fill=participation)) + geom_bar(stat ='identity') +
        ggtitle("2020년 참여유형별 노인일자리사업 성별에 따른 참여자 수") +
        geom_text(aes(label=participation_count),position = position_stack(vjust=0.5)) +
        xlab("성별") + ylab("참여자 수")
) %>% ggplotly()


