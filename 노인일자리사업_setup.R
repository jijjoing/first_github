library(tidyverse)
library(plotly)
library(caret) # 데이터 처리 패키지
library(corrplot)  # 상관계수 그래프 



read.csv("00_jy/src/노인일자리사업 실태조사 참여노인 데이터 정보(2020).csv") -> old_raw



length(names(old_raw))
str(old_raw)

old_raw %>% select(ID,                   # 설문번호
                   SQ2,                  # 성별 (1:남, 2:여)
                   SQ3,                  # 연령 (1:60~64, 2:65~69, 3:70~74, 4:75~79, 5:80세 이상)
                   SQ11,                 # 지역 - code_region_b
                   # SQ12,                 # 시군구(1:시, 2:군, 3:구) - code_region_m
                   # SQ13,                 # 읍면동(1:읍, 2:면, 3:동) - code_region_s
                   SQ5,                  # 수행기관 및 유형 - institution
                   QA02,                 # 참여 유형(1:신규, 2:지속) - participation
                   QA04,                 # 사업지원 이유(1순위) - reason
                   # QF04,                 # 사업 최초신청 전 경제상태 (1:매우나쁨, 2:나쁜 편, 3:보통, 4:좋은 편, 5:매우 좋음) - be_finances
                   QF0401,               # 사업 참여한 후 (현재) 경제상태 - af_finances
                   # QC01,                 # 참여 전 건강상태(1:전혀 건강하지 않음, 2: 건강하지 않은 편, 3:보통, 4:건강한 편, 5:매우 건강함) - be_health
                   QC0101,               # 참여 후 건강상태 - af_health
                   BRQA01) %>%           # 사업유형 - type
            rename(sex = SQ2,       
                   ages = SQ3,          
                   code_region_b = SQ11,        
                   # code_region_m = SQ12,        
                   # code_region_s = SQ13,         
                   institution = SQ5,       
                   participation = QA02,
                   reason = QA04,
                   # be_finances = QF04,
                   af_finances = QF0401,
                   # be_health = QC01,
                   af_health = QC0101,
                   type = BRQA01) -> old    
    


tribble(
    ~type, ~type_r,
    1, "노 노 케어",
    2, "장애인 봉사",
    3, "다문화가정 봉사",
    4, "한부모가정 봉사",
    5, "청소년선도 봉사",
    6, "생활시설 이용자지원",
    7, "학교급식 자원봉사",
    8, "스쿨존 교통지원 봉사",
    9, "CCTV 상시관제",
    10, "보육시설 봉사",
    11, "지역 아동센터 봉사",
    12, "도서관 봉사",
    13, "공원 놀이터 등 공공시설 봉사",
    14, "문화재 시설 봉사",
    15, "공공의로 및 복지시설 봉사",
    16, "지역사회 환경 개선봉사",
    17, "주정차 질서계도 봉사",
    18, "어린이 안심 등.하교 지원봉사",
    19, "기타 공공 시설 봉사",
    20, "건강체조 취미 생활지도",
    21, "문화공연 활동",
    22, "체험활동 지원",
    23, "경륜전수 활동",
    24, "사회 서비스형 사업",
    25, "공동작업장 운영 사업",
    26, "식품제조 및 판매 사업",
    27, "공산품 제작 및 판매 사업",
    28, "매장 운영 사업",
    29, "지역 영농 사업",
    30, "기타 제조 및 판매 사업",
    31, "영유아 및 청소년 교육지원",
    32, "아파트 택배 사업",
    33, "지하철 택배 사업",
    34, "세차 및 세탁 사업",
    35, "소상공인 협력 사업",
    36, "기타서비스 제공형",
    37, "인력파견형 사업"
) -> list_type

tribble(
    ~institution, ~institution_r,
    1, "지자체",
    2, "노인복지관",
    3, "노인복지센터",
    4, "대한노인회",
    5, "시니어클럽",
    6, "지역문화원",
    7, "사회복지관",
    8, "기타"
) -> list_institution

tribble(
    ~reason, ~reason_r,
    1, "생계비 마련",
    2, "용돈 마련",
    3, "사회참여 및 관계 형성의 수단",
    4, "건강 유지의 수단",
    5, "자아실현 및 자기발전 수단",
    6, "여가시간 활용 수단",
    7, "일을 통한 즐거움",
    8, "기타"
) -> list_reason


tribble(
    ~code_region_b, ~code_region_br,
    1, "서울",
    2, "부산",
    3, "대구",
    4, "인천",
    5, "광주",
    6, "대전",
    7, "울산",
    8, "경기도",
    9, "강원도",
    10, "충청북도",
    11, "충청남도",
    12, "전라북도",
    13, "전라남도",
    14, "경상북도",
    15, "경상남도",
    16, "제주"
) -> list_region_b


table(old$af_finances)
table(old$af_health)

# 데이터 내부 속성 변경 / 파생변수 생성
old %>% mutate(sex = factor(sex, levels = c(1,2), labels = c("male", "female"))) %>% 
        mutate(participation_x = factor(participation, levels = c(1,2), labels = c("신규", "지속"))) %>% 
        mutate(af_health_x = ifelse(af_health %in% c(1, 2), "bed",  ifelse(af_health %in% c(4, 5), "good", NA))) %>% 
        mutate(af_finances_x = ifelse(af_finances %in% c(1, 2), "bed",  ifelse(af_finances %in% c(4, 5), "good", NA))) %>% 
        mutate(ages_x = ifelse(ages %in% c(60:69), "60대", 
                        ifelse(ages %in% c(70:79), "70대",
                        ifelse(ages %in% c(80:89), "80대", "90대")))) -> old_new

# int 타입 num  타입으로 변경
old_new %>% map_if(is.integer, as.numeric) %>% as.data.frame() -> old_new


# ---------------------------------------------------------------------------------------------------------------------- #

read.csv("00_jy/src/연령별_성별_혼인상태별_고령자_60세_이상_시군구_20211201163800.csv") -> all


# 2020년도 인구 정제 작업 / column 이름 변경
all %>% filter(행정구역별=="전국") %>% select(연령별,X2020,X2020.6,X2020.11) %>% 
        rename(age = 연령별, total = X2020, male_tot = X2020.6, female_tot = X2020.11) -> all_pop
# 타입 변경
all_pop %>% mutate(total = as.numeric(total), male_tot = as.numeric(male_tot), female_tot = as.numeric(female_tot)) -> all_pop
        
# 내용명 변경
all_pop[1,1] <- "tot" 
all_pop[7,1] <- "85_up" 



all_pop %>% mutate(age = ifelse(age %in% c("60~64","65~69"), "60대", 
                         ifelse(age %in% c("70~74","75~79"), "70대", 
                         ifelse(age %in% c("80~84","85_up"), "80대",NA)))) %>% 
            filter(!is.na(age)) -> all_pop1 
            # group_by(age) 



all_pop1[,1] -> ageg        # 이름 저장
all_pop1[,-1] -> all_pop1   # 이름 제외 저장

all_pop1

sapply(all_pop1[1:2,], sum) -> six
sapply(all_pop1[3:4,], sum) -> sev
sapply(all_pop1[5:6,], sum) -> eight

as.data.frame(rbind(six, sev, eight)) -> all_pop2
row.names(all_pop2) <- c("60대", "70대", "80대")






# ------------------------------------------------------------------------------------------------------------------ #

old_new %>% filter(ages_x!="90대") %>% 
    group_by(ages_x) %>% 
    summarise(count = n()) -> olld

olld %>% map_if(is.integer,as.numeric) %>% as.data.frame() -> olld

cbind(all_pop2, olld) -> all_old

all_old[,-4] -> population







save(old,old_new,list_institution,list_reason,list_region_b,list_type,all_pop2,population,file = "old.rda")









# 상관계수 연습(전체 인구-> 참여자 수로 연습함)
    
population %>% select(total,count) %>% 
    cor() %>% round(2) %>% print() -> pop_cor

cor(population$total, population$count)
corrplot(pop_cor, method = "pie", tl.srt = 45)


