library(dplyr)
library(stringr)
library(quantmod)

#데이터베이스 샘플데이터
#데이터는 현재날짜 5월26일 기준으로 넣음

#추가로 필요한 데이터

#EXAMPLE => 메뉴&옵션, 주문내역, 예약 테이블

#Count는 그냥 다 1로 함
#7일동안 140건 주문, 그 중 30건만 예약, 예약 손님 모두 온다고 가정 
#1인: 80명, 2인: 20, 3인:20, 4인: 20
#seq(1, 140, 1) #모두 sample(c(), 140, replace=TRUE), 
#seq(1, 140, 7) #2명 sample(c(), 20, replace=TRUE)
#seq(1, 140, 7)
#seq(3, 140, 7) #3명
#seq(3, 140, 7)
#seq(3, 140, 7)
#seq(4, 140, 7) #4명
#seq(4, 140, 7)
#seq(4, 140, 7)
#seq(4, 140, 7)
#option 1개나 0개 => count도 다 1개로..
Example <- data.frame(order_id = c(seq(1, 140, 1), seq(1, 140, 7), seq(1, 140, 7), seq(3, 140, 7), seq(3, 140, 7), seq(3, 140, 7), seq(4, 140, 7), seq(4, 140, 7), seq(4, 140, 7), seq(4, 140, 7)),
                      menu_name = c(sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 140, replace=TRUE),
                                    sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE), sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE),
                                    sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE), sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE), sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE),
                                    sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE), sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE), sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE), sample(c("소고기샤브-9900", "소고기토마토샤브-10900", "소고기커리샤브-10900", "한우샤브_맑은육수-15900", "한우샤브_매운육수-15900", "한우샤브_커리육수-15900"), 20, replace=TRUE)),
                      option_name = c(sample(c("NULL_0", "소고기_4000", "한우_8000", "모듬_3000", "모듬버섯_1500", "어묵꼬치_1500", "모듬떡_ 1000", "만두쌀떡_1000", "모듬채소_1500", "국수추가_1000", "영양죽_1000", "생수_500", "콜라_1000"), 320, replace=TRUE)),
                      count = c("1"))

Example$menu_price <- as.numeric(str_split_fixed(Example$menu_name, "-", 2)[, 2])
Example$menu_name <- str_split_fixed(Example$menu_name, "-", 2)[, 1]
Example$option_price <- as.numeric(str_split_fixed(Example$option_name, "_", 2)[, 2])
Example$option_name <- str_split_fixed(Example$option_name, "_", 2)[, 1]
Example$total_price <- Example$menu_price + Example$option_price

head(Example)
str(Example)
summary(Example)
Example %>% arrange(order_id)

#정렬
Example <- Example %>% arrange(order_id)
reserve  <- reseve %>% arrange(order_id)

#예약 테이블 랜덤 설정
res_num <- data.frame(order_id = sample(c(1:140), 30, replace = FALSE), reseration = c("TRUE"))
res_num <- res_num %>% arrange(order_id)

Example <- left_join(Example, res_num, by = c("order_id" = "order_id"))

reserve <- subset(Example, reseration == TRUE) #Example로 추출된 예약 테이블 정보
Reserve <- left_join(as.data.frame(res_num), Sample, by = c("order_id" = "order_id")) #Sample로 추출된 예약 정보
Reserve <- left_join(Reserve, reserve %>% group_by(order_id) %>% summarise(people_count = n()), by = c("order_id" = "order_id"))

#이거 이용했음 추가코드
Reserve1 <- left_join(as.data.frame(res_num), Sample, by = c("order_id" = "order_id")) #Sample로 추출된 예약 정보
Reserve1 <- left_join(Reserve1, reserve %>% group_by(order_id) %>% summarise(people_count = n()), by = c("order_id" = "order_id"))
head(Reserve1)
head(Reserve)

#Example_tot_price => 주문테이블 가격
Example_tot_price <- Example %>% group_by(order_id) %>% summarise(final_price = sum(total_price))

Sample_Customer <- left_join(Sample, Customer, by = c("custom_id" = "custom_id"))
discount_order_id <- Sample_Customer[Sample_Customer$ewhain == TRUE, 1]

subset_discount <- subset(Example_tot_price, order_id %in% discount_order_id)
subset_discount$final_price <- subset_discount$final_price*0.9
subset_not_discount <- subset(Example_tot_price, !order_id %in% discount_order_id)

Example_tot_price <- bind_rows(subset_discount, subset_not_discount) %>% arrange(order_id)
#이대생 10퍼센트 적용 

#Example_order => 주문내역 테이블 
Example_Order <- Example %>% group_by(order_id, menu_name) %>% summarise(menu_count = n())

#SAMPLE => 주문 테이블
time1 <- c(1103, 1135, 1204, 1215, 1217, 1228, 1239, 1242, 1310, 1331, 1501, 1738, 1741, 1805, 1810, 1817, 1822, 1836, 1919, 2000)
time2 <- time1 + 3
time3 <- time1 + 5
time4 <- time1 + 6
time5 <- time1 + 9
time6 <- time1 + 13
time7 <- time1 + 14

custom_id = c()

Sample <- data.frame(order_id = c(1:140),
                     order_date = as.Date(c(rep("2019-05-20", 20), rep("2019-05-21", 20), rep("2019-05-22", 20), rep("2019-05-23", 20), rep("2019-05-24", 20), rep("2019-05-25", 20), rep("2019-05-26", 20))),
                     order_time = c(time3, time2, time5, time1, time4, time6, time7),
                     table_id = c(sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE), sample(c(1:10), 10, replace = FALSE)),
                     custom_id = c(sample(c(1:70), 140, replace = TRUE)),
                     total_price = left_join(Sample, Example_tot_price, by = ("order_id" = "order_id"))$final_price)

#추가코드
Sample1 <- Sample
order_time <- Sample$order_time #저장용

Sample$order_time <- as.character(Sample$order_time)
head(Sample)
h <- substr(Sample$order_time, 1, 2)
m <- substr(Sample$order_time, 3, 4)
s <- sample(c(10:59), 30)
Sample$order_time <- paste0(h, sep = ':', m, sep = ':', s)

head(Sample)
summary(Sample)
str(Sample)

#------------------------------------------------------------------------------


#메뉴
Menu <- data.frame(menu_name = c("소고기샤브", "소고기토마토샤브", "소고기커리샤브", "한우샤브_맑은육수", "한우샤브_매운육수", "한우샤브_커리육수"),
                   menu_price = c("9900", "10900", "10900", "15900", "15900", "15900"))

write.csv(Menu, "C:/Project/data/Menu.csv", row.names = FALSE)

#옵션
Option <- data.frame(menu_name = c("소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", 
                                   "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브",
                                   "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브",
                                   "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수",
                                   "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", 
                                   "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수"),
                     option_name = c("소고기", "모듬", "모듬버섯", "어묵꼬치", "모듬떡", "만두쌀떡", "모듬채소", "국수추가", "영양죽", "생수", "콜라",
                                     "소고기", "모듬", "모듬버섯", "어묵꼬치", "모듬떡", "만두쌀떡", "모듬채소", "국수추가", "영양죽", "생수", "콜라",
                                     "소고기", "모듬", "모듬버섯", "어묵꼬치", "모듬떡", "만두쌀떡", "모듬채소", "국수추가", "영양죽", "생수", "콜라",
                                     "한우", "모듬", "모듬버섯", "어묵꼬치", "모듬떡", "만두쌀떡", "모듬채소", "국수추가", "영양죽", "생수", "콜라",
                                     "한우", "모듬", "모듬버섯", "어묵꼬치", "모듬떡", "만두쌀떡", "모듬채소", "국수추가", "영양죽", "생수", "콜라",
                                     "한우", "모듬", "모듬버섯", "어묵꼬치", "모듬떡", "만두쌀떡", "모듬채소", "국수추가", "영양죽", "생수", "콜라"),
                     option_price = c("4000", "3000", "1500", "1500", "1000", "1000", "1500", "1000", "1000", "500", "1000", 
                                      "4000", "3000", "1500", "1500", "1000", "1000", "1500", "1000", "1000", "500", "1000",
                                      "4000", "3000", "1500", "1500", "1000", "1000", "1500", "1000", "1000", "500", "1000",
                                      "8000", "3000", "1500", "1500", "1000", "1000", "1500", "1000", "1000", "500", "1000",
                                      "8000", "3000", "1500", "1500", "1000", "1000", "1500", "1000", "1000", "500", "1000",
                                      "8000", "3000", "1500", "1500", "1000", "1000", "1500", "1000", "1000", "500", "1000"))

write.csv(Option, "C:/Project/data/Option.csv", row.names = FALSE)

#재료
Ingredient <- data.frame(ing_name = c("소고기재료", "한우재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "국수면", "쌀", "어묵", "계란", "물", "탄산", "다시다", "커리", "토마토"),
                         stock = c("220", "190", "130", "130", "130", "150", "150", "150", "150", "200", "200", "250", "470", "350", "500", "1000", "1000", "740", "680", "150"),
                         expiration_date = c("5", "5", "4", "4", "4", "2", "2", "3", "6", "6", "10", "60", "150", "3", "20", "500", "450", "365", "365", "5"),
                         ing_price = c("4000", "8000", "500", "500", "80", "500", "500", "100", "150", "150", "400", "200", "350", "220", "130", "40", "30", "50", "450", "370"),
                         recevie_date=as.Date(c("2019-05-25", "2019-05-25", "2019-05-26", "2019-05-26", "2019-05-26", "2019-05-25", "2019-05-25", "2019-05-26", "2019-04-17", "2019-04-17", "2019-05-22", "2019-05-20", "2019-05-11", "2019-05-26", "2019-05-21", "2019-04-15", "2019-04-15", "2019-05-07", "2019-05-07", "2019-05-24")),
                         supplier = c("농협", "농협", "농산물도매", "농산물도매", "농산물도매", "이마트", "이마트", "농산물도매", "코스트코", "코스트코", "이마트", "코스트코", "이마트", "이마트", "이마트", "코스트코", "코스트코", "코스트코", "코스트코", "농산물도매"),
                         ing_cost = c("2000", "5000", "300", "300", "30", "300", "350", "40", "60", "60", "220", "90", "120", "90", "70", "20", "25", "15", "230", "160"))

write.csv(Ingredient, "C:/Project/data/Ingredient.csv", row.names = FALSE)

#메뉴_옵션 #order에 따라 쌓이는 데이터
Menu_Option <- data.frame(menu_name = Example$menu_name,
                          option_name = Example$option_name,
                          order_id = Example$order_id,
                          count = Example$count)

write.csv(Menu_Option, "C:/Project/data/Menu_Option.csv", row.names = FALSE)

#메뉴_재료
Menu_Ingredient <- data.frame(menu_name = c("소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브", "소고기샤브",
                                             "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브", "소고기토마토샤브",
                                             "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브", "소고기커리샤브",
                                             "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수", "한우샤브_맑은육수",
                                             "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", "한우샤브_매운육수", 
                                             "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수", "한우샤브_커리육수"),
                              ing_name = c("소고기재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "다시다",
                                           "소고기재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "커리",
                                           "소고기재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "토마토",
                                           "한우재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "다시다",
                                           "한우재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "커리",
                                           "한우재료", "배추", "단호박", "숙주", "느타리버섯", "팽이버섯", "청경채", "치즈떡", "쌀떡", "만두", "토마토"))

write.csv(Menu_Ingredient, "C:/Project/data/Menu_Ingredient.csv", row.names = FALSE)

#옵션_재료
Option_Ingredient <- data.frame(option_name = c("소고기", "한우", 
                                                "모듬", "모듬", "모듬", "모듬", "모듬", "모듬", "모듬",
                                                "모듬버섯", "모듬버섯", 
                                                "어묵꼬치", 
                                                "모듬떡", "모듬떡",
                                                "만두쌀떡", "만두쌀떡",
                                                "모듬채소", "모듬채소", "모듬채소", "모듬채소", 
                                                "국수추가",
                                                "영양죽", "영양죽", 
                                                "생수", "콜라"),
                                ing_name = c("소고기재료", "한우재료", 
                                             "배추", "단호박", "숙주", "청경채", "치즈떡", "쌀떡", "만두",
                                             "느타리버섯", "팽이버섯",
                                             "어묵",
                                             "치즈떡", "쌀떡",
                                             "만두", "쌀떡",
                                             "배추", "단호박", "숙주", "청경채",
                                             "국수면",
                                             "쌀", "계란",
                                             "물", "탄산"))

write.csv(Option_Ingredient, "C:/Project/data/Option_Ingredient.csv", row.names = FALSE)

#데이터 생성 시 겹치지 않게
#테이블 번호 1-10개
#날짜는 일주일치 현재기준 2019-05-26

#주문
Order <- data.frame(order_id = Sample$order_id,
                    order_date = Sample$order_date, 
                    order_time = Sample$order_time,
                    custom_id = Sample$custom_id,
                    table_id = Sample$table_id,
                    total_price = Example_tot_price$final_price)

Order1 <- data.frame(order_id = Sample$order_id,
                    order_date = Sample$order_date, 
                    order_time = Sample$order_time,
                    table_id = Sample$table_id,
                    total_price = Example_tot_price$final_price)

Order2 <- data.frame(order_id = Sample$order_id,
                     order_date = Sample$order_date, 
                     order_time = Sample$order_time,
                     custom_id = Sample$custom_id,
                     table_id = Sample$table_id,
                     total_price = Example_tot_price$final_price)

#총액 10퍼센트 할인되어야함.

write.csv(Order, "C:/Project/data/Order.csv", row.names = FALSE)
write.csv(Order1, "C:/Project/data/Order.csv", row.names = FALSE)
write.csv(Order2, "C:/Project/data/Order.csv", row.names = FALSE)


#주문내역
Order_info <- data.frame(order_id = Example_Order$order_id,
                         menu_name = Example_Order$menu_name,
                         count = Example_Order$menu_count)

write.csv(Order_info, "C:/Project/data/Order_info.csv", row.names = FALSE)

#고객

#70명
name1 <- sample(c("김", "이", "박", "최", "정", "강", "조", "윤", "장"), 70, replace = TRUE)
name2 <- sample(c("선", "정", "혜", "지", "예", "소", "수", "이", "다", "유"), 70, replace = TRUE)
name3 <- sample(c("애", "승", "원", "림", "아", "영", "환", "우", "연", "현"), 70, replace = TRUE)
name <- paste0(name1, name2, name3, collapse = NULL)

p1 <- c(rep("010", 70))
p2 <- sample(c(1000:9999), 70)
p3 <- sample(c(1000:9999), 70)
phone <- paste0(p1, sep="-", p2, sep="-", p3, collapse = NULL)
#전화번호 바뀜...

Customer <- data.frame(custom_id = c(1:70),
                       custom_name = name,
                       phone_num = phone,
                       ewhain = sample(c(TRUE, FALSE), 70, replace = TRUE))

#추가 코드
phone_num <- Customer$phone_num #원래 폰번호 저
Customer$phone_num <- sub("70-", "010-", Customer$phone_num)  

write.csv(Customer, "C:/Project/data/Customer.csv", row.names = FALSE)

#예약
Reservation <- data.frame(reserve_id = Reserve$order_id,
                          people_count = Reserve$people_count,
                          reserve_date = Reserve$order_date,
                          reserve_time = Reserve$order_time,
                          custom_id =  Reserve$custom_id)

Reservation2 <- data.frame(reserve_id = Reserve1$order_id,
                          people_count = Reserve1$people_count,
                          reserve_date = Reserve1$order_date,
                          reserve_time = Reserve1$order_time,
                          custom_id =  Reserve1$custom_id)

#추가코드
Reservation1 <- Reservation
Reservation1$reserve_time <- as.character(Reservation1$reserve_time)
head(Reservation1)
h <- substr(Reservation1$reserve_time, 1, 2)
m <- substr(Reservation1$reserve_time, 3, 4)
s <- sample(c(10:59), 30)
Reservation1$reserve_time <- paste0(h, sep = ':', m, sep = ':', s)

write.csv(Reservation, "C:/Project/data/Reservation.csv", row.names = FALSE)
write.csv(Reservation2, "C:/Project/data/Reservation.csv", row.names = FALSE)
