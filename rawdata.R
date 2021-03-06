D2016 <- read.csv("C:/Project/data/상가업소_201612_01.csv", header = T)
D2017 <- read.csv("C:/Project/data/상가업소_201712_011.csv", header = F)
D2018 <- read.csv("C:/Project/data/상가업소_201812_01.csv", header = T)

D2016 <- D2016[,-c(3, 10, 11, 12, 14, 16, 18, 20:35, 37)]
D2017 <- D2017[,-c(3, 10, 11, 12, 14, 16, 18, 20:35, 37)]
D2018 <- D2018[,-c(3, 10, 11, 12, 14, 16, 18, 20:35, 37)]

colnames(D2017) <- c("상가업소번호", "상호명", "상권업종대분류코드", "상권업종대분류명 ", "상권업종중분류코드", 
                     "상권업종중분류명", "상권업종소분류코드", "상권업종소분류명", "시도명", "시군구명", "행정동명",
                     "법정동명", "층정보", "경도", "위도")

D2016 <- D2016[D2016$상권업종대분류코드 == 'Q', ]
D2017 <- D2017[D2017$상권업종대분류코드 == 'Q', ]
D2018 <- D2018[D2018$상권업종대분류코드 == 'Q', ]

D2016 <- D2016[D2016$시군구명 %in% c("서대문구", "마포구"), ]
D2017 <- D2017[D2017$시군구명 %in% c("서대문구", "마포구"), ]
D2018 <- D2018[D2018$시군구명 %in% c("서대문구", "마포구"), ]

D2016 <- D2016[,-c(3, 5, 7)]
D2017 <- D2017[,-c(3, 5, 7)]
D2018 <- D2018[,-c(3, 5, 7)]

D2016 <- D2016[,-c(11:12)]
D2017 <- D2017[,-c(11:12)]
D2018 <- D2018[,-c(11:12)]

head(D2016)
head(D2017)
head(D2018)

str(D2016)
str(D2017)
str(D2018)

summary(D2016$상호명)

write.csv(D2016, "C:/Project/data/상가업소_2016_new.csv", row.names = FALSE)
write.csv(D2017, "C:/Project/data/상가업소_2017_new.csv", row.names = FALSE)
write.csv(D2018, "C:/Project/data/상가업소_2018_new.csv", row.names = FALSE)
