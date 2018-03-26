
#==========================================================
#   Mini Project for Data Wrangling and Visualization
#   Data Sources: 
#   - http://www.bbc.com/news/science-environment-37578899
#   - http://dantri.com.vn/the-gioi/vi-sao-nhung-nguoi-doat-giai-nobel-ngay-cang-cao-tuoi-20161008095047624.htm
#   - http://api.nobelprize.org/v1/laureate.csv
#==========================================================

# Load dữ liệu: 

rm(list = ls())
library(tidyverse)
library(magrittr)
path <- "http://api.nobelprize.org/v1/laureate.csv"
laureate <- read_csv(path)

# Xem qua dữ liệu: 
laureate %>% head()
laureate$gender %>% table()

# Chỉ lấyra bộ dữ liệu mà giải trao cho cá nhân hoặc nhóm cá nhân 
# chứ không phải tổ chức: 

mydf2 <- laureate %>% filter(gender != "org")

# Phân bố về giới tính: 

mydf2 %>% 
  group_by(gender) %>% 
  count() -> gender_df


theme_set(theme_minimal())

gender_df %>% 
  ggplot(aes(gender, n)) + 
  geom_col()

# Phân bố giới tính theo nghành: 
mydf2 %>% 
  group_by(gender, category) %>% 
  count() %>% 
  na.omit() %>% 
  ggplot(aes(category, n, fill = gender)) + 
  geom_col(position = "fill")

df2 %>% 
  group_by(gender, category) %>% 
  count() %>% 
  na.omit() %>% 
  ggplot(aes(gender, n)) + 
  geom_col() + 
  facet_wrap(~ category, scales = "free")


# 25 Quốc gia sinh ra nhiều nhà khoa học dành giải Nobel nhất: 

top25 <- mydf2 %>% 
  group_by(bornCountry) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1:25)

# Cái hình này cần hiệu chỉnh ở hai điểm (now Polannd) và tên (the Netherlands): 
top25 %>% ggplot(aes(reorder(bornCountry, n), n, fill = bornCountry)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  coord_flip() +  
  theme_bw() + 
  geom_text(aes(label = n), hjust = -.3, size = 3, colour = "black") + 
  labs(x = NULL, 
       y = NULL, 
       title = "The 25 Countries With the Most Nobel Laureates", 
       caption = "Data Source: Nobelprize.org")


# 25 trường đại học có nhiều giải Nobel nhất: 
mydf2 %>% 
  filter(!is.na(name)) %>% 
  group_by(name) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1:25) %>% 
  ggplot(aes(reorder(name, n), n, fill = name)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  coord_flip() +  
  theme_bw() + 
  geom_text(aes(label = n), hjust = -.3, size = 3, colour = "black") + 
  labs(x = NULL, 
       y = NULL, 
       title = "The 25 Universities With the Most Nobel Laureates", 
       caption = "Data Source: Nobelprize.org")

# Tính tuổi tại thời điểm đoạt giải. Chú ý phân biệt  hàm year()
# của gói lubridate và biến year (năm đoạt  giải) của chủ  nhân: 
library(lubridate)
mydf2 <- mydf2 %>% 
  mutate(age = year - year(born))

# Ở các bộ môn như  Hóa, Văn Học, Hòa  Bình và Vật Lý
# nữ giới thường nhiều tuổi hơn nam khi có  giải: 
mydf2 %>% filter(!is.na(category)) %>% 
  ggplot(aes(gender, age, fill = gender)) + 
  geom_boxplot(show.legend = F, alpha = 0.5) + 
  facet_wrap(~ category, scales = "free")


#  Tổng thể thì  nữ vẫn  già hơn đồng nghiệp nam của họ: 
mydf2 %>% filter(!is.na(category)) %>% 
  ggplot(aes(gender, age, fill = gender)) + 
  geom_boxplot(show.legend = F, alpha = 0.5) + 
  theme_bw()

# Phân bố về  tuổi của các nhà khoa học khi nhận giải: 
mydf2 %>% filter(!is.na(category)) %>% 
  ggplot(aes(category, age, fill = category)) + 
  geom_boxplot(show.legend = F, alpha = 0.5)

# Trừ Hòa Bình, độ tuổi khi nhận giải của  các nhà khoa học đang tăng dần theo thời gian: 

mydf2 %>% filter(!is.na(category)) %>% 
  ggplot(aes(year, age)) + 
  geom_point() + 
  geom_smooth(method = "loess") + 
  facet_wrap(~ category) + 
  theme_bw() + 
  labs(x = NULL,
       y = "Age (years)", 
       title = "Age of Nobel Laureates Over Time by Field")

# Hoặc  kiểu khác: 
mydf2 %>% filter(!is.na(category)) %>% 
  ggplot(aes(year, age, colour = category)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method = "loess", show.legend = FALSE, colour = "blue") + 
  facet_wrap(~ category) + 
  theme_bw() + 
  labs(x = NULL,
       y = "Age (years)", 
       title = "Age of Nobel Laureates Over Time by Field")


# Mật độ các nhà khoa học được giải Nobel theo nơi sinh của họ: 

nation <- mydf2 %>% 
  group_by(bornCountryCode) %>% 
  count() %>% 
  ungroup() %>% 
  rename(iso2 = bornCountryCode, 
         numberPrize = n)


library(countrycode)
code_name <- countrycode(nation$iso2, "iso2c", "country.name")

nation %<>% mutate(country_name = as.character(code_name))


library("highcharter")
highchart(type = "map") %>% 
  hc_add_series_map(map = worldgeojson, df = nation, value = "numberPrize", joinBy = "iso2") %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "")




