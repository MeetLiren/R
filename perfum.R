library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(ggthemes)


get_perfum <- function(url){
  
  page <- read_html(url)
  
  `商业香水top200` <- tibble(
    # 抓取香水名称
    row1 = page %>%
      html_nodes("div.trade-article div.trade-content-article h2 a") %>% 
      html_text(),
    
    # 抓取香水评分
    row2 = page%>%
      html_nodes("div.score span.tiaoz") %>% 
      html_text(),
    
    #抓取香水详情网址
    row3 = page %>% 
      html_nodes("div.trade-article div.trade-content-article h2 a") %>% 
      html_attr("href") %>% 
      paste0("https://www.nosetime.com", .),
    
    #抓取香水具体细节
    row4 = page %>% 
      html_nodes("div.intro") %>% 
      html_text() %>%  
      str_replace_all("\n\t\t\t\t\t\t\t", "") %>% 
      str_replace_all("\n\t\t\t\t\t\t", "") %>% 
      sub("\\s+", "", .)
  
  )
  
}


urls1 <- str_c("https://www.nosetime.com/top200.php?type=trade&page=",1:10,"#list")

Sys.time(10)

perfums <- urls1 %>% 
  map_df(get_perfum)

perfums
View(perfums)

perfums$row1[11] = paste0(perfums$row1[11], ", 1970") 
perfums$row1[19] = paste0(perfums$row1[19], ", 2007")
perfums$row1[171] = paste0(perfums$row1[171], ", 2006")
perfums$row1[195] = paste0(perfums$row1[195], ", 1960")
perfums$row1[46] = paste0("爱马仕 ", perfums$row1[46])
perfums$row1[53] = paste0("爱马仕 ", perfums$row1[53])

aa <- tibble(
  
  `名称` = perfums$row1,

  `品牌` = perfums$row1 %>% 
    regexpr(" ", .) %>% 
    map(1) %>%  
    substr(map(perfums$row1, 1), 1, .),
  
  `评分` = perfums$row2 %>%
    regexpr("分", .) %>% 
    map(., ~. - 1) %>% 
    substr(perfums$row2, 1, .) %>% 
    map(1) %>% 
    as.numeric(),
  
  `年份` = perfums$row1 %>% 
    regexpr(",", .) %>% 
    map(1) %>%
    substring(map(perfums$row1, 1), .) %>% 
    str_replace_all(", ", "") %>% 
    map(1) %>% 
    as.numeric(),
  
  `属性` = perfums$row4 %>% 
    regexpr("属性：", .) %>% 
    map(1) %>% 
    map(~. + 3) %>% 
    substr(map(perfums$row4, 1), ., map(., ~. + 2)),
  
  `香调` = perfums$row4 %>% 
    regexpr(" 香调：", .) %>% 
    map(1) %>% 
    map(~. + 4)%>%
    substring(map(perfums$row4, 1), ., last = 100000L)
    
)

aa
View(aa)
########################################

y = c(
  nrow(filter(aa, `属性` == "男香 ")),
  nrow(filter(aa, `属性` == "女香 ")),
  nrow(filter(aa, `属性` == "中性香"))
  )
labels = c("男香", "女香", "中性香")
pie(y, labels, col=rainbow(3), radius = 0.9)

#########################################
mytable <- table(aa$`品牌`)
mytable
df <- as.data.frame(mytable) %>% 
  arrange(-Freq)

mytable1 <- table(aa$年份) %>% 
  as.data.frame() %>% 
  arrange(-Freq)

df %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = Freq, hjust = -0.5, color = Var1), show.legend = TRUE)+
  coord_flip()

p = df %>% 
  arrange(Freq) %>% 
  filter(Freq >= 10)
  
####################################################### 
aa %>%
  filter(`属性` == "男香 ") %>% 
  ggplot(aes(x = `年份`, y = `评分`))+
  geom_point(size = 3)

aa %>%
  filter(`属性` == "女香 ") %>% 
  ggplot(aes(x = `年份`, y = `评分`))+
  geom_point(size = 3)

aa %>%
  filter(`属性` == "中性香") %>% 
  ggplot(aes(x = `年份`, y = `评分`))+
  geom_point(size = 3)

aa %>% 
  ggplot(aes(x = `年份`, y = `评分`))+
  geom_point(size = 3)


View(arrange(aa, aa$年份))
############################################################

getdetail <- function(url){
  
  page <- read_html(url)
  
  tibble(
    `五星` = page %>% 
      html_nodes("ul.item_score div.nows:nth-child(1)") %>% 
      html_text()
  )
}

getdetail_safely <- safely(getdetail)

jobdetail <- perfums$row3 %>% 
  map(getdetail_safely)

b = jobdetail %>% 
  map_df("result") 

e = b$五星 %>% 
  regexpr("%", .) %>% 
  map(., ~. - 1) %>% 
  substr(b$五星, 1, .) %>% 
  as.numeric() %>% 
  map(., ~. * 0.01)

mys <- as.numeric()
mystar_std <- as.numeric()
for(i in 1:200){
   s = e[(5 * i - 4):(5 * i)] %>% 
     as.numeric() 
   
   mys[[i]] <- s[1]*10 + s[2]*8 + s[3]*6 + s[4]*4 + s[5]*2
   
   mystar_std[[i]] <- sum(c(
     (10 - mys[[i]])^2 * s[1]), 
     ((8 - mys[[i]])^2 * s[2]),
     ((6 - mys[[i]])^2 * s[3]), 
     ((4 - mys[[i]])^2 * s[4]), 
     ((2 - mys[[i]])^2 * s[5])) %>% 
     sqrt() %>% 
     print()
   
} 
##############################################################################

ss <- tibble(
  `名称` = aa$名称,
  `评分` = aa$评分,
  aa$香调,
  mys,
  mystar_std
)


ss %>% 
  arrange(-mystar_std)
View(ss)
###############################################################################
ss %>% 
  ggplot(aes(x = mys, y = mystar_std)) +
  geom_point()

fit <- lm(ss$mystar_std ~ ss$mys, data = ss)
summary(fit)
abline(fit)
plot(
  ss$mys,ss$mystar_std,
  xlab = "mys",
  ylab = "mystar_std"
)
coefficients(fit)

################################################################
sss <- tibble(
  mys,
  mystar_std
)
cor(sss, method = "pearson")


################################################################

#2018/11/23

##########################################################

