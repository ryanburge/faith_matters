library(tidyverse)
library(forcats)
library(extrafont)
library(car)

faith <- read_csv("https://raw.githubusercontent.com/ryanburge/faith_matters/master/faith2011.csv")


faith <- faith %>% 
  mutate(womchrch = as.numeric(WOMCHR11)) %>%
  mutate(final = recode(womchrch, "4=3; 5=4"))
 

faith <- faith %>% 
  mutate(final = recode(final, "1= 'Disagree Strongly';
                        2= 'Disagree Somewhat';
                        3= 'Agree Somewhat';
                        4= 'Agree Strongly'; else =0"))

faith$final <- as.factor(faith$final)

faith$final <- fct_relevel(faith$final, "Disagree Strongly", "Disagree Somewhat", "Agree Somewhat", "Agree Strongly")


full <- faith %>% 
  filter(final != 0) %>% 
  count(final, wt = WGTTOT11) %>%  mutate(weight = prop.table(n))

full %>% 
  filter(final != 0) %>% 
  ggplot(., aes(x=final, y=weight)) + geom_col(fill = "seagreen1", color = "black")  + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Response", y= "Percent of Sample", title = "Women should be allowed to be priests or clergy in my house of worship?", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="female_clergy_overall.png", type = "cairo-png", width = 15, height =12)


bap <- faith %>% 
  filter(BAPT11 ==1 & final != 0) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n), label = c("Southern Baptists")) 

cath <- faith %>% 
  filter(RELIG11 ==2 & final != 0) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n), label = c("Roman Catholics")) 

compare <- bind_rows(bap, cath)

compare %>% 
  ggplot(., aes(x=final, y=weight)) + geom_col(fill = "darkorchid4", color = "black")  + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Response", y= "Percent of Sample", title = "Agreement with Female Clergy", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) + facet_grid(.~label)

ggsave(file="cath_sbc_female_clergy.png", type = "cairo-png", width = 20, height =12)


gender <- faith %>% 
  group_by(GENDER11) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(GENDER11) %>% 
  mutate(GENDER11 = as.numeric(GENDER11)) %>% 
  mutate(gender = recode(GENDER11, "1= 'Male'; 2 = 'Female' ")) %>% 
  filter(final != 0) %>% 
  select(gender, final, weight)

gender %>% 
  ggplot(., aes(x=final, y=weight, fill = gender)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - By Gender", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))  +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())

ggsave(file="gender_female_clergy.png", type = "cairo-png", width = 15, height =12)


pid <- faith %>% 
  filter(PARTY11 <= 3) %>% 
  group_by(PARTY11) %>% 
  count(final, wt =  WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(PARTY11) %>% 
  mutate(partyid = as.numeric(PARTY11)) %>%
  mutate(pid = recode(partyid, "1 = 'Republican'; 2= 'Democrat'; 3= 'Independent'")) %>% 
  filter(final != 0) %>% 
  select(pid, final, weight)

pid %>% 
  ggplot(., aes(x=final, y=weight, fill = pid)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - Party Identification", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_manual("legend", values = c("Democrat" = "dodgerblue3", "Independent" = "gray", "Republican" = "firebrick3"))

ggsave(file="pid_female_clergy.png", type = "cairo-png", width = 15, height =12)


gay <- faith %>% 
  filter(GAYWED11 <= 2) %>% 
  group_by(GAYWED11) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(GAYWED11) %>% 
  mutate(gaywed = as.numeric(GAYWED11)) %>%
  mutate(gay = recode(gaywed, "0= 'Neither'; 1 = 'Civil Unions'; 2= 'Gay Marriage'")) %>% 
  filter(final != 0) %>% 
  select(gay, final, weight) %>% 
  mutate(gay = as.factor(gay)) 


gay$gay <- fct_relevel(gay$gay, "Neither", "Civil Unions", "Gay Marriage")


gay %>% 
  ggplot(., aes(x=final, y=weight, fill = gay)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - Gay Marriage", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_manual("legend", values = c("Gay Marriage" = "dodgerblue3", "Civil Unions" = "gray", "Neither" = "firebrick3"))

ggsave(file="gay_female_clergy.png", type = "cairo-png", width = 15, height =12)


attend <- faith %>% 
  filter(RELATT11 <= 9) %>% 
  group_by(RELATT11) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(RELATT11) %>% 
  mutate(relatend = as.numeric(RELATT11)) %>%
  mutate(attend = recode(relatend, "1 = 'Several Times a Week'; 2= 'Every Week'; 3 = 'Nearly Every Week'; 4= '2-3 Times a Month'; 5= 'About Once a Month'; 6= 'Several Times a Year'; 7='About Once or Twice a Year'; 8 ='Less than Once a Year'; 9 ='Never' ")) %>% 
  filter(final != 0) %>% 
  select(attend, final, weight) %>% 
  mutate(attend = as.factor(attend)) 

attend$attend <- fct_relevel(attend$attend, "Never", "Less than Once a Year", "About Once or Twice a Year", "Several Times a Year", "About Once a Month", "2-3 Times a Month", "Nearly Every Week", "Every Week", "Several Times a Week")


attend %>% 
  ggplot(., aes(x=final, y=weight, fill = attend)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - Church Attendance", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())

ggsave(file="attend_female_clergy.png", type = "cairo-png", width = 15, height =12)


faith <- faith %>% 
  mutate(age = recode(AGE11, "18:35 =1; 36:54 =2; 55:64 =3; 65:99 =4; else =0"))
  
age <- faith %>% 
  filter(age != 0) %>% 
  group_by(age) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(age) %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(age = recode(age, "1 = '18-35'; 2= '36-54'; 3 = '55-64'; 4= 'Over 65'")) %>% 
  filter(final != 0) %>% 
  select(age, final, weight) %>% 
  mutate(age = as.factor(age)) 

age %>% 
  ggplot(., aes(x=final, y=weight, fill = age)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - Age", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())

ggsave(file="age_female_clergy.png", type = "cairo-png", width = 15, height =12)


