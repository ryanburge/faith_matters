library(tidyverse)
library(forcats)
library(extrafont)
library(car)

faith <- read_csv("https://raw.githubusercontent.com/ryanburge/faith_matters/master/faith2011.csv")


faith <- faith %>% 
  mutate(womchrch = as.numeric(WOMCHR11)) %>%
  mutate(final = recode(womchrch, "4=3; 5=4"))
 
faith <- faith %>% 
  mutate(wom2 = recode(final, "3:4=1; else=0"))

faith <- faith %>% 
  mutate(final = recode(final, "1= 'Disagree Strongly';
                        2= 'Disagree Somewhat';
                        3= 'Agree Somewhat';
                        4= 'Agree Strongly'; else =0"))

faith$final <- as.factor(faith$final)

faith$final <- fct_relevel(faith$final, "Disagree Strongly", "Disagree Somewhat", "Agree Somewhat", "Agree Strongly")


full11 <- faith %>% 
  filter(final != 0) %>% 
  count(final, wt = WGTTOT11) %>%  mutate(weight = prop.table(n)) %>% mutate(year = c("2011"))

full %>% 
  filter(final != 0) %>% 
  ggplot(., aes(x=reorder(final, -weight), y=weight)) + geom_col(fill = "seagreen1", color = "black")  + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Response", y= "Percent of Sample", title = "Women should be allowed to be priests or clergy in my house of worship?", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans")) +
  theme(plot.title = element_text(face="bold"))

ggsave(file="female_clergy_overall.png", type = "cairo-png", width = 16, height =12)


bap <- faith %>% 
  filter(BAPT11 ==1 & final != 0) %>% 
  count(wom2, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n), label = c("Southern Baptists")) 

cath <- faith %>% 
  filter(RELIG11 ==2 & final != 0) %>% 
  count(wom2, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n), label = c("Roman Catholics")) 

umc <- faith %>% 
  filter(METH11 ==1 & final != 0) %>% 
  count(wom2, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n), label = c("United Methodists")) 

compare <- bind_rows(bap, cath, umc) %>% filter(wom2 ==1) %>% select(label, weight)

compare %>% 
  ggplot(., aes(x=reorder(label, -weight), y=weight)) + geom_col(fill = "darkorchid4", color = "black")  + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Response", y= "Percent of Sample", title = "Agreement with Female Clergy by Tradition", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=42, family="KerkisSans")) +
  theme(plot.title = element_text(face="bold"))

ggsave(file="cath_sbc_meth_female_clergy_2011.png", type = "cairo-png", width = 20, height =12)


gender <- faith %>% 
  group_by(GENDER11) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(GENDER11) %>% 
  mutate(GENDER11 = as.numeric(GENDER11)) %>% 
  mutate(gender = recode(GENDER11, "1= 'Male'; 2 = 'Female' ")) %>% 
  filter(final != 0) %>% 
  select(gender, final, weight)

gender$final <- fct_relevel(gender$final, "Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree Strongly")

gender %>% 
  ggplot(., aes(x=final, y=weight, fill = gender)) + geom_col(position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Support for Female Clergy - By Gender", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=42, family="KerkisSans"))  +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())  +
  theme(plot.title = element_text(face="bold"))

ggsave(file="gender_female_clergy.png", type = "cairo-png", width = 16, height =12)


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

pid$final <- fct_relevel(pid$final, "Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree Strongly")


pid %>% 
  ggplot(., aes(x=final, y=weight, fill = pid)) + geom_col(position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Support for Female Clergy - Party Identification", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=42, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_manual("legend", values = c("Democrat" = "dodgerblue3", "Independent" = "gray", "Republican" = "firebrick3")) +
  theme(plot.title = element_text(face="bold"))

ggsave(file="pid_female_clergy.png", type = "cairo-png", width = 16, height =12)


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
gay$final <- fct_relevel(gay$final, "Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree Strongly")


gay %>% 
  ggplot(., aes(x=final, y=weight, fill = gay)) + geom_col(position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Support for Female Clergy - Gay Marriage", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=28, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_manual("legend", values = c("Gay Marriage" = "dodgerblue3", "Civil Unions" = "gray", "Neither" = "firebrick3")) +
  theme(plot.title = element_text(face="bold"))

ggsave(file="gay_female_clergy.png", type = "cairo-png", width = 16, height =12)


attend <- faith %>% 
  filter(RELATT11 <= 9) %>% 
  filter(final != 0) %>% 
  mutate(relatend = as.numeric(RELATT11)) %>%
  mutate(attend  = recode(relatend, "1:2=1; 3:4= 2; 5:6=3; 7:8=4; 9=5")) %>% 
  group_by(attend) %>% 
  count(final, wt = WGTTOT11) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(attend) %>% 
  mutate(attend = recode(attend, "1 = 'Weekly'; 2= 'Monthly'; 3 = 'Several Times a Year'; 4= 'Once a Year'; 5= 'Never'")) %>% 
  select(attend, final, weight) %>% 
  mutate(attend = as.factor(attend)) 

attend$attend <- fct_relevel(attend$attend, "Never", "Once a Year", "Several Times a Year", "Monthly", "Weekly")

attend$attend <- fct_rev(attend$attend)
attend$final <- fct_relevel(attend$final, "Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree Strongly")



attend %>% 
  ggplot(., aes(x=final, y=weight, fill = attend)) + geom_col(position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Support for Female Clergy - Church Attendance", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=42, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())  + 
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(face="bold"))


ggsave(file="attend_female_clergy.png", type = "cairo-png", width = 16, height =12)


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


age$final <- fct_relevel(age$final, "Agree Strongly", "Agree Somewhat", "Disagree Somewhat", "Disagree Strongly")

age %>% 
  ggplot(., aes(x=final, y=weight, fill = age)) + geom_col(position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Support for Female Clergy - Age", caption = "Data: Faith Matters 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=42, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(face="bold"))



ggsave(file="age_female_clergy.png", type = "cairo-png", width = 16, height =12)

full2$final <- fct_rev(full2$final)

full2 %>% 
  ggplot(., aes(x=final, y=weight, fill = year)) + geom_col(position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Women should be allowed to be priests or clergy in my house of worship", caption = "Data: Faith Matters 2006 + 2011" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=26, family="KerkisSans"))  +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())  +
  theme(plot.title = element_text(face="bold"))


ggsave(file="both_years_approve.png", type = "cairo-png", width = 16, height =12)

