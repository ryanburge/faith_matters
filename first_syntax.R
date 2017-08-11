library(tidyverse)
library(forcats)
library(extrafont)
library(car)

faith <- read_dta("D://faith_matters/faith.DTA")


faith <- faith %>% 
  mutate(womchrch = as.numeric(womchrch)) %>%
  mutate(w1 = recode(womchrch, "1=1; else= 0")) %>% 
  mutate(w2 = recode(womchrch, "2=1; else=0")) %>% 
  mutate(wom2 = as.numeric(womchrha)) %>% 
  mutate(ww1 = recode(wom2, "1=1; else=0")) %>% 
  mutate(ww2 = recode(wom2, "2=1; else=0")) %>% 
  mutate(strongyes = w1 + ww1) %>% 
  mutate(yes = w1 + ww2) %>% 
  mutate(no = w2 +ww2) %>% 
  mutate(strongno = w2 + ww1) %>% 
  mutate(strongyes = recode(strongyes, "2=4; else=0")) %>% 
  mutate(yes = recode(yes, "2=3; else=0")) %>% 
  mutate(no = recode(no, "2=2; else=0")) %>% 
  mutate(strongno = recode(strongno, "2=1; else=0")) %>% 
  mutate(final = strongyes + yes + no + strongno) 

faith <- faith %>% 
  mutate(final = recode(final, "1= 'Strongly Disagree';
                                2= 'Disagree';
                                3= 'Agree';
                                4= 'Strongly Agree'"))

faith$final <- as.factor(faith$final)

faith$final <- fct_relevel(faith$final, "Strongly Disagree", "Disagree", "Agree", "Strongly Agree")

  
faith %>% 
  filter(final != 0) %>% 
  ggplot(., aes(x=final)) + geom_bar(fill = "seagreen1", color = "black", aes(y = (..count..)/sum(..count..)))  + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Response", y= "Percent of Sample", title = "Women should be allowed to be priests or clergy in my house of worship?", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))

faith$reborn <- recode(faith$bornagn, "1='Born Again'; else='Not Born Again'")

faith$reborn <- as.factor(faith$reborn)

faith %>% 
  filter(final != 0) %>% 
  filter(bapt ==1) %>% 
  ggplot(., aes(x=final)) + geom_bar(fill = "seagreen1", color = "black", aes(y = (..count..)/sum(..count..)))  + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - Southern Baptists", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) 
 
gender <- faith %>% 
  group_by(gender) %>% 
  count(final, wt = weight) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(gender) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = recode(gender, "1= 'Male'; 2 = 'Female' ")) %>% 
  filter(final != 0) %>% 
  select(gender, final, weight)

gender %>% 
  ggplot(., aes(x=final, y=weight, fill = gender)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - By Gender", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))  +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())

pid <- faith %>% 
  filter(partyid <= 3) %>% 
  group_by(partyid) %>% 
  count(final, wt = weight) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(partyid) %>% 
  mutate(partyid = as.numeric(partyid)) %>%
  mutate(pid = recode(partyid, "1 = 'Republican'; 2= 'Democrat'; 3= 'Independent'")) %>% 
  filter(final != 0) %>% 
  select(pid, final, weight)

pid %>% 
  ggplot(., aes(x=final, y=weight, fill = pid)) + geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Response", y= "Percent of Sample", title = "Female Clergy - Party Identification", caption = "Data: Faith Matters 2006" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_manual("legend", values = c("Democrat" = "dodgerblue3", "Independent" = "gray", "Republican" = "firebrick3"))

gay <- faith %>% 
  filter(gaywed <= 2) %>% 
  group_by(gaywed) %>% 
  count(final, wt = weight) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(gaywed) %>% 
  mutate(gaywed = as.numeric(gaywed)) %>%
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
  theme(text=element_text(size=18, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank()) + 
  scale_fill_manual("legend", values = c("Gay Marriage" = "dodgerblue3", "Civil Unions" = "gray", "Neither" = "firebrick3"))

attend <- faith %>% 
  filter(relatend <= 9) %>% 
  group_by(relatend) %>% 
  count(final, wt = weight) %>%
  mutate(weight = prop.table(n)) %>% 
  ungroup(relatend) %>% 
  mutate(relatend = as.numeric(relatend)) %>%
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
  theme(text=element_text(size=18, family="KerkisSans"))   +
  theme(legend.position = "bottom") + theme(legend.title=element_blank())



