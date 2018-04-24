library(tidyverse)
library(lubridate)


# read and parse the access log
log_file <- read_log("access.log")

# drop the first two columns that are filled with NA's
log_file %>%
  select(-c(X2,X3)) -> log_file

# set meaningful column names
colnames(log_file) <- c("client_ip","timestamp","request_type",
                        "status_code","size_of_reply","referer_URL","user_agent")

# convert timestamp to a proper date format
log_file$timestamp <- dmy_hms(log_file$timestamp)

log_file %>%
  select(client_ip) %>%
  group_by(client_ip) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




# plot count by daily,weekly,monthly

log_file %>%
  group_by(month =  as.factor(month(timestamp)), year =  as.factor(year(timestamp))) %>%
  summarise(visits = n()) %>%
  ggplot(., aes(month, year))+
  geom_tile(aes(fill = visits)) +
  scale_fill_gradient(low = "white", high = "red")

log_file %>%
  group_by(day_of_week = as.factor(wday(timestamp))) %>%
  summarise(visits = n()) %>%
  ggplot(., aes(day_of_week,visits))+
  geom_bar(aes(fill = day_of_week), stat = "identity")


log_file %>%
  group_by(month = as.factor(month(timestamp))) %>%
  summarise(visits = n()) %>%
  ggplot(., aes(month,visits))+
  geom_bar(aes(fill = month), stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom")

#

log_file %>%
  group_by(date =date(timestamp)) %>%
  summarise(visits = n()) %>%
  ggplot(., aes(date,visits))+
  geom_line(size = .2,alpha = .65) +
  scale_y_log10()


#
log_file %>%
  group_by(hour_of_day = (hour(timestamp)),day_of_week = as.factor(wday(timestamp))) %>%
  summarise(visits = n()) %>%
  ggplot(., aes(day_of_week,hour_of_day))+
  geom_tile(aes(fill = visits))+
  scale_fill_gradient(low = "yellow", high = "red")


log_file %>%
  group_by(status_code) %>%
  summarise(count = n()) %>%
  mutate(proportion_in_percentage = (count/sum(count))*100) %>%
  arrange(desc(count)) 


log_file %>%
  filter(status_code == 404) %>%
  group_by(client_ip) %>%
  summarise(count =n()) %>%
  arrange(desc(count)) %>%
  head(n=25) %>%
  ggplot(., aes(client_ip,count))+
  geom_bar(stat = "identity") +
  aes(x=reorder(client_ip,count))+
  coord_flip() 

log_file %>%
  filter(status_code == 404) %>%
  group_by(client_ip) %>%
  summarise(count =n()) %>%
  arrange(desc(count)) %>%
  left_join(., log_file) %>%
  group_by(request_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

log_file %>%
  filter(client_ip == "93.158.147.8") %>%
  group_by(year = as.factor(year(timestamp)) ,month = as.factor(month(timestamp)),
           request_type, status_code =  as.factor(status_code)) %>%
  summarise(count = n()) %>%
  ggplot(., aes(month,count))+
  geom_bar(aes(fill = year),stat = "identity")+
  facet_wrap(~request_type, ncol = 2) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

log_file %>%
  filter(client_ip == "93.158.147.8") %>%
  select(user_agent)


log_file %>%
  filter(client_ip == "93.158.147.8") %>%
  group_by(user_agent) %>%
  summarise(count = n())


log_file %>%
  filter(str_detect(log_file$user_agent,"[A-z]{1,}Bot|[A-z]{1,}bot")) %>%
  pull(user_agent) %>%
  str_extract(.,"[A-z]{1,}Bot|[A-z]{1,}bot") %>%
  data_frame(botname =.) %>%
  group_by(botname) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
