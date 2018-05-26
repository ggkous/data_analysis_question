library(RODBC)
library(DBI)
library(RMySQL)
library(gWidgets)
library(rJava)
library(xlsxjars)
library(dbConnect)
library(gWidgets)
library(xlsx)
library(RMySQL)
library(dplyr)

#connect database
connect <-
  dbConnect(
    MySQL(),
    dbname = "visit",
    username = "root",
    password = "iT!14115",
    host = "localhost"
  )
dbListTables(connect)

df <-  dbGetQuery(connect , "select * from visitors")
#format datetime
df$date.time <- as.POSIXct(df$time, format = "%H:%M:%S")

#str(df)

#How many visits are in the data set?
num.visit <- nrow(df)
#print(num.visit)
#ANS:357912 num.visit

#How many distinct users are in the data set?
distinct.user <-  nrow(distinct(select(df, user_id)))
#print(distinct.user)
#ANS: 64265 distinct.user

#How many distinct pages are in the data set?
distinct.page <-  nrow(distinct(select(df, page_id)))
#print(distinct.page)
#ANS: 15163 distinct.page

#Which hour gives the smallest number of visits?
library(lubridate)
df$date.time <- as.POSIXct(df$time, format = "%H:%M:%S")

#find the range of hour and provide standard format
df <-
  df %>% mutate(time.hour.range = paste(
    format(floor_date(date.time, 'hour'), "%H:%M:%S"),
    format(floor_date(date.time, 'hour') + 60 * 59, "%H:%M:%S"),
    sep = "-"
  ))

df.hour.range <-
  df %>% group_by(time.hour.range) %>% summarize(count.visit = n()) %>% arrange(count.visit)

#ANS: the smallest number of visits
print(df.hour.range[1, c('time.hour.range', 'count.visit')])

#Which hour gives the largest number of visits?
tail(df.hour.range, 1)

#Which page has the largest number of visits in the data set?
#What is the corresponding number of visits?
page.visitors.count <-
  df %>% group_by(page_id) %>% mutate(count.visitors = n())

page.max.visitors <-
  unique(page.visitors.count[which(page.visitors.count$count.visitors == max(page.visitors.count$count.visitors)), c("page_id", 'count.visitors')])

#print(page.max.visitors)
#  page_id count.visitors
#  1    3897          26625

library(ggplot2)
library(plotly)
#Q1: Number of visits per hour

#change order for the hour
df.hour.range <- df.hour.range %>% arrange(time.hour.range)

visit.per.hour <-
  tidyr::separate(df.hour.range,
                  time.hour.range,
                  into = c("start.hr", "end.hr"),
                  "-")

visit.per.hour$hour <-
  factor(visit.per.hour$start.hr, labels = c(0:23))

q1 <- ggplot(visit.per.hour, aes(x = hour, y = count.visit)) +
  geom_bar(stat = "identity", aes(fill = hour)) +
  labs(title = "Number of visits per hour",
       x = "hour",
       y = "count",
       fill = "hour")
q1.plotly <- ggplotly(q1)
#print(q1.plotly)

#Q2: Cumulative number of visits per hour
total <- 0
visit.per.hour$total.visits <-
  total + cumsum(visit.per.hour$count.visit)

q2 <- ggplot(visit.per.hour, aes(x = hour, y = total.visits)) +
  geom_bar(stat = "identity", aes(fill = hour)) +
  labs(title = "Cumulative number of visits per hour",
       x = "hour",
       y = "count",
       fill = "hour")
q2.plotly <- ggplotly(q2)
print(q2.plotly)

#Number of new visitors per hour

#find unique visitor and their first visit time
new.visitor.first.visit <-
  df %>% group_by(user_id, time.hour.range) %>% summarise(unique_visitor_count = n_distinct(user_id)) %>% filter(row_number() == 1)


#count the total of new visitor
new.visits <-
  new.visitor.first.visit %>% group_by(time.hour.range) %>% summarise(total.new.visitor = sum(unique_visitor_count))
new.visits$hour <-
  factor(new.visits$time.hour.range, labels = c(0:23))

#visualization
q3 <- ggplot(new.visits, aes(x = hour, y = total.new.visitor)) +
  geom_bar(stat = "identity", aes(fill = hour)) +
  labs(title = "Number of new visitors per hour",
       x = "hour",
       y = "count",
       fill = "hour")
q3.plotly <- ggplotly(q3)
print(q3.plotly)


#Cumulative number of new visitors per hour
total <- 0
new.visits$cum.total.visits <-
  total + cumsum(new.visits$total.new.visitor)

q4 <- ggplot(new.visits, aes(x = hour, y = cum.total.visits)) +
  geom_bar(stat = "identity", aes(fill = hour)) +
  labs(
    title = "Cumulative number of new visitors per hour
    ",
    x = "hour",
    y = "count",
    fill = "hour"
  )
q4.plotly <- ggplotly(q4)
print(q4.plotly)

library(htmlwidgets)
save.graph <- function(graph, name) {
  directory <- paste0("D:/hk01", name, ".html")
  
  htmlwidgets::saveWidget(as_widget(graph),
                          selfcontained = TRUE,
                          file = directory)
}
save.graph(q1.plotly, "q1")
save.graph(q2.plotly, "q2")
save.graph(q3.plotly, "q3")
save.graph(q4.plotly, "q4")