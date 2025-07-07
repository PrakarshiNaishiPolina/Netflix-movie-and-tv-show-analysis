library(readxl)
library(tidyverse)
library(lubridate)

netflix<-read_excel("netflix_titles.csv.xlsx")

names(netflix)<-tolower(gsub(" ","_",names(netflix)))

str(netflix)

glimpse(netflix)

summary(netflix)

netflix$date_added<-mdy(netflix$date_added)
netflix$year_added<-year(netflix$date_added)

# Movie vs TV Show count

plot1<-netflix %>% count(type) %>% ggplot(aes(x=type,y=n,fill=type))+
  geom_col()+
  labs(title="Movies vs TV Shows",x="Type",y="Count")+
  theme_minimal()

ggsave("movies vs tvshows.png",plot=plot1,width=6,height=4,dpi=300)

# Content Added over Years

plot2<-netflix %>% filter(!is.na(year_added))%>%
  count(year_added)%>%
  ggplot(aes(x=year_added,y=n))+
  geom_line(color="brown",size=1.5)+
  labs(title="Netflix Content Added Over Time",x="Year Added",y="Count")+
  theme_minimal()

ggsave("content added over years.png",plot=plot2,width=6,height=4,dpi=300)

# Most Common Ratings

plot3<-netflix %>%
  count(rating, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(rating, n), y = n)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Netflix Ratings", x = "Rating", y = "Count") +
  theme_minimal()

ggsave("ratings.png",plot=plot3,width=6,height=4,dpi=300)

# Top 10 Genres

plot4<-netflix %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(listed_in, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Genres on Netflix", x = "Genre", y = "Count") +
  theme_minimal()

ggsave("top 10 genres.png",plot=plot4,width=6,height=4,dpi=300)

# Top Countries producing content

plot5<-netflix %>%filter(!is.na(country))%>%separate_rows(country,sep=",")%>%
  count(country,sort=TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(country,n),y=n))+
  geom_col(fill="orange")+
  coord_flip()+
  labs(title="Top 10 Countries Producing Netflix Content",x="Country",y="Count")+
  theme_minimal()

ggsave("top 10 countries.png",plot=plot5,width=6,height=4,dpi=300)

# Movie Duration Distribution

plot6<-netflix %>% filter(type=="Movie")%>%
  mutate(duration_num=as.numeric(str_remove(duration," min")))%>%
  ggplot(aes(x=duration_num))+
  geom_histogram(fill="darkgreen",bins=30)+
  labs(title="Distribution of Movie Durations",x="Duration(minutes)",y="Count")+
  theme_minimal()

ggsave("movie duration distribution.png",plot=plot6,width=6,height=4,dpi=300)

# TV Shows by Number of Seasons

plot7<-netflix%>% filter(type=="TV Show")%>%mutate(seasons=as.numeric(str_remove(duration," Season[s]?")))%>%
  count(seasons)%>%
  filter(!is.na(seasons)) %>%
  ggplot(aes(x=seasons,y=n))+
  geom_col(fill="hotpink")+
  labs(title="Number of TV Shows by Season Count",x="Seasons",y="Count")+
  theme_minimal()

ggsave("tv shows by number of seasons.png",plot=plot7,width=6,height=4,dpi=300)

# Top 10 Directors

plot8<-netflix %>%filter(!is.na(director))%>%
  separate_rows(director,sep=", ")%>%
  count(director,sort=TRUE)%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(director,n),y=n))+
  geom_col(fill="steelblue")+
  coord_flip()+
  labs(title="Top 10 Directors on Netflix",x="Director",y="Count")+
  theme_minimal()

ggsave("top 10 directors.png",plot=plot8,width=6,height=4,dpi=300)

