#sales dataset
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(stringi)

#dataframes creation from given dataset 

#Sales Dataset
df1<-read_excel('Data/SaleData.xlsx',sheet='Sales Data')

#imdb Dataset
df2<-read_delim("Data/imdb.csv",delim=",",quote=",",escape_backslash=TRUE,escape_double=FALSE)

#Diamonds Dataset
df3<-read.csv('Data/diamonds.csv',stringsAsFactors = FALSE)

#movies_metadata dataset
df4<-read_delim("Data/movie_metadata.csv",delim=",",quote=",",escape_backslash=TRUE,escape_double=FALSE)


#Questions 
#Q1.
#use the sales data (df1)
least_sales<- function(df)
{
  df_1<-aggregate(df$Sale_amt,by=list(item=df$Item),FUN=min)
  return(df_1)
}

#ans1<-least_sales(df1)

# Q2 compute total sales at each year X region
#use df1
sales_year_region<-function(df)
{
  df$year<-format(as.Date(df$OrderDate , format="%d/%m/%Y"),"%Y")
  summary<-df %>% group_by(year,Region) %>%  summarise(Net = sum(Sale_amt))
  return(summary)
}

#ans2<-sales_year_region(df1)

# Q3 append column with no of days difference from present date to each order date
#use df1
days_diff<-function(df)
{
  df_copy<-df
  df_copy$days_diff<-as.numeric(Sys.Date()- as.Date(df$OrderDate))
  return(df_copy)
}

#ans3<-days_diff(df1)

# Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
#use df1
mgr_slsman<-function(df)
{
  summary<-lapply(split(df$SalesMan, df$Manager) , unique)
  return(summary)
}


#ans4<-mgr_slsman(df1)

# Q5 For all regions find number of salesman and number of units
#use df2
slsman_units<-function(df)
{
  summary<-df %>% group_by(Region) %>%  summarise(total = sum(Sale_amt),count=n_distinct(SalesMan))
  return(summary)
}


#ans5<-slsman_units(df1)

# Q6 Find total sales as percentage for each manager
#use df1
sales_pct<-function(df)
{
  summary<-df %>% group_by(Manager) %>% summarise(total = sum(Sale_amt))
  summary1<-mutate(summary, mtcars_new = 100*total /sum(summary$total))
  return(summary1)
}

#ans6<-sales_pct(df1)

#-----------------use the imdb dataset df2

# Q7 get imdb rating for fifth movie of dataframe
fifth_movie<-function(imdb)
{
  return(imdb[5,'imdbRating'])
}

#ans7<-fifth_movie(df2)

# Q8 return titles of movies with shortest and longest run time
movies<-function(df)
{
  df<-df%>% drop_na(duration)
  return(arrange(df,duration)[c(1,nrow(df)),'title'])
}

#ans8<-movies(df2)

# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
sort_df<-function(df.2)
{
  df.2<-df.2%>% drop_na(year)
  df.2<-df.2%>% drop_na(imdbRating)
  return(arrange(df.2,year,desc(imdbRating)))
}

#ans9<-sort_df(df2)

# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
subset_df<-function(df.2)
{
  return(subset(df.2,between(duration,30,180)))
}

#ans10<-subset_df(df2)
#--------------------use the diamonds dataset df3

# Q11 count the duplicate rows of diamonds DataFrame.
dupl_rows<-function(df)
{
  return(nrow(df)-nrow(unique(df)))
}

#ans11<-dupl_rows(df3)

# Q12 droping those rows where any value in a row is missing in carat and cut columns
drop_row<-function(df)
{
  df<-df%>% drop_na(cut,carat)
  return(df)
}

#ans12<-drop_row(df3)

# Q13 subset only numeric columns
sub_numeric<-function(df)
{
  ans<-select_if(df,is.numeric)
  return(ans)
}

#ans13<-sub_numeric(df3)

# Q14 compute volume as (x*y*z) when depth > 60 else 8
volume<-function(df)
{
  df$x <- as.numeric(df$x) 
  df$y <- as.numeric(df$y) 
  df$z <- as.numeric(df$z) 
  df$volume=df$x*df$y*df$z
  df_try<-df
  a<-which(df_try$depth<60)
  df_try[a,'volume']=8
  return(df_try)
}

#ans14<-volume(df3)

# Q15 impute missing price values with mean
impute<-function(df)
{
  df[is.na(df$price),]['price']=mean(df$price,na.rm=TRUE)
  ans<-df
  return(ans)
}
#ans15<-impute(df3)

#Bonus Questions:-
#Q1.
"
Generate a report that tracks the various Genere combinations for each type year on year. 
The result data frame should contain type, Genere_combo, year, avg_rating, min_rating, max_rating, 
total_run_time_mins"""

#use the imdb dataset(df2, already loaded in start)
f<-function(x, output) {
  a<-paste(names(x[which(x==1)]),collapse=" ")
  return(a)
}

bonus1<-function(df)
{
  df<-df %>% drop_na()
  df_filter<-data.frame("year"=df$year,"type"=df$type,"imdbRating"=df$imdbRating,"duration"=df$duration)
  df1<-df[,17:44]
  df_filter['genre_combo']<-apply(df1,1,f)
  summary<-df_filter %>% group_by(type,genre_combo,year) %>% summarise(Average_rating = mean(imdbRating),Minimum=min(imdbRating),Maximum=max(imdbRating),total_duration=sum(duration))
  return(summary)
}


#ansb1<-bonus1(df2)

#Q2.
"
Is there a relation between the length of a movie title and the ratings ? 
Generate a report that captures the trend of the number of letters in movies titles over years. 
We expect a cross tab between the year of the video release and the quantile that length fall 
under. The results should contain year, min_length, max_length, num_videos_less_than25Percentile, 
num_videos_25_50Percentile , num_videos_50_75Percentile, num_videos_greaterthan75Precentile 
"""

#use the imdb dataset(df2)

#custom funcions created 
l25<-function(a)
{
  val=quantile(as.numeric(unlist(a)),0.25)
  b=which(a<val)
  return(length(b))
}

b25_50<-function(df)
{
  val1=quantile(as.numeric(unlist(df)),0.25)
  val2=quantile(as.numeric(unlist(df)),0.5)
  b=which(df>=val1 & df<val2)
  return(length(b))
}

b50_75<-function(df)
{
  val1=quantile(as.numeric(unlist(df)),0.5)
  val2=quantile(as.numeric(unlist(df)),0.75)
  b=which(df>=val1 & df<val2)
  return(length(b))
}

g75<-function(df)
{
  val=quantile(as.numeric(unlist(df)),0.75)
  b=which(df>=val)
  return(length(b))
}

bonus2<-function(df)
{
  #1ST PART for length of movie title vs imdbRatings
  df<-df %>% drop_na()
  df_filter<-df[,'title']
  df_filter$length_of_title=lapply(df_filter$title ,stri_length)
  df_filter$rating<-df$imdbRating
  df_filter$length_of_title <- as.numeric(df_filter$length_of_title) 
  ans<-df_filter[,c('length_of_title','rating'),drop=FALSE]
  plot(as.numeric(df_filter$length_of_title),as.numeric(df_filter$rating),xlab = 'length of movie',ylab = 'imdbRating')
  correlation_matrix = cor(ans)
  print("correlation_matrix:")
  print(correlation_matrix)
  
  #2nd part--title length vs year
  df2<-df[,'year']
  df2$length<-df_filter$length_of_title
  summary<-df2 %>% group_by(year) %>% summarise(Min_len = min(length),Max_len=max(length),num_videos_less_than25Percentile=l25(length),num_videos_25_50Percentile=b25_50(length),num_videos_50_75Percentile=b50_75(length),num_videos_greaterthan75Precentile=g75(length))
  return(summary)
}


#ansb2<-bonus2(df2)

#q3
"
In diamonds data set Using the volumne calculated above, create bins that have equal population within them. 
Generate a report that contains cross tab between bins and cut. 
Represent the number under each cell as a percentage of total"""
#use diamonds dataset df3

bonus3<-function(df)
{
  df1<-volume(df)
  df1$bins<-ntile(df1$volume, 5)
  #df1$bins=as.numeric(bin_data(df$volume,bins=5,binType="quantile",boundaryType="(lorc"))
  ans<-table(df1$bins,df1$cut)
  ans<-prop.table(ans)
  ans<-as.data.frame.matrix(ans)
  ans<-ans[,-1]
  ans<-ans*100
  return(ans)
}


#ansb3<-bonus3(df3)

#Q4.
"
Generate a report that tracks the Avg. imdb rating year on year, in the last 10 years, for movies that are 
top performing. You can take the top 10% grossing movies every year. Add the number of top performing movies 
under each genere in the report as well."

#custom functions for this Questions
#use the movie_metadata dataset(df4) for this question
bonus4<-function(df)
{
  df<-df%>% drop_na(title_year)
  g<- df %>% group_by(title_year) 
  top10<- g %>% top_frac(0.1,gross) %>%group_by(title_year,genres) %>% summarise(avg_imdb=mean(imdb_score),count_movies=n())
  return(top10)
}


#ansb4<-bonus4(df4)

#Q5 
"
Bucket the movies into deciles using the duration. Generate the report that tracks various features like 
nomiations, wins, count, top 3 geners in each decile. 
"""
#use the imdb dataset df2

f<-function(x, output) {
  a<-paste(names(x[which(x==1)]),collapse=" ")
  return(a)
}


bonus5<-function(df)
{
  df<-df %>% drop_na()
  df_filter<-data.frame("nrOfNominations"=df$nrOfNominations,"nrOfWins"=df$nrOfWins,"imdbRating"=df$imdbRating)
  df_filter$decile=as.numeric(bin_data(df$duration,bins=10,binType="quantile",boundaryType="(lorc"))
  df1=df[,17:44]
  df_filter['genre_combo']<-apply(df1,1,f)
  g<- df_filter %>% group_by(decile) 
  
  df2<-g %>% summarise(no_of_nominations=sum(nrOfNominations),no_of_wins=sum(nrOfWins),count=n())
  
  
  df3<- g %>% top_n(3,imdbRating) %>% group_by(decile) %>% summarise(Top_3_genres = paste(genre_combo, collapse = ","))
  
  df2$Top_genre=df3$Top_3_genres
  df_final<-head(df2,-1)
  return(df_final)
}


#ansb5<-bonus5(df2)