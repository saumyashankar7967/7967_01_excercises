# -*- coding: utf-8 -*-
"""
Created on Fri Jan 17 15:18:59 2020

@author: saumya.shankar
"""
#libraries importing

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import math
import json

#dataframes creation from given dataset 

#Sales Dataset
df1=pd.read_excel('Data/SaleData.xlsx')

#imdb Dataset
df2=pd.read_csv('Data/imdb.csv',escapechar="\\")

#Diamonds Dataset
df3=pd.read_csv('Data/diamonds.csv')

#movies_metadata dataset
df4=pd.read_csv('Data/movie_metadata.csv',escapechar="\\")


#Questions 
#Q1.
#use the sales data (df1)
def least_sales(df):
    # write code to return pandas dataframe
    ls = df.groupby(["Item"])["Sale_amt"].min().reset_index()
    return(ls)
    
#ans1=least_sales(df1)

# Q2 compute total sales at each year X region
#use df1
def sales_year_region(df):
    # write code to return pandas dataframe
    
    df['Year']=df['OrderDate'].dt.year
    ls2=df.groupby(['Year','Region'])
    ls2_final=ls2.agg({'Sale_amt' : 'sum'})#This is giving total sales in each year and region 
    return ls2_final

#ans2=sales_year_region(df1)

# Q3 append column with no of days difference from present date to each order date
#use df1
def days_diff(df):
    # write code to return pandas dataframe
    df['days_diff']=pd.to_datetime("now")-df['OrderDate']
    return df

#ans3=days_diff(df1)

# Q4 get dataframe with manager as first column and  salesman under them as lists in rows in second column.
#use df1
def mgr_slsmn(df):
    # write code to return pandas dataframe
    ls4=df.groupby(['Manager'])
    ls4_final=ls4.agg({'SalesMan' : 'unique'})
    return ls4_final

#ans4=mgr_slsmn(df1)

# Q5 For all regions find number of salesman and number of units
#use df2
def slsmn_units(df):
    # write code to return pandas dataframe
    ls5=df.groupby(['Region'])
    ls5_final=ls5.agg({'SalesMan':'nunique','Sale_amt':'sum'})
    ls5_final.columns=['salesman_count','total_sales']
    return ls5_final

#ans5=slsmn_units(df1)

# Q6 Find total sales as percentage for each manager
#use df1
def sales_pct(df):
    # write code to return pandas dataframe
    ls6=df.groupby(['Manager'])
    ls6_final=ls6.agg({'Sale_amt':'sum'})
    ls6_final.columns=['total sales in percentage']
    sum_final=0
    for i in range(0,len(ls6_final)):
        sum_final=int(sum_final+ls6_final['total sales in percentage'][i])
    ls6_final['total sales in percentage']=ls6_final['total sales in percentage']/sum_final*100
    return ls6_final

#ans6=sales_pct(df1)

#-----------------use the imdb dataset df2
    
# Q7 get imdb rating for fifth movie of dataframe
def fifth_movie(df):
	# write code here
    return df['imdbRating'][4]

#ans7=fifth_movie(df2)

# Q8 return titles of movies with shortest and longest run time
def movies(df):
	# write code here
    df1=df.dropna(subset=['duration'], how='all')
    df1.sort_values("duration",inplace=True)
    d={'shortest':df1['title'].iloc[0],'longest':df1['title'].iloc[len(df1)-1]}
    df_final=pd.DataFrame(list(d.items()))
    return df_final

#ans8=movies(df2)

# Q9 sort by two columns - release_date (earliest) and Imdb rating(highest to lowest)
def sort_df(df):
	# write code here
    df1=df.dropna(subset=['year'], how='all')
    df1=df1.dropna(subset=['imdbRating'], how='all')
    df1.sort_values(['year', 'imdbRating'], ascending=[True, False],inplace=True)
    return df1

#ans9=sort_df(df2)

# Q10 subset revenue more than 2 million and spent less than 1 million & duration between 30 mintues to 180 minutes
def subset_df(df):
	# write code here
    df1=df
    ls10_final=df1[(df1['duration']>=30) & (df1['duration']<=180)]
    return ls10_final

#ans10=subset_df(df2)
#--------------------use the diamonds dataset df3
    
# Q11 count the duplicate rows of diamonds DataFrame.
def dupl_rows(df):
	# write code here
    ls11=df.groupby(df.columns.tolist(),as_index=False).size()
    ls11_final=pd.DataFrame(ls11)
    ls11_final.columns=['count']
    ls11_fin=ls11_final[ls11_final['count']>1]#this will give only duplicate rows not the rows with count 1
    return ls11_fin

#ans11=dupl_rows(df3)
    
# Q12 droping those rows where any value in a row is missing in carat and cut columns
def drop_row(df):
	# write code here
    df1=df.dropna(subset=['carat'], how='all')
    df1=df1.dropna(subset=['cut'], how='all')
    return df1

#ans12=drop_row(df3)

# Q13 subset only numeric columns
def sub_numeric(df):
	# write code here
    #here in the dataset some of the columns though they are numeric their datatypes are object
    # so first we can also change that
    """cols=['carat','z']
    df[cols] = df[cols].apply(pd.to_numeric, errors='coerce')"""
    ls_13=df._get_numeric_data()
    return ls_13

#ans13=sub_numeric(df3)

# Q14 compute volume as (x*y*z) when depth > 60 else 8
def volume(df):
	# write code here
    cols=['carat','z']
    df[cols] = df[cols].apply(pd.to_numeric, errors='coerce')
    df['volume']=df['x']*df['y']*df['z']
    df['volume'][df['depth']<60]=8
    return df

#ans14=volume(df3)
    
# Q15 impute missing price values with mean
def impute(df):
	# write code here
    df['price']=df['price'].fillna(df['price'].mean())
    return df

#ans15=impute(df3)

#Bonus Questions:-
#Q1.
    """
Generate a report that tracks the various Genere combinations for each type year on year. 
The result data frame should contain type, Genere_combo, year, avg_rating, min_rating, max_rating, 
total_run_time_mins"""

#use the imdb dataset(df2, already loaded in start)
def bonus1(df):
    df0=df.dropna()
    df1=df0.iloc[:,16:44]
    df2=df0.filter(['year','type','imdbRating','duration'],axis=1)
    df2['genre_combo']=df1.T.apply(lambda x:" ".join(x.index[x==1]),axis=0)
    lsb1=df2.groupby(['type','year','genre_combo'])
    lsb1_final=lsb1.agg({'imdbRating' :{'Average': 'mean','Maximum': 'max','Minimum': 'min'},'duration':{'total_run_time_mins':'sum'}})
    return(lsb1_final)
    
#ansb1=bonus1(df2)
    
#Q2.
"""
Is there a relation between the length of a movie title and the ratings ? 
Generate a report that captures the trend of the number of letters in movies titles over years. 
We expect a cross tab between the year of the video release and the quantile that length fall 
under. The results should contain year, min_length, max_length, num_videos_less_than25Percentile, 
num_videos_25_50Percentile , num_videos_50_75Percentile, num_videos_greaterthan75Precentile 
"""

#use the imdb dataset(df2)

#custom funcions created 
def l25(df):
    val=df.quantile(0.25)
    return(len(df[df<val]))
def b25_50(df):
    val1=df.quantile(0.25)
    val2=df.quantile(0.5)
    return(len(df[(df>=val1) & (df<val2)]))
def b50_75(df):
    val1=df.quantile(0.50)
    val2=df.quantile(0.75)
    return(len(df[(df>=val1) & (df<val2)]))
def g75(df):
    val=df.quantile(0.75)
    return(len(df[df>=val]))

def bonus2(df):
    df1=df.dropna()
    # relationship between the length of a movie title and the ratings
    df_plot=df1.filter(['imdbRating'],axis=1)
    df_plot['length']=df1['title'].apply(lambda x: len(str(x)))
    sns.jointplot(x='length',y='imdbRating',data=df_plot,kind='reg')
    corrmat = df_plot.corr()
    print("Following correlation will show the correlation b/w ImdbRating and length")
    print(corrmat)
    f, ax = plt.subplots(figsize =(9, 8)) 
    sns.heatmap(corrmat, ax = ax, cmap ="YlGnBu", linewidths = 0.1) 
    #relation between length of movie title lengths over year by year
    df_plot['year']=df1['year']
    df_analysis=df_plot.filter(['year','length'],axis=1)
    ls_an=df_analysis.groupby(['year'])
    ls_an_final=ls_an.agg({'length':{'Minimum':'min','Maximum':'max','num_videos_less_than25Percentile':l25,'num_videos_25_50Percentile':b25_50,'num_videos_50_75Percentile':b50_75,'num_videos_greaterthan75Precentile':g75}})
    return(ls_an_final)

#ansb2=bonus2(df2)

#q3
    """
    In diamonds data set Using the volumne calculated above, create bins that have equal population within them. 
 Generate a report that contains cross tab between bins and cut. 
 Represent the number under each cell as a percentage of total"""
#use diamonds dataset df3
 
def bonus3(df):
    df=volume(df)
    df['Bins']=pd.qcut(df['volume'],5,labels=False)
    df2=pd.crosstab(df.Bins,df.cut,margins=True,normalize=True)*100
    return(df2)
    
#ansb3=bonus3(df3)

#Q4.
    """
    Generate a report that tracks the Avg. imdb rating year on year, in the last 10 years, for movies that are 
top performing. You can take the top 10% grossing movies every year. Add the number of top performing movies 
under each genere in the report as well."""

#custom functions for this Questions
#use the movie_metadata dataset(df4) for this question
def value(x):
    val=math.floor(0.1*len(x))
    if(val==0):
        return 1
    else:
        return val

def dict_return(x):
    all_freq = {} 
    for i in x: 
        if i in all_freq: 
            all_freq[i] += 1
        else: 
            all_freq[i] = 1
    return json.dumps(all_freq)
    
def bonus4(df):
    df0=df.dropna()
    df1=df0.filter(['title_year','gross','genres','imdb_score'])
    df1_g=df1.groupby(['title_year'])

    g = df1_g.apply(lambda x:x.sort_values(by=['gross'],ascending=False).head(value(x))).drop('title_year',axis=1).reset_index()

    #this one is for each year top movies average movies imdb Ratings
    g1=g.groupby(['title_year']).agg({'imdb_score':{'Average':'mean'}}).reset_index()

    #now for each year count of movies(only top 10 %) under each genre_combo
    g2=g.groupby(['title_year'])['genres'].apply(dict_return).reset_index()
    g_dict=g2['genres'].apply(lambda x:json.loads(x))
    g_df=pd.DataFrame(g_dict.tolist())
    g_df.fillna(0,inplace=True)

    #so returning a final matrix that can be used for training
    df_final=pd.concat([g1,g_dict,g_df], axis=1)
    return(df_final)

#ansb4=bonus4(df4)
    
#Q5 
    """
    Bucket the movies into deciles using the duration. Generate the report that tracks various features like 
 nomiations, wins, count, top 3 geners in each decile. 
    """
#use the imdb dataset df2
    
def bonus5(df):
    df=df.dropna()
    df1=df.iloc[:,16:44]
    df2=df.filter(['duration','nrOfNominations','nrOfWins','fn','imdbRating'],axis=1)
    df2['genre_combo']=df1.T.apply(lambda x:" ".join(x.index[x==1]),axis=0)

    df2['decile'] = pd.qcut(df2['duration'], 10, labels=False)
    df2_groups=df2.groupby(['decile'])
    df2_groups_final=df2_groups.agg({'nrOfNominations':'sum','nrOfWins':'sum','fn':{'count':'count'}}).reset_index()
    
    g = df2.groupby(['decile']).apply(lambda x:x.sort_values(by=['imdbRating'],ascending=False).head(3)).drop('decile',axis=1).reset_index()
    d_final=g.groupby(['decile'])['genre_combo'].apply(lambda x: "{%s}" % ', '.join(x)).reset_index()
    df2_groups_final['top_genres']=d_final['genre_combo']
    return(df2_groups_final)

#ansb5=bonus5(df2)

    
    
    
    









    