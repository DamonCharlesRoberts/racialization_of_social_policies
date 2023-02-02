from GoogleNews import GoogleNews
# pip install newspaper3k
from newspaper import Article
from newspaper import Config
import pandas as pd
import nltk
nltk.download('punkt')
import datetime

user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'
config = Config()
config.browser_user_agent = user_agent
start_date = datetime.date(2019, 1, 1)
end_date = datetime.date(2019, 1, 11)
delta = datetime.timedelta(days = 1)
while start_date <= end_date:
    start_date += delta
    googlenews=GoogleNews(start=start_date,end=start_date,lang='eng')
    googlenews.search('green new deal')
    result=googlenews.result()
    df=pd.DataFrame(result)
    print(df.head())
    for i in range(0,5):
        googlenews.getpage(i)
        result=googlenews.result()
        df=pd.DataFrame(result)
    list=[]
    for ind in df.index:
        try:
            dict={}
            article = Article(df['link'][ind],config=config)
            article.download()
            article.parse()
            article.nlp()
            dict['Date']=df['date'][ind]
            dict['Media']=df['media'][ind]
            dict['Title']=article.title
            dict['Article']=article.text
            dict['Summary']=article.summary
            list.append(dict)
        except:
            pass
news_df=pd.DataFrame(list)
news_df.to_excel("articlesgnd_2021-10-26.xlsx")