from GoogleNews import GoogleNews
# pip install newspaper3k
from newspaper import Article
from newspaper import Config
import pandas as pd
import nltk

user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/98.8.30 (KHTML, like Gecko) Version/13.2.1 Safari/98.8.30'

config = Config()

config.browser_user_agent = user_agent
df = pd.DataFrame()
list = []
news_df = pd.DataFrame()

def scrapeit(phrase, start_date, end_date):
    googlenews = GoogleNews(start = start_date, end = end_date, lang = 'eng')
    googlenews.search(phrase)
    result = googlenews.result()
    df.append(result)
    for i in range(0,20):
        googlenews.getpage(i)
        result = googlenews.result()
        df.append(result)
    for ind in df.index:
        try:
            dict = {}
            article = Article(df['link'][ind], config = config)
            article.download()
            article.parse()
            article.nlp()
            dict['Date'] = df['date'][ind]
            dict['Media'] = df['media'][ind]
            dict['Title']=article.title
            dict['Article']=article.text
            dict['Summary']=article.summary
            list.append(dict)
        except:
            pass
    news_df.append(list)
    news_df.to_csv(phrase + '.csv')

start_date = '01/01/2019'
end_date = '08/11/2020'
scrapeit('healthcare reform', start_date, end_date)