from GoogleNews import GoogleNews
# pip install newspaper3k
from newspaper import Article
from newspaper import Config
import pandas as pd
import nltk
nltk.download('punkt')

user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'
config = Config()
config.browser_user_agent = user_agent
googlenews=GoogleNews(start='01/01/2019',end='08/11/2020',lang='eng')
googlenews.search('healthcare reform')
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
news_df.to_excel("articleshealthcare.xlsx")

