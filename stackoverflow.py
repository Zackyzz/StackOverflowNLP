import pandas as pd
from bs4 import BeautifulSoup
from nltk.stem.snowball import SnowballStemmer
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import LinearSVC
from sklearn import metrics

df = pd.read_csv('g1.csv')
tags=['javascript','python','java','c#','c++','php','swift','ruby','scala']

df['Question']=df['Title']+' '+df['Body']
df.drop(['Id','Score','Body','Title'], inplace=True, axis=1)
df['Question']=df['Question'].apply(lambda x: BeautifulSoup(x, 'html.parser').get_text())

df['Tag']=df['Tag'].apply(lambda x: [element for element in x.split() if element in tags])
df['Tag']=df['Tag'].apply(lambda x: ' '.join(str(elem) for elem in x))

blanks = []

for index, row in df.iterrows():
    if not any(c.isalpha() for c in row['Tag']):
        blanks.append(index)

df.drop(blanks, inplace=True)

df['Tag']=df['Tag'].apply(lambda x: x.split()[0])

s_stemmer = SnowballStemmer(language='english')

def stem_text(text,stemmer = s_stemmer):
    return ' '.join([stemmer.stem(w) for w in text.rstrip().split()])

df['Question']=df['Question'].apply(lambda x: stem_text(x))

X_train, X_test, y_train, y_test = train_test_split(df['Question'], df['Tag'], test_size=0.3)

with open('sw.txt') as f:
    sw = [line.rstrip() for line in f]

text_clf = Pipeline([('tfidf', TfidfVectorizer(stop_words=sw)),
                     ('clf', LinearSVC())])

text_clf.fit(X_train, y_train)

predictions = text_clf.predict(X_test)

print(metrics.accuracy_score(y_test,predictions))
print(metrics.classification_report(y_test,predictions))

my_question = "I want to learn python because it's widely used in the Machine Learning field, it got a lot of nice libraries such as pandas, nltk, spacy, scipy, sklearn and so on. Although it's not as fast as C, python got a decent speed due to the optimized implementation of the algorithms."

print(text_clf.predict([my_question]))