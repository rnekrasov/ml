
# coding: utf-8

# In[2]:


import pandas as pd
file=pd.read_csv("C:/QRG/Python/nekrasov.txt",sep="\t",names=["text"])
df=pd.DataFrame(file)
print(df[0:5])


# In[98]:


import gensim
from gensim.parsing.preprocessing import preprocess_string
from gensim.parsing.preprocessing import strip_punctuation
from gensim.parsing.preprocessing import stem_text
from gensim.parsing.preprocessing import remove_stopwords

punctuations="!?()-—««—"
stop_words=["и","он","что"]

#преобразуем текстовый файл в последовательность слов
#делаем нормализацию текста (раскладка, пунктуация, стоп-слова)
"""
s=str(pd.read_csv("C:/QRG/Python/tolstoy.txt",sep="\t",names=["text"]))
s=unicode(s,"utf-8")
s=s.lower()
s=s.split(".")
#print(s)

no_punct=""
for char in s:
    if char not in punctuations:
            no_punct=no_punct+char
s=no_punct
"""

#предобработка предложений
doc=gensim.models.word2vec.LineSentence("C:/QRG/Python/nekrasov.txt")
doc2=gensim.models.word2vec.LineSentence("C:/QRG/Python/tolstoy.txt")
#bigram=gensim.models.Phrases(doc2)
#print(bigram[doc2])

#начинаем инициализировать и тренировать модель word2vec
model = gensim.models.Word2Vec(min_count=0)
#для фраз подавать на вход bigram[doc2]
model.build_vocab(doc2,min_count=0)
model.train(doc2,total_examples=model.corpus_count,epochs=5)

#интерпретация модели
model.wv.vocab
#model.wv.most_similar(positive=["Болконский"],negative=["Безухов"])
#model.wv.most_similar("Кутузов",topn=150)
model.wv.similarity("Кутузов","лошадь")
#model.wv["Безухов"]


# In[99]:


model.wv.similarity("Кутузов","французы")


# In[100]:


model.wv.similarity("Кутузов","Москва")

