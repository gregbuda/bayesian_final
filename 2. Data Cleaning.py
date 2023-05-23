# -*- coding: utf-8 -*-
"""
Created on Mon Apr 24 20:49:29 2023

@author: gregb
"""

import re
from sklearn.base import TransformerMixin
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer
import spacy
import os
import pandas as pd
from ast import literal_eval
import nltk
import scipy.sparse as sp
nltk.download('punkt')
#!python -m spacy download es_core_news_sm

os.chdir('C:/Users/gregb/OneDrive/Asztali gép/UPC_master/BAY/Final Project/Webscrape')
df_total = pd.read_csv('scraped_data.csv')
len(df_total)

# Convert labels to list
df_total['labels'] = df_total['labels'].apply(literal_eval)
df_total.labels[0]

# Fill in rooms if it's in label
def get_habitacion(lst): return int([s.split()[0] for s in lst if 'habitación' in s][0]) if any(
    'habitación' in s for s in lst) else None

df_total['habitaciones'] = df_total['labels'].apply(get_habitacion)
df_total.no_rooms.fillna(df_total.habitaciones, inplace=True)
df_total = df_total.drop('habitaciones', axis=1)

# Fill in aseos if it's in label
def get_aseo(lst): return int([s.split()[0] for s in lst if 'aseo' in s][0]) if any(
    'aseo' in s for s in lst) else None

df_total['aseos'] = df_total['labels'].apply(get_aseo)
df_total.bathroom.fillna(df_total.aseos, inplace=True)
df_total = df_total.drop('aseos', axis=1)

# Missing values
(df_total.isnull().sum() / len(df_total)) * 100
df_total.drop('main_desc', axis=1).dropna()

# List of labels
df_exploded = df_total.explode('labels')
df_exploded.labels.value_counts().head(30)
del df_exploded

# Extract labels
specific_items = ['Ascensor', 'Calefacción', 'Aire acondicionado',
                  'Plaza parking', 'Piscina comunitaria', 'Amueblado']
for item in specific_items:
    df_total[item] = df_total['labels'].apply(lambda x: 1 if item in x else 0)

# Let's just keep the nonmissing lines
df_total = df_total.drop('main_desc', axis=1).dropna()


############################ TEXT CLEANING ######################
# Text cleaning
nlp = spacy.load('es_core_news_sm')

# Use this class to pre precess the text

class TextPreprocessor(TransformerMixin):
    def transform(self, X, **transform_params):
        return [clean_text(text) for text in X]

    def fit(self, X, y=None, **fit_params):
        return self

# import spacy
# nlp = spacy.load('es_core_news_sm')
# words = ['guapo', 'guapa', 'guapos', 'piso', 'metro']
# for word in words:
#     token = nlp(word)[0]
#     lemma = token.lemma_
#     print(f'{word} -> {lemma}')


def clean_text(text):
    doc = nlp(text)
    tokens = []
    for token in doc:
        # Check if the token is a valid Spanish word and not a stop word
        if token.is_alpha and token.norm_ in nlp.vocab and not token.is_stop:
            # Lemmatize and add the token to the list of valid tokens
            lemma_token = token.lemma_.lower()
            tokens.append(lemma_token)
    return " ".join(tokens)


pipeline = Pipeline([
    ('preprocessor', TextPreprocessor()),
    ('vectorizer', CountVectorizer(binary=True))  # binary matrix
])

X = df_total['long_desc']
document_term_matrix = pipeline.fit_transform(X)

#sp.save_npz('sparse_matrix.npz', document_term_matrix)
#document_term_matrix = sp.load_npz('sparse_matrix.npz')

# Into DF
document_term_df = pd.DataFrame.sparse.from_spmatrix(document_term_matrix)
document_term_df.columns = pipeline.named_steps['vectorizer'].get_feature_names()

# Cut columns with numbers
numeric_cols = [col for col in document_term_df.columns if re.match('^\d+$', col) is None]
document_term_df = document_term_df[numeric_cols]

# Calculate the frequency of each term over the documents
term_frequencies = document_term_df.sum() / len(document_term_df.index)
term_frequencies_df = pd.DataFrame(term_frequencies, columns=['frequency'])
term_frequencies_df = term_frequencies_df.sort_values(by='frequency', ascending=False)
print(term_frequencies_df.head(20))
print(term_frequencies_df.tail(100))

# Cut those with less than 10 df
term_frequencies = document_term_df.sum()
sum(term_frequencies > df_total.shape[0]*0.01)

#The lower and upper threshold
filtered_terms = term_frequencies[ (term_frequencies >= df_total.shape[0]*0.005) & (term_frequencies <= df_total.shape[0]*0.5)].index
filtered_df = document_term_df[filtered_terms]
filtered_df.to_csv('document_term_matrix.csv',index=False)