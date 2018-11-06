import nltk
import re
from bs4 import BeautifulSoup
import pandas as pd


class reddit_text_preprocessing:
    def __init__(self,text):
        self.__text = text
        abb = pd.read_csv('abbreviatons.csv')
        abb = abb.set_index('abbreviation')['long_form'].T.to_dict()
        self.__abbreviations = abb
    
    @property
    def text(self):
        return self.__text
    
    @text.setter
    def text(self, text):
        self.__text = text
    
    @text.deleter
    def text(self):
        del self.__text
        
    def lower_case(self, to_lower = True):
        text = self.__text
        if to_lower:
            text = ' '.join([ word.strip().lower() for word in text.split()])
        
        self.__text = text
        return self
    
    def process_html(self):
        text = self.__text
        text = BeautifulSoup(text, 'lxml').get_text()
        self.__text = text
        return self
    
    def remove_urls(self, remove_urls = True):
        text = self.__text
        if remove_urls:
            text = re.sub('http?://[A-Za-z0-9./]+','',text)
            text = re.sub('https?://[A-Za-z0-9./]+','',text)       
        self.__text = text
        return self
    
    def decode_text(self):
        text = self.__text
        try:
            text = text.decode("utf-8-sig").replace(u"\ufffd", "?")
        except:
            text = text
        self.__text = text
        return self
    
    def stopwords_remove(self, stopwords = nltk.corpus.stopwords.words('english')):
        text = self.__text
        if stopwords:
            text = " ".join([word for word in text.split() if word not in stopwords])
        self.__text = text
        return self

    def tokenize(self, tokenizer= nltk.tokenize.WordPunctTokenizer()):
        text = self.__text
        if tokenizer:
            text = tokenizer.tokenize(text)
            text = (" ".join(text)).strip()
        self.__text = text
        return self

    def lemmatize(self, lemmatizer = nltk.stem.wordnet.WordNetLemmatizer()):
        text = self.__text
        if lemmatizer:
            text = ' '.join([lemmatizer.lemmatize(word, 'v') for word in text.split()])
        self.__text = text
        return self
    
    def stem(self, stemmer):
        text = self.__text
        # mostly stemmers are stemmer = nltk.stem.PorterStemmer()
        if stemmer:
            text = ' '.join([stemmer.stem(word) for word in text.split()])
        self.__text = text
        return self
    
    def replace_abbreviations(self):
        text = self.__text
        abbreviations = self.__abbreviations
        
        substrs = sorted(abbreviations, key=len, reverse=True)
        text = ' '.join([ word.strip().upper() for word in text.split()])

        # Create a big OR regex that matches any of the substrings to replace
        regexp = re.compile('|'.join(map(re.escape, substrs)))

        # For each match, look up the new string in the replacements
        text = regexp.sub(lambda match: abbreviations[match.group(0)], text)
        
        self.__text = text
        return self
        
        
        
        