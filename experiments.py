from collections import defaultdict
from gensim import corpora

from gensim.test.utils import common_texts
from gensim.models.doc2vec import Doc2Vec, TaggedDocument

raw_corpus = ["Human machine interface for lab abc computer applications",
              "A survey of user opinion of computer system response time",
              "The EPS user interface management system",
              "System and human system engineering testing of EPS",
              "Relation of user perceived response time to error measurement",
              "The generation of random binary unordered trees",
              "The intersection graph of paths in trees",
              "Graph minors IV Widths of trees and well quasi ordering",
              "Graph minors A survey"]

# Create a set of frequent words
stoplist = set('for a of the and to in'.split(' '))
# Lowercase each document, split it by white space and filter out stopwords
texts = [[word for word in document.lower().split() if word not in stoplist]
         for document in raw_corpus]

# Count word frequencies
frequency = defaultdict(int)
for text in texts:
    for token in text:
        frequency[token] += 1

# Only keep words that appear more than once
processed_corpus = [
    [token for token in text if frequency[token] > 1] for text in texts]

dictionary = corpora.Dictionary(processed_corpus)

# possible memory bottleneck
bow_corpus = [dictionary.doc2bow(text) for text in processed_corpus]


documents = [TaggedDocument(doc, [i])
             for i, doc in enumerate(processed_corpus)]

vec_size = 20

print "Documents are ", documents

model = Doc2Vec(size=vec_size,
                alpha=0.025,
                min_alpha=0.025,
                min_count=1,
                workers=4)

model.build_vocab(documents)

for epoch in range(1000):
    model.train(documents,
                total_examples=model.corpus_count,
                epochs=model.epochs)
    model.alpha -= 0.002  # decrease the learning rate
    model.min_alpha = model.alpha  # fix the learning rate, no decay

print model.wv.most_similar("human")

#print dictionary
