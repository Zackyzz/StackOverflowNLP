Code for [https://zackyzz.github.io/PredictingSOTags.pdf]("Predicting Stack Overflow tags")

This code can comes in two forms: using Racket (where everything is implemented from scratch) and using Python (using existing libraries, however it works way faster).

The Racket Part the structure is as follows: so_extraction.rkt does the text preprocessing part (this also uses the sw.txt files containing the stop-words and so_bestwords.txt used to give more importance to the frequency of the words in this file), so_selection.rkt computes the information gain for each token and so_final.rkt builds up the final dataset, after selecting the most important tokens. SVM.rkt is an implementation of the Sequential Minimal Optimization variant for Support Vector Machines which is used to classify the dataset.

For Python there is stackoverflow.py which aims to mimic what is described above in Racket, however without using feature selection. Supplimentary for classification only using the data preprocessed in Racket, there is predicting.py which mimics SVM.rkt (but also other classification algorithms from sklearn).
