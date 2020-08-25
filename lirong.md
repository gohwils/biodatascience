# Wang Li Rong

![lirong_image](/images/lirong.png)

## Unravelling the doppelgänger effect in machine learning and bioinformatics data
Many machine learning models trained on bioinformatics data use standard cross validation for model evaluation under the assumption that each row in the dataset is independent from one another. However, this is often not the case when it comes to bioinformatics data given the presence of hidden duplicates. This problem is called the “doppelgänger effect” which occurs when training and test data are similar by coincidence leading to an inflation in measured performance of classification models when using standard cross validation. 
Using known examples such as breast cancer signature prediction, we will be exploring ways to identify the presence of doppelgänger effect in datasets while attempting to reduce the impacts of this effect by removing “doppelgänger effect” causing genes or by using datasets from different sources for test and training sets. For instance, using different cell lineages, different tissues or research groups. Finally, we will try to identify what makes a valid pair of training and test data where we can get good results due to the classifiers learning properly, and not because the data are too similar.
