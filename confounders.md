## Dealing with confounders in omics analysis
There is a theory–practice gap that exists when theoretical statistics are applied on real-world data. 

In high-throughput omics data (e.g., genomics, proteomics, and transcriptomics), statistics is commonly deployed to help understand the mechanism underpinning disease onset and progression. In clinical practice, we use statistics to identify differentiating features from high-throughput omics data. These features are critical as biomarkers for diagnosis, guiding treatment, and prognosis. Unlike monogenic disorders, many challenging diseases (e.g., cancer) are polygenic, requiring multigenic signatures to counteract etiology and human variability issues.

In the course of analyzing biological data for differential features such as genes or proteins, we observe that it is a fairly common phenomenon where the null hypothesis is rejected for extraneous reasons (or confounders), rather than because the alternative hypothesis is relevant to the disease phenotype. The situation when null hypothesis are wrongly rejected due to the presence of confounding issues is also known as the Anna Karenina effect [1].


There are many ways a null hypothesis can be rejected. In the example shown, only one is the correct basis for rejection. The rest all lead towards false rejection of the null. The ease with which statistical null hypotheses can be rejected is known as the Anna Karenina effect (so named for the famous addage: Happy families are all alike; every unhappy family is unhappy in its own way).

![AKP image 1](https://gohwils.github.io/biodatascience/images/AKP_image1.png)


Confounders lead to universality and reproducibility problems. Confounders can include batch effects, poor experiment design, inappropriate sample size, and misapplied statistics. Current literature mostly blames poor experiment design and overreliance on the highly fluctuating P-value. But we think that more can be done.

The subject of this research theme is to explore a deeper rethink on hypothesis statement construction, null distribution appropriateness and relevant test-statistics.

### Reference
[1] Goh WWB, Wong LS. Dealing with confounders in -omics analysis. Trends in Biotechnology, 36(5):488-498, May 2018






## Relevant publications
1.	Goh WWB, Wong LS. Dealing with confounders in -omics analysis. Trends in Biotechnology, 36(5):488-498, May 2018
2.	Goh WWB, Wong LS. Integrating networks and proteomics: moving forward. Trends in Biotechnology, 34(12):951-959, Dec 2016
3.	Goh WWB, Wong LS. Design principles for clinical network-based proteomics. Drug Discovery Today, 21(7):1130-1138, Jul 2016 
4.	Goh WWB, Wong LS. Computational proteomics: Developing a robust analytical strategy. Drug Discovery Today, 19(3):266-274, Mar 2014
5.	Zhao Y, Wong LS, Goh WWB. Doing quantile normalization right. BMC Bioinformatics, Accepted
6.	Goh WWB, Wong LS. Turning straw into gold: how to build robustness into gene signature inference. Drug Discovery Today, 24(1):31-36, Jan 2019
7.	Goh WWB, Wong LS. Breast Cancer Signatures Are No Better Than Random Signatures Explained. Drug Discovery Today, 23(11):1818-1823, Nov 2018
8.	Wang W, Sue ACH, Goh WWB. Null-hypothesis statistical testing in clinical proteomics: With great power comes great reproducibility. Drug Discovery Today, 22(6):912-918, June 2017
9.	Wang W, Goh WWB. Sample-to-sample p-value variability and its implications in multivariate analysis. International Journal of Bioinformatics Research and Applications, 14(3):235-254, Jan 2018

