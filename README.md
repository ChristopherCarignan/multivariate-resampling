# multivariate-resampling
Performs resampling (both under- and over-sampling) of observations in multivariate data frame

Consider the following scenario: you have a multivariate data set containing different numbers of observations between different levels of a factor, e.g., different number of samples or repetitions for different participants in a study. You need to make sure that you have the same number of observations for each of these levels. 

You could under-sample the entire data set via random sampling, so that the number of observations in each level is equal to a small number; this isn't optimal because you throw away a lot of data in the process.

You could over-sample the entire data set via random sampling, so that the number of observations in each level is equal to a large number; this isn't optimal because you create duplicate sets of multivariate features in the process, which is detrimental to, e.g., accurate training of neural networks.

The R function multivar_resamp allows for resampling of a data set to a target number of observations, either under-sampling or over-sampling as appropriate. In the case of over-sampling, new observations are created in a bootstrap fashion by creating weighted averages of features between randomly selected nearest neighbors in a multivariate space. Verification that no duplicated observations is included in the over-sampling process.


Example of over-sampling of feature with a uni-modal distribution:
![unimodal](https://github.com/ChristopherCarignan/multivariate-resampling/blob/master/oversamp_unimodal.png)

Example of over-sampling of feature with a bi-modal distribution:
![bimodal](https://github.com/ChristopherCarignan/multivariate-resampling/blob/master/oversamp_bimodal.png)

Example of over-sampling of feature with a multi-modal distribution:
![multimodal](https://github.com/ChristopherCarignan/multivariate-resampling/blob/master/oversamp_multimodal.png)

Example of over-sampling of feature with a skewed distribution:
![skewed](https://github.com/ChristopherCarignan/multivariate-resampling/blob/master/oversamp_skewed.png)
