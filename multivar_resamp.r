# Filename: multivar_resamp.r
# Date: 2019-06-26
# Author: Christopher Carignan
# Email: c.carignan@phonetik.uni-muenchen.de
# Institution: Institute of Phonetics and Speech Processing (IPS), Ludwig-Maximilians-Universität München, Munich, Germany
# Description:
#   Converts an audio file to a 3D spectrogram; (optionally) saves as a stereolithography (STL) file for 3D printing
# Arguments:
#   inputfile (required): filepath string associated with WAV audio file
#   outputfile (optional): filepath sring associated with STL file to save
#   sampfreq: re-sampling frequency
#   axisnorm: keep the x- and y-axes scaling (FALSE) or normalize so that they have the same dimensions (TRUE)
#   preemph: value for pre-emphasis of audio
#   window: option for windowing audio using a variety of Matlab/Octave compatible filters found in the "signal" package
#     ex: 'bartlett', 'blackman', 'hamming', 'hanning', 'triang'


# load required libraries
require(pracma)

multivar_resamp <- function (inputdata, groupby, resampnum, features) {

  outputdata <- c()
  for (group in unique(inputdata[[groupby]])) {
    subdata <- inputdata[inputdata[[groupby]]==group,]
    
    if (nrow(subdata) > resampnum) { # more samples than the desired number?
      # undersample to the desired number
      newdata <- subdata[sample(nrow(subdata), resampnum, replace=F), ]
      
      # print a success message
      print(paste0("'",groupby,"' level named '", group, "' has been under-sampled from ", nrow(subdata), " to ", resampnum, " observations"), quote=F)
      
    } else {
      if (nrow(subdata) == resampnum) { # same number of samples as the desired number?
        # keep the original data as-is
        newdata <- subdata
        
        # print a success message
        print(paste0("'",groupby,"' level named '", group, "' has been kept at ", nrow(subdata), " observations"), quote=F)
        
      } else { # fewer samples than the desired number?
        # oversample!
        oversamp              <- resampnum - nrow(subdata) # number of new observations to use in oversampling
        sub.scaled            <- subdata # original data to scale
        means                 <- apply(sub.scaled[,features], 2, mean) # get the original feature means
        stdevs                <- apply(sub.scaled[,features], 2, sd) # get the original feature standard deviations
        sub.scaled[,features] <- scale(subdata[,features]) # scale the original features
        oversamp.data         <- c() # oversampled data to build
        
        for (samp in 1:oversamp) {
          # randomly choose an observation from the scaled feature matrix
          this.samp   <- sub.scaled[sample(nrow(sub.scaled), 1), ]
          # select all of the OTHER observations from the scaled feature matrix
          other.samps <- sub.scaled[which(row.names(sub.scaled)!=row.names(this.samp)), ]
          
          # calculate Euclidean distances between the selected observation and all other observations
          dists     <- as.numeric(pracma::distmat(as.matrix(this.samp[, features]), as.matrix(other.samps[, features])))
          # find the neighbors nearest to the selected observation
          # number of nearest neighbors = 1% of total number of original observations
          neighbors <- sort(dists)[1:round(nrow(subdata) * 0.01)]
          
          # while loop which ensures that no duplicate observations are created in the oversampling process
          n.check <- 0
          while (n.check == 0) {
            # randomly select one of the nearest neighbors
            n.dist    <- sample(neighbors, 1)
            neighbor  <- which(dists == n.dist)
            
            # create a new observation by calculating a weighted average of the feature vectors 
            # associated with the selected observation and its selected nearest neighbor
            new.features <- (
              sub.scaled[which(row.names(sub.scaled)==row.names(other.samps[neighbor,])), features] / n.dist + 
                sub.scaled[which(row.names(sub.scaled)==row.names(this.samp)), features] * n.dist
            ) / (n.dist + 1/n.dist) 
            
            # convert weighted features back to their respective original scales
            # using the means and standard deviations of the original features
            new.features <- (new.features * stdevs) + means
            
            # replace old features with new features
            this.samp[,features] <- new.features
            
            # check if it is a duplicate oversampled observation
            dup.check <- duplicated(rbind(oversamp.data,this.samp))
            
            # if it is NOT a duplicate, exit the loop and add it to the data frame
            # if it IS a duplicate, run this loop again with a different nearest neighbor
            if (length(dup.check[dup.check==TRUE])==0) {
              n.check <- 1
            }
          }
          # add new observation to data frame
          oversamp.data <- rbind(oversamp.data,this.samp)
        }
        # add oversampled data to original data frame
        newdata <- rbind(subdata,oversamp.data)
        
        # print a success message
        print(paste0("'",groupby,"' level named '", group, "' has been over-sampled from ", nrow(subdata), " to ", resampnum, " observations"), quote=F)
      } 
    }
    # add resampled data to output dataset
    outputdata <- rbind(outputdata,newdata)
  }
  return(outputdata)
}