# Filename: multivar_resamp.r
# Date: 2019-06-26
# Author: Christopher Carignan
# Email: c.carignan@phonetik.uni-muenchen.de
# Institution: Institute of Phonetics and Speech Processing (IPS), Ludwig-Maximilians-Universität München, Munich, Germany
# Description:
#   Performs resampling (both under- and over-sampling) of observations in a multivariate data frame
# Input arguments:
#   inputdata: data frame to use in resampling
#   groupby: string matching the column name containing the group labels to use in resampling
#   resampnum: number of observations used as the resampling target
#   features: a list of strings matching the column names of the features/dimensions used in creating new observations
# Output arguments:
#   outputdata: data frame with resampled observations; 
#               every level of 'groupby' will contain a number of observations equal to 'resampnum'

# Example function call:
# resamp.dat <- multivar_resamp(my.dat, "speaker", 5000, c("F1","F2","F3","P0","P1"))

# Main function
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
        # let's oversample!
        oversamp              <- resampnum - nrow(subdata) # number of new observations to use in oversampling
        sub.scaled            <- subdata # original data to scale
        means                 <- apply(as.matrix(sub.scaled[,features]), 2, mean) # get the original feature means
        stdevs                <- apply(as.matrix(sub.scaled[,features]), 2, sd) # get the original feature standard deviations
        sub.scaled[,features] <- scale(subdata[,features]) # scale the original features
        oversamp.data         <- c() # oversampled data to build
        
        # set zero-variance columns to 0
        zero.vars <- unique(which(is.na(sub.scaled), arr.ind=T)[,2])
        sub.scaled[,zero.vars] <- 0
        
        for (samp in 1:oversamp) {
          # randomly choose an observation from the scaled feature matrix
          this.samp   <- sub.scaled[sample(nrow(sub.scaled), 1), ]
          
          # select all of the OTHER observations from the scaled feature matrix
          other.samps <- sub.scaled[which(row.names(sub.scaled)!=row.names(this.samp)), ]
          
          # calculate Euclidean distances between the selected observation and all other observations
          dists <- apply(as.matrix(other.samps[,features]), 1, function(x) sqrt(sum((x - as.matrix(this.samp[,features]))^2)))
          
          # sort by distance
          neighbors <- sort(dists)
          
          # while loop which ensures that no duplicate observations are created in the oversampling process
          n.check <- 0
          while (n.check == 0) {
            # select one of the neighbors from within a Gaussian PDF
            # possible duplicates are ignored in two steps
            n.dist    <- sample(neighbors[neighbors>0], 
                                prob = dnorm(1:length(neighbors[neighbors>0]),0,round(0.3413*length(neighbors[neighbors>0]))),
                                size = 1)
            neighbor  <- which(dists == n.dist)[1]
            
            # create a new observation by calculating a weighted average of the feature vectors 
            # associated with the selected observation and its selected neighbor:
            s.dist <- (n.dist-min(dists))/diff(range(dists))
            
            # the farther the distance, the more the features resemble the selected observation
            # the closer the distance, the more the features resemble the selected neighbor
            new.features <- (
              (1-s.dist) * (sub.scaled[which(row.names(sub.scaled)==row.names(other.samps[neighbor,])), features]) +
                s.dist * (sub.scaled[which(row.names(sub.scaled)==row.names(this.samp)), features])
            )
            
            # convert weighted features back to their respective original scales
            # using the means and standard deviations of the original features
            new.features <- (new.features * stdevs) + means
            
            # replace old features with new features
            this.samp[,features] <- new.features
            
            # check if it is a duplicate oversampled observation
            dup.check <- duplicated(rbind(oversamp.data,this.samp))
            
            # if it is NOT a duplicate, exit the loop and add it to the data frame
            # if it IS a duplicate, run this loop again with a different neighbor
            if (length(dup.check[dup.check==TRUE])==0) {
              n.check <- 1
            }
          }
          # add new observation to data frame
          oversamp.data <- rbind(oversamp.data,this.samp)
        }
        # replace zero-variance columns with original data
        oversamp.data[,zero.vars] <- unique(subdata[,zero.vars])
        
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