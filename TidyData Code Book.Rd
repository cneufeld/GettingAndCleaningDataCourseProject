\name{Code Book}
\alias{Code Book}
\docType{data}
\title{
Getting and Cleaning Data Course Project Code Book
}
\description{
This Code Book describes the data set created by the run_analysis function for the Getting And Cleaning Data Class Project.  
}
\usage{
After downloading and unzipping the data for the project, load the source for this project and run the 'run_analysis()' function.  It will create a "tidy" dataset with the following format, and save it to a file "tidydata.csv".
}
\format{
  A data frame with 180 observations on the following 68 variables.
  \describe{
    \item{\code{Activity}}{a factor with levels \code{WALKING} \code{WALKING_UPSTAIRS} \code{WALKING_DOWNSTAIRS} \code{SITTING} \code{STANDING} \code{LAYING}}
    \item{\code{SubjectId}}{a factor with levels \code{1} \code{3} \code{5} \code{6} \code{7} \code{8} \code{11} \code{14} \code{15} \code{16} \code{17} \code{19} \code{21} \code{22} \code{23} \code{25} \code{26} \code{27} \code{28} \code{29} \code{30} \code{2} \code{4} \code{9} \code{10} \code{12} \code{13} \code{18} \code{20} \code{24}}
    \item{\code{TimeBodyAccMeanX}}{a numeric vector}
    \item{\code{TimeBodyAccMeanY}}{a numeric vector}
    \item{\code{TimeBodyAccMeanZ}}{a numeric vector}
    \item{\code{TimeBodyAccStdX}}{a numeric vector}
    \item{\code{TimeBodyAccStdY}}{a numeric vector}
    \item{\code{TimeBodyAccStdZ}}{a numeric vector}
    \item{\code{TimeGravityAccMeanX}}{a numeric vector}
    \item{\code{TimeGravityAccMeanY}}{a numeric vector}
    \item{\code{TimeGravityAccMeanZ}}{a numeric vector}
    \item{\code{TimeGravityAccStdX}}{a numeric vector}
    \item{\code{TimeGravityAccStdY}}{a numeric vector}
    \item{\code{TimeGravityAccStdZ}}{a numeric vector}
    \item{\code{TimeBodyAccJerkMeanX}}{a numeric vector}
    \item{\code{TimeBodyAccJerkMeanY}}{a numeric vector}
    \item{\code{TimeBodyAccJerkMeanZ}}{a numeric vector}
    \item{\code{TimeBodyAccJerkStdX}}{a numeric vector}
    \item{\code{TimeBodyAccJerkStdY}}{a numeric vector}
    \item{\code{TimeBodyAccJerkStdZ}}{a numeric vector}
    \item{\code{TimeBodyGyroMeanX}}{a numeric vector}
    \item{\code{TimeBodyGyroMeanY}}{a numeric vector}
    \item{\code{TimeBodyGyroMeanZ}}{a numeric vector}
    \item{\code{TimeBodyGyroStdX}}{a numeric vector}
    \item{\code{TimeBodyGyroStdY}}{a numeric vector}
    \item{\code{TimeBodyGyroStdZ}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkMeanX}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkMeanY}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkMeanZ}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkStdX}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkStdY}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkStdZ}}{a numeric vector}
    \item{\code{TimeBodyAccMagMean}}{a numeric vector}
    \item{\code{TimeBodyAccMagStd}}{a numeric vector}
    \item{\code{TimeGravityAccMagMean}}{a numeric vector}
    \item{\code{TimeGravityAccMagStd}}{a numeric vector}
    \item{\code{TimeBodyAccJerkMagMean}}{a numeric vector}
    \item{\code{TimeBodyAccJerkMagStd}}{a numeric vector}
    \item{\code{TimeBodyGyroMagMean}}{a numeric vector}
    \item{\code{TimeBodyGyroMagStd}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkMagMean}}{a numeric vector}
    \item{\code{TimeBodyGyroJerkMagStd}}{a numeric vector}
    \item{\code{FFTBodyAccMeanX}}{a numeric vector}
    \item{\code{FFTBodyAccMeanY}}{a numeric vector}
    \item{\code{FFTBodyAccMeanZ}}{a numeric vector}
    \item{\code{FFTBodyAccStdX}}{a numeric vector}
    \item{\code{FFTBodyAccStdY}}{a numeric vector}
    \item{\code{FFTBodyAccStdZ}}{a numeric vector}
    \item{\code{FFTBodyAccJerkMeanX}}{a numeric vector}
    \item{\code{FFTBodyAccJerkMeanY}}{a numeric vector}
    \item{\code{FFTBodyAccJerkMeanZ}}{a numeric vector}
    \item{\code{FFTBodyAccJerkStdX}}{a numeric vector}
    \item{\code{FFTBodyAccJerkStdY}}{a numeric vector}
    \item{\code{FFTBodyAccJerkStdZ}}{a numeric vector}
    \item{\code{FFTBodyGyroMeanX}}{a numeric vector}
    \item{\code{FFTBodyGyroMeanY}}{a numeric vector}
    \item{\code{FFTBodyGyroMeanZ}}{a numeric vector}
    \item{\code{FFTBodyGyroStdX}}{a numeric vector}
    \item{\code{FFTBodyGyroStdY}}{a numeric vector}
    \item{\code{FFTBodyGyroStdZ}}{a numeric vector}
    \item{\code{FFTBodyAccMagMean}}{a numeric vector}
    \item{\code{FFTBodyAccMagStd}}{a numeric vector}
    \item{\code{FFTBodyBodyAccJerkMagMean}}{a numeric vector}
    \item{\code{FFTBodyBodyAccJerkMagStd}}{a numeric vector}
    \item{\code{FFTBodyBodyGyroMagMean}}{a numeric vector}
    \item{\code{FFTBodyBodyGyroMagStd}}{a numeric vector}
    \item{\code{FFTBodyBodyGyroJerkMagMean}}{a numeric vector}
    \item{\code{FFTBodyBodyGyroJerkMagStd}}{a numeric vector}
  }
}
\details{
The dataset contains 68 columns.  The first two columns identify the Activity and the Subject.  The remaining columns contain the average values for a variety of accelerometer and gyroscope measurements as described below (NOTE: Taken from the source data "features_info.txt" file).  To make the data columns more readable, the suffixes "-std()" and "-mean()" were replaced by proper camel cased names "Std" and "Mean" (without the brackets).  The prefix "t" was replaced with the word "Time", the prefix "f" was replaced with the word "FFT" (for Fast Fourier Transform).  

From the source documentation features_info.txt:

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

}
\source{
A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{

}
\keyword{datasets}
