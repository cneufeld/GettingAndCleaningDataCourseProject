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
    \item{\samp{TimeBodyAccMean()-X}}{a numeric vector}
    \item{\samp{TimeBodyAccMean()-Y}}{a numeric vector}
    \item{\samp{TimeBodyAccMean()-Z}}{a numeric vector}
    \item{\samp{TimeBodyAccStd()-X}}{a numeric vector}
    \item{\samp{TimeBodyAccStd()-Y}}{a numeric vector}
    \item{\samp{TimeBodyAccStd()-Z}}{a numeric vector}
    \item{\samp{TimeGravityAccMean()-X}}{a numeric vector}
    \item{\samp{TimeGravityAccMean()-Y}}{a numeric vector}
    \item{\samp{TimeGravityAccMean()-Z}}{a numeric vector}
    \item{\samp{TimeGravityAccStd()-X}}{a numeric vector}
    \item{\samp{TimeGravityAccStd()-Y}}{a numeric vector}
    \item{\samp{TimeGravityAccStd()-Z}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkMean()-X}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkMean()-Y}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkMean()-Z}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkStd()-X}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkStd()-Y}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkStd()-Z}}{a numeric vector}
    \item{\samp{TimeBodyGyroMean()-X}}{a numeric vector}
    \item{\samp{TimeBodyGyroMean()-Y}}{a numeric vector}
    \item{\samp{TimeBodyGyroMean()-Z}}{a numeric vector}
    \item{\samp{TimeBodyGyroStd()-X}}{a numeric vector}
    \item{\samp{TimeBodyGyroStd()-Y}}{a numeric vector}
    \item{\samp{TimeBodyGyroStd()-Z}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkMean()-X}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkMean()-Y}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkMean()-Z}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkStd()-X}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkStd()-Y}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkStd()-Z}}{a numeric vector}
    \item{\samp{TimeBodyAccMagMean()}}{a numeric vector}
    \item{\samp{TimeBodyAccMagStd()}}{a numeric vector}
    \item{\samp{TimeGravityAccMagMean()}}{a numeric vector}
    \item{\samp{TimeGravityAccMagStd()}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkMagMean()}}{a numeric vector}
    \item{\samp{TimeBodyAccJerkMagStd()}}{a numeric vector}
    \item{\samp{TimeBodyGyroMagMean()}}{a numeric vector}
    \item{\samp{TimeBodyGyroMagStd()}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkMagMean()}}{a numeric vector}
    \item{\samp{TimeBodyGyroJerkMagStd()}}{a numeric vector}
    \item{\samp{FFTBodyAccMean()-X}}{a numeric vector}
    \item{\samp{FFTBodyAccMean()-Y}}{a numeric vector}
    \item{\samp{FFTBodyAccMean()-Z}}{a numeric vector}
    \item{\samp{FFTBodyAccStd()-X}}{a numeric vector}
    \item{\samp{FFTBodyAccStd()-Y}}{a numeric vector}
    \item{\samp{FFTBodyAccStd()-Z}}{a numeric vector}
    \item{\samp{FFTBodyAccJerkMean()-X}}{a numeric vector}
    \item{\samp{FFTBodyAccJerkMean()-Y}}{a numeric vector}
    \item{\samp{FFTBodyAccJerkMean()-Z}}{a numeric vector}
    \item{\samp{FFTBodyAccJerkStd()-X}}{a numeric vector}
    \item{\samp{FFTBodyAccJerkStd()-Y}}{a numeric vector}
    \item{\samp{FFTBodyAccJerkStd()-Z}}{a numeric vector}
    \item{\samp{FFTBodyGyroMean()-X}}{a numeric vector}
    \item{\samp{FFTBodyGyroMean()-Y}}{a numeric vector}
    \item{\samp{FFTBodyGyroMean()-Z}}{a numeric vector}
    \item{\samp{FFTBodyGyroStd()-X}}{a numeric vector}
    \item{\samp{FFTBodyGyroStd()-Y}}{a numeric vector}
    \item{\samp{FFTBodyGyroStd()-Z}}{a numeric vector}
    \item{\samp{FFTBodyAccMagMean()}}{a numeric vector}
    \item{\samp{FFTBodyAccMagStd()}}{a numeric vector}
    \item{\samp{FFTBodyBodyAccJerkMagMean()}}{a numeric vector}
    \item{\samp{FFTBodyBodyAccJerkMagStd()}}{a numeric vector}
    \item{\samp{FFTBodyBodyGyroMagMean()}}{a numeric vector}
    \item{\samp{FFTBodyBodyGyroMagStd()}}{a numeric vector}
    \item{\samp{FFTBodyBodyGyroJerkMagMean()}}{a numeric vector}
    \item{\samp{FFTBodyBodyGyroJerkMagStd()}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(TidyData)
## maybe str(TidyData) ; plot(TidyData) ...
}
\keyword{datasets}
