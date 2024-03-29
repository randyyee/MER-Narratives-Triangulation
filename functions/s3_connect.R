library(aws.s3)
library(readxl)
library(paws)
library(jsonlite)

#' @export
#' @title s3_connect()
#'
#' @description Standard function for connecting to S3.
#'
#'
s3_connect <- function() {
  
  #Set the profile and region here
  Sys.setenv(SECRET_NAME = Sys.getenv("SECRET_NAME"),
             AWS_DEFAULT_REGION = Sys.getenv("AWS_REGION"),
             AWS_REGION = Sys.getenv("AWS_REGION"))
  
  svc <- secretsmanager()
  
  #Put the name of the secret which contains the aws key info
  see <- svc$get_secret_value(
    SecretId = Sys.getenv("SECRET_NAME")
  )
  
  see <- fromJSON(see$SecretString)
  
  #Fill in the strings
  Sys.setenv(AWS_ACCESS_KEY_ID = see$aws_access_key,
             AWS_SECRET_ACCESS_KEY = see$aws_secret_access_key,
             AWS_DEFAULT_REGION = Sys.getenv("AWS_REGION"),
             AWS_REGION = Sys.getenv("AWS_REGION")
  )
  
  #Delete the secret info as its now an env variable
  rm(see)
  rm(svc)
  
  
}