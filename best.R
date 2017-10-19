best <- function(state, outcome) {
  outcomedf<-data.frame(as.character(c("heart attack", "heart failure", "pneumonia")), c("Heart.Attack", "Heart.Failure","Pneumonia"))
  names(outcomedf)<-c("outcome","variable")
  
    ## Read outcome data
  x<-read.csv("outcome-of-care-measures.csv")

  ## Check that state and outcome are valid
  if(!(state %in% x$State)){
    stop('invalid state')
  }
  if(!(outcome %in% outcomedf$outcome)){
    stop('invalid outcome')
  }
  beginvar<-"Hospital.30.Day.Death..Mortality..Rates.from."
  vartosearch<-paste(beginvar,as.character(outcomedf$variable[outcomedf$outcome==outcome]),sep="")
  xsub<-x[!is.na(x[[vartosearch]]) & x[[vartosearch]]!="Not Available" & x$State==state,]

  x.s <- xsub[order(as.numeric(as.character(xsub[[vartosearch]])), xsub$Hospital.Name), ]
  
  ## Return hospital name in that state with lowest 30-day death rate
  as.character(x.s$Hospital.Name[1])
}