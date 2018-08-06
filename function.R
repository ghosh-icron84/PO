mydataRead<-function(x="LME Sheet.csv"){
  data1 <- read.csv(x, sep=",",colClasses=c(Date="character",Date="character","character","character","character","numeric","numeric","numeric","numeric","numeric","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),header=TRUE)
  attach(data1)
  return(data1)
}

insertRow<-function(existingDF, newentry, r){
  lim<-length(existingDF[,28])
  if (r<=lim)
  {
    existingDF[seq(r+1,nrow(existingDF)+1),]<-
      existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,]<-newentry
    existingDF
    write.csv(existingDF,file ="LME Sheet.csv",row.names=FALSE)}
}
changeRow<-function(existingDF, newentry, r){
  lim<-length(existingDF[,28])
  if (r<=lim)
  {
    existingDF[r,]<-newentry
    existingDF
    write.csv(existingDF,file = "LME Sheet.csv",row.names=FALSE)}
}
newRow<-function(existingDF, newentry, Names){
  existingDF<-rbind(existingDF,newentry)
  names(existingDF)<-Names
  write.csv(existingDF,file = "LME Sheet.csv",row.names=FALSE)
}

deleteLastRow<-function(existingDF){
  r<-(NROW(existingDF))
  existingDF<-existingDF[-r,]
  write.csv(existingDF,file = "LME Sheet.csv", row.names= FALSE) 
}
deleteARow<-function(existingDF,r){
  existingDF<-existingDF[-r,]
  write.csv(existingDF,file = "LME Sheet.csv",row.names= FALSE) 
}
