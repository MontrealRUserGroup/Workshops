
a<-as.matrix(read.table('attendees.csv',col.names=FALSE,sep=','))

winner<-a[sample.int(length(a[,1]),1)]

cat('\n\n\n\n And the winner is... ',winner,'! \n\n')

