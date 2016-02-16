
brackets<-function(x,y){paste(x , '(' , y , ')' , sep="")}

tab2var<-function(XX, YY){
  
base<-addmargins(table(XX,YY))
last.col<-base[, length(base[1,])]
last.col<-last.col[-length(last.col)]
last.col.per<- round(prop.table(last.col)*100,1)

last.row<-base[length(base[,1]),]
Total<-last.row[length(last.row)]
Total.per <- 100
last.row<-last.row[-length(last.row)]
last.row.per<- round(prop.table(last.row)*100,1)

base1<-table(XX,YY)
base1.per<-round(prop.table(base1,2)*100,1)

meat<-rbind(base1.per, last.row.per)
last.col.Total<-c(last.col.per, Total.per)

meat.per<-cbind(meat, last.col.Total)
base
n1.m<-dim(base)[1]
n2.m<-dim(base)[2]
meat.tab<-matrix(brackets(base, meat.per ),n1.m,n2.m)

row.name.base <- rownames(base)
row.name.base1<- row.name.base[-length(row.name.base)]
row.name.base.final<-c(row.name.base1, "Total")

col.name.base <- colnames(base)
col.name.base1<- col.name.base[-length(col.name.base)]
col.name.base.final<-c("",col.name.base1, "Total")

meat.tab1 <- cbind(row.name.base.final, meat.tab)
colnames(meat.tab1) <-col.name.base.final

return(meat.tab1)}
