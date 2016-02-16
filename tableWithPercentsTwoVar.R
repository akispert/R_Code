brackets<-function(x,y){paste(x , '(' , y , ')' , sep="")}

tab3var<-function(XX, YY, ZZ){
  

base<-addmargins(table(XX,ZZ,YY))
base.f<-ftable(base, row.vars= "XX", col.vars=c("YY","ZZ"))

rr<-stats:::format.ftable(base.f, quote=F)
row.n<-dim(rr)[1]
col.n<-dim(rr)[2]
name.subcat1<-cbind(as.character(unique(YY)), "Sum")
subcat1<-length(unique(YY))+1
subcat2<-length(unique(ZZ))
mat.final<-list()

for (i in 1:subcat1){

mat1<-base[,,YY=name.subcat1[i]]
prop.table(mat1)

last.col<-mat1[, length(mat1[1,])]
last.col<-last.col[-length(last.col)]
last.col.per<- round(prop.table(last.col)*100,1)

last.row<-mat1[length(mat1[,1]),]
Total<-last.row[length(last.row)]
Total.per <- 100
last.row<-last.row[-length(last.row)]
last.row.per<- round(prop.table(last.row)*100,1)

base1<-mat1[-length(mat1[,1]),-length(mat1[1,]) ]
base1.per<-round(prop.table(base1,2)*100,1)
base1.per[is.na(base1.per)]<-0

meat<-rbind(base1.per, last.row.per)
last.col.Total<-c(last.col.per, Total.per)

meat.per<-cbind(meat, last.col.Total)

n1.m<-dim(mat1)[1]
n2.m<-dim(mat1)[2]
meat.tab<-matrix(brackets(mat1, meat.per ),n1.m,n2.m)

row.name.mat1 <- rownames(mat1)
row.name.mat1<- row.name.mat1[-length(row.name.mat1)]
row.name.mat1.final<-c(row.name.mat1, "Total")

col.name.mat1 <- colnames(mat1)
col.name.mat1<- col.name.mat1[-length(col.name.mat1)]
col.name.mat1.final<-c("",col.name.mat1, "Total")

meat.tab1 <- cbind(row.name.mat1.final, meat.tab)
meat.tab1 <-rbind(as.character(col.name.mat1.final),meat.tab1)

col.name.vect<-matrix("", 1,(n2.m+1))
col.name.vect[1,2]<-name.subcat1[i]

colnames(meat.tab1)<-col.name.vect
mat.final[[i]]<-meat.tab1
}

temp1<-dim(mat.final[[1]])[2]-1
tab1<-mat.final[[1]][,c(1:temp1)]
for (i in 2:(subcat1-1)){
  tab2<-mat.final[[i]]
  col.n<-dim(tab2)[2]
  tab1<-cbind(tab1, tab2[,c(2:(col.n-1))])
  
}
return(tab1)}

#tab.final<- cbind(tab1,mat.final[[7]][,col.n] )
