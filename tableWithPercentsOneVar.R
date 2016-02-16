brackets<-function(x,y){paste(x , '(' , y , ')' , sep="")}

tab1var<-function(XX){
  
  
  base<-addmargins(table(XX))
  last.val<-base[length(base)]
  
  last.val.per<- round(prop.table(last.val)*100,1)
  
  base1<-table(XX)
  base1.per<-round(prop.table(base1)*100,1)
  
  meat.per<-c(base1.per, last.val.per)
  
  n1.m<-dim(base)[1]
  n2.m<-dim(base)[2]
  meat.tab<-brackets(base, meat.per )
  
  row.name.base <- rownames(base)
  row.name.base1<- row.name.base[-length(row.name.base)]
  row.name.base.final<-c(row.name.base1, "Total")
  
  
  meat.tab1 <- cbind(row.name.base.final, meat.tab)
  
  return(meat.tab1)}
