#Latin square permutations

latinRand=function(w,nperm,row,col){
	#formatting
	if(sum(factor(w)==w)>0) w=data.matrix(w)
	if(ncol(w)==1) w=as.vector(w)
	#permutations
	if(is.vector(w)){
		n=length(unique(w))
		rowperm=replicate(nperm,rep(c(1,sample(2:n)),n))
		colperm=replicate(nperm,as.vector(matrix(rep(sample(1:n),n),nrow=n,ncol=n,byrow=TRUE)))
		lapply(1:nperm,function(j) w[order(col,row)][order(colperm[,j],rowperm[,j])])
	}else{
		n=length(unique(w[,1]))
		rowperm=replicate(nperm,rep(c(1,sample(2:n)),n))
		colperm=replicate(nperm,as.vector(matrix(rep(sample(1:n),n),nrow=n,ncol=n,byrow=TRUE)))
		lapply(1:nperm,function(j) w[order(col,row),][order(colperm[,j],rowperm[,j]),])
	}
}
