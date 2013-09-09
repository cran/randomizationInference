#permutation of assignment vector using complete randomization

completeRand=function(w,nperm){
	#formatting
	if(sum(factor(w)==w)>0) w=data.matrix(w)
	if(ncol(w)==1) w=as.vector(w)
	#permutations
	if(is.vector(w)) lapply(1:nperm,function(i) sample(w))
	else lapply(1:nperm,function(i) w[sample(length(w[,1])),])
}
