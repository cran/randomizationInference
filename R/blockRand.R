#permutation of assignment vector using randomized block design

blockRand=function(w,nperm,block){
	#formatting
	if(sum(factor(w)==w)>0) w=data.matrix(w)
	if(ncol(w)==1) w=as.vector(w)
	#permutations
	if(is.vector(w)) lapply(1:nperm,function(i) w[shuffle(length(w),permControl(strata=block,within=Within(type="free")))])
	else lapply(1:nperm,function(i) w[shuffle(length(w[,1]),permControl(strata=block,within=Within(type="free"))),])
}
