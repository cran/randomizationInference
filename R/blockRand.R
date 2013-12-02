#permutation of assignment vector using randomized block design

blockRand=function(w,nperm,block){
      #formatting
      if(sum(factor(w)==w)>0) w=data.matrix(w)
      if(ncol(w)==1) w=as.vector(w)
      #permutations
      ctrl = how(within = Within(type = "free"), blocks = as.factor(block))
      if(is.vector(w)){
		set = shuffleSet(length(w), nset = nperm, control = ctrl)
		lapply(1:nperm,function(i) w[set[i,]])
	}else{
		set = shuffleSet(length(w[,1]), nset = nperm, control = ctrl)
		lapply(1:nperm,function(i) w[set[i,],])
	}
}