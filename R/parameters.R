parameters <-
function(r,kr,i0,c,N,d){
	if(length(c) == 0){return(NULL)}
	comb = rep(-1,length(c))
	comb[d < 0] = -Inf
	comb[kr <= 0] = -Inf
	comb[N-2*c-1 < 0] = -Inf
	comb[N-4*c+i0+r-1 < 0] = -Inf
	if(any(comb == -1)){
		comb[comb == -1] = 2*lchoose(2*c[comb == -1]-2,i0)+lfactorial(i0)+						#region I
				log(kr[comb == -1])+												#region R
				2*(lfactorial(N-2*c[comb == -1]-1) - lfactorial(N-4*c[comb == -1]+i0+r-1))+		#region IIa + IIb
				lfactorial(N-4*c[comb == -1]+i0+r-1) - lfactorial(N-1)				#region III
	}
	return(exp(comb))
}
