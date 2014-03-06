P_ceq <-
function(i,c,N){
	if(length(c) == 0){return(NULL)}
	res = rep(-1,length(c))
	if(i == 0){
		return(0)
	}
	if(i >= 2){res[c==1] = 0} #only the first two points can be at distance 1
	if(any(i < 2 & c==1)){
		res[i < 2 & c==1] = P_cge_ale(2,2,1,N)
	}
	if((N %% 2) == 0 && i == N-2){ #special case N even, c = N/2
		res[c == N/2] = exp(2*lchoose(N-2,i-1)+lfactorial(i-1)-lfactorial(N-1))
	}
	if(i == N-1){
		res[c==floor(N/2)] = 1
	}
	#r = 2
	#i0 = i - 1
	#d = 2*c -2 -i0
	ind = res == -1
	if(any(ind)){
		cvals = c[ind]
		d = 2*cvals-i-1
		res[ind] = parameters(2,6*d^2+6*d+2,i-1,cvals,N,d)+
				#r = 3
				#i0 = i-1
				parameters(3,4*d^3,i-1,cvals,N,d);
		#i0 = i-2
		if(i-2 >= 0 && i <= N-1){ #i0 <= N -r <=> i <= N-r+2
			d = 2*cvals -i
			res[ind] = res[ind] + parameters(3,4*d^3,i-2,cvals,N,d);
		}
		#r = 4
		#i0 = i-1
		d = 2*cvals-i-1
		res[ind] = res[ind] + parameters(4,d^2*(d-1)^2,i-1,cvals,N,d);
		#i0 = i-2
		if(i-2 >= 0 && i <= N-2){ #i0 <= N -r <=> i <= N-r+2
			d = 2*cvals -i
			res[ind] = res[ind] + parameters(4,d^2*(d-1)^2,i-2,cvals,N,d);
			#i0 = i-3
			if(i-3 >= 0 && i<=N-1){
				d = 2*cvals -i+1
				res[ind] = res[ind] + parameters(4,d^2*(d-1)^2,i-3,cvals,N,d);
			}
		}
	}#if any(ind)
	return(res)
}
