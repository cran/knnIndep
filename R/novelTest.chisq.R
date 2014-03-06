novelTest.chisq <-
function(xdata,ydata,maxi=length(xdata)-1){
	rx= rank(xdata,ties.method="random")	
	ry= rank(ydata,ties.method="random")	
	N = length(rx)
	
	paths = sapply(1:maxi,generate.paths,rx,ry,N)
	stats = sapply(1:maxi,function(i){
		if(i==1){
			fi = P_ceq(i,1:floor(N/2),N)
		}else{
#			fi = c(0,rowMeans(pcgivena2[2:floor(N/2),paths[i-1,],i]-pcgivena2[1:(floor(N/2)-1),paths[i-1,],i]))
			fi = c(0,colMeans(sapply(2:floor(N/2),function(c){Pc_givena(i,rep(c,maxi),paths[i-1,],N)-Pc_givena(i,rep(c-1,maxi),paths[i-1,],N)})))
		}
		df = sum(fi > 0) -1
		ei = table(factor(paths[i,],levels=1:floor(N/2)))/N
		stat = sum((ei-fi)^2/fi,na.rm=T)
		return(c(stat,df))
	})
	stat = sum(stats[1,])
	res = list(statistic=stat,p.value=1-pchisq(stat,df=sum(stats[2,])))	
	class(res) = "htest"
	return(res)
}
