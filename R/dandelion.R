dandelion <- function(fact_load,bound=0.5)
{
	if(class(fact_load) != "loadings")
	{
		 cat(" Example : dandelion(loading object,bound=0)\n")
		 stop("please use a loadings object")

	}
	
	if((bound < 0) || (bound > 1))
	{
		stop("bound must be between 0 and 1")
	}	
########################################################################################################################################################
 load_grid <- function(fact_load1,fact_load2,x3,y3,x2,y2,tempx,tempy)
 {
	# Shifting from Red to Blue 
 	if(fact_load1 > 0 && fact_load2 < 0)
 	{
 		x4 <- tempx + ((x3-tempx)*(1/100))
 		y4 <- tempy + ((y3-tempy)*(1/100))
 		polygon(c(x4,x2,tempx),c(y4,y2,tempy),col=rgb(1,0.4,0),border=rgb(1,0.4,0))
 
 		for( k in 2:100 )
	      {
 			tempx4 <- x4 ; tempy4 <- y4
 			x4 <- tempx + ((x3-tempx)*(k/100))
 			y4 <- tempy + ((y3-tempy)*(k/100))
 			polygon(c(x4,x2,tempx4),c(y4,y2,tempy4),col=rgb(1-(k/100),0.4,k/100),border=rgb(1-(k/100),0.4,k/100))
 		}
 	}
	# Shifting from Blue to Red 
 	else if(fact_load1 < 0 && fact_load2 > 0) 
 	{
 		x4 <- tempx + ((x3-tempx)*(1/100))
 		y4 <- tempy + ((y3-tempy)*(1/100))
 		polygon(c(x4,x2,tempx),c(y4,y2,tempy),col=rgb(0,0.4,1),border=rgb(0,0.4,1))
 
 		for( k in 2:100 )
 		{
 			tempx4 <- x4 ; tempy4 <- y4
 			x4 <- tempx + ((x3-tempx)*(k/100))
 			y4 <- tempy + ((y3-tempy)*(k/100))
 			polygon(c(x4,x2,tempx4),c(y4,y2,tempy4),col=rgb(k/100,0.4,1-(k/100)),border=rgb(k/100,0.4,1-(k/100)))
 		}
 	}
 	else if(fact_load1 < 0 && fact_load2 < 0) 
 	{
 		polygon(c(x3,x2,tempx),c(y3,y2,tempy),col=rgb(1,0.4,0),border=rgb(1,0.4,0))
 	}
 	else if(fact_load1 > 0 && fact_load2 > 0) 
 	{
 		polygon(c(x3,x2,tempx),c(y3,y2,tempy),col=rgb(0,0.4,1),border=rgb(0,0.4,1))
 	}
 }
##################################################################################

	old_par <- par("fig","mar")
	factor <- ncol(fact_load) 
      commun_fact <- apply (fact_load ,1 , function(x) sum(x^2))
	unique_fact <- 1 - commun_fact 
	lambda <-NULL 
      length_star <- 360/nrow(fact_load)
  
 	for ( j in 1:factor) 
      {
 		lambda <- c(lambda,sum(fact_load[,j]^2))
  	}	

	lambda2 <- sort(lambda,decreasing=TRUE)
	deneme <- NULL

	for( i in 1:factor) deneme <- c(deneme,which(lambda2[i]==lambda)) 
      lambda <- lambda[deneme] 
	fact_load <- fact_load[,deneme]
  	count <- NULL

  	for( i in 1:factor)
  	{
  		for(j in 1:nrow(fact_load))
  		{
  			if(abs(fact_load[j,i])==max(abs(fact_load[j,]))) count <- c(count,j)
  		}
  	}
 
  	fact_load <- fact_load[count,]
 


  	aci <- 360*(lambda/nrow(fact_load))
      limit <- NULL
  	for ( m in 1:factor ) 
  	{
  		if( aci[m] > 180 ) 
 		{ 
 			temp <- 360-aci[m]
 			limit[m] <- sqrt(2*(1-cos(temp*pi/180)))    
            }
 		else limit[m] <- sqrt(2*(1-cos(aci[m]*pi/180)))                    
  	}
 
      degreef <- 270
      maxxk <- minxk <- 1
      maxyk <- minyk <- 1
 
      xmaxlimit <- cos(degreef*pi/180) 
      ymaxlimit <- sin(-degreef*pi/180) 
      xminlimit <- xmaxlimit 
      yminlimit <- ymaxlimit 
  	degreef <- (degreef+aci[1])%%360  
  
      for( k in 2:length(lambda)) 
  	{
 		
      	xlimit <- cos(degreef*pi/180)
            if(xlimit >= xmaxlimit){ xmaxlimit <- xlimit ; maxxk = k }
            if(xlimit < xminlimit){ xminlimit <- xlimit ; minxk =k }
             
            ylimit <- sin(-degreef*pi/180)
            if(ylimit >= ymaxlimit){ ymaxlimit <- ylimit ; maxyk = k }
            if(ylimit < yminlimit){ yminlimit <- ylimit ; minyk =k }
 
            degreef <- (degreef+aci[k])%%360   
      }     
      
      if((max(limit)/2) >= xmaxlimit) xmaxlimit <- max(limit)/2
      if((max(limit)/2) < xminlimit)  xminlimit <- max(limit)/2
      xminlimit <- xminlimit-limit[minxk]/2
      xmaxlimit <- xmaxlimit+limit[maxxk]/2
      yminlimit <- yminlimit-limit[minyk]/2
      ymaxlimit <- ymaxlimit+limit[maxyk]/2
      
	old_par <- par("fig","mar")  
      par(mar=c(0,0,0,0))
      par(fig=c(0,0.6,0,1))
      plot(1,type="n",axes=FALSE,xlab="",ylab="",
      xlim=c(xminlimit,xmaxlimit),ylim=c(yminlimit,ymaxlimit),mar=c(0,0,0,0))
      box()
      degreev <- degreef <- 270        
 
  	for ( i in 1:factor) 
  	{
      	x2 = cos(degreef*pi/180)
        	y2 = sin(-degreef*pi/180)
        	lines(c(0,x2),c(0,y2), type="l",col="black")
        	degreef <- (degreef+aci[i])%%360 
 
       	xm = x2 + (limit[i]/2)*cos(degreev*pi/180)
       	ym = y2 + (limit[i]/2)*sin(-degreev*pi/180)     
  		if(abs(fact_load[1,i]) <= bound) datanew <- 0
       	else datanew <- (abs(fact_load[1,i])-bound)/(1-bound)        
  
 		x3 = x2 + ((limit[i]/2)*datanew)*(cos(degreev*pi/180))
       	y3 = y2 + ((limit[i]/2)*datanew)*(sin(-degreev*pi/180))
 		initx <- x3 ; inity <- y3
 		if(fact_load[1,i] > 0)  polygon(c(x3,x2),c(y3,y2),col=rgb(0,0.4,1),border=rgb(0,0.4,1))
 		if(fact_load[1,i] < 0)  polygon(c(x3,x2),c(y3,y2),col=rgb(1,0.4,0),border=rgb(1,0.4,0))
             
 		lines(c(x3,xm),c(y3,ym), type="l",col="grey")
            degreev <- (degreev+360/nrow(fact_load))%%360 
 
 		for (j in 2:(nrow(fact_load)+1))
 		{   
       		xm = x2 + (limit[i]/2)*cos(degreev*pi/180)
       		ym = y2 + (limit[i]/2)*sin(-degreev*pi/180)     
  
 			if(j==(nrow(fact_load)+1))
 			{
 				if(abs(fact_load[1,i]) <= bound) datanew <- 0
       			else datanew <- (abs(fact_load[1,i])-bound)/(1-bound) 
 			}
 			else
 			{
 				if(abs(fact_load[j,i]) <= bound) datanew <- 0
       			else datanew <- (abs(fact_load[j,i])-bound)/(1-bound) 
 			}
 
 			tempx <- x3 ; tempy <- y3
  			x3 = x2 + ((limit[i]/2)*datanew)*(cos(degreev*pi/180))
       		y3 = y2 + ((limit[i]/2)*datanew)*(sin(-degreev*pi/180))
 
 			if(j==(nrow(fact_load)+1)) load_grid(fact_load[1,i],fact_load[nrow(fact_load),i],x3,y3,x2,y2,tempx,tempy)
 			else 
 			{
 				load_grid(fact_load[j,i],fact_load[j-1,i],x3,y3,x2,y2,tempx,tempy)
 				degreev <- (degreev+360/nrow(fact_load))%%360
 			}
             	lines(c(x3,xm),c(y3,ym), type="l",col="grey")             
 		} 
         
        }
 
        x2 = cos(degreef*pi/180)
        y2 = sin(-degreef*pi/180)
        lines(c(0,x2),c(0,y2), type="l",col="black",lty=2)

#####################################################################################
# Ploting Uniquenesses 

  	  degreev <- 270
        par(fig=c(0.6,1,0,0.35),new=TRUE)
  	  par(mar=c(0,0,0.9,0))
        plot(1:10,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),axes=FALSE,main="uniquenesses")
  	  datapx <- NULL
  	  datapy <- NULL 
	         
        for(i in 1:nrow(fact_load))  
  	  {
        	x3 = cos(degreev*pi/180)
        	y3 = sin(-degreev*pi/180)
 	  	x4 = 1.25*cos(degreev*pi/180)
        	y4 = 1.25*sin(-degreev*pi/180)
 	  	x5 = unique_fact[i]*cos(degreev*pi/180)
        	y5 = unique_fact[i]*sin(-degreev*pi/180)
 
            srt2 <- 360 - degreev
		if(((360/nrow(fact_load))*(i-1)) > 180 ) srt2 <- srt2 - 180
        	
		lines(c(0,x3),c(0,y3),type="l",col="grey")
        	text(x4,y4,paste(abbreviate(rownames(fact_load)[i])),cex=0.8,srt=srt2) 
        	degreev <- (degreev+360/nrow(fact_load))%%360  
 	  	datapx <- c(datapx,x5)
        	datapy <- c(datapy,y5)
        }          
 
  	  polygon(datapx,datapy,col="blue",border="blue")
 
###########################################################################
# Ploting Communalities
 
  	  degreev <- 270
        par(fig=c(0.6,1,0.35,0.7),new=TRUE)
        plot(1:10,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),axes=FALSE,main="communalities")
   
  	  datapx <- NULL
  	  datapy <- NULL 
	  
 
   	  for(i in 1:nrow(fact_load))  
  	  {
        	x3 = cos(degreev*pi/180)
        	y3 = sin(-degreev*pi/180)
 		x4 = 1.25*cos(degreev*pi/180)
        	y4 = 1.25*sin(-degreev*pi/180)
 		x5 = commun_fact[i]*cos(degreev*pi/180)
  		y5 = commun_fact[i]*sin(-degreev*pi/180)

 		srt2 <- 360 - degreev
		if(((360/nrow(fact_load))*(i-1)) > 180 ) srt2 <- srt2 - 180
        	
		lines(c(0,x3),c(0,y3),type="l",col="grey")
        	text(x4,y4,paste(abbreviate(rownames(fact_load)[i])),cex=0.8,srt=srt2) 
        	degreev <- (degreev+360/nrow(fact_load))%%360  
 		datapx <- c(datapx,x5)
            datapy <- c(datapy,y5)
        }          
 
   	  polygon(datapx,datapy,col="blue",border="blue")
 
 
###########################################################################
  
  	  par(mar=c(0,0,0,0))
  	  par(fig=c(0.6,1,0.7,1),new=TRUE)
        library(gplots)      
  	  ara <- cbind(round(lambda, digits=2),
   	  round(cumsum(lambda/nrow(fact_load)),digits=2))
 
  	  rownames(ara) <- paste("fac.",1:factor,sep=" ")
  	  colnames(ara) <- c("Var.","Ratio")
        textplot(ara,valign="top",halign="left",mar=c(0,0,0,0))
        box()
                  
	  par(old_par)
  	  invisible()
  }

