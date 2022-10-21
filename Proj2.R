

#create function pone

pone <- function( n,k, strategy,nreps){
  
  #this function estimate the probability of single prisoner to find their number
 if (strategy==1) {
   #strategy 1 true 
    prisoner=1:2*n
    for (i in nreps){
     #prisoners number
     prisoners= 1:2*n
     
     #random boxes
     boxes= sample(1:2*n)
     
     #cards numbers
     cards= 1:2*n
     
     #randomely put card in boxes
     inside_box= boxes[prisoner]
     
   }
   
   
   #if strategy 1 choose apply
 }
 else if (strategy==2){
   
   # go with strategy 2
 }
 else {
   
   # go with strategy 3 
 }
  
}


#CB
#Pall

pall<- function(n,strategy, nreps){
  k<- 1:2*n     #All prisoner numbers
  #setting probability of full success to 1 to begin
  pfs<-1
  for (i in k){ #looping over all prisoners
    #Individual prisoner probability of escaping
    ipp<-pone(n,i,strategy,nreps) 
    pfs<- pfs*ipp #multiplying probability of full success by individual probability
    #Once loop terminates, all prisoner probabilities will have been multiplied
    #leaving final probability of all prisoners escaping
  }
  
}