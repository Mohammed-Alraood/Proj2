

#create function pone

pone <- function( n,k, strategy,nreps){
  
  #this function estimate the probability of single prisoner to find their number
 if (strategy==1) {
   #strategy 1 true 
    prisoner=1:2n
    for (i in nreps){
     #prisoners number
     prisoners= 1:2n
     
     #random boxes
     boxes= sample(1:2n)
     
     #cards numbers
     cards= 1:2n
     
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