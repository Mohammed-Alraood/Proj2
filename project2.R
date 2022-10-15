
#Function that calculates the probability of the prisoner escaping
#input n: number of boxes the prisoner can open, 2*n prisoners
  # k: prisoner number
  # Strategy number used for number search
  #nreps: the number of times to replicate for accurate probability
  #function returns a number between 0 and 1, being the probability of the prisoner
    #escaping using corresponding strategy
pone <- function( n,k, strategy,nreps){
  count<-0 #counter set at 0, to add to it, every time a prisoner is successful
  
  for (i in 1:nreps){  #replicating the simulation nreps amount of times
    cards=sample(1:(2*n))  #randomly assigning the card numbers to 2*n boxes
    
    
    if (strategy==1) { #If strategy 1, do the following
      card<-cards[k]  #Choose the box with the prisoners number on, assign card with the card number inside the box
      for (i in (1:n-1)){ #looping over n-1 boxes, since the prisoner already opened one box
        if (card==k){ #if the card is equal to the prisoners number
          count<-count+1 #add one to the success counter, as the prisoner was successful
          break}      #hence break out of the loop, he is finished
        else { #if the number is not the prisoners number
          card=cards[card] #open the box corresponding to the card, reassign card value with card from the new box.
                }}
    }
    
    
    if (strategy ==2){
      box<- sample(cards,1) #randomly choose 1st box
      card<-cards[box] #opening random box and assigning the card number inside
      for (i in (1:n-1)){ #looping over n-1 boxes, since the prisoner already opened one box
        if (card==k){ #if the card is equal to the prisoners number
          count<-count+1 #add one to the success counter, as the prisoner was successful
          break}      #hence break out of the loop, he is finished
        else { #if the number is not the prisoners number
          card=cards[card] #open the box corresponding to the card, reassign card value with card from the new box.
        }}
      
      
      
    }
    
    if (strategy==3){
      for(i in 1:n){ #opening n boxes
        box<-sample(cards,1) #random box selected every time
        card<-cards[box] #assigning card to the card number found in the box
        if (card==k){ #if card is the same as the prisoners number
          count<-count +1 #adds one to the success count
          break} #break, the prisoner is finished and has been successful
        }
        
      }
      
      
      
    
    
    
    
    
    
    
  }
  prob<-count/nreps #the probability of the prisoner breaking free, is the number of successes/ total replicates
  return(prob) #returns the probability
}


pall<- function(n,strategy, nreps){
  k<- 1:(2*n)     #All prisoner numbers
  
  #setting probability of full success to 1 to begin
  pfs<-1
  for (i in k){ #looping over all prisoners
    
    #Individual prisoner probability of escaping
    ipp<-pone(n,i,strategy,nreps) 
    pfs<- pfs*ipp #multiplying probability of full success by individual probability
    #Once loop terminates, all prisoner probabilities will have been multiplied
    #leaving final probability of all prisoners escaping 
  }
  return(pfs)
}









