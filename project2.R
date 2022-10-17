
#Function that calculates the probability of the prisoner escaping
#input n: number of boxes the prisoner can open, 2*n prisoners
  # k: prisoner number
  # Strategy number used for number search
  #nreps: the number of times to replicate for accurate probability
  #function returns a number between 0 and 1, being the probability of the prisoner
pone <- function( n,k, strategy,nreps){
  count<-0 #counter set at 0, to add to it, every time a prisoner is successful
  
  for (i in 1:nreps){  #replicating the simulation nreps amount of times
    cards=sample(1:(2*n))  #randomly assigning the card numbers to 2*n boxes
    
    
    if (strategy==1) { #If strategy 1, do the following
      card<-cards[k]  #Choose the prisoners number box, assign card with the card number inside
      for (i in (1:n-1)){ #looping over n-1 boxes, since the prisoner already opened one box
        if (card==k){ #if the card is equal to the prisoners number
          count<-count+1 #add one to the success counter, as the prisoner was successful
          break}      #hence break out of the loop, he is finished
        else { #if the number is not the prisoners number
          card=cards[card] #open the box corresponding to the card, reassign card value with card inside
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


#Example for n=5

ranp<- sample(1:(2*5)) #selecting a random prisoner
for(i in 1:3){
 ip5<- pone(5,ranp,i,1000) #calculating individual probability for random prisoner, for each strategy
 cat(c("\nThe Probability of a random prisoner success out of 5 with strategy", i, "is:", ip5))
 jp5<- pall(5,i,1000) #calculating joint probabilities
 cat(c("\nThe Probability of all 5 prisoners escaping with strategy", i, "is:", sprintf("%.5f", jp5)))
 }

#Example for n=50

ranp<- sample(1:(2*50)) #selecting a random prisoner
for(i in 1:3){
  ip50<- pone(50,ranp,i,1000) #calculating individual probability for random prisoner, for each strategy
  cat(c("\nThe Probability of a random prisoner out of 50 success with strategy", i, "is:", ip50))
  jp50<- pall(50,i,1000) #calculating joint probabilities
  cat(c("\nThe Probability of all 50 prisoners escaping with strategy", i, "is:", sprintf("%.5f", jp50)))
}






