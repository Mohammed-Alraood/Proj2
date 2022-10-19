
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
    cards<-sample(1:(2*n))  #randomly assigning the card numbers to 2*n boxes
    
    if (strategy==1) { #If strategy 1, do the following
      card<-cards[k]  #Choose the prisoners number box, assign card with the card number inside
      for (i in (1:n-1)){ #looping over n-1 boxes, since the prisoner already opened one box
        if (card==k){ #if the card is equal to the prisoners number
          count<-count+1 #add one to the success counter, as the prisoner was successful
          break}      #hence break out of the loop, he is finished
        else { #if the number is not the prisoners number
          card<-cards[card] #open the box corresponding to the card, reassign card value with card inside
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
          card<-cards[card] #open the box corresponding to the card, reassign card value with card from the new box.
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




##Function that estimates the probability of loop lengths from 1:2n atleast once in random shuffling of cards to boxes
dloop <- function(n,nreps){
  k<- 1:(2*n) #all the prisoners
  
  #empty vector to store each time a loop is found
  #entry for loops[i] will contain how many times a loop of length i is found
  loops<- integer(2*n) 
  for (i in (1:nreps)){ # repeating the simulation nreps times
    cards<- sample(1:(2*n)) #randomly shuffling the cards
    for (ii in k){ #looping over each prisoner
      box<- sample(cards,1) #randomly selecting a box to begin with
      card<-cards[box] #assigning the card found in that box to card  
      
      #opened vector will contain all of the cards the prisoner has found in sequential order
      opened<- c(card) # assign first entry as the first card found
      for (j in 1:((2*n)-1)){ #now loop to open all other boxes
        if (card==ii){  #if prisoner card found
          break        #break loop, no cycle found
        }
        else{
          card<-cards[card] #assign the next card to the current card
          opened<-append(opened,card) # append this entry to the found cards vector
        }
      }
      
      #last entry of opened vector will have to be removed for the next loop to work correctly
      
      opened<- opened[-(length(opened))] 
      
      if( card %in% opened){ #if the card was previously found
        #means card was found more than once and we have found a loop
        oi<- which(opened %in% card) #indices of the opened vector that contain the current card
        
        if (length(oi)==1){  # this condition for small n, such as 1, card will only be contained once
          ll<-(length(opened)+1)-oi #finding how many boxes were opened in the loop
        }
        else{
          ll<- oi[2]-oi[1] #otherwise, difference in indices is how many boxes opened between
          #ll is the loop length
        }
        
        #add one to the number of loops of length ll stored in loops
        #loops esentially tallies how many times we come across each loop length in the simulation
        loops[ll]=loops[ll]+1
      }
      
      
    }
    
    
  }
  
  
  
  
  
  #to find probabilities, divide total number of each length by nreps
  loops=loops/nreps
  return(loops)
  
}
