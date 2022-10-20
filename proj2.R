#Group #36
#Group Member: Chloe Bircham s2415006, Mohammed Alraood s2227255, Xinyan Chen s2304087
#contribution: 
#Chloe: 
#Mohammed: create the github project, work in last part of the assignment
#Xinyan: 

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

fixcards<- function(n){
  #Function that can be called once in pall
  #used to set same distribution of cards
  carda<- sample(1:(2*n)) #randomly shuffle cards
  return (carda)
}

stratp <- function( n,k, strategy,cards){ #function that takes main part of pone to be called later in pall
  esc<-0 #sets the counter of if the prisoner escapes to 0.
     #randomly assigning the card numbers to 2*n boxes
    if (strategy==1) { #If strategy 1, do the following
      card<-cards[k]  #Choose the prisoners number box, assign card with the card number inside
      for (i in (1:n-1)){ #looping over n-1 boxes, since the prisoner already opened one box
        if (card==k){ #if the card is equal to the prisoners number
          esc<-1 #assigns the value of prisoner escaping to be 1.
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
          esc<-1 #sets the value of prisoner escaping to 1
          break}      #hence break out of the loop, he is finished
        else { #if the number is not the prisoners number
          card<-cards[card] #open the box corresponding to the card, reassign card value with card from the new box.
        }}  
    }
    
    if (strategy==3){
      for(i in 1:n){ #opening n boxes
        box<-sample(cards,1) #random box selected every time
        card<-cards[box] #sets the value of prisoner escaping to 1.
        if (card==k){ #if card is the same as the prisoners number
          esc<-1 #adds one to the success count
          break} #break, the prisoner is finished and has been successful
      
    }  
  }
  return(esc) #returns value of 0 or 1, if the value is 1, prisoner escaped, if 0 prisoner did not escape.
}

pall<- function(n,strategy, nreps){
  #input: n, where 2*n is number of prisoners
  #       strategy chosen
  #       nreps: number of times the simulation is to be repeated
  #function that calculates the probability of all prisoners escaping
  totesc<-0 #to being with, the number of simulations where all prisoners escape is obviously 0
  for (m in (1:nreps)){ #repeating simulation nreps times
    cards<-fixcards(n) # calling fixcards so that cards stay fixed when looping through all prisoners
    totalp<-0 #to sum the prisoners that escape in one simulation, to begin with, it is 0  
    k<- 1:(2*n)     #All prisoner numbers
    
    for (i in k){ #looping over all prisoners
      #does the prisoner escape? if so, assign does=1, if not, does=0
      does<-stratp(n,i,strategy,cards) 
      #add does to the total number of prisoners that have escaped
      #after all prisoners have attempted, totalp is the total number who succeeded
      totalp<-totalp+does
    }

    if (totalp==(2*n)){ #if all prisoners escaped, add 1 to the number of simulations that were 100% successful
      totesc<- totesc +1
    }
  }
  prob<- totesc/nreps #dividing by nreps gives the probability of 100% of prisoners succeeding
  return(prob)
}


#Example for n=5, when we have 10 prisoners
ranp<- sample(1:(2*5)) #selecting a random prisoner
for(i in 1:3){
 ip5<- pone(5,ranp,i,1000) #calculating individual probability for random prisoner, for each strategy
 cat(c("\nThe Probability of a random prisoner success out of 5 with strategy", i, "is:", ip5))
 jp5<- pall(5,i,1000) #calculating joint probabilities
 cat(c("\nThe Probability of all 5 prisoners escaping with strategy", i, "is:", sprintf("%.5f", jp5)))
 }

#Example for n=50, when we have 100 prisoners
ranp<- sample(1:(2*50)) #selecting a random prisoner
for(i in 1:3){
  ip50<- pone(50,ranp,i,1000) #calculating individual probability for random prisoner, for each strategy
  cat(c("\nThe Probability of a random prisoner out of 50 success with strategy", i, "is:", ip50))
  jp50<- pall(50,i,1000) #calculating joint probabilities
  cat(c("\nThe Probability of all 50 prisoners escaping with strategy", i, "is:", sprintf("%.5f", jp50)))
}

##The above shows surprising results that strategies 2 and 3 do not differ too much 
##When n=5,the probability of individual success is about 40%-50%, among which the probability of strategy 1 is 48.4% which is the largest, and the probability of strategy 3 is the smallest which is 39.7%. 
##However, only strategy 1 has a probability of these five prisoners to escape jointly, which is 33.7%. The remaining two strategies both have a joint probability of 0, which means they have almost no chance of successfully escaping.
##When n=50, the probability of individual success is not much different from that when n=5. The probability of strategy 1 slightly increased to 52.1 percent, while it of strategy 2 decreased to a minimum of 37 percent.
##The probability of a joint escape also does not change significantly from n=5, except for strategy 1, which drops slightly to 31.9%. The remaining two strategies still have no chance of successfully escaping.


##Function that estimates the probability of loop lengths from 1:2n at least once in random shuffling of cards to boxes
#inputs to the function are the arguments n: number of prisoners, nreps: number of replication
#output of function dloop is a 2n vector contains estimated probability of each loop length from 1 to 2n occurring at least once
#In a random shuffling of cards to boxes based on strategy 1. 
dloop<- function(n,nreps){
  k<- 1:(2*n) #all the prisoners
  #empty vector to store each time a loop is found
  #entry for loops[i] will contain how many times a loop of length i is found
  aloops<- integer(2*n)
  for (i in (1:nreps)){ # repeating the simulation nreps times
    loops<- integer(2*n) #to store if a loop of length i is found
    cards<- sample(1:(2*n)) #randomly shuffling the cards
    for (ii in k){ #looping over each prisoner
      card<-cards[ii]
      opened<- c(card)
      for (ij in (1:(2*n)-1)){
        card<-cards[card]
        opened<- append(opened,card)
      }
      
      ioc<- which(opened %in% card)
      ll<- ioc[2]-ioc[1]
      loops[ll]<-1
    }
    aloops<-aloops+loops
  }
  prob<- aloops/nreps
  return(prob)
}

#estimate the probabilities of lengeth of loops
prob_of_loops<- dloop(50,1000)#calling dloop function to estimate probability for n=50 with 1000 replications
print(prob_of_loops) #print 2n-vector of probabilites 
plot(prob_of_loops, main="Visualization of probability of loops", xlab = "loop") #visualize the probabilities

#assessing the probabilities that no loop longer than 50
sum_prob_more_50<- sum(prob_of_loops[51:length(prob_of_loops)]) #sum probabilities more than 50
prob_of_no_50 <- 1-sum_prob_more_50
cat("The probability of no loop more than 50 is: ",prob_of_no_50, ".") #printing probability of no loop more than 50

