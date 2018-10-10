#Settings
player<- function(x){
  if(x%%2==0)return("A")
  else return("B")
}
tran <- function(x){
  if(x=="A"||x=="O") return("O")
  else if (x=="B"||x=="X") return("X")
  else return(" ")
}

judge <- function(x){
  if(x=="O") return(1)
  else if (x=="X") return(-1)
  else return(0)
}

i <- 0
com <-""
TTT <- as.character(c(1:9))

#Start Game
while(i != 10){
  error<- 0
  #check input
  while(error!= 1){
    if(i == 0){
      cat("Welcome to play OOXX Game! Enter 1~9 to choose position and enter /exit/ to leave the game.")
    }
    cat("Round",i,"\n")
    
    cat("Now is player ",player(i),"'s turn!\n",sep="")
    
    com <- readline(paste("Player",player(i),"input(1~9) :"))
    
    if (com == "exit"){
      print("Bye-Bye!!")
      break
    } 
    com <- as.integer(com)
    if(com >= 1 && com <= 9)  {
      error<- 1
      if(TTT[com]=="O"||TTT[com]=="X"){
        error<- 0
        cat("This position is already occupied!\n")
      }
      
    }
    else {
      error<- 0
      cat("Invalid input! Please re-enter!\n")
    }
  }
  
  #exiting
  if (com == "exit"){
    break
  } 
  
  #implement O/X
  TTT[com] <- player(i)
  meow <- 0
  for(j in c(1:9)) {
    TTT[j] <- tran(TTT[j])
  }
  
  cat(TTT[1],"|",TTT[2],"|",TTT[3],"\n_____\n",TTT[4],"|",TTT[5],"|",TTT[6],"\n_____\n",TTT[7],"|",TTT[8],"|",TTT[9],"\n**************\n",sep="")
  
  #judge win
  for(a in c(0:2)){
    p <- 0
    q <- 0
    for (b in c(1:3)) {
      p<- p + judge(TTT[3*a+b])
      q<- q + judge(TTT[3*b-a])
    }
  }
  if(p==3||q==3){
    cat("Player",player(i),"wins!\n")
    break
  }
  else if(p==-3||q==-3){
    cat("Player",player(i),"wins!\n")
    break
  }
  else if(TTT[5]==TTT[1]&&TTT[5]==TTT[9]&&TTT[5]!=" "){
    cat("Player",player(i),"wins!\n")
    break
  }
  else if(TTT[5]==TTT[3]&&TTT[5]==TTT[7]&&TTT[5]!=" "){
    cat("Player",player(i),"wins!\n")
    break
  }  
  else if (i==8){
    cat("End in a draw!!!\n")
    break
  }
  i <- i+1
}
