#-----------------------------------------------------#
# Title:  Number Guessing Game                        #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   01.03.17                                    #
#-----------------------------------------------------#

numberGame <- function() {
  
  .ng <- function() {
    cat('\014')
    number <- sample(1:10, 1)
    guessesTaken = 0
    while(guessesTaken < 5) {
      cat('Take a guess: \n')
      guess <- readline()
      guessesTaken <- guessesTaken + 1
      
      if(guess < number) {
        cat('Your guess is too low. Remaining guesses:', 
            5 - guessesTaken, '\n\n')
      }
      if(guess > number) {
        cat('Your guess is too high. Remaining guesses:', 
            5 - guessesTaken, '.\n\n')
      }
      if(guess == number) {
        break()
      }
    }
    
    if(guess == number){
      cat('\014')
      message('\nCongrats, ', user_input, '!\n\n', 
              'You guessed in ', guessesTaken, ' attempts.\n')
    }
    if(guess != number){
      cat('\014')
      message('\nYOU LOSE, ', user_input, '.\n\n', 
              'I was thinking of ', number, '.\n')
    }
  }
  
  guessesTaken = 0
  
  cat('\014')
  
  line1 <- '+---+---+---+---+---+---+\n'
  line2 <- '| T | H | E |   |   |   |\n'
  line3 <- '| N | U | M | B | E | R |\n'
  line4 <- '| G | A | M | E |   |   |\n'
  line5 <- '\n by Brandon Monier\n'
  
  message(line1, line2, line1, line3, line1, line4, line1, line5)
  
  cat('Hello. What is your name?\n')
  user_input <- readline()
  
  cat('\nWell,', user_input, ', I am thinking of a number between 1 and 10.',
      'Do you want to play (yes/no)?\n')
  yn_input <- readline()
  
  if(tolower(yn_input) == 'yes') {
    .ng()
  } else if(tolower(yn_input) == 'no') {
    cat('Okay, have it your way. Goodbye!')
  } else {
    message('WRONG CHOICE, BRAH.')
  }
  
}
