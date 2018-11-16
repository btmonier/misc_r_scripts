# General loop functions
ul <- rnorm(40)
print('This loop calculates the square of the first 10 elements of vector ul')

usq <- 0
for(i in 1:10)
{
  usq[i] <- ul[i]*ul[i]
  print(usq[i])
}
print(i)

# Nested for: multiplication table
mymat <- matrix(nrow = 30, ncol = 30)

for(i in 1:dim(mymat)[1])
{
  for(j in 1:dim(mymat)[2])
  {
    mymat[i, j] <- i*j
  }
}
View(mymat)
mat <- data.frame(mymat)
View(mymat)

# Repeat loop
readinteger <- function()
{
  n <- readline(prompt="Please, enter your ANSWER: ")
}

repeat
{
  response<-as.integer(readinteger());
  if (response==42)
  {
    print("Well done!");
    break
  } else print("Sorry, the answer to whaterver the question MUST be 42");
  
}

# Graph loop
plotHistFunc <- function(x, na.rm = TRUE) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i])) + geom_histogram(alpha = .5,fill = "dodgerblue")
    ggsave(plots,filename=paste("myplot",nm[i],".png",sep=""))
  }
}