load('armdata.RData')

## Experiment 1, Person 1, Repetition x 
#armdata[[1]][[1]][[1]]

## Experiment 1, Person x, Repetition 1 
## armdata[[1]][[x]][[1]]

## Experiment x, Person 1, Repetition 1 
## armdata[[x]][[1]][[1]]

## Experiment 1, Person 1, Repetition 1, Percent traversed 1, Coordinate x
#armdata[[1]][[1]][[1]][1,1]

## Experiment 1, Person 1, Repetition 1, Percent traversed 5, Coordinate z
#armdata[[1]][[1]][[1]][5,3]

## Experiment 1, Person 1, Repetition 1 til 10
# armdata[[1]][[1]][1:10]
