psgrps<- read.table("ps05.txt", header = TRUE, sep = ",")
# (a)
name05<- as.character(psgrps$name)
# Extract the name from the ps05 data frame as a character vector.

name.list<- strsplit(name05, " ")
# split the first name and last name by " ".
# name. list is a list and each component contain one or two element

last.name<- sapply(name.list, function(x) x[2])
# use sapply to indicate the second element of each component.
# NA appears when no second element (no last name).

no.last<- c(which(is.na(last.name) == TRUE))
# find the indicaties of NA (no last name).
no.last


# (b)
last.name<- sapply(name.list, function(x) x[2])
first.name<- sapply(name.list, function(x) x[1])
# use sapply to indicate the first element of each component as first name.

fullname<- cbind(first.name, last.name)
# column conbind the first.name vector and last.name vector to a new matrix full name.

fullname


#(c)
psgrps.data<- data.frame(first.name, last.name, psgrps$email, psgrps$ps1, psgrps$ps2)
names(psgrps.data)<- c("first", "last", "email", "ps1", "ps2")
# create a data frame contain first.name, last.name, email, ps1, ps2 and name each column properly.

sort.index<- order(psgrps.data$last,psgrps.data$first, na.last = FALSE, decreasing = FALSE)
# find out the sort index with sort last name first with na in the front and then sort first name with ascending order

psgrps.x<- psgrps.data[sort.index,]
# apply the index to the data frame and named it psgrps.x

print(psgrps.x, row.names = FALSE)
# print data frame without row.names


#(d)
ps1.mates<- numeric()
ps2.mates<- numeric()
grp.mates<- list()
for(i in 1:length(psgrps.x$first)){
  ps1.group <- c(which(psgrps.x$ps1 == psgrps.x$ps1[i]))
  # find out the PS1 group for each students with the same group name
  
  ps1.mates<- c(ps1.group[!ps1.group == i])
  # remove the student himself only left the group mates. PS1 group mates
  
  ps2.group <- c(which(psgrps.x$ps2 == psgrps.x$ps2[i]))
  ps2.mates<- c(ps2.group[!ps2.group == i])
  # find PS2 group mates for each student.
  
  grp.mates[[i]]<- c(ps1.mates,ps2.mates)
  # combind result as a list.
} 
names(grp.mates)<- psgrps.x$first
# name each components with student first name.

grp.mates


#(e)
repeat.index<- numeric()
ps1.mates<- numeric()
ps2.mates<- numeric()
for(i in 1:length(psgrps.x$first)){
  ps1.group <- c(which(psgrps.x$ps1 == psgrps.x$ps1[i]))
  ps1.mates<- c(ps1.group[!ps1.group == i])
  ps2.group <- c(which(psgrps.x$ps2 == psgrps.x$ps2[i]))
  ps2.mates<- c(ps2.group[!ps2.group == i])
  matching<- ps1.mates %in% ps2.mates
  # matching each ps1 group mates pair and ps2 group mates pair
  
  if(length(matching[matching == TRUE]) > 0){
    repeat.index[i] <- i
    # if the matching contain TRUE, which is assigned to the same group twice.
    # Then output the indicate.
  }
  repeats<- c(repeat.index[!is.na(repeat.index)])
  # Remove the NA element (student who do not assinged to the same group twice).
}
repeats


#(f)
psgrps.new<- psgrps.x
repeat{
  psgrps.new$ps2 <- sample(rep(LETTERS[1:8], each = 3), replace = FALSE)
  # random assign a group name to each students
  
  mates.check<- list()
  check.result<- logical()
  for(i in 1:length(psgrps.new$first)){
    ps1.group <- c(which(psgrps.new$ps1 == psgrps.new$ps1[i]))
    student.mate.ps1<- c(ps1.group[!ps1.group == i])
    ps2.group <- c(which(psgrps.new$ps2 == psgrps.new$ps2[i]))
    student.mate.ps2<- c(ps2.group[!ps2.group == i])
    mates.check[[i]]<- c(student.mate.ps1 %in% student.mate.ps2)
    # store the matching reslut of each students in a list
    
    check.result<- unlist(mates.check)
    # unlist the mates.check and now it's a logical vector
  }
  if(length(check.result[check.result == TRUE]) == 0){
    # If there is no "TRUE" contained in the check.result vector, which means no repeat assigned       student, then break. If contained repeat assigned, re-assign again.
    break
  }
}
psgrps.new

## the function to test if the new data frame is contain repeat assigned student. The function shows as follow.
test.repeat<- function(x){
  ps1.mates<- numeric()
  ps2.mates<- numeric()
  repeat.index<- numeric()
  repeats<- numeric()
  for(i in 1:24){
    ps1.group <- c(which(x$ps1 == x$ps1[i]))
    ps1.mates<- c(ps1.group[!ps1.group == i])
    ps2.group <- c(which(x$ps2 == x$ps2[i]))
    ps2.mates<- c(ps2.group[!ps2.group == i])
    matching<- ps1.mates %in% ps2.mates
    if(length(matching[matching == TRUE]) > 0){
      repeat.index[i] <- i
    }
    repeats<- c(repeat.index[!is.na(repeat.index)])
  }
  if(length(repeats) == 0){
    cat("no repeat")
  }
  else{
    return(repeats)
  }
}
test.repeat(psgrps.new)


