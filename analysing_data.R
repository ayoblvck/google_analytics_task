head(dailyCalories_merged)
dailyCalories_merged$dayOfWeek<- wday(dailyCalories_merged$ActivityDay, label = TRUE)
head(dailyCalories_merged)
dailyCalories_merged %>% 
  group_by(Id)

ungroup(dailyCalories_merged)
dailyCalories_merged %>% 
  summarise(Calories = sum(Calories))

class(dailyCalories_merged$ActivityDay)

View(dailyActivity_merged)
dailyActivity_merged %>% 
  group_by(Id) %>% 
  summarise(sum_steps = sum(TotalSteps))

dailyActivity_merged$dayofWeek <- wday(dailyActivity_merged$ActivityDate, label = TRUE)
days_people_step_the_most<- dailyActivity_merged %>% 
  count(dayofWeek) %>% 
  sequenceange(desc(n))

mode_day_of_week <- dailyActivity_merged %>% 
  group_by(Id, dayofWeek) %>% 
  summarise(sum_steps = sum(TotalSteps), sum_distance = sum(TotalDistance))

mode_day_of_week %>% 
  group_by(dayofWeek) %>% 
  summarise(max_step = max(sum_steps))

ggplot(days_people_step_the_most, aes(x=dayofWeek, y=n))+
  geom_bar(stat = "identity") +
  labs(title = "Count of steps")

weightLogInfo_merged$dayofWeek <- wday(weightLogInfo_merged$Date, label = TRUE)
merged_weightLog <- left_join(summary_1, subset(weightLogInfo_merged, select = c("Id", "WeightKg", "BMI")), by= "Id")

merged_weightLogClean <- merged_weightLog %>% drop_na()
View(merged_weightLogClean)
View(weightLogInfo_merged)
merged_weightLogClean$total_steps_scaled <- merged_weightLogClean$total_steps / 10000

#weight/total steps
ggplot(merged_weightLogClean, aes(WeightKg, total_steps_scaled, color = colorcol) )+   
  geom_line()

ggplot(merged_weightLogClean, aes(x = WeightKg)) +
  geom_line(aes(y = total_steps_scaled), color = "red") +
  geom_line(aes(y = BMI), color = "blue")

summary_1 <- mode_day_of_week %>% 
  group_by(Id) %>% 
  summarise(total_steps = sum(sum(sum_steps)))


longest_increasing_subsequence <- function() {
  nums = c(10, 9, 2, 5, 3, 7, 101, 18)
  n <- length(nums)
  if (n == 0) {
    return(0)
  }
  
  dp <- rep(1, n)
  
  for (i in 2:n) {
    # 9, 2, 5, 3, 7, 101, 18
    for (j in 1:(i-1)) {
      # when i = 1, j = 1, 2
      if (nums[i] > nums[j]) {
        # 5 > 2
        dp[i] <- max(dp[i], dp[j] + 1)
        #1[3] <- max(1[3], 1[1] + 1)
        print(dp[i])
      }
    }
  }
  dp <- rep(1,n)
  dp <- dp[1]+ 1
  print(dp)
  
  return(max(dp))
}

nums <- c(10, 9, 2, 5, 3, 7, 101, 18)
print(longest_increasing_subsequence(nums))

#check if it is a pallindrome
check_pallindrome <- function(word){
  reversed <- strsplit(word, split = "")[[1]]
  finalword <- paste(reversed, collapse = "")
  return(if(finalword == word) {
    cat("this is a Pallindrome ")}
         else{"this is not a palindrome"})
}

#check century is a year
century <- function(year){
  if (!as.numeric(year)){
    return("not a valid year")
  }
  century <- floor((as.integer(year)/100)+1)
  return(century)
}

check_even <- function(num){
  ans = c()
  for (i in test){
    if ((i %% 2) == 0){
      ans <- append(ans, i)
    }
  }
  return(sum(ans))
}
test <- c(1,2,3,4,5)
check_even(test)

#function that takes a string as input and returns the length of the string
check_string_length <- function(string_value){
  #return(length(strsplit(string_value, split = "")[[1]]))
  return(nchar(string_value))
}

check_string_length("bravo")

#given a list of integers find max product that can be obtained from any three of the integers
max_product<- function(list_integers){
  list_integers <- sort(list_integers, decreasing = TRUE)
  value = list_integers[1:3]
  product = prod(value)
  return(product)
}
integer3 <- c(2,3,5,6,7,9,11,13)
max_product(integer3)

#return two numbers that add up to a target
match_target <- function(value, target){
  output <- list()
  for (num in value){
  x <- (target - num)
  if (x %in% value){
    output <- append(output,list(c(x, num)))
  }
  #else{return("no two values match target")}
  }
  return (output)
}

match_target <- function(value, target){
  output <- vector()
  for (i in seq_along(value)){
    complement <- (target - value[i])
    if (complement %in% value){
      output <- list(Name = c(complement, value[i]))
    }
    #else{return("no two values match target")}
  }
  return (output)
}
match_target(integer3, 8)


a= {}
b= []

aassgghhjjkl
if a in a

longest_substring <- function(s, k) {
  # number of characters in string
  n <- nchar(s)
  frequency for each value
  freq <- rep(0, 26)
  start <- 1
  max_len <- 0
  num_distinct <- 0
  
  #loop to check what value is in the string
  for (end in 1:n) {
    idx <- utf8ToInt(substr(s, end, end)) - utf8ToInt("a") + 1
    
    #add 1 to the index of freq found
    freq[idx] <- freq[idx] + 1
    
    #check if the freq[idx} is equal to 1, then add it to num_distinct
    if (freq[idx] == 1) {
      num_distinct <- num_distinct + 1
    }
    
    while (num_distinct > k) {
      idx <- utf8ToInt(substr(s, start, start)) - utf8ToInt("a") + 1
      freq[idx] <- freq[idx] - 1
      if (freq[idx] == 0) {
        num_distinct <- num_distinct - 1
      }
      start <- start + 1
    }
    max_len <- max(max_len, end - start + 1)
  }
  return(max_len)
}

str
k <- 3
str <- "aabacbebebe"
print(longest_substring(s, k))


longest_substring_k_distinct_characters <- function(str, k) {
  n <- nchar(str)
  freq <- integer(26)
  left <- 1
  right <- 1
  max_len <- 0
  
  # move right pointer until we have k distinct characters in the window
  while (right <= n) {
    
    freq[utf8ToInt(str[left]) - utf8ToInt('a')] <- freq[utf8ToInt(str[left]) - utf8ToInt('a')] + 1
    
    # move left pointer to remove the first character from the window
    while (max(freq) > k) {
      freq[utf8ToInt(str[left]) - utf8ToInt('a')] <- freq[utf8ToInt(str[left]) - utf8ToInt('a')] - 1
      left <- left + 1
    }
    
    max_len <- max(max_len, right - left + 1)
    right <- right + 1
  }
  
  return(max_len)
}

val <- as.integer(str[1])


check_duplicate <- function(sequenceay){
  len <- length(sequenceay)
  value <- vector()
  for (i in sequenceay){
    if (!(i %in% value)){
      value <- append(value, i)
    }
  }
 
  return(list(value, length(value)))
}
  
check_duplicate(x)
x <- c(1,1,1,9,3,5,7,2,4,6, 1.5)

check_second_smallest_integer <- function(x) {
  #check for distinct
  # check <- vector()
  # for (i in x){
  #   if(!(i %in% check)){
  #     check <- append(check, i)
  #   }
  # }
  # sorted <- sort(check)
  # second_val <- sorted[2]
  num <- sort(unique(x))
  return(num[2])
}

check_second_smallest_integer(x)


# Question 4: Write a function in R that takes 
# in a list of strings and returns a new list with only the strings that contain the letter 'a'.
test <- c("alpha", "beta", "jet")

check_string <- function(x){
  new_string <- x[grep("a", x)]
  return(new_string)}

check_string(test)

Write a function in R that takes in a list of integers and returns the nth Fibonacci number.
check_fibonacci <- function(size){
  x <- vector()
  x[1] <- 1
  x[2] <- 1
  for (i in 2:10){
    x[i+1] = x[i] + x[i-1]
  }
  return(x)
}

fibonacci <- function(n) {
  if (n <= 1) {
    print(c(n, "a"))
    return(n)
    
  } else {
    print(n)
    return(fibonacci(n-1) + fibonacci(n-2))
   
    
  }
}

fibonacci(4)
check_fibonacci <- function(n){
  return((n-1) + check_fibonacci())
}
check_fibonacci(10)

#Question 1: Write a function in R that takes in a list of 
#integers and returns the sum of all the even numbers in the list.

sum_even <- function(integers){
  new_list <- vector()
  for (i in integers){
    if(i%%2 == 0){
     new_list <- append(new_list, i)
}
    
  }
  return(sum(new_list))
}
sum_even(ans)
ans <- c(1, 2, 3, 4, 5, 100, 20)

#Question 2: Write a function in
#R that takes in a list of integers and returns the product of all the numbers 
#that are greater than 10.

sum_product_10<- function(test){
  box <- vector()
  for (i in test){
    if (i > 10){
      box <- append(box, i)
    }
  }
  return(prod(box))
}

sum_product_10(ans)

#Question 3: Write a function in R 
#that takes in a list of integers and 
#returns the length of the longest increasing subsequence in the list.

longestIncreasingSubsequence <- function(numbers) {
  n <- length(numbers)
  if (n == 0) {
    return(0)
  }
  lengths <- rep(1, n)
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      if (numbers[j] < numbers[i]) {
        lengths[i] <- max(lengths[i], lengths[j] + 1)
      }
    }
  }
  return(max(lengths))
}

x<- c(1,4,5,6,7,7,8,9,9,9, 2, 3,4)
longestIncreasingSubsequence(x)

# Question 4: Write a function in R that 
# takes in a list of strings and returns the length of the longest common suffix
# shared by all the strings.

len_common_suffix <- function(list_1){
  n <- length(list_1)
  end <- length(i)
  lengths <- rep(1, n)
  for (i in 1:n){
   word <- strsplit(list_1[i])
   for (j in -(i:length(word))){
     lengths[i] <- word[j]
   }
   
     
    }
  }
}


solution <- function(statues) {
  sum_vec <- integer()
  statues <- sort(statues)
  if (length(statues) < 2) {
    return(0)
  }else {
    for (i in 2:length(statues)){
      if ((statues[i] - statues[i - 1]) > 1) {
        sum_vec <- (sum_vec + ((statues[i] - statues[i - 1]) - 1))
      }
      print(sum_ve
    }
    return(sum_vec)
  }
}

val <- c(1,2,3,2)
solution(val)

 1,2,1,2

solution(val)
solution <- function(sequence) {
  check <- vector()
  for (i in 2:length(sequence)){
    if (sequence[i] <= sequence[i - 1]) {
      check <- append(check, sequence[i])
      sequence <- sequence[-i]
      
      if ((!([i] == length(sequence))) 
          return(sequence[i] <= sequence[i-1])
  }
  
  if (length(check) > 1) {
    return(FALSE)
  }else {
    return(TRUE)
  }
  }
  
  is_strictly_increasing <- function(sequence) {
    for (i in length(sequence)){
      
    }
    n <- length(sequence)
    removed <- 0
    
    is_strictly_increasing <- function(sequence = c(1,2,3,5,4,4,3)) {
      n <- length(sequence)
      removed <- FALSE
      
      for (i in 1:(n-1)) {
        if (sequence[i] >= sequence[i+1]){
          if (removed) {
            return(FALSE)
          } else {
            if (i == 1 || sequence[i-1] < sequence[i+1]) {
              sequence <- sequence[-i]
            } else {
              sequence <- sequence[-(i+1)]
            }
            removed <- TRUE
            n <- n - 1
            i <- i - 1
          }
        }
      }
      
      return(TRUE)
    }
    
    is_strictly_increasing(var)
    n <- length(sequence)
    removed <- 0
    
    for (i in 2:n) {
      if (sequence[i] <= sequence[i-1]) {
        removed <- removed + 1
        
        if (removed > 1) {
          return(FALSE)
        }
        
        if (i == 2 || sequence[i] > sequence[i-2]) {
          sequence[i-1] <- sequence[i]
        } else {
          sequence[i] <- sequence[i-1]
        }
      }
    }
    
    
    check_sequence <- function(ans){
      check = 0
      ans2 <- ans
      for (i in 2:length(ans)){
        if (ans[i] <= ans[i-1]){
          ans2 <- ans2[-i]
          check <- check + 1}}
          if(check > 1){return (FALSE)}
        
        
      
      
      for ( i in 2:length(ans2)){
        print(ans2)
        if (ans2[i] <= ans2[i-1]){
          check <- check + 1
        }
      } 
        if (check > 1){return(FALSE)}
      else{return (TRUE)}
    }
    
    ans<- c(1,2,1,4,5)
    check_sequence(ans)
    
    matrice <- matrix(c(1,0,1,0,2,2,3,1,0), nrow = 3)
    class(matrice)
    check_matrices <- function(matrice){
      total_sum = 0
      for (i in 1:NCOL(matrice)){
        for (j in 1:NROW(matrice)){
          print(c(i,j))
          #print(c("This is ",i,j))
          if((matrice[j,i] != 0)){
            total_sum <- total_sum + matrice[j,i]
            print(total_sum)
          }
          else {
             break
         }
      }
      }
      print(class(total_sum))
    return(total_sum)
      
      }
    check_matrices(matrice)
    
    for i in range(len(matrix)):
      for j in range(len(matrix[0])):
      # If the current element is suitable for the CodeBots
      if matrix[i][j] != 0 and (i == 0 or matrix[i-1][j] != 0):
      # Add the current element to the total sum
      total_sum += matrix[i][j]
    
    
    solution <- function(matrix) {
      sum <- 0
      for (i in 1:NROW(matrix)){
        for (j in 1:NCOL(matrix)){
          if ((matrix[i, j] != 0) && (i == 1 || matrix[(i - 1), j] != 0)) {
            sum <- sum + matrix[i, j]
          }
        }
      }
      return(as.integer(sum))
    }
    
    solution(matrice)

    
    zero_indices <- which(matrix == 0, arr.ind = TRUE)
    
    # Subset the matrix to exclude the rows below the 0 elements
    suitable_matrix <- matrix[1:min(zero_indices[,1])-1, ]
    
    # Return the sum of the suitable matrix
    return(sum(suitable_matrix))  
    
    inputArray = c("aba", "aa", "ad", "vcd", "aba")
    m
    name <- vector()
    highest_num <- max(nchar(inputArray))
    for (i in 1: length(inputArray){
      if(inputArray[i] == highest_num){
        name <- append(name, inputArray[i] )
      }
        
    }
    
    solution <- function(inputArray) {
      name <- vector()
      highest_num <- max(nchar(inputArray))
      for (i in 1:length(inputArray)) {
        if (nchar(inputArray[i]) == highest_num){
          name <- append(name, inputArray[i] )
        }
        
      }}
      
      solution(inputArray)
      
      solution <- function(s1, s2) {
        s1<- "abcde"
        s2<- "abcs"
        s1_new <- strsplit(s1, "")
        
        s2_new <- strsplit(s2, "")
        s2
        count <- 0
        for(i in s1_new){print(s1_new %in% s2_new)}
        (any(i %in% s2))}
          if(any(i %in% s2)){
            count <- count + 1
          }
        }
       print(any())
        print(count)
}

solution <- function(n){
  n<- 3457
  n <- as.character(n)
  length(n)
  half <- nchar(n) / 2
  print(half)
  first_half <- strsplit(substr(n, 1, half), "")
  print(first_half)
  second_half <- strsplit(substr(n, (half+1), nchar(n)), "")
  print(second_half)
  return((sum(as.integer(first_half[[1]]))) == (sum(as.integer(second_half[[1]]))))
}


solution <- function(a) {
  empty <- vector()
  result <- vector()
  a2 <- sort(a)
  for(i in seq_along(a)) {
    if (a[i] == -1) {
      empty <- append(empty, i)
    }
  }
  for (i in seq_along(a2)) {
    if (!i %in% empty) {
      result <- append(result, a2[i])
    } else {
      result <- append(result, -1)
      result <- append(result, a2[i])
    }
  }
  return(result)
}



a2<- c(-1, 8, 5, 4, -1, 7, 2, -1, 1)
#-1, 1, 2, 4, -1, 5, 7, -1, 8
a3 <- sort(a2[a2 != -1])
output <- vector()
store <- vector()
for(i in seq_along(a2)){
  if (a2[i] == -1){
    store <- append(store, i)
  }}
   for (i in 1:length(a2)){ 
     if(i %in% store){ 
       print(c("upper", i))
       output[i] <- -1
       output <- append(output, a3[i], after = length(output))
       print(c("upper", output))
       next}
     
    if ((!(i %in% store)) && !(is.na(a3[i]))){
       print(c('lower', i))
       output <- append(output, a3[i])
       print(c('lower', output))
       next
     }
   }



print(output)


a2<- c(8, 5, 4, 7, 2, 1)
index <- which(a2 == -1)
a3 <- sort(a2[-index])
for (i in index){
  if (i == 1){
    a3 <- c(-1, a3 )}
  else{
  a3 <- append(a3, -1, after=(i-1))}
}
print(a3)
}

solution <- function(a) {
  index <- which(a == -1)
  if (is.na(index)) {
    return(sort(a))
  } else { 
    a3 <- sort(a[ -index])
    for (i in index){
      if (i == 1) {
        a3 <- c(-1, a)
      } else {
        a3 <- append(a3, -1, after = (i - 1))
      }
    }
  }
  return(a3)
}

solution(a2)


solution <- function(inputString) {
  word <- strsplit(inputString, "")
  bracket <- grep("(||)", word[[1]])
  n  length(bracket)
  
  for (i in seq_along(bracket){
    word[[1]][bracket[i]]:word[[1]]
  }
  
  # Write a function that reverses characters in (possibly nested) parentheses in the input string.
  # 
  # Input strings will always be well-formed with matching ()s.
  # 
  # Example
  
  eg: aba(gyh)huj(hok)
    hjjj(hjk(jhjh))
  vectory <- vector()
  for i in word{
    while (i!= '('){
      vectory<-append(vector,a )
    }
  }
  inputstring <- "abra(cad)(dab)ra"
  
(huh(hgjh)(ghtin))
  
  
  
  stack <- list()
  output_string <- ""
  word <- strsplit(inputstring, "")[[1]]
  word
  for (char in word ) {
    
      if(char!="("){
      stack<- append(stack, char)
      }
  }
  print(stack)
      
      
      
  
  
 ####     
      
      
      
      while (substring != "(") {
        substring <- stack[[length(stack)]]
        substring
        stack <- stack[-length(stack)]
        stack
        output_string <- paste0(output_string, substring)
        output_string}}}
      }
      
      output_string <- paste0(output_string, substring)
    } else {
      stack[[length(stack) + 1]] <- char
    }
  }
  
  output_string
}



stack <- list()
output_string <- ""

for (char in strsplit(input_string, "")[[1]]) {
  if (char == ")") {
    substring <- ""
    
    while (substring != "(") {
      substring <- stack[[length(stack)]]
      stack <- stack[-length(stack)]
      output_string <- paste0(output_string, substring)
    }
    
    output_string <- paste0(output_string, substring)
    if (length(stack) > 0 && stack[[length(stack)]] != "(") {
      reversed_substring <- paste0(rev(strsplit(stack[[length(stack)]], "")[[1]]), collapse = "")
      output_string <- paste0(output_string, reversed_substring)
      stack[[length(stack)]] <- ""
    }
  } else {
    stack[[length(stack) + 1]] <- char
  }
}

output_string
}
This function works the same way as the modified function from the previous question. It uses a stack to keep track of the characters within parentheses and reverses them when a closing parenthesis is encountered. It then appends the reversed substring to the output string and replaces the top of the stack with an empty string.

Here are some example usages of the function:
  
  r
Copy code
input_string <- "(bar)"
output_string <- reverse_in_parentheses(input_string)
print(output_string)  # Output: "rab"

input_string <- "foo(bar)baz"
output_string <- reverse_in_parentheses(input_string)
print(output_string)  # Output: "foorabbaz"

inp <- "foo(bar)baz(blim)"
output_string <- reverse_in_parentheses(input_string)
print(output_string)  # Output: "foorabbazmilb"

input_string <- "foo(bar(baz))blim"
output_string <- reverse_in_parentheses(input_string)
print(output_string)  # Output: "foobazrabblim"
All of these examples produce the expected output.


R



h <- list(c(1,2,3,4), c(4,5,6,7))
unlist(h)
h[[1]][1]
r<-"hjjhjh"
r<- strsplit(r, "")
unlist(r)
a<- c(1,2,3,4,5) 
b<- c(2,1,3,4,5)
solution <- function(a, b) {
  notSame = length(which((a==b)==F))
  if(notSame>2||notSame==1)
    return(FALSE)
  if(notSame==0)
    return(TRUE)
  else{
    loc=which((a==b)==F)
    return(ifelse(all(a[c(loc[1],loc[2])]==b[c(loc[2],loc[1])]),TRUE,FALSE))     
  }
}

solution(a,b)
