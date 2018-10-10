 
#	Import the Bollywood data set in Rstudio in a variable named bollywood

  #reading the csv file
  bollywood <- read.csv("bollywood.csv")
  View(bollywood)


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector(in the same order)
     
        #finding the last 10 movie names(in that order)
	last_10 <- tail(bollywood$Movie,10)
 	last_10
	 
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
     
        #finding the total number of NA values in the entire bollywood data frame
	na_bollywood <-  sum(is.na(bollywood))
	na_bollywood  
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
        #finding the movie name based on the maximum TCollection calculated
 	top_movie <- bollywood[which.max(bollywood$Tcollection),1]
  	top_movie

  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

  	#finding the movie name based on the second highest TCollection calculated
  	#this is done by sorting Tcollection column in descending order and finding the second element from top
  	top_2_movie <- bollywood[which(bollywood$Tcollection==sort(bollywood$Tcollection,decreasing=T)[2]),1]
  	top_2_movie
	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
        #extract Tcollection column in each of the three cases and find the three sums seperately
        shahrukh_collection <- sum(shahrukh$Tcollection)
  	shahrukh_collection  
    
	akshay_collection <-  sum(akshay$Tcollection)
  	akshay_collection 
    
	amitabh_collection <- sum(amitabh$Tcollection)
  	amitabh_collection  
    
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.

        #since bollywood$Verdict column is of factor type, the below function can be used
	summary(bollywood$Verdict)
   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  
#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

 	#finding the movie names based on the maximum Ocollection, Wcollection, Fwcollecion and TCollection calculated
  	top_omovie <- bollywood[which.max(bollywood$Ocollection),1]
  	top_wmovie <- bollywood[which.max(bollywood$Wcollection),1]
  	top_fwmovie<- bollywood[which.max(bollywood$Fwcollection),1]
 	top_tmovie <- bollywood[which.max(bollywood$Tcollection),1]
  	movie_result <- c(top_omovie,top_wmovie,top_fwmovie,top_tmovie)
  	movie_result

	

   
    


    
    
    