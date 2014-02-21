"Seattle Colleges report enrollments via web, 
but it's clunky and divided by campus and quarter.  
So, I need to make a consistent file name that 
separates the information in order to combine it 
into a tidy, workable csv, titled like: "
#CentralSummer2013Enrollments


,  term , str(year) , "enrollments")

make_name <-function(year){
  school = c("Central", "North", "South")
  quarter = c("Summer", "Fall", "Winter", "Spring")
  
  for (college in school){
      for (term in quarter){
        print (paste0(college,term,year,"enrollments.xls"))
    }
  }
  
}