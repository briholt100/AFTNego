__author__ = 'bholt'

"""
Seattle Colleges report enrollments via web, 
but it's clunky and divided by campus and quarter.  
So, I need to make a consistent file name that 
separates the information in order to combine it 
into a tidy, workable csv, titled like: 
"CentralSummer2013Enrollments"
"""


def make_name(year):
    school = ["Central", "North", "South"]
    quarter = ["Summer", "Fall", "Winter", "Spring"]
    for college in school:
        for term in quarter:
            print college + term + str(year) + "enrollments"