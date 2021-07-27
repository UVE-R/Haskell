--function to check if a number is within bounds
in_range min max x = 
    x>=min && x<=max

main = print(in_range 0 5 3) --call the function
