print("Hello there!")
print("General Kenobi!")
?time

fruit_size = c(2, 4, 7, 5, 3, 2)
fruit_col = c('g', 'y', 'r', 'g', 'b', 'r')


fruit

fruit=data.frame(size=fruit_size,col=fruit_col)

fruit[fruit$col=="r",fruit$size>5,]

#BOOLEAN INDICATOR+FILTER
fruit %>% filter(col=="r" & size>5 | col!="r" & size<5)
bol_vector=fruit$col=="r" & fruit$size>5 | fruit$col!="r" & fruit$size<5
bol_vector


#ROW SLICE
data=mtcars
data=data %>% filter(carb>2)
aa=mtcars[c(3,8,12),]
sum(aa$disp)


#list creation
bad_list=list(na=c(NA,NA,NA),chars=c("a","b","c"),bools=c(FALSE,FALSE,TRUE))
bad_list
?data.frame
  