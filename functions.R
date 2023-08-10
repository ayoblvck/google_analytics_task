#function to get time of day
Convert_to_TimeofDay <- function(datecolumn){
  hour <- hour(datecolumn)
  category <- case_when(
    hour>= 0 & hour< 12 ~ 1,
    hour >= 12  & hour< 18 ~ 2,
    TRUE ~ 3
  )
  
  return(category)
}


# functionto create a pie chart

create_pie_chart <- function(data, y_column,category_column, title){
  ggplot(data, aes(x ="", y = y_column, fill = factor(category_column, levels = factor(category_column)))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y")+
    labs(title = "title ", fill = "TimeofDay") +
    scale_fill_manual(values = c("1" = "#007fff", "2" = "#324ab2", "3" = "#008080"), 
                      labels = c( "morning", 
                                  "afternoon",
                                  "evening")) +
    geom_text(aes(label=y_column), position = position_stack(vjust = 0.5)) +
    theme_void()
}