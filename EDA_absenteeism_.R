


##  Info about the dataset: https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work

# load libraries
library('readr')
library('plyr')
library('dplyr')
library('ggplot2')
library('gridExtra')

#download file to current directory
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip',
              destfile = 'data.zip')

#unzip file
unzip('data.zip', exdir = './dataset')

#store dataframe in df variable
df <- read_delim('./dataset/Absenteeism_at_work.csv', delim = ";", locale=locale(decimal_mark = "."))

##################################
### DATA PREPARATION/CLEANING ###
##################################

#get all column names to lower case
colnames(df) <- tolower(colnames(df))

#replace spaces in column names with "_"
names(df) <- gsub(" ", "_", names(df))

#check missing values
missing <- is.na(df)
sum(missing)
# no missing values in the dataframe

#output a summary of all features
summary(df)

#based on the summary and on features names, convert some features to categorical
colnames(df) #get column names for indices
factors_indices <- c(1:5,11,13,15,16)

#loop to convert attributes to factors
for (i in factors_indices)
{
  df[[i]] <- as.factor(df[[i]])
}


#check summary again
summary(df)

#change some levels of categorical features for better understanding
levels(df$day_of_the_week) = c("Mon","Tue","Wed","Thu","Fri") #changes the levels of "Days of the week" attribute
levels(df$seasons) = c('Summer', 'Autumn', 'Winter', 'Spring')
levels(df$education) = c('High school','Graduate','Postgraduate','Master or Doctor')
levels(df$social_drinker) = c('Nondrinker','Drinker')
levels(df$social_smoker) = c('Nonsmoker','Smoker')
df$disciplinary_failure <- as.logical(df$disciplinary_failure)

#check summary again
summary(df)

#check levels of some features
levels(df$id) #employees from #1 to #36
levels(df$reason_for_absence) #reasons for absence from #0 to #28 (reason #0 has no info on the dataset; to be check later)
levels(df$month_of_absence) #month of absence from #0 to #12 (month #zero doesn't make sense; to be check later)
levels(df$hit_target) # several levels of hit_target feature from "81" to "100"


#investigate month #zero
month_zero <- df[df$month_of_absence == 0,]
month_zero
# employees id 4,8 and 35 - just 3 records. No absent time for these records.

employees_month_zero <- df[df$id == 4 | df$id == 8 | df$id == 35,]
employees_month_zero
# employees 4 and 35 have just one record of absence (in month zero) with no reason and no absence time
# employee 8 has two records of absence (one of them is disciplinary failure; other is month zero)
# Conclusions: 
#   - employees 4 and 35 never missed a day of work;
#   - employee 8 had just one record and it was a disciplinary failure (no absence time);

# investigate reason for absence #zero
reason_zero <- df[df$reason_for_absence == 0,]
# all those occurences have zero time of absence
# all those occurences are due to the disciplinary failure
# it seems that disciplinary failure doesn't count for absence time
# however, I will imput the most common value of absent time in the dataset

# mode function definition
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#get the absence time mode
absence_mode <- Mode(df$absenteeism_time_in_hours) # 8 hours; seems to be daily work journey

#We store disciplinary failure indices
fail_indices <- which(df$disciplinary_failure==T)
#We set absenteeism hours caused by disciplinary failure to 8 hours
df$absenteeism_time_in_hours[fail_indices] <- absence_mode


#I will mutate my dataframe adding a column with the reason for absence written in full. 
#"reasons_full.txt" is another dataset that contains the reasons for absence written in full
reasons_full <- read_delim('reasons_full.txt', delim='\t',col_names = "Reason")

#Drop any string that occurs up to (and including) the first space in order to have only the 
#reasons written in full
reasons_full <- sub(".*? ", "", reasons_full$Reason)

#Add a column to our "reasons_full" dataframe with the ordered list of reasons so we can join it later 
#to my main dataframe
reasons_full <- data.frame(reason_for_absence = c(0:28), reason_full = reasons_full)

#convert reason_for_absence as factor in reasons_full dataframe
reasons_full$reason_for_absence <- as.factor(reasons_full$reason_for_absence)

#Join both datasets
df <- inner_join(df,reasons_full,by='reason_for_absence')



#########################
#### VISUALISATION 1 ###
########################
#exploring reason for absence to understand what is most common


reason_chart <- ggplot(df, aes(reason_full)) + labs(x = "Reason for absence", y = "Occurences")
reason_chart <- reason_chart + geom_bar(stat="count", fill="black")
reason_chart
#impossible to read horizontal axis

reason_chart <- reason_chart + coord_flip() #rotating bar chart
reason_chart 


#plot total of hours per reasons of absence instead
#this time grouping the dataset by reason for absence so we can reorder in descent fashion
absence_by_reason <- group_by(df,reason_full)
#summarise the total number of hours per reason
reason_total_hours <- summarise(absence_by_reason,absenteeism_time_in_hours=sum(absenteeism_time_in_hours))
reason_total_hours
#plot
reason_hours_plot <- ggplot(reason_total_hours, aes(x=reorder(reason_full,absenteeism_time_in_hours), y=absenteeism_time_in_hours)) + labs(x = "Reason for absence", y = "Total absenteeism in hours")
reason_hours_plot  <- reason_hours_plot  + geom_bar(stat="identity", fill="black") + coord_flip()
reason_hours_plot

#plot only the top 10 reasons
reason_total_hours <- reason_total_hours[order(-reason_total_hours$absenteeism_time_in_hours),] #reorder
absence_by_reason_top10 <- reason_total_hours[1:10,] #subset only top 10
reason_hours_top10_plot1 <- ggplot(absence_by_reason_top10, aes(x=reorder(reason_full,absenteeism_time_in_hours), y=absenteeism_time_in_hours)) + labs(x = "Reason for absence", y = "Total absenteeism in hours")
reason_hours_top10_plot1 <- reason_hours_top10_plot1  + geom_bar(stat="identity", fill="black") + coord_flip()
reason_hours_top10_plot1

#some reasons are too descriptive so I will abreviate the reasons (levels)
levels(absence_by_reason_top10$reason_full)[levels(absence_by_reason_top10$reason_full)=="Diseases of the musculoskeletal system and connective tissue "] <- "Muscuskeletal diseases"
levels(absence_by_reason_top10$reason_full)[levels(absence_by_reason_top10$reason_full)=="Injury, poisoning and certain other consequences of external causes "] <- "Injury, poisoning or external causes"
levels(absence_by_reason_top10$reason_full)[levels(absence_by_reason_top10$reason_full)=="Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified "] <- "Nonclassified symptoms"

#plot with abreviated reasons
reason_hours_top10_plot2 <- ggplot(absence_by_reason_top10, aes(x=reorder(reason_full,absenteeism_time_in_hours), y=absenteeism_time_in_hours)) + labs(x = "Reason for absence", y = "Total absenteeism in hours")
reason_hours_top10_plot2 <- reason_hours_top10_plot2  + geom_bar(stat="identity", fill="darkgrey",width = 0.75) + coord_flip()
reason_hours_top10_plot2

#remove default grey background and grid lines to use grey colour in bars 
reason_hours_top10_plot3 <- reason_hours_top10_plot2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "darkgrey"), axis.ticks = element_line(colour = "darkgrey"))
reason_hours_top10_plot3

#remove y axis label and add title
reason_hours_top10_plot4 <- reason_hours_top10_plot3 + theme(axis.title.y = element_blank()) + ggtitle("What are the main reasons for absence?")
reason_hours_top10_plot4

#remove axis lines
reason_hours_top10_plot5 <- reason_hours_top10_plot4 + theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.line = element_blank())
reason_hours_top10_plot5

#align title and horizontal axis title
reason_hours_top10_plot6 <- reason_hours_top10_plot5 + theme(plot.title=element_text(hjust = 0.3), axis.title.x=element_text(hjust = 0.075, vjust=2.12) )
reason_hours_top10_plot6

#highlight main reasons for absence and add values as text labels
reason_hours_top10_plot7 <- reason_hours_top10_plot6 + geom_bar(data=subset(absence_by_reason_top10, absenteeism_time_in_hours>500), aes(x=reason_full, y=absenteeism_time_in_hours), fill="darkred", stat="identity",width = 0.75)
reason_hours_top10_plot7 <- reason_hours_top10_plot7 + geom_text(aes(label=absence_by_reason_top10$absenteeism_time_in_hours),hjust=1.1, color="white")
reason_hours_top10_plot7 


#highlight disciplinary failure and add values as text labels
reason_hours_top10_plot8 <- reason_hours_top10_plot7 + geom_bar(data=subset(absence_by_reason_top10, absenteeism_time_in_hours==320), aes(x=reason_full, y=absenteeism_time_in_hours), fill="darksalmon", stat="identity",width = 0.75)
reason_hours_top10_plot8 <- reason_hours_top10_plot8 + geom_text(aes(label=absence_by_reason_top10$absenteeism_time_in_hours),hjust=1.1, color="white")
reason_hours_top10_plot8


#########################
#### VISUALISATION 2 ###
########################
# seasonal behaviour of absenteeism

season_absence <- ggplot(df, aes(x=seasons,y=absenteeism_time_in_hours)) + geom_bar(stat = "identity",width = 0.5,fill="black") + labs(x = "Seasons", y = "Total absenteeism in hours")
season_absence



#focusing on top 2 reasons from previous visualisation


#abreviate reasons for absence in main dataframe as well
levels(df$reason_full)[levels(df$reason_full)=="Diseases of the musculoskeletal system and connective tissue "] <- "Muscuskeletal diseases"
levels(df$reason_full)[levels(df$reason_full)=="Injury, poisoning and certain other consequences of external causes "] <- "Injury, poisoning or external causes"
levels(df$reason_full)[levels(df$reason_full)=="Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified "] <- "Nonclassified symptoms"


#filter top 2 reasons in our dataset
df_top2_reasons <- filter(df, reason_full %in% c("Muscuskeletal diseases","Injury, poisoning or external causes" ))

#plot barchart with total absence of top 2 reasons
top2_reasons_plot <- ggplot(df_top2_reasons, aes(x=seasons, y=absenteeism_time_in_hours)) + labs(x = "Seasons", y = "Total absenteeism in hours") 
top2_reasons_plot <- top2_reasons_plot + geom_bar(stat="identity", fill="black", width=0.75) + facet_wrap(df_top2_reasons$reason_full)
top2_reasons_plot

#remove ggplot defaults and apply aesthetics operaions
top2_reasons_plot2 <- top2_reasons_plot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(), axis.line = element_line("grey"),axis.line.x= element_blank(),
                              axis.ticks.x = element_blank(),axis.title.x = element_blank(),
                              axis.ticks.y = element_line(colour = "darkgrey"), axis.text.y = element_text(colour="black"),
                              axis.text.x=element_text(color=c("black","black","red","black")), strip.background = element_rect(color="white", fill="white", size=1),
                              strip.text=element_text(size=11))


#add title with center alignment
top2_reasons_plot3 <- top2_reasons_plot2 + ggtitle("When preventive measures should be adopted?") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,500)
top2_reasons_plot3




#boxplot to better understand main reasons of absence 
top2_reasons_boxplot <- ggplot(df_top2_reasons, aes(x=seasons, y=absenteeism_time_in_hours)) + labs(x = "Seasons", y = "Absenteeism in hours") + facet_wrap(df_top2_reasons$reason_full)
top2_reasons_boxplot <- top2_reasons_boxplot + geom_boxplot(varwidth = F) #varwidth=T means it is proportional to occurences
top2_reasons_boxplot

#apply improvement of aesthetic operations
top2_reasons_boxplot2 <- top2_reasons_boxplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(), axis.line = element_line("grey"),
                                                             axis.ticks.x = element_blank(),axis.line.x= element_blank(),
                                                             axis.ticks.y = element_line(colour = "darkgrey"), axis.text.y = element_text(colour="black"),
                                                             strip.background = element_rect(color="white", fill="white", size=1),
                                                             strip.text.x = element_blank())

#plot two plots above in same plot
gridplot <- grid.arrange(top2_reasons_plot3, top2_reasons_boxplot2, nrow = 2)


#########################
#### VISUALISATION 3 ###
########################
#assessing impact if preventive measures are adopted

#if preventive measures are adopted it is expected a reduction of 50% of absence in top2 reasons for absence
df$absenteeism_time_expect <- df$absenteeism_time_in_hours
df <- within(df, absenteeism_time_expect[reason_full == 'Muscuskeletal diseases' | reason_full == 'Injury, poisoning or external causes'] <- df$absenteeism_time_in_hours[reason_full == 'Muscuskeletal diseases' | reason_full == 'Injury, poisoning or external causes']*0.5)

#subset dataframe to remove month zero
df <- df[df$month_of_absence!=0,]


#plot absenteeism by month
#group my month
absence_by_month <- group_by(df,month_of_absence)
#summarise the total number of hours per month (actual and expected)
month_total_hours <- summarise(absence_by_month,absenteeism_time_in_hours=sum(absenteeism_time_in_hours), absenteeism_time_expect=sum(absenteeism_time_expect))
month_total_hours

#change levels of months

#define grey color to use
grey = "grey38"
#define size of point
point_size=3
#define normal linethickness
normal_thick = 0.8
#define bold linethickness
bold_thick = 1.2

#start plot iterations
absence_by_month_plot1 <- ggplot(month_total_hours, aes(x=month_of_absence)) + labs(x = "Month", y = "Total absenteeism in hours") #group=1 because month is a factor (not a continuous data)
absence_by_month_plot1 <- absence_by_month_plot1 + geom_line(aes(y=absenteeism_time_in_hours, group=1),color = grey) + geom_line(aes(y=absenteeism_time_expect, group=1),color = "darkred") + ylim(0,800)
absence_by_month_plot1 


#change levels of months
levels(month_total_hours$month_of_absence) = c("Und","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")



#create vector of x axis labels to focus
winter_months_colour <- ifelse( month_total_hours$month_of_absence %in% c("Apr","May","Jun"), "darkred", grey )


#remove ggplot defaults and apply aesthetics operaions
absence_by_month_plot2 <- absence_by_month_plot1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(), axis.line = element_line(grey),axis.line.x= element_line(colour = grey),
                                                             axis.ticks.x = element_blank(),axis.text.x=element_text(color=winter_months_colour),
                                                             axis.ticks.y = element_line(colour = grey ), axis.text.y = element_text(colour=grey ))

absence_by_month_plot2
                                                             
#subset winter months and non-winter months to edit for different focus
month_total_hours_jan_feb <- month_total_hours[1:4,]
month_total_hours_mar_jun <- month_total_hours[4:6,]
month_total_hours_jul_dec <- month_total_hours[6:12,]

#plot by layers to customize subsets
#plot current absenteeism time in hours
absence_by_month_plot3 <- ggplot(month_total_hours, aes(x=month_of_absence,y=absenteeism_time_in_hours)) + geom_line(group=1, color=grey, size=normal_thick) + labs(x = "Month", y = "Total absenteeism in hours") + ylim(0,800) #group=1 because month is a factor (not a continuous data)
absence_by_month_plot3 <- absence_by_month_plot3 +  geom_point(group=1, color=grey,size=point_size)

#plot 1st subset of expected time in hours
absence_by_month_plot3 <- absence_by_month_plot3 + geom_line(data=month_total_hours_jan_feb,aes(y=absenteeism_time_expect, group=1),color = "darkred",linetype="dashed",size=normal_thick) +
                                                   geom_point(data=month_total_hours_jan_feb,aes(y=absenteeism_time_expect, group=1),size=point_size,color = "darkred",size=point_size)


#plot 3rd subset of expected time in hours
absence_by_month_plot3 <- absence_by_month_plot3 + geom_line(data=month_total_hours_jul_dec,group=1,aes(y=absenteeism_time_expect),color = "darkred",linetype="dashed",size=normal_thick) + 
                                                   geom_point(data=month_total_hours_jul_dec,aes(y=absenteeism_time_expect, group=1),size=point_size,color = "darkred",size=point_size)

#plot 2nd subset of expected time in hours
absence_by_month_plot3 <- absence_by_month_plot3 + geom_line(data=month_total_hours_mar_jun,group=1,aes(y=absenteeism_time_expect),color = "darkred",linetype="dashed",size=bold_thick) +
                                                   geom_point(data=month_total_hours_mar_jun,group=1,aes(y=absenteeism_time_expect),color = "darkred",size=point_size)

absence_by_month_plot3


#remove ggplot defaults and apply aesthetics operaions
absence_by_month_plot4 <- absence_by_month_plot3 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                                      panel.grid.minor = element_blank(), axis.line = element_line(grey),axis.line.x= element_line(colour = grey),
                                                                      axis.ticks.x = element_blank(),axis.text.x=element_text(color=winter_months_colour),
                                                                      axis.ticks.y = element_line(colour = grey ), axis.text.y = element_text(colour=grey ))
#add title
absence_by_month_plot5 <- absence_by_month_plot4 + ggtitle("What is the impact if preventive measures are adopted?")

#add annotations
absence_by_month_plot6 <- absence_by_month_plot5 + annotate(geom="text", x=5, y=250, label="Expected Absence", color="darkred") + 
                                                  annotate(geom="text", x=5.5, y=200, label="(preventive measures are more effective in Apr-Jun months)", color="darkred", size=3) + 
                                                  annotate(geom="text", x=5.1, y=600, label="Current Absence", color=grey)


sum_current_abs <- sum(month_total_hours$absenteeism_time_in_hours)
sum_expect_abs <- round(sum(month_total_hours$absenteeism_time_expect),0)

relative_drecrease <- round((sum_current_abs-sum_expect_abs)/sum_current_abs*100,1)


absence_by_month_plot7 <- absence_by_month_plot6 + annotate(geom="text", x=11.5, y=250, label="Total of 4658 hrs", color="darkred",size=3.5) + 
                                                   annotate(geom="text", x=11.5, y=200, label="(less 14.4%)", color="darkred", size=3) + 
                                                   annotate(geom="text", x=11.5, y=600, label="Total of 5444 hrs", color=grey,size=3.5)

absence_by_month_plot7





