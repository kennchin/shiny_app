#function to source the required functions

get_functions <- function()
{
  ## main functions ##
  source('./R/convert_date.R')        #function to convert to date type
  source('./R/check_nodb.R')
  source('./R/calculate.R')           #function to calculate the number of days to progression;otherwise,number of days to last contact date
  source('./R/extract_col.R')         #function to extract desired columns
  source('./R/latest.R') 
  source('./R/check_date_format.R')   #function to check if the date columns are in correct format
  source('./R/comb_columns.R')        #function to combine progression and relapse used in check_prog_relapsed.R
  source('./R/check_prog_relapsed.R')
  source('./R/empty_as_na.R')         #function to fill blanks with "NA" 
  
  ## plot functions ##
  source('./R/plot1.R')
  source('./R/plot2.R')
  source('./R/plot3.R')
  source('./R/plot4.R')
  source('./R/plot_overall.R')
  source('./R/plot_overall_prog.R')
  source('./R/plot_overall_best.R')
  source('./R/plot_overall_vital.R')
  
  ## Monitor functions ##
  source('./R/Report_12345nodb.R')
  source('./R/Report_67890nodb.R')
  
  ## Death functions ##
  source('./R/Report_death_12345_nodb.R')
  source('./R/Report_death_67890_nodb.R')
  
  ## Demo functions ##
  source('./R/Report_demo_12345_nodb.R')
  source('./R/Report_demo_67890_nodb.R')

}