
library(ggplot2)
library(rvest)

# Function to check if a string is in English
is_english <- function(text) {
  # Use regular expression to check if the string contains only English letters and allow spaces, commas, and other special characters in job titles
  grepl("^[A-Za-z0-9[:space:],[:punct:]]+$", text)
}

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

base_url <- 'https://www.jobstreet.com.my/jobs?page='
num_pages <- 2  # Change this to the number of pages you want to scrape

all_job_titles <- c()  # Initialize an empty vector to store all job titles
all_companies <- c()  # Initialize an empty vector to store all company names
all_job_types <- c() # Initialize an empty vector to store all job types
all_locations <- c()  # Initialize an empty vector to store all job locations
all_classifications <- c()  # Initialize an empty vector to store all job classifications
all_posted_dates <- c()  # Initialize an empty vector to store all posted dates

# Loop through pages to scrape job data
for (page in 1:num_pages) {
  url <- paste0(base_url, page)
  webpage <- read_html(url)
  
  # Extract job titles
  job_title_nodes <- html_nodes(webpage, '.uo6mkd')
  job_titles <- html_text(job_title_nodes)
  all_job_titles <- c(all_job_titles, job_titles)
  
  # Extract company names
  company_nodes <- html_nodes(webpage, '.a1msqi6q .lnocuoa :nth-child(2)')
  companies <- html_text(company_nodes)
  all_companies <- c(all_companies, companies)
  
  # Extract job types by visiting individual job pages
  job_links <- paste0('https://www.jobstreet.com.my', html_attr(html_nodes(webpage, '.a1msqi6m .lnocuov a'), 'href'))
  
  # Using nested web scraping concept to extract the job types
  for (link in seq_along(job_links)) {
    webpage_job <- read_html(job_links[link])
    job_types_nodes <- html_nodes(webpage_job, '.a1msqi6u:nth-child(3) .a1msqir+ .a1msqir')
    job_types <- html_text(job_types_nodes)
    all_job_types <- c(all_job_types, job_types)
  }

  # Extract job locations
  location_nodes <- html_nodes(webpage, '.a1msqi6q .a1msqi6u ._1wkzzau0 .szurmz4 .a1msqi6m:nth-child(1) .lnocuo7')
  locations <- html_text(location_nodes)
  all_locations <- c(all_locations, locations)
  
  # Extract job classifications
  classification_nodes <- html_nodes(webpage, '.a1msqibu:nth-child(5)')
  classifications <- html_text(classification_nodes)
  all_classifications <- c(all_classifications, classifications)
  
  # Extract posted dates
  posted_date_nodes <- html_nodes(webpage, '.szurmz6 .lnocuo22.lnocuo7')
  dates <- html_text(posted_date_nodes)
  all_posted_dates <- c(all_posted_dates, dates)
}

# Remove open and close brackets from job classifications
all_classifications <- gsub("\\(|\\)", "", all_classifications)

# Remove the cities from job locations and keeping the states
all_locations <- gsub(".*\\, ", "", all_locations)

# Create a vector to store formatted dates
formatted_dates <- c()

# Remove values that are "Salary (MYR)"
all_posted_dates <- all_posted_dates[!grepl("Salary \\(MYR\\)", all_posted_dates)]

# Format the dates into year-month-day
for (date in all_posted_dates) {
  if (grepl("d ago", date)) {
    # Calculate date difference for days ago
    formatted_date <- Sys.Date() - as.numeric(gsub("\\D", "", date))
  } else if (grepl("h ago", date)) {
    # Calculate date difference for hours ago
    formatted_date <- Sys.Date() - as.numeric(gsub("\\D", "", date)) / 24
  } else if (grepl("m ago", date)) {
    # Calculate date difference for minutes ago
    formatted_date <- Sys.Date() - as.numeric(gsub("\\D", "", date)) / (24 * 60)
  } else {
    # handle other cases as needed
    formatted_date <- NA
  }
  formatted_dates <- c(formatted_dates, format(as.Date(round(formatted_date)), "%Y-%m-%d"))
}

# Create a data frame with the extracted information
job_data <- data.frame(
  JobTitle = all_job_titles,
  CompanyName = all_companies,
  JobType = all_job_types,
  JobLocation = all_locations,
  JobClassification = all_classifications,
  PostedDate = formatted_dates
)

# Remove records with non-English job titles
job_data <- job_data[sapply(job_data$JobTitle, is_english), ]

# Display the extracted information
print(job_data)

# Specify the file path where you want to save the CSV file
csv_file_path <- "Job_Data.csv"

# Write the data frame to a CSV file
write.csv(job_data, file = csv_file_path, row.names = FALSE)

# Print a message indicating that the data has been saved
cat("\nData has been saved to", csv_file_path, "\n")

# Plot 1: Bar Plot - Job Classification Count
plot1 <- ggplot(job_data, aes(x = reorder(JobClassification, table(JobClassification)[JobClassification]), y = ..count.., fill = JobClassification)) +
  geom_bar(stat = "count", show.legend = TRUE) +
  labs(title = "Job Classification Count", x = "Job Classification", y = "Job Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() # Flip the coordinates for horizontal bars

# Plot 2: Bar Plot - Job Location Count
plot2 <- ggplot(job_data, aes(x = reorder(JobLocation, table(JobLocation)[JobLocation]), y = ..count.., fill = JobLocation)) +
  geom_bar(stat = "count", show.legend = TRUE) +
  labs(title = "Job Location Count", x = "Job Location", y = "Job Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() # Flip the coordinates for horizontal bars

# Plot 3: Bar Plot - Company Name and Job Classification
plot3 <- ggplot(job_data, aes(x = CompanyName, fill = JobClassification)) +
  geom_bar() +
  theme(axis.text.x = element_text(hjust = 1)) +
  labs(title = "Company Name and Job Classification", x = "Company Name", y = "Count") +
  theme(legend.position = "right", legend.text = element_text(size = 8)) +
  coord_flip() # Flip the coordinates for horizontal bars

# Plot 4: Pie Chart - Job Types Distribution
plot4 <- ggplot(job_data, aes(x = "", fill = JobType)) +
  geom_bar(width = 1) +
  labs(title = "Job Types Distribution") +
  coord_polar(theta = "y") +
  geom_text(stat = "count", aes(label = paste0(stat(count))), position = position_stack(vjust = 0.5), size = 5) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# Plot 5: Stacked Bar Plot - Job Locations by classifications
plot5 <- ggplot(job_data, aes(x = JobLocation, fill = JobClassification)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Job Locations by Classifications", x = "Job Location", y = "Job Count", fill = "Job Classification")

# Save all plots to a PDF file
pdf_file_path <- "Job_Plots.pdf"
pdf(pdf_file_path, width = 14, height = 8)
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
dev.off()

# Print a message indicating that the plots have been saved
cat("\nPlots have been saved to", pdf_file_path, "\n")
