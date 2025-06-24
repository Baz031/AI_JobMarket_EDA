library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 999)

job_data <- read.csv("C:/Users/barry/Downloads/ai_job_dataset.csv")

head(job_data)
str(job_data)

# Drop unnecessary columns
job_data <- job_data %>% select(-application_deadline, -job_description_length)

# Check for missing data
sum(is.na(job_data))

# Check unique values for any missing/ outlying values

unique(job_data$job_title)
unique(job_data$salary_currency)
unique(job_data$experience_level)
unique(job_data$employment_type)
unique(job_data$company_location)
unique(job_data$company_size)
unique(job_data$employee_residence)
unique(job_data$remote_ratio)
unique(job_data$required_skills)
unique(job_data$education_required)
unique(job_data$years_experience)
unique(job_data$industry)
unique(job_data$company_name)
unique(job_data$benefits_score)

# Convert data types to appropriate format
str(job_data)

# To factor

# Salary currency
job_data$salary_currency <- as.factor(job_data$salary_currency)

# Experience level
job_data$experience_level <- as.factor(job_data$experience_level)

# Employment type
job_data$employment_type <- as.factor(job_data$employment_type)

# Complany Location
job_data$company_location <- as.factor(job_data$company_location)

# Company size
job_data$company_size <- as.factor(job_data$company_size)

# Employee Residence
job_data$employee_residence<- as.factor(job_data$employee_residence)

# Remote Ratio

# Education Required
job_data$education_required <- as.factor(job_data$education_required)

# Industry
job_data$industry <- as.factor(job_data$industry)

str(job_data)

#Extract lists of skills into separate columns

job_data <- job_data %>%
  separate(required_skills, into = paste0("Skill", 1:5), sep = ",\\s*")


# Questions and Analysis

#1. What is the average salary for job postings by experience level? 
mean_salary_experience <- job_data %>% group_by(experience_level) %>% summarise(mean_salary = mean(salary_usd)) %>% arrange(desc(mean_salary))

mean_salary_experience %>% ggplot(aes(x = factor(experience_level, level = c("EN", "MI", "SE", "EX")), y = mean_salary, fill = experience_level)) + geom_col() + ggtitle("Mean Salary of Job Postings According to Experience Level") + xlab("Experience Level") + ylab("Average Salary") + theme(legend.position = "none")

                                                                                        
#   How does salary differ across company sizes?
mean_salary_experience_company_size <- job_data %>% group_by(company_size, experience_level) %>% summarise(mean_salary = mean(salary_usd)) %>% arrange(desc(mean_salary))
mean_salary_experience_company_size

ggplot(mean_salary_experience_company_size, aes(x = factor(company_size, level = c("S", "M", "L")), y = factor(experience_level, level = c("EN", "MI", "SE", "EX")), fill = mean_salary)) + geom_tile() + scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Average Salary according to Experience Level and Company Size",
       x = "Company Size",
       y = "Experience Level",
       fill = "Average Salary")

#2. Which countries have the highest average remote_ratio for jobs requiring a Master’s degree?

remote_ranked_location <- job_data %>% filter(education_required == "Master") %>% group_by(company_location) %>% summarise(mean_remote = mean(remote_ratio)) %>% arrange(desc(mean_remote))
remote_ranked_location[1:5,]
  
#3. What are the top 3 most frequently listed skills for jobs with high salary (top 10%)?.
high_salary_skills <- job_data %>% filter(salary_usd >= quantile(salary_usd, probs = 0.9)) %>% group_by(Skill1) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(3)
high_salary_skills

#4. What job titles require the greatest amount of experience?
job_data %>% group_by(job_title) %>% summarise(median_exp = median(years_experience)) %>% arrange(desc(median_exp)) %>% head(5)

#5. What do salaries look like for jobs that list both ‘Deep Learning’ and ‘Python’ as required skills? 
deep_learning_python <- job_data %>% filter((Skill1 == "Deep Learning"|Skill2 == "Deep Learning"|Skill3 == "Deep Learning"|Skill4 == "Deep Learning"|Skill5 == "Deep Learning") & (Skill1 == "Python"|Skill2 == "Python"|Skill3 == "Python"|Skill4 == "Python"|Skill5 == "Python")) %>% select(salary_usd) 

hist(deep_learning_python$salary_usd, main = "Distribution of Salaries for Python/ Deep Learning Jobs", xlab = "Salary (USD)")


#6. How has the average salary changed over time in 2024?
job_data$posting_date <- as.POSIXct(job_data$posting_date)
ads_2024 <- job_data %>% filter(posting_date > "2023-12-31" & posting_date < "2024-12-31") %>% mutate(month = format(posting_date,"%m")) %>% group_by(month) %>% summarise(avg_salary = mean(salary_usd))

ads_2024 %>% plot(type = "l", main = "Monthly Change in Average Salary for 2024", xlab = "Month", ylab = "Average Salary")

#7. Which industry showed the greatest month-to-month salary growth between Oct 2024 and March 2025?
oct_2024_to_mar_2025 <- job_data %>% filter(posting_date > "2024-10-01" & posting_date < "2025-03-31") %>% mutate(year_month = format(posting_date, "%y-%m")) %>% group_by(industry, year_month) %>% arrange(year_month) %>% summarise(avg_salary = mean(salary_usd)) %>% mutate(percent_change = (avg_salary - lag(avg_salary))/lag(avg_salary) *100)

oct_2024_to_mar_2025 %>% group_by(industry) %>% summarise(avg_pct_change = mean(percent_change, na.rm = TRUE)) %>% ggplot(aes(y = industry, x = avg_pct_change, fill = avg_pct_change > 0)) + geom_col() +
  labs(title = "Average Monthly Percentage Change in Salary per Industry (Oct 2024 to March 2025)", x = "Average Percentage Change", y = "Industry") + theme(legend.position = "none")


#8. Which months tend to have the highest number of job postings across all industries?

monthly <- job_data %>% mutate(month = format(posting_date,"%m")) %>% group_by(month) %>% summarise(n = n())
plot(monthly$month, monthly$n, type = "l", main = "Total postings by month", xlab = "Month", ylab = "No. of Job Postings")
  
#9. Which companies have posted the most fully remote jobs and what is the average salary at each?

job_data %>% filter(remote_ratio == 100) %>% group_by(company_name) %>% summarise(n = n(), avg_salary = mean(salary_usd)) %>% arrange(desc(n))
  
#10. Which industries offer better non-monetary benefits?

job_data %>% group_by(industry) %>% summarise(avg_benefits = mean(benefits_score)) %>% arrange(desc(avg_benefits))

#11. Which type of employment and company most often corresponds to salaries in the top 10%.

job_data %>% filter(salary_usd > quantile(salary_usd,0.9)) %>% group_by(company_size, employment_type) %>% summarise(n = n()) %>% arrange(desc(n))

#12. What are the top 5 job titles for PhD holders in terms of salary, and which industries do they belong to?

job_data %>% filter(education_required == "PhD") %>% group_by(industry, job_title) %>% summarise(avg_salary = mean(salary_usd)) %>% arrange(desc(avg_salary)) %>% head(5)
  
#13. How does the average salary vary according to company location and employee residence location?

job_data %>% group_by(employee_residence, company_location) %>% summarise(avg_salary = mean(salary_usd)) %>% arrange(desc(avg_salary)) %>% ggplot(aes(x = employee_residence, y = company_location, fill = avg_salary)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Average Salary by Employee Residence and Company Location",
       x = "Employee Residence",
       y = "Company Location",
       fill = "Avg Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#14. Which countries are more likely to offer remote work and what do the salary and benefits look like?


job_data %>% group_by(company_location) %>% summarise(avg_rr = mean(remote_ratio), avg_salary = mean(salary_usd), avg_benefits = mean(benefits_score)) %>% filter(avg_rr > 50)

#15. Is there a difference in experience for the same job title between domestic and international employees?

resident_employees <- job_data %>% filter(employee_residence == company_location) %>% select(years_experience)

non_resident_employees <- job_data %>% filter(employee_residence != company_location) %>% select(years_experience)

t.test(resident_employees$years_experience, non_resident_employees$years_experience) # No difference in experience for same job title between domestic and international employees
  
#16. Which combination of Skill1 and Skill2 appears most frequently in high-paying jobs (top 25%)?

job_data %>% filter(salary_usd > quantile(salary_usd, 0.25)) %>% mutate(skill_min = pmin(Skill1, Skill2), skill_max = pmax(Skill1, Skill2), skills_one_two = paste(skill_min, "-", skill_max)) %>% group_by(skills_one_two) %>% summarise(n = n()) %>% arrange(desc(n))
                                                                        
#17. Among jobs requiring Kubernetes as a skill, what is the average salary, and how does it vary by industry?

job_data %>% filter(if_any(c(Skill1, Skill2, Skill3, Skill4, Skill5), ~ .x == "Kubernetes")) %>% group_by(industry) %>% summarise(avg_salary = mean(salary_usd)) %>% arrange(desc(avg_salary)) %>% ggplot(aes(x = industry, y = avg_salary, fill = industry)) + geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position =  "none") + labs(title = "Average Salary per Industry for Jobs Requiring Kubernetes", x = "Industry", y = "Average Salary")

#18. For all postings mentioning both ‘Python’ and ‘SQL’ across any skill columns, what's the average years_experience and salary?

job_data %>% filter((if_any(c(Skill1, Skill2, Skill3, Skill4, Skill5), ~ .x == "Python") & (if_any(c(Skill1, Skill2, Skill3, Skill4, Skill5), ~ .x == "SQL")))) %>% summarise(avg_exp = mean(years_experience), avg_salary = mean(salary_usd))


#19. How many elite remote roles are there (salary in the top 10%, benefits_score in the top 25%, and remote_ratio = 100), and what’s their company location and job title?

elite_roles <- job_data %>% filter((salary_usd > quantile(salary_usd, 0.9)) & (benefits_score > quantile(benefits_score, 0.75)) & (remote_ratio == 100))

elite_roles %>% nrow()

ggplot(elite_roles, aes( x = company_location,y = job_title, fill = salary_usd)) + geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Salaries of Elite Roles by Job Title and Company Location",
       x = "Company Location",
       y = "Job Title",
       fill = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


