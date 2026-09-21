df <- read.csv("/Users/Renfei/Downloads/archive/ai_jobs_salaries_clean.csv")

nrow(df)

# key vars are gonna be:
# work year, for longitudinal purposes
# because this is a longitudinal study obviousy given the database is about salaries over time and correlation to AI


# second key var is salary, because thats the whole point of the thing and what it will be plotted

# third key var is to group by role family, so probably making some sort of scatter plot

# on site and remote maybe relevant who knows

# salary on y axis, yearo n x axis, by family group

#create line plot for average salary over time
year_avg <- tapply(df$salary_in_usd, df$work_year, mean)
plot (year_avg,
      type = "l",
      xlab = "year (order)",
      ylab = "avg salary",
      main = "Average Salary Over Time (2020-2026)"
      )

#and bar plot
barplot (year_avg,
      type = "l",
      xlab = "year (order)",
      ylab = "avg salary",
      main = "Average Salary Over Time (2020-2026)"
)

#THEN, line plot by average salaries of EACH job family group over time
# so df$family_group and whatever

#checking how many unique family groups first
# length(unique(df$role_family))
# 11






