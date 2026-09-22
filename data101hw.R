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
      xlab = "year (2020-2026)",
      ylab = "avg salary",
      main = "Average Salary Over Time (2020-2026)"
      )

#and bar plot
barplot (year_avg,
      xlab = "year (2020-2026)",
      ylab = "avg salary",
      main = "Average Salary Over Time (2020-2026)"
)

#THEN, line plot by average salaries of EACH job family group over time
# so df$family_group and whatever

#checking how many unique family groups first
# length(unique(df$role_family))
# 11
#this would be so much easier if we could use for loops but wtv

table(df$role_family)

fam1 <- subset(df, role_family == "Analytics Manager")
fam2 <- subset(df, role_family == "Data Scientist")
fam3 <- subset(df, role_family == "Other / Unclassified")
fam4 <- subset(df, role_family == "AI Engineer")
fam5 <- subset(df, role_family == "Data Analyst")
fam6 <- subset(df, role_family == "Computer Vision")
fam7 <- subset(df, role_family == "Data Engineer")
fam8 <- subset(df, role_family == "ML Engineer")
fam9 <- subset(df, role_family == "Research Scientist")
fam10 <- subset(df, role_family == "AI Architect")
fam11 <- subset(df, role_family == "NLP")

#data sanitizer since it was obviously messing with the range of the plots
clean_sal <- subset(df, salary_outlier_flag == "False")

#same min and max for every plot for consistency, div by 1000 so it stops going into scientific notation
sal_min <- min(clean_sal$salary_in_usd) / 1000
sal_max <- max(clean_sal$salary_in_usd) / 1000

fam1_avg <- tapply(fam1$salary_in_usd, fam1$work_year, mean) / 1000
plot(fam1_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Analytics Manager: Avg Salary Over Time")

fam2_avg <- tapply(fam2$salary_in_usd, fam2$work_year, mean) / 1000
plot(fam2_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Data Scientist: Avg Salary Over Time")

# i had claude do the rest of the families since i am not typing all of that

fam3_avg <- tapply(fam3$salary_in_usd, fam3$work_year, mean) / 1000
plot(fam3_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Other/Unclassified: Avg Salary Over Time")

fam4_avg <- tapply(fam4$salary_in_usd, fam4$work_year, mean) / 1000
plot(fam4_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "AI Engineer: Avg Salary Over Time")

fam5_avg <- tapply(fam5$salary_in_usd, fam5$work_year, mean) / 1000
plot(fam5_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Data Analyst: Avg Salary Over Time")

fam6_avg <- tapply(fam6$salary_in_usd, fam6$work_year, mean) / 1000
plot(fam6_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Computer Vision: Avg Salary Over Time")

fam7_avg <- tapply(fam7$salary_in_usd, fam7$work_year, mean) / 1000
plot(fam7_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Data Engineer: Avg Salary Over Time")

fam8_avg <- tapply(fam8$salary_in_usd, fam8$work_year, mean) / 1000
plot(fam8_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "ML Engineer: Avg Salary Over Time")

fam9_avg <- tapply(fam9$salary_in_usd, fam9$work_year, mean) / 1000
plot(fam9_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "Research Scientist: Avg Salary Over Time")

fam10_avg <- tapply(fam10$salary_in_usd, fam10$work_year, mean) / 1000
plot(fam10_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "AI Architect: Avg Salary Over Time")

fam11_avg <- tapply(fam11$salary_in_usd, fam11$work_year, mean) / 1000
plot(fam11_avg, type = "l", ylim = c(sal_min, sal_max), xlim = c(0,6),
     xlab = "Year (2020-2026)", ylab = "Avg Salary (USD, thousands)",
     main = "NLP: Avg Salary Over Time")



# then a plot combining all 11 unique role families, sanitized of outlier in range

# boxplot because i can't use lines

# all 11 families on one plot, each a distinct color, single boxplot() call
fam_colors <- c("red", "blue", "darkgreen", "purple", "orange",
                "brown", "deeppink", "cyan", "magenta", "black", "gold")

boxplot(salary_in_usd /1000 ~ role_family, data = df,
        col = fam_colors,
        las = 2, 
        cex.axis = 0.5, xlab = "", ylab = "Salary (USD)",
        main = "Salary Distribution by Role Family")

barplot(table(df$work_year), xlab = "Year", ylab = "Number of Salary Records",
        main = "The AI Job Market Explosion (2020-2025)")


# unclassified taking up half the thing
mosaic_data <- subset(df, role_family != "Other / Unclassified")

mosaicplot(table(mosaic_data$work_year, mosaic_data$role_family),
           main = "Role Family Emergence Over Time (2020-2025)",
           xlab = "Year", ylab = "Role Family",
           cex.axis = 0.6, las = 1)

boxplot(df$salary_in_usd /1000, data = df,
        las = 2, xlab = "", ylab = "Salary (USD, Thousands)",
        main = "Overall Salary Distribution")
