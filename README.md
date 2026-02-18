# Google Data Analytics Certification Capstone Project: *How Can a Wellness Technology Company Play It Smart?*

![Google Data Analytics Professional Certificate](https://github.com/droche01/google-data-analytics-certification-capstone-project/blob/15282418fa3cc2c7f9cd671c32b571291718dc30/DylanRoche_GoogleDataAnalytics_ProfessionalCertificate_Coursera%20A1JJX41RKH5Q_February2026_page-0001.jpg)

The live report can be found here: [Bellabeat Case Study Live Report](https://droche01.github.io/google-data-analytics-certification-capstone-project/)

## Description
In the capstone project for the **Google Data Analytics Professional Certificate**, I analyzed publicly available Fitbit daily activity and sleep data to identify key trends, relationships, and behavioral patterns pertinent to **Bellabeat**--a health and fitness technology company that develops smart wellness products for women. 
Utilizing these insights, I explored how they could be applied to Bellabeat's **Leaf** product--a wellness tracker than can be worn as a bracelet, necklace, or clip--to better understand user behavior and inform data-driven product and marketing strategies.

## Dataset Information
The dataset used for this project can be found here: [FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit)

Key columns of the base dataset include:

`Id`, `ActivityDate`, `TotalDistance`, `VeryActiveMinutes`, `FairlyActiveMinutes`, `LightlyActiveMinutes`, `SedentaryMinutes`, `Calories`, `TotalMinutesAsleep`,`TotalTimeInBed`

## Technologies Used
- Microsoft Excel
- R
- tidyverse
- dplyr
- ggplot2
- ggpubr
- ggcorrplot
- GitHub Pages

## Installation
To run this project locally, install **R** and **R Studio** on your machine. Once done, proceed with the steps below:

1. Clone the repository:
   ```bash
   git clone https://github.com/droche01/google-data-analytics-certification-capstone-project.git
   
2. Navigate to the project directory:
       ```bash
   cd google-data-analytics-certification-capstone-project
3. Open the project file:
Click on the ``DylanRoche_GoogleDataAnalyticsCertification_CapstoneProject_February2026.Rproj`` file to launch it in RStudio.
4. Install dependencies:
    ```r
   install.packages(c("tidyverse", "dplyr", "ggplot2", "ggpubr", "ggcorrplot"))
5. Render the report:
Open the ``DylanRoche_GoogleDataAnalyticsCertification_CapstoneProject_February2026.Rmd`` file in RStudio and click the **Knit** button at the top left-hand side of the editor. This will process the code and generate the final ``index.html`` report.

## Analysis Highlights
- Dataset cleaning, preprocessing, and outlier detection 
- Feature engineering, such as the `IsWeekend`, `SleepEfficiency`, and `BehavioralProfile` columns, to enable deeper analysis
- Comparative analysis of weekday vs. weekend activity levels, sleep duration, and sleep efficiency
- Calculation of average daily step count, distance traveled, and minutes asleep
- Correlation analysis between average daily calories and average daily steps, total minutes asleep and sleep efficiency, total steps and minutes asleep, and more
- Visualizations of participant distribution, activity and sleep patterns, and active minute categories
- Linear regression modeling to survey relationships between key variables

## Key Insights and Findings
- **Weekday vs. Weekend Activity Patterns**: Average step counts and distance traveled are slightly higher on weekdays than on weekdays, which could be due to more structured daily routines (e.g., commuting to work or school).
- **Weekday vs. Weekend Sleep Patterns**: Average minutes spent asleep are slightly higher on weekends than on weekdays, which could be due to people waking up on earlier on weekdays for work or school, fewer obligations on weekends, and the body's circandian rhythm.
- **Correlation between Average Daily Steps and Average Daily Calories Burned**: There is a strong positive correlation between average daily steps and average daily calories burned, suggesting that as steps increase, calories burned tend to increase.
- **Correlation between Total Minutes Asleep and Sleep Efficiency**: There is a weak positive relationship between total minutes asleep and sleep efficiency, suggesting that longer sleep duration is only slightly associated with higher sleep efficiency.
- **Correlation between Total Daily Steps and Total Minutes Asleep**: There is a weak negative relationship between total daily steps and total minutes asleep, suggesting that there is only a slight tendency for higher activity to be associated with shorter sleep duration.
- **Correlation between Calories Burned and Sleep Efficiency**: There is a weak positive relationship between calories burned and sleep efficiency, suggesting that there is a modest tendency for higher sleep quality to be associated with greater energy expenditure.

