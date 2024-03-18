# Statistics for Business Analytics I

### Dataset Description
The `salary` data frame contains information about 474 employees hired by a Midwestern bank between 1969 and 1971. It was created for an Equal Employment Opportunity (EEO) court case involving wage discrimination. The file contains beginning salary (SALBEG), salary now (SALNOW), age of respondent (AGE), seniority (TIME), gender (SEX coded 1 = female, 0 = male), among other variables.

### Tasks

1. **Read and Understand the Dataset**
    - Read the dataset "salary.sav" as a data frame and use the function `str()` to understand its structure.

2. **Summary Statistics and Visualization**
    - Get the summary statistics of the numerical variables in the dataset and visualize their distribution (e.g., use histograms etc). Identify variables that appear to be normally distributed and provide reasoning.

3. **Hypothesis Testing - Beginning Salary**
    - Use the appropriate test to examine whether the beginning salary of a typical employee can be considered equal to $1000. Interpret the results and justify the choice of the test.

4. **Hypothesis Testing - Beginning vs. Current Salary**
    - Test if there is any significant difference between the beginning salary (SALBEG) and the current salary (SALNOW). Construct a new variable for the difference (SALNOW – SALBEG) and test if, on average, it is equal to zero. Justify the choice of the test.

5. **Gender-Based Salary Differences**
    - Investigate if there is any difference in the beginning salary (SALBEG) between the two genders. Justify the test used to assess this hypothesis and interpret the results.

6. **Age Group Analysis**
    - Cut the AGE variable into three categories so that the observations are evenly distributed across categories. Assign the cut version of AGE into a new variable called age_cut. Investigate if, on average, the beginning salary (SALBEG) is the same for all age groups. If there are significant differences, identify the groups that differ by making pairwise comparisons. Justify the choice of the test used.

7. **Proportion Comparison**
    - By making use of the factor variable minority, investigate if the proportion of white male employees is equal to the proportion of white female employees.

### Deliverable

A PDF file documenting the process of completing each task along with the interpretation of results, including necessary justifications and explanations.
