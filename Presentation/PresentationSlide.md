Listen to Your Heart
========================================================
class: title-slide
author: WIE2003 Group 10
date: 7 Jun 2021
autosize: true
transition: rotate
Group Member:
<small>
- Daryl Gan En-Wei (U2005414/1)
- Ng Jun Jing (U2005387/1)
- Tan Yi Yung (U2005295/1)
- Theeveeyan Raj (U2005320/1)
</small>

Introduction
========================================================

Coronary Heart Disease has been the top cause of death in Malaysia for some time. We wish to develop an application that can predict the likelihood for a person to have heart disease using some indicators of heart disease. Thus, people can know it earlier and get early treatment. So,
we came up with this 'Listen to Your Heart' app that we hope it will benefits the general public.

Questions:
<small>
- What are the indicators of having a heart disease? 
- Can we predict heart disease with those indicators?
</small>

Link to our project on GitHub: <https://github.com/darylGan/Listen-to-your-Heart>

Data Cleaning
========================================================

We get our data set from <https://www.kaggle.com/rashikrahmanpritom/heart-attack-analysis-prediction-dataset>.
<small>
- There are 14 columns and 309 rows.
- We removed **NA** or **N/A** or **null** value in this data set.
- We removed the inappropriate value from the data set. For example, **number of major vessels colored by flourosopy**(caa) should only have value from 0 to 3. Therefore, we removed the rows with caa=4.
</small>

![e](https://raw.githubusercontent.com/darylGan/Heart-Attack-Analysis/main/caa4.jpg)


- Now, we have 14 columns and 292 rows.


Data Analysis
========================================================

After analyzing the data, we chose some variables that are statistically significant, which are:
<small>
  - chest pain type(cp)
  - exercise induced angina(exng)
  - ST depression induced by exercise relative to rest(oldpeak)
  - number of major vessels colored by flourosopy(caa)
</small>

We used those variables as the indicators of heart disease to train a logistic regression model.

<small>
Alice has **chest pain with atypical angina**. She **seldom do exercise**, which may make her heart weaker. Her **oldpeak is quite low**. Her **number of major vessels colored by flourosopy** are quite **less**. This made her **prone** to have **heart disease**.
</small>

Listen to Your Heart
========================================================
left: 50%
<small>
Function: 
- Perform heart disease prediction
- Explore the dataset used using:
  - table
  - graph plot
  - bar chart

Link to app: 

<small><https://theeveeyanraj4321.shinyapps.io/ListenToYourHeart/></small>

</small>

***

![App preview](https://raw.githubusercontent.com/darylGan/Heart-Attack-Analysis/main/App_screenshot.png)