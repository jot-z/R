1. Potential approaches
The potential approaches are changing rate table and some particular models.
1.1 Changing rate table
Changing rate table is a table with time and food types. Each cell in this table represent the relative change rate of frequency in this month.
Equation: Relative change rate of frequency in this month = (frequency of a particular food in this month – frequency in last month)/ frequency in last month

1.2 Potential useful models
Potential useful models are Dynamic Topic Modeling (LDA) and Topic Over Time model.
Dynamic Topic Modeling allows the words that are most strongly associated with a given topic to vary over time whether the characteristics of individual topics vary over time.
Topic Over Time model is similar to a structural topic model whose time covariates are continuous. Topics are fixed, but their relative prevalence and correlations can vary, so this model can show whether certain topics become more or less prevalent over time.










2. My method
2.1 Matrix of time vs food
I combined ingredient txt file with the Facebook posts files to build a matrix.
The row names are number from 1 to 60, which are 01/2011 to 12/2015.
The column names are ingredients from the ingredient txt.
Each cell means the frequency of a specific ingredient appearing in one particular month.
This is how the matrix looks like.


Based on several reference articles, some food types are more likely to have specific trends.
These are the types I chose, including pumpkin, blueberry, turkey, cheese, haddock, salmon, trout, pecan, kale, marshmallow, tea and avocado.
2.2 Highest 5
I also choose the highest 5 ingredients, which are cake, cheese, chicken, cream and chocolate.



2.3 My own list
I create my own list to include other ingredients that are not in the ingredient txt file.
My own list is the following.







3. Preliminary results
I plot the time series of all these ingredients.


3.1 My chosen ingredients for plot:
pumpkin, blueberry, turkey, cheese, haddock, salmon, trout, pecan, kale, marshmallow, tea and avocado
cake, cheese, chicken, cream and chocolate
zoodle, spiralizer, dietary, granola, local, organic, kimchi, mocktails, poke, bowl

From the plots, we can see 5 kinds of food trends, which are seasonal trends, increasing trend, wavy trend, sudden trend, and new food trend.


3.2 Seasonal trend
Some festival related food and have seasonal trends. 
Pumpkin and turkey are festival related food.
From the time series plot, we can see that the frequency of pumpkin reached the highest point in October every year, and the frequency of turkey reached the highest point in November every year.

Blueberry and pecan are seasonal food.
From the time series plot, we can see that the frequency of blueberry reached the highest point in July every year, and the frequency of pecan reached the highest point in November every year.




3.3 Increasing trend
Cauliflower, tea, avocado, fish, kale, marshmallow, and granola have increasing trend.
The 5 top ingredients (cake, cheese, chicken, cream and chocolate) also have this increasing trend, except that their frequency range is larger (1000-15000).
These kinds of food are very normal in every supermarket.











3.4 Wavy trend
Haddock and trout have wavy trend.
From their time series plots, we can see that they do not have specific increasing or decreasing trend, and their frequency is very low compared to other types of type. The highest frequency in a month is less than 70, but the lowest frequency is higher than 0.
This shows that some people posted these food, but not a lot of people posted them.



3.5 Sudden trend 
Salmon and kimchi have an abnormal sudden increase in one month.
The frequency of salmon reached the highest point in 06/2015. This may relate to the news about salmon raised on land.
The frequency of kimchi reached the highest point in 10/2015.
To understand this sudden trend, we need in-depth knowledge of this particular food industry.
Reference: 
https://news.nationalgeographic.com/2015/06/150607-salmon-aquaculture-canada-fish-farm-food-world/ 



3.6 New food trend 
Zoodle/spiralizer, mocktails and poke bowl are the new food trends.
Also, people pay more attention about dietary, local and organic food.

From the time series plot of zoodle/spiralizer, we can see that zoodle was noticed by people from the early 2015. 

From the time series plot of mocktails, we can see that mocktails was noticed by people from the 2013, but the popularity of it did not increase a lot in 2014 and 2015. 



From the time series plot of poke/bowl, we can see that poke/bowl became popular from 2013, and kept being more popular since then.  


People started to post more about dietary, local and organic food in their Facebook from 2011, and kept posting a lot from then.



4. Conclusion
From these plots, we can see different kinds of food trends, which are seasonal trends, increasing trend, wavy trend, sudden trend, and new food trend. Different trends are related to different kinds of food with some certain common features.

To detect the trend of a particular food on time, we can collect Facebook posts every month and based on how early a detection can be useful, we can take actions when certain range of word frequency increase appears.



