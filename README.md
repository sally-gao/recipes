# Allrecipes.com Ingredient Analysis

This is an ongoing data analysis project. 

**Steps:**
1. Web scraping (DONE)
2. Data cleaning and processing (ONGOING)
3. Regression model
4. Topic modeling/clustering

## Objectives and Methods
Given a dataset of recipes and their associated ratings, I want to generate a list of the top ingredients within the recipe set and:

* a) Create a regression model that successfully predicts a recipe’s rating using its ingredients as predictors;

* b) Discover which combinations of ingredients are associated with high ratings, and predict which of two recipes will be more successful given that they share at least one primary ingredient.

The regression methods that I will explore include multiple linear regression and tree-based methods such as a random forest.
To discover which ingredients tend to appear together, I will use latent Dirichlet Allocation, a generative model that represents documents as a mixture of topics. I will also explore clustering techniques such as K-means clustering and hierarchical clustering.
