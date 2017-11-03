from bs4 import BeautifulSoup
import requests
import time
import pandas as pd
import re
import os
import random

# list of potentially valid recipe IDs
ids = list(range(6698, 6820, 3))
ids.extend(list(range(6865, 6984, 3)))
ids.extend(list(range(7245, 7533, 7)))
ids.extend(list(range(8652, 8887, 3)))
ids.extend(list(range(9023, 9471, 10)))
ids.extend(list(range(10033, 10813, 7)))
ids.extend(list(range(12682, 12852, 3)))
ids.extend(list(range(14469, 14925, 7)))
ids.extend(list(range(15004, 15679, 5)))
ids.extend(list(range(15908, 16239, 3)))
ids.extend(list(range(16348, 16700, 5)))
ids.extend(list(range(16895, 17562, 5)))
ids.extend(list(range(20476, 20876, 3)))
ids.extend(list(range(21014, 22180, 7)))
ids.extend(list(range(23315, 23847, 3)))
ids.extend(list(range(24202, 24530, 3)))
ids.extend(list(range(25037, 26317, 7)))
ids.extend(list(range(26692, 27072, 3)))
ids.extend(list(range(50435, 50644, 3)))
ids.extend(list(range(51283, 51711, 7)))
ids.extend(list(range(65894, 66069, 3)))
ids.extend(list(range(80388, 86230, 50)))
ids.extend(list(range(143082, 143281, 2)))
ids.extend(list(range(217981, 218929, 3)))
ids.extend(list(range(232127, 232529, 3)))
ids.extend(list(range(238654, 240001, 10)))
ids.extend(list(range(240007, 240695, 3)))
ids.extend(list(range(245442, 245666, 3)))
ids.extend(list(range(246274, 246629, 3)))
ids.extend(list(range(255277, 256398, 3)))


user_agent_list = ['Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36',
                   'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36',
                   'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36',
                   'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Safari/604.1.38',
                   'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36']

urls = ["http://allrecipes.com/recipe/"+str(id) for id in ids]

pages = []

for url in urls[3300:3400]:

    time.sleep(5)

    recipe = requests.get(url, headers={'user-agent': random.choice(user_agent_list)})
    soup = BeautifulSoup(recipe.text, 'lxml')

    if soup.find(content="Allrecipes - Server Error"):
        continue
    
    elif soup.find(content="Allrecipes - File Not Found"):
        continue

    elif soup.find("meta").attrs['content']=='Johnsonville® Three Cheese Italian Style Chicken Sausage Skillet Pizza Recipe':
        continue

    else:
        pages.append(soup)

rows_list = []

for page in pages:
    # get basic variables
    dish_name = page.find("h1", itemprop="name").text

    rating = float(page.find(property="og:rating").attrs['content'])

    review_count = int(page.find(itemprop="reviewCount").attrs['content'])

    submitter = page.find(class_="submitter__name").text

    made = page.find(class_="total-made-it").attrs['data-ng-init']
    made_it = int(re.sub("\D","",made))

    servings = int(page.find(id="metaRecipeServings").attrs['content'])

    if page.find(class_="calorie-count")!=None:
        cals = page.find(class_="calorie-count").text
        calories = int(re.sub("\D","",cals))
    else:
        calories = None

    if page.find(itemprop="prepTime")!=None:
        prep_time = page.find(itemprop="prepTime").text
    else:
        prep_time = None

    if page.find(itemprop="cookTime")!=None:
        cook_time = page.find(itemprop="cookTime").text
    else:
        cook_time = None

    if page.find(class_="ready-in-time")!=None:
        total_time = page.find(class_="ready-in-time").text
    else:
        total_time = None

    # Get list of ingredients
    ingredients = []

    for ingredient in page.find_all(itemprop="ingredients"):
        ingredients.append(ingredient.text)

    # Get directions
    directions = []

    for direction in page.find_all(class_="recipe-directions__list--item"):
        directions.append(direction.text)

    # store row as dictionary
    newdict = {'DishName':dish_name,
                'Rating': rating,
                'ReviewCount': review_count,
                'Submitter': submitter,
                'MadeIt': made_it,
                'Servings': servings,
                'Calories': calories,
                'PrepTime': prep_time,
                'CookTime': cook_time,
                'Totaltime': total_time,
                'Ingredients': ingredients,
                'Directions': directions}

    rows_list.append(newdict)


# create dataframe from rows_list
column_headers = ['DishName', 'Rating', 'ReviewCount', 'Submitter', 'MadeIt',
                  'Servings', 'Calories', 'PrepTime', 'CookTime', 'Totaltime',
                  'Ingredients', 'Directions']

recipes_df = pd.DataFrame(rows_list, columns=column_headers)

# write to file
file = '/Users/sally/Projects/recipes/recipes.csv'

if os.path.isfile(file):
    outfile = open(file, 'a')
    recipes_df.to_csv(outfile, index=False, encoding='utf-8', header=False)
    outfile.close()

else:
    outfile = open(file, 'w')
    recipes_df.to_csv(outfile, index=False, encoding='utf-8')
    outfile.close()
