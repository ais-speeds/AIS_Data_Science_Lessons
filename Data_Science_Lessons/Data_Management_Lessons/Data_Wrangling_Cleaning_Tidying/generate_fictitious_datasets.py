# Import necessary libraries
import pandas as pd
import numpy as np
import random

# Set the number of athletes to be generated in each dataset
number_of_athletes = 7

# Function to generate random dates within a specified range
def random_dates(start, end, n=number_of_athletes):
    start_u = start.value // 10**9
    end_u = end.value // 10**9
    return pd.to_datetime(np.random.randint(start_u, end_u, n), unit='s').strftime('%d-%m-%Y')

# Function to generate random names, both male and female
def generate_name(gender):
    # List of possible first male and female names
    first_names_male = ["James", "John", "Robert", "Michael", "William"]
    first_names_female = ["Mary", "Kate", "Jennifer", "Tiffany", "Elizabeth"]
    # List of possible last names
    last_names = ["Smith", "Johnson", "Williams", "White", "Jones"]
    # Randomly select a first and last name based on gender
    first_name = random.choice(first_names_male if gender == 'M' else first_names_female)
    last_name = random.choice(last_names)
    # Combine first and last name
    return f"{first_name}_{last_name}"

# Function to randomize column names with varying formatting
def randomize_column_name(name):
    # List of functions for varying string formatting (upper, lower, etc)
    transformations = [str.upper, str.lower, str.title]
    # Split the column name if it contains an underscore or space and randomize its formatting
    if "_" in name or " " in name:
        name_parts = name.split("_")
        separator = random.choice([" ", "_"])  # Choose randomly between space and underscore
        name = separator.join(random.choice(transformations)(part) for part in name_parts)
    else:
        # Apply one of the string formatting functions to the name at random
        name = random.choice(transformations)(name)
    # Randomly decide whether to add leading and trailing spaces to the name
    if random.random() < 0.8:
        name = "   " + name + "   "
    return name

# Function to generate randomized datasets
def generate_dataset(file_num):
    # Set a random sedd for reproducibility
    np.random.seed(file_num)
    # Randomly assign genders and ages to athletes
    genders = np.random.choice(['M', 'F'], number_of_athletes)
    ages = np.random.randint(18, 40, number_of_athletes)
    # Randomly select cycling teams and competition cetegories
    cycling_teams = np.random.choice(["Queensland", "Victoria", "Tasmania"], number_of_athletes)
    cycling_categories = np.random.choice(["Road", "Mountain", "Track"], number_of_athletes)
    # Combine team and category names
    team_category = [f"{team}_{category}" for team, category in zip(cycling_teams, cycling_categories)]
    
    # Dictionary mapping randomized column names to randomly generated data
    data = {
        'Race_Date': random_dates(pd.to_datetime('2023-01-01'), pd.to_datetime('2023-12-31'), number_of_athletes),
        'Team': team_category,
        'Participant': [generate_name(genders[i]) for i in range(number_of_athletes)],
        'Gender_Age': [f'{genders[i]}{ages[i]}' for i in range(number_of_athletes)],
        'Heart_Rate': np.random.randint(60, 180, number_of_athletes).astype(object),
        'Distance_km': np.round(np.random.uniform(30, 100, number_of_athletes), 2).astype(object),
        'VO2_Max': [f"{np.random.randint(30, 80)} mL/kg/min" for _ in range(number_of_athletes)],
    }
    
    # Create a Dataframe from the data dictionary
    df = pd.DataFrame(data)
    
    # Generate sequential IDs for each row, starting from an offset based on the file number
    df['ID'] = range((file_num - 1) * number_of_athletes + 1, file_num * number_of_athletes + 1)
    # Make ID the first column in the DataFrame
    df = df[['ID'] + [col for col in df.columns if col != 'ID']]
    
    # Define columns where missing values can be introduced, excluding 'ID' and 'Participant'
    potential_columns_for_missing_values = [col for col in df.columns if col not in ['ID', 'Participant']]
    
    # Introduce missing values in the specified columns at random; set the frequency of missing values
    for col in potential_columns_for_missing_values:
        df.loc[df.sample(frac=0.15).index, col] = np.random.choice([None, 'NA'])
    
    # Randomly replace existing rows with duplicates, avoiding adjacent positions
    num_duplicates = random.randint(1, 1)
    positions = list(range(len(df)))
    
    random.shuffle(positions)
    duplicate_positions = positions[:num_duplicates]

    for replace_position in duplicate_positions:
        # Select a random row to duplicate
        duplicate_row = df.sample(n=1, replace=True).copy()
        # Update the ID of the duplicate row to match the replace position
        duplicate_row['ID'] = replace_position + 1 + ((file_num - 1) * number_of_athletes)
        # Replace the row at the chosen position with the duplicate
        df.iloc[replace_position] = duplicate_row.iloc[0]

    # Apply randomized column names and save the DataFrame to a csv file
    df.rename(columns=consistent_column_names, inplace=True)
    df.to_csv(f'cycling_untidy_fictional_data_{file_num}.csv', index=False)

    return df

# Generate a consistent set of randomized column names for each dataset
def random_column_names_generator():
    original_names = ['Race_Date', 'Team', 'Participant', 'Gender_Age',
                      'Heart_Rate', 'Distance_km', 'VO2_Max']
    return {name: randomize_column_name(name) for name in original_names}

consistent_column_names = random_column_names_generator()

# Generate datasets
datasets = [generate_dataset(i) for i in range(1, 6)]
print(datasets[0])