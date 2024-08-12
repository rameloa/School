import pandas as pd
import matplotlib.pyplot as plt

# Define the initial parameters
initial_market_size = 1_000_000_000  # $1 billion
growth_rate = 0.05  # 5%
years = 10

# Create a DataFrame to hold the data
data = {'Year': [2024 + i for i in range(years)],
        'Market Size': [initial_market_size * ((1 + growth_rate) ** i) for i in range(years)]}

df = pd.DataFrame(data)

# Plotting the data
plt.figure(figsize=(10, 6))
plt.plot(df['Year'], df['Market Size'] / 1_000_000_000, marker='o')
plt.title('Simulated Golf Market Growth Over 10 Years')
plt.xlabel('Year')
plt.ylabel('Market Size (in billions of $)')
plt.grid(True)
plt.show()