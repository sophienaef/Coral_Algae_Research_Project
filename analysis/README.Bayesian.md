# Current Steps

A. Finding approximations for SST for the locations we have

B. Ensuring location points are sampled equally enough - "proven" how? something about transition testing?

loop x1000 (

C. Rarefaction of data - using R?

D. Bayseian Blocks (includes fitness function) - Multi-Clade Switching Probability code

)
---

Bayesian Blocks to detect "regime shifts" or sharp transitions in coral symbiont data. Instead of using a sliding window (which often smears results), Bayesian Blocks identifies the optimal locations for "breakpoints" where the species composition changes significantly along the temperature gradient.

### 1. Why Bayesian Blocks?

The Bayesian Block algorithm (originally from Scargle et al.) finds the optimal partition of data by maximizing a fitness function. In the coral study:

The X-axis: Temperature gradient (independent variable)
The Y-axis: Presence/absence of specific symbiont clade
The Goal: To find the temperature T_{crit} where the probability of finding Species A drops and Species B rises.

### 2. Data

 # A. Rarefaction accounting for the smallest sampling size in a location
   -- Pick up randomly a number of samplings in each location as in the smallest sampling size for a specific coral sp.
   -- Pull together all the sites for a specific coral sp.
   -- Do it at least 1000 times

 # B. A time-series style format, even though your "time" is actually Temperature

Column A Temperature (sorted from lowest to highest - or proxy
Column B Binary (0 or 1) for a specific clade


### 3. Implementation in Python (using `astropy`)

The `astropy` library contains the most robust implementation of the Bayesian Blocks algorithm.

```python
import numpy as np
import matplotlib.pyplot as plt
from astropy.stats import bayesian_blocks

# 1. Load your data
# temp = your temperature array
# species_presence = your 0/1 or count data

# 2. Compute the optimal bins (the "Blocks")
# p0 is the prior on the number of bins. 
# Smaller p0 = fewer, more significant blocks.
edges = bayesian_blocks(temp, species_presence, fitness='events', p0=0.01)

# 3. Visualize the transitions
plt.hist(temp, bins=edges, weights=species_presence, 
         histtype='stepfilled', alpha=0.2, label='Symbiont presence')
plt.xlabel('Temperature (°C)')
plt.ylabel('Symbiont Presence')
plt.show()

```

### 4. Interpreting the Transition

The `edges` array returned by the function represents the transition points. Sharp Shifts: If a block edge appears at 29.5°C, it suggests a biological threshold where the symbiotic clades undergo a non-linear shift.


### 5. Connecting GPA to these Blocks

Since your previous work links **Genotype-to-Phenotype Architecture (GPA)** to biodiversity:

* You can use Bayesian Blocks to see if **Modular architectures** show more "stair-step" transitions (distinct blocks) along the gradient compared to **Correlational architectures**, which might show more gradual, noisy transitions.
* The likelihood method you developed can be applied *within* each Bayesian block to see if the underlying genetic architecture changes before the species actually disappears (an early warning signal).


### Summary Checklist

1. Standardize: Ensure your temperature points are sampled equally enough across locations to justify a transition test.
2. Fitness Function: Use `fitness='events'` for presence/absence data or `fitness='measures'` for continuous abundance data.
3. Validation: Use the Rarefaction samplings approach -- re-run the Bayesian blocks on your data multiple times to see if the transition temperatures remain stable.



Example to calculate the switching probability between three symbiotic clades. We treat each Bayesian Block as a discrete "thermal state." We then look at the proportion of each clade within those blocks to see how the dominance shifts.

Here a Python implementation that iterates through the blocks and calculates the relative probability for each clade.

### Python Code: Multi-Clade Switching Probability

```python
import numpy as np
import pandas as pd
from astropy.stats import bayesian_blocks

# 1. Setup Data
# Assuming 'temp' is your sorted temperature gradient
# 'clade_labels' is an array of 0, 1, or 2 representing the three clades
data = pd.DataFrame({
    'temp': temp,
    'clade': clade_labels
})

# 2. Define the Blocks (using a global fitness to find common transition points)
# We find edges based on the overall "event" of a sample being taken
edges = bayesian_blocks(data['temp'], p0=0.01)

# 3. Calculate Switching Probabilities
results = []

for i in range(len(edges)-1):
    t_start, t_end = edges[i], edges[i+1]
    
    # Filter data within this specific thermal block
    block_data = data[(data['temp'] >= t_start) & (data['temp'] < t_end)]
    
    # Count occurrences of each clade
    counts = block_data['clade'].value_counts(normalize=True).to_dict()
    
    # Ensure all 3 clades are represented (even if 0%)
    prob_clade_0 = counts.get(0, 0.0)
    prob_clade_1 = counts.get(1, 0.0)
    prob_clade_2 = counts.get(2, 0.0)
    
    results.append({
        'block_start': t_start,
        'block_end': t_end,
        'prob_C1': prob_clade_0,
        'prob_C2': prob_clade_1,
        'prob_C3': prob_clade_2
    })

# Convert to DataFrame for analysis
switching_df = pd.DataFrame(results)
print(switching_df)

```

### Patterns

Output gives you a "Transition Matrix" across the gradient.

* Dominance Shift: You will likely see Clade 1 (e.g., heat-sensitive) have a probability of 0.9 in the low-temperature blocks, dropping to 0.1 in the high-temperature blocks.
* The "Switch" Point: The block where the probability crosses 0.5 for a new clade is your critical thermal threshold (T_{crit}).





