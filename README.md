# modelling-max-ev

## Data Collection
All data obtained from Baseball Savant's Statcast Search tool at baseball_savant.com/statcast_search. Query parameters:
- Pitch result: In play
- Player type: Batter
- Season type: Regular Season

Separate queries by month to avoid timeouts, and save in form savant_data_MMM_YY.csv. Run data_merging.py to merge the files into one dataset. Then from the root of the repository:
```
modelling-max-ev % python scripts/merge_data.py data 17 24
```