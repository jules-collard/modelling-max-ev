import pandas as pd
import argparse, glob, os

def merge_data(path: str, start: int, end: int) -> pd.DataFrame:
    filenames = []
    for year in range(start, end+1):
        filenames.extend(glob.glob(os.path.join(path, f"savant_data_*_{year}.csv")))

    cols = ["pitch_type", "game_date", "player_name", "batter", "events", "zone", "stand", "p_throws", "balls", "strikes", "launch_speed", "launch_angle", "at_bat_number", "woba_value", "woba_denom"]
    monthly_data = [pd.read_csv(f)[cols].rename(columns={'batter':'batter_id'}) for f in filenames]
    data = pd.concat(monthly_data)
    return data

def save_data(data: pd.DataFrame, path: str, start: int, end: int):
    data.to_csv(os.path.join(path, f"savant_data_20{start}_20{end}.csv"), index=False)

def main():
    parser = argparse.ArgumentParser(description="Merge monthly statcast data queries")
    parser.add_argument("directory", help="Path to directory containing .csv files")
    parser.add_argument("start_year", help="YY (inclusive)")
    parser.add_argument("end_year", help="YY (inclusive)")
    args = parser.parse_args()

    data = merge_data(args.directory, int(args.start_year), int(args.end_year))
    save_data(data, args.directory, int(args.start_year), int(args.end_year))


if __name__ == "__main__":
    main()