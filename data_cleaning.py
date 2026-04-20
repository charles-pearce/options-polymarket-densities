"""
data_cleaning.py

Organises options_data/ and polymarket_data/ into a unified data/ directory.

Output structure:
    data/{TICKER}/{TICKER}_{YYYY-MM-DD}_td.csv   -- ThetaData options (EOD)
    data/{TICKER}/{TICKER}_{YYYY-MM-DD}_pm.csv   -- Polymarket weekly market (csv → csv)

Only dates present in BOTH sources are kept for each stock.
Weeks with fewer than 5 trading days (holidays / half-days) are excluded.
"""

import re
import shutil
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
ROOT = Path(__file__).parent
OPTIONS_DIR = ROOT / "options_data"      # {TICKER}_theta_data/ subfolders
PM_DIR = ROOT / "polymarket_data"        # {ticker}_weekly_market/ subfolders
OUT_DIR = ROOT / "data"

# Month name → zero-padded month number
MONTH_MAP = {
    "january": "01", "february": "02", "march": "03", "april": "04",
    "may": "05", "june": "06", "july": "07", "august": "08",
    "september": "09", "october": "10", "november": "11", "december": "12",
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def parse_theta_files(ticker: str) -> dict[str, Path]:
    """
    Scan options_data/{TICKER}_theta_data/ for CSV files.
    Returns {date_str: path} where date_str is 'YYYY-MM-DD'.
    """
    folder = OPTIONS_DIR / f"{ticker}"
    if not folder.exists():
        return {}
    result = {}
    for f in folder.glob("*.csv"):
        # e.g. AAPL_2025-11-21_theta_eod.csv
        m = re.search(r"(\d{4}-\d{2}-\d{2})", f.name)
        if m:
            result[m.group(1)] = f
    return result


def parse_pm_date(filename: str) -> str | None:
    """
    Parse a Polymarket filename like 'aapl-week-november-21-2025.csv'
    into 'YYYY-MM-DD'.
    """
    # pattern: {ticker}-week-{month}-{day}-{year}.csv
    m = re.search(
        r"-week-([a-z]+)-(\d{1,2})-(\d{4})",
        filename.lower(),
    )
    if not m:
        print("no pm files")
        return None
    month_name, day, year = m.group(1), m.group(2), m.group(3)
    month = MONTH_MAP.get(month_name)
    if not month:
        return None
    print(f"{year}-{month}-{int(day):02d}")
    return f"{year}-{month}-{int(day):02d}"


def parse_pm_files(ticker: str) -> dict[str, Path]:
    """
    Scan polymarket_data/{ticker}_weekly_market/ for csv files.
    Returns {date_str: path} where date_str is 'YYYY-MM-DD'.
    """
    print(f"ticker: {ticker}")
    print(f"pm dir: {PM_DIR}")
    folder = PM_DIR / f"{ticker.lower()}_weekly_market"
    print(folder)
    if not folder.exists():
        return {}
    result = {}
    for f in folder.glob("*.csv"):
        date = parse_pm_date(f.name)

        if date:
            result[date] = f
    return result


def has_full_week(td_path: Path) -> bool:
    """Return True if the TD file contains exactly 5 observation days."""
    df = pd.read_csv(td_path, usecols=["Date"])
    return df["Date"].nunique() == 5


def rename_csv(src: Path, dst: Path) -> None:
    """
    Rename/move a csv file to a new directory
    :param src: The current file name/location
    :param dst: The new file name/location
    :return: None
    """
    Path(src).rename(dst)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    # Discover stocks present in options_data/
    theta_stocks = sorted(
        str(d.name.replace("_theta_data", ""))
        for d in OPTIONS_DIR.iterdir()
        if d.is_dir() and d.name.endswith("_theta_data")
    )
    print(theta_stocks)
    print(f"Found {len(theta_stocks)} stocks with theta data: {', '.join(theta_stocks)}")

    summary: list[dict] = []

    for ticker in theta_stocks:
        td_files = parse_theta_files(ticker)
        pm_files = parse_pm_files(ticker)

        shared_dates = sorted(set(td_files) & set(pm_files))
        td_only = sorted(set(td_files) - set(pm_files))
        pm_only = sorted(set(pm_files) - set(td_files))

        print(f"\n{ticker}:")
        print(f"  TD dates : {len(td_files)}  PM dates : {len(pm_files)}  "
              f"matched : {len(shared_dates)}")
        if td_only:
            print(f"  TD-only (excluded): {', '.join(td_only)}")
        if pm_only:
            print(f"  PM-only (excluded): {', '.join(pm_only)}")

        if not shared_dates:
            print(f"  No matching dates — skipping {ticker}")
            continue

        stock_dir = OUT_DIR / ticker
        stock_dir.mkdir(parents=True, exist_ok=True)

        holiday_excluded = []
        kept = []

        for date in shared_dates:
            if not has_full_week(td_files[date]):
                holiday_excluded.append(date)
                continue
            kept.append(date)

            # Copy theta CSV
            td_dst = stock_dir / f"{ticker}_{date}_td.csv"
            shutil.copy2(td_files[date], td_dst)

            # Convert polymarket csv → CSV
            pm_dst = stock_dir / f"{ticker}_{date}_pm.csv"
            rename_csv(pm_files[date], pm_dst)

        if holiday_excluded:
            print(f"  Holiday/half-day excluded: {', '.join(holiday_excluded)}")

        summary.append({
            "ticker": ticker,
            "kept": len(kept),
            "td_only_excluded": len(td_only),
            "pm_only_excluded": len(pm_only),
            "holiday_excluded": len(holiday_excluded),
        })

    print("\n" + "=" * 50)
    print("Summary")
    print("=" * 50)
    for row in summary:
        print(
            f"  {row['ticker']:6s}  kept={row['kept']}  "
            f"holiday_excl={row['holiday_excluded']}  "
            f"td_only_excl={row['td_only_excluded']}  pm_only_excl={row['pm_only_excluded']}"
        )
    print(f"\nOutput written to: {OUT_DIR}")


if __name__ == "__main__":
    main()
