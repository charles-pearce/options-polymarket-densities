import httpx
import csv
import io
import os
from datetime import datetime, timedelta, date
import pandas as pd
import yfinance as yf
import warnings
warnings.filterwarnings('ignore')

# Base URL always the same
# Make sure ThetaTerminal is running
# Change SYMBOL as needed
# Will create one CSV per expiration, containing all trading days in the week leading up to it
BASE_URL = "http://127.0.0.1:25503/v3"
SYMBOL = 'TSLA' 
OUTPUT_DIR = f"{SYMBOL}_theta_data"
os.makedirs(OUTPUT_DIR, exist_ok=True)

COLUMNS = [
    "symbol", "expiration", "strike", "right",
    "created", "last_trade",
    "open", "high", "low", "close",
    "volume", "count",
    "bid_size", "bid_exchange", "bid", "bid_condition",
    "ask_size", "ask_exchange", "ask", "ask_condition",
]

# Generate all weekly Friday expirations from 2025-11-21 through 2026-03-27
first_exp = date(2025, 11, 21)
last_exp  = date(2026, 3, 27)
expirations = []
d = first_exp
while d <= last_exp:
    expirations.append(d)
    d += timedelta(weeks=1)

print(f"Processing {len(expirations)} weekly expirations for {SYMBOL}")

for expiration in expirations:
    exp_str   = expiration.strftime('%Y-%m-%d')
    # Monday of the expiration week
    week_start = expiration - timedelta(days=expiration.weekday())
    start_str  = week_start.strftime('%Y-%m-%d')

    output = os.path.join(OUTPUT_DIR, f"{SYMBOL}_{exp_str}_theta_eod.csv")
    print(f"Expiration {exp_str}")

    raw_params = {
        'start_date': start_str,
        'end_date': exp_str,
        'symbol': SYMBOL,
        'expiration': exp_str,
    }

    # Collect trading days in the week
    dates_to_run = []
    cur = week_start
    while cur <= expiration:
        if cur.weekday() < 5:
            dates_to_run.append(cur)
        cur += timedelta(days=1)

    print("Dates:", [d.strftime("%Y-%m-%d (%A)") for d in dates_to_run])

    all_rows = []
    for day in dates_to_run:
        day_str = day.strftime("%Y%m%d")
        params = dict(raw_params)
        params['start_date'] = day_str
        params['end_date']   = day_str
        url = BASE_URL + '/option/history/eod'

        try:
            with httpx.stream("GET", url, params=params, timeout=60) as response:
                if response.status_code >= 400:
                    print(f"Skipping {day.strftime('%Y-%m-%d')} (HTTP {response.status_code})")
                    continue
                for line in response.iter_lines():
                    for row in csv.reader(io.StringIO(line)):
                        if row and row[0] != 'symbol':
                            all_rows.append(row)
        except httpx.HTTPStatusError as e:
            print(f"Skipping {day.strftime('%Y-%m-%d')} ({e.response.status_code})")
            continue

    if not all_rows:
        print(f"No data returned — skipping {output}")
        continue

    df = pd.DataFrame(all_rows, columns=COLUMNS)

    # Type conversions
    df["strike"]     = df["strike"].astype(float)
    df["bid"]        = pd.to_numeric(df["bid"], errors="coerce")
    df["ask"]        = pd.to_numeric(df["ask"], errors="coerce")
    df["close"]      = pd.to_numeric(df["close"], errors="coerce")
    df["volume"]     = pd.to_numeric(df["volume"], errors="coerce")
    df["Expiration"] = pd.to_datetime(df["expiration"]).dt.date
    df["Date"]       = pd.to_datetime(df["created"]).dt.date

    # Rename to match schema
    df = df.rename(columns={
        "symbol": "Underlying",
        "right":  "OptionType",
        "strike": "Strike",
        "open":   "Open",
        "high":   "High",
        "low":    "Low",
        "close":  "Close",
        "volume": "Volume",
        "count":  "Transactions",
        "bid":    "Bid",
        "ask":    "Ask",
    })

    # Mid price
    df["Mid"] = (df["Bid"] + df["Ask"]) / 2

    # DTE and Tau
    df["DTE"] = (pd.to_datetime(df["Expiration"]) - pd.to_datetime(df["Date"])).dt.days
    df["Tau"] = df["DTE"] / 365.0

    # Spot price from yfinance
    min_date = pd.to_datetime(df["Date"].min())
    max_date = pd.to_datetime(df["Date"].max()) + pd.Timedelta(days=1)

    print("Fetching spot prices...")
    spot_raw = yf.download(SYMBOL, start=min_date, end=max_date, auto_adjust=True, progress=False)
    spot_raw.columns     = spot_raw.columns.get_level_values(0)
    spot_raw.index       = pd.to_datetime(spot_raw.index).date
    spot_raw.index.name  = "Date"
    spot_df = spot_raw[["Close"]].rename(columns={"Close": "SpotClose"}).reset_index()

    df = pd.merge(df, spot_df, on="Date", how="left")

    # Risk-free rate from yfinance (^IRX = 13-week T-bill)
    print("  Fetching risk-free rate...")
    tbill_raw = yf.download("^IRX", start=min_date, end=max_date, auto_adjust=True, progress=False)
    tbill_raw.columns    = tbill_raw.columns.get_level_values(0)
    tbill_raw.index      = pd.to_datetime(tbill_raw.index).date
    tbill_raw.index.name = "Date"
    rate_df = tbill_raw[["Close"]].rename(columns={"Close": "Rate"}).reset_index()
    rate_df["Rate"] = rate_df["Rate"] / 100.0

    df = pd.merge(df, rate_df, on="Date", how="left")
    df["Rate"] = df["Rate"].ffill().bfill()

    col_order = [
        "Date", "Expiration", "Underlying", "OptionType", "Strike",
        "Open", "High", "Low", "Close", "Volume", "Transactions",
        "Mid", "Bid", "Ask", "SpotClose", "Rate", "Tau", "DTE",
    ]
    df = df[col_order].sort_values(["Date", "OptionType", "Strike"]).reset_index(drop=True)

    df.to_csv(output, index=False)
    print(f"Saved {output}  ({len(df)} rows)")

print("Done for all expirations")