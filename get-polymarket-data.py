import datetime
from datetime import timedelta
from fileinput import close

import requests
import websockets
from fontTools.misc.plistlib import end_date
from py_clob_client.client import *
from py_clob_client.clob_types import *
from pygments.lexer import include
from scipy.stats import norm
import pandas as pd
import os
import pandas_market_calendars as mcal

#documentation: https://docs.polymarket.com/trading/orderbook
SESSION = requests.Session()
every_price = []
os.makedirs("prices per stock", exist_ok=True)
for stock in ["aapl", "googl", "nvda", "pltr", "nflx", "meta", "open", "tsla", "msft", "amzn"]:
    every_price_per_stock = []
    folder_name = f"{stock}_weekly_market"
    os.makedirs(folder_name, exist_ok=True)
    date = "march-27-2026"
    date_obj = pd.to_datetime(date)

    while date != "november-14-2025":
        #get token and save it
        slug = f"{stock}-week-{date}"

        r = SESSION.get(f"https://gamma-api.polymarket.com/events/slug/{slug}",
                        params={"limit": 100})
        if r.status_code != 200:
            print(f"error: {stock} {date}")
            # if friday doesn't work try thursday. If that also doesn't work, go to next week
            slug = f"{stock}-week-{(pd.to_datetime(date) - datetime.timedelta(days = 1)).strftime("%B-%d-%Y").lower()}"
            r = requests.get(f"https://gamma-api.polymarket.com/events/slug/{slug}")
            if r.status_code != 200:
                # adjust date for next iteration
                date_obj = pd.to_datetime(date) - datetime.timedelta(days=7)
                date = date_obj.strftime("%B-%#d-%Y").lower()
                continue

        market_info = r.json()

        # print(market_info)
        try:
            ids_yes = {item.get("groupItemTitle"): json.loads(item["clobTokenIds"])[json.loads(item["outcomes"]).index("Yes")] for item in market_info["markets"]}
            ids_no = {json.loads(item["clobTokenIds"])[json.loads(item["outcomes"]).index("Yes")]: json.loads(item["clobTokenIds"])[json.loads(item["outcomes"]).index("No")] for item in market_info["markets"]}
            outcomes_yes = {item.get("groupItemTitle"): int(round(float(json.loads(item["outcomePrices"])[json.loads(item["outcomes"]).index("Yes")]), 1)) for item in market_info["markets"]}

        except Exception as e:
            print(f"skip {date} because of error: {e}")
            date_obj = pd.to_datetime(date) - datetime.timedelta(days=7)
            date = date_obj.strftime("%B-%#d-%Y").lower()
            continue


        all_markets = []

        print(date)

        # get closing time of stock market on end day (end of the polymarket) s.t. we don't get data on prices during dispute window
        nyse = mcal.get_calendar('NYSE')
        schedule = nyse.schedule(start_date=date_obj, end_date=date_obj)
        close_time = schedule.loc[date_obj, 'market_close'].tz_convert('America/New_York')
        # find the timestamp of the start and end of the market (assuming market opens 12 days earlier)
        start = int(pd.Timestamp(date_obj - timedelta(days = 12), tz='America/New_York').timestamp())
        end = int(pd.Timestamp(close_time).timestamp())

        cboe_equity_cal = mcal.get_calendar('CBOE_Equity_Options')

        for k,s in ids_yes.items():
            try:
                response = SESSION.get(
                    "https://clob.polymarket.com/prices-history",
                    params={
                        "market": s,  # despite the name, this takes a token ID
                        "startTs": start,
                        "endTs": end,
                        "fidelity": 1,  # data points every 60 minutes
                    }
                )

                response_no = SESSION.get(
                    "https://clob.polymarket.com/prices-history",
                    params={
                        "market": ids_no[s],  # despite the name, this takes a token ID
                        "startTs": start,
                        "endTs": end,
                        "fidelity": 1,  # data points every 60 minutes
                    }
                )

                # print(f"response: {response.json()}")
                history_yes = response.json()["history"]
                df_yes = pd.DataFrame(history_yes)
                df_yes = df_yes.rename(columns={'t': 'timestamp', 'p': "price_yes"})

                history_no = response_no.json()["history"]
                df_no = pd.DataFrame(history_no)
                df_no = df_no.rename(columns={'t': 'timestamp_no', 'p': "price_no"})

                df = pd.merge_asof(
                    df_yes,
                    df_no,
                    left_on="timestamp", right_on="timestamp_no",
                    direction="nearest", tolerance=60
                )
                df.dropna(subset=["price_yes"])

                df["outcome_yes"] = outcomes_yes.get(k)

                df["bracket"] = k.replace("$", "")
                df["market"] = slug
                df["enddate"] = date_obj.strftime("%d/%m/%Y")

                # 3. Convert the Unix timestamp to a readable date
                time = pd.to_datetime(df['timestamp'], unit='s', utc=True)

                # get days and seconds to expiration
                # end_date_et = date_obj.tz_localize('UTC').tz_convert('US/Eastern')
                df["DTE"] = (date_obj.date() - time.dt.date).apply(lambda x: x.days)

                df["STE"] = end- df["timestamp"]

                #Convert from UTC to US/Eastern
                df['time_et'] = time.dt.tz_convert('US/Eastern').dt.tz_localize(None)
                df = df.sort_values('time_et')


                chicago_time = df["time_et"].dt.tz_localize('US/Eastern').dt.tz_convert('America/Chicago')

                schedule = cboe_equity_cal.schedule(
                    start_date=chicago_time.min().date(),
                    end_date=chicago_time.max().date()
                )

                schedule_lookup = {
                    date.date(): (row['market_open'], row['market_close']) for date, row in schedule.iterrows()
                }
                # print(schedule_lookup)


                def check_open(ts):
                    day = ts.date()
                    if day not in schedule_lookup:
                        return False
                    market_open, market_close = schedule_lookup[day]
                    return market_open <= ts <= market_close

                # Vectorized membership check
                df['market_open'] = chicago_time.apply(check_open)

                # sort columns
                df = df[["time_et", "timestamp", "price_yes", "price_no", "DTE", "STE", "bracket", "market", "enddate",
                         "market_open", "outcome_yes"]]

                every_price_per_stock.append(df)

                #select all days for which there is data
                all_days = df['time_et'].dt.date.unique()
                targets = pd.DataFrame({'target_time': pd.to_datetime(all_days)})
                #set target time to 16:00
                targets['target_time'] = targets['target_time'] + pd.Timedelta(hours=16)
                targets = targets.sort_values('target_time')


                #find the closest time to 4pm on each day (can be at most 10 minutes away from 4pm)
                final_df = pd.merge_asof(
                    targets,
                    df,
                    left_on='target_time',
                    right_on='time_et',
                    direction='nearest', #could also do "backward" if we only want data before 4pm (only makes a few seconds difference)
                    tolerance=pd.Timedelta(minutes=10)
                )
                # remove days, where no data was close enough to 4pm
                final_df = final_df.dropna(subset=["price_yes"])

                all_markets.append(final_df)
            except Exception as e:
                print(f"error: {e} in make df for {stock} {date}")
                continue



        df = pd.concat(all_markets)
        df.to_csv(f"{folder_name}\\{slug}.csv", index = False)

        # adjust date for next iteration
        date_obj = pd.to_datetime(date) - datetime.timedelta(days=7)
        date = date_obj.strftime("%B-%#d-%Y").lower()

    stock_df = pd.concat(every_price_per_stock)
    every_price.append(stock_df)
    stock_df.to_csv(f"prices per stock\\{stock}_all_prices.csv", index = False)

every_price_df = pd.concat(every_price)
every_price_df.to_parquet(f"all_prices.parquet", index = False)



