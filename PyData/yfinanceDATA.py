import yfinance as yf
import pandas as pd
from datetime import date

ACTIFS = {
    "SNP500": "^GSPC",
    "CAC40":  "^FCHI",
    "Or":     "GC=F",
    "BTC":    "BTC-USD",
}

END = date.today().strftime("%Y-%m-%d")

def get_data(actifs_dict, end):
    frames = []
    for nom, ticker in actifs_dict.items():
        try:
            data = yf.download(ticker, start="1900-01-01", end=end,
                               interval="1mo", auto_adjust=True, progress=False)
            if data.empty:
                print(f"[!] {nom} — aucune donnée")
                continue
            data = data.copy()
            data.columns = [col[0] if isinstance(col, tuple) else col for col in data.columns]
            df = pd.DataFrame()
            df["Date"]              = data.index.strftime("%Y-%m-%d")
            df["Actif"]             = nom
            df["Prix_Cloture"]      = data["Close"].values
            df["Prix_Ouverture"]    = data["Open"].values
            df["Prix_Haut"]         = data["High"].values
            df["Prix_Bas"]          = data["Low"].values
            df["Volume"]            = data["Volume"].values
            df["Rendement_Pct"]     = data["Close"].pct_change().values * 100
            df["Prix_Cloture_Norm"] = (data["Close"] / data["Close"].iloc[0] * 100).values
            frames.append(df)
            print(f"[OK] {nom} — {len(df)} mois | {df['Date'].iloc[0]} → {df['Date'].iloc[-1]}")
        except Exception as e:
            print(f"[ERR] {nom} : {e}")
    return pd.concat(frames, ignore_index=True).sort_values(["Actif", "Date"]).reset_index(drop=True)

df = get_data(ACTIFS, END)
df.to_csv("actifs_mensuels.csv", index=False)
print(f"\nShape : {df.shape}")
print(df.head(8).to_string())