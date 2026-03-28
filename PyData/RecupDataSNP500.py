"""
=============================================================
ENRICHISSEMENT DATASET S&P 500 — VERSION 2 ROBUSTE
=============================================================
INSTALLATION :
    pip install fredapi yfinance pandas numpy

USAGE :
    1. Place ce script dans le même dossier que snp500df.csv
    2. Vérifie que ta clé FRED est bien renseignée ci-dessous
    3. Lance : python enrichissement_snp500_v2.py
=============================================================
"""
import ssl
ssl._create_default_https_context = ssl._create_unverified_context
import pandas as pd
import numpy as np
import warnings
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────────────────
# 0. CONFIGURATION — MODIFIE ICI
# ─────────────────────────────────────────────────────────
FRED_API_KEY = "9b32f7f9baf853b3e46e734aa2ceca07"
INPUT_FILE   = "snp500df.csv"
OUTPUT_FILE  = "snp500_enrichi.csv"

# ─────────────────────────────────────────────────────────
# 1. IMPORTS AVEC DIAGNOSTIC
# ─────────────────────────────────────────────────────────
print("=" * 60)
print("ENRICHISSEMENT S&P 500 — DÉMARRAGE")
print("=" * 60)

try:
    from fredapi import Fred
    print("✓ fredapi importé")
except ImportError:
    print("✗ fredapi manquant → lance : pip install fredapi")
    exit()

try:
    import yfinance as yf
    print("✓ yfinance importé")
except ImportError:
    print("✗ yfinance manquant → lance : pip install yfinance")
    exit()

# ─────────────────────────────────────────────────────────
# 2. CONNEXION FRED
# ─────────────────────────────────────────────────────────
print("\n📡 Connexion à FRED...")
try:
    fred = Fred(api_key=FRED_API_KEY)
    # Test de connexion avec une série simple
    test = fred.get_series("FEDFUNDS", observation_start="2020-01-01",
                                       observation_end="2020-06-01")
    print(f"✓ Connexion FRED réussie ({len(test)} points de test récupérés)")
except Exception as e:
    print(f"✗ Erreur connexion FRED : {e}")
    print("  → Vérifie ta clé API sur fred.stlouisfed.org")
    exit()

# ─────────────────────────────────────────────────────────
# 3. CHARGEMENT DU DATASET ORIGINAL
# ─────────────────────────────────────────────────────────
print(f"\n📂 Chargement de {INPUT_FILE}...")
try:
    df = pd.read_csv(INPUT_FILE, sep=";", decimal=",", header=0)
    df.columns = df.columns.str.strip()
    df["Date"] = pd.to_datetime(df["Date"])
    df = df.rename(columns={"Moy": "SNP500_Prix"})
    df = df[["Date", "SNP500_Prix"]].copy()   # on garde uniquement ces 2 colonnes
    df = df.set_index("Date").sort_index()
    print(f"✓ {len(df)} observations ({df.index[0].date()} → {df.index[-1].date()})")
except Exception as e:
    print(f"✗ Erreur chargement CSV : {e}")
    exit()

start = df.index[0]
end   = df.index[-1]

# ─────────────────────────────────────────────────────────
# 4. FONCTION FRED ROBUSTE
# ─────────────────────────────────────────────────────────
resultats = {}   # dictionnaire de suivi des récupérations

def get_fred_serie(series_id, col_name, transform=None):
    """
    Récupère une série FRED, la rééchantillonne en mensuel (début de mois),
    et applique une transformation optionnelle.
    transform = None   → valeur brute
    transform = 'pct'  → variation % mensuelle
    transform = 'diff' → différence absolue mensuelle
    """
    try:
        s = fred.get_series(series_id,
                            observation_start=start,
                            observation_end=end)
        if s.empty:
            raise ValueError("Série vide")

        # Rééchantillonnage mensuel robuste
        s.index = pd.to_datetime(s.index)
        s = s.resample("ME").last()          # fin de mois
        s.index = s.index.to_period("M").to_timestamp("M")
        s.index = s.index + pd.offsets.MonthBegin(0)

        s.name = col_name

        if transform == "pct":
            s = s.pct_change() * 100
        elif transform == "diff":
            s = s.diff()

        n_valid = s.notna().sum()
        resultats[col_name] = f"✓ {n_valid} obs"
        print(f"   ✓ {series_id:<25} → {col_name} ({n_valid} obs valides)")
        return s

    except Exception as e:
        resultats[col_name] = f"✗ ERREUR: {e}"
        print(f"   ✗ {series_id:<25} → ERREUR : {e}")
        return pd.Series(name=col_name, dtype=float)

# ─────────────────────────────────────────────────────────
# 5. RÉCUPÉRATION DES VARIABLES FRED
# ─────────────────────────────────────────────────────────

# ── 5A. MACROÉCONOMIQUE ──────────────────────────────────
print("\n📊 [1/6] Variables Macroéconomiques...")

Fed_Taux            = get_fred_serie("FEDFUNDS",      "Fed_Taux_Directeur")
Fed_Variation       = get_fred_serie("FEDFUNDS",      "Fed_Taux_Variation",        transform="diff")
CPI_Niveau          = get_fred_serie("CPIAUCSL",      "CPI_Niveau")
CPI_Variation       = get_fred_serie("CPIAUCSL",      "CPI_Variation_Pct",         transform="pct")
Chomage             = get_fred_serie("UNRATE",        "Taux_Chomage")
Chomage_Var         = get_fred_serie("UNRATE",        "Chomage_Variation",         transform="diff")
Prod_Indus          = get_fred_serie("INDPRO",        "Production_Industrielle")
Prod_Indus_Pct      = get_fred_serie("INDPRO",        "Production_Indus_Pct",      transform="pct")
Ventes_Detail       = get_fred_serie("RSXFS",         "Ventes_Detail")
Ventes_Detail_Pct   = get_fred_serie("RSXFS",         "Ventes_Detail_Pct",         transform="pct")

# ── 5B. TAUX ET SPREADS ──────────────────────────────────
print("\n📊 [2/6] Taux et Spreads...")

Taux_10ans          = get_fred_serie("DGS10",         "Taux_10ans")
Taux_2ans           = get_fred_serie("DGS2",          "Taux_2ans")
Taux_3mois          = get_fred_serie("TB3MS",         "Taux_3mois")
Spread_10_2         = get_fred_serie("T10Y2Y",        "Spread_10ans_2ans")
Spread_10_3m        = get_fred_serie("T10Y3M",        "Spread_10ans_3mois")
CS_IG               = get_fred_serie("BAMLC0A0CM",    "Credit_Spread_IG")
CS_HY               = get_fred_serie("BAMLH0A0HYM2",  "Credit_Spread_HY")
TED_Spread          = get_fred_serie("TEDRATE",       "TED_Spread")

# ── 5C. RISQUE ET VOLATILITÉ ─────────────────────────────
print("\n📊 [3/6] Risque et Volatilité...")

VIX_Niveau          = get_fred_serie("VIXCLS",        "VIX_Niveau")
VIX_Variation       = get_fred_serie("VIXCLS",        "VIX_Variation",             transform="diff")

# ── 5D. VALORISATION ─────────────────────────────────────
print("\n📊 [4/6] Valorisation...")

CAPE                = get_fred_serie("CAPE",           "CAPE_Shiller")

# ── 5E. SENTIMENT ────────────────────────────────────────
print("\n📊 [5/6] Sentiment...")

Michigan            = get_fred_serie("UMCSENT",       "Sentiment_Michigan")
Michigan_Var        = get_fred_serie("UMCSENT",       "Sentiment_Michigan_Var",    transform="diff")
Conf_Conso          = get_fred_serie("CSCICP03USM665S","Confiance_Consommateurs")

# ── 5F. VARIABLES EXTERNES ───────────────────────────────
print("\n📊 [6/6] Variables Externes...")

Petrole             = get_fred_serie("MCOILWTICO",    "Petrole_WTI")
Petrole_Pct         = get_fred_serie("MCOILWTICO",    "Petrole_WTI_Pct",           transform="pct")
EURUSD              = get_fred_serie("DEXUSEU",       "EURUSD")
EURUSD_Pct          = get_fred_serie("DEXUSEU",       "EURUSD_Pct",                transform="pct")
M2                  = get_fred_serie("M2SL",          "M2_Masse_Monetaire")
M2_Pct              = get_fred_serie("M2SL",          "M2_Variation_Pct",          transform="pct")
Taux_Immo           = get_fred_serie("MORTGAGE30US",  "Taux_Hypothecaire_30ans")

# ─────────────────────────────────────────────────────────
# 6. VOLATILITÉ RÉALISÉE VIA YAHOO FINANCE
# ─────────────────────────────────────────────────────────
print("\n📊 Volatilité réalisée via Yahoo Finance...")
try:
    sp_daily = yf.download(
        "^GSPC",
        start=start - pd.DateOffset(months=1),
        end=end + pd.DateOffset(months=1),
        interval="1d",
        progress=False,
        auto_adjust=True
    )

    # Gestion multi-index yfinance
    if isinstance(sp_daily.columns, pd.MultiIndex):
        sp_daily = sp_daily["Close"]["^GSPC"]
    else:
        sp_daily = sp_daily["Close"]

    sp_daily.index = pd.to_datetime(sp_daily.index)
    daily_ret = sp_daily.pct_change().dropna()

    vol_realisee = (daily_ret
                    .groupby(pd.Grouper(freq="ME"))
                    .std() * np.sqrt(252) * 100)
    vol_realisee.index = vol_realisee.index.to_period("M").to_timestamp("M")
    vol_realisee.index = vol_realisee.index + pd.offsets.MonthBegin(0)
    vol_realisee.name  = "Volatilite_Realisee_Ann"

    n = vol_realisee.notna().sum()
    resultats["Volatilite_Realisee_Ann"] = f"✓ {n} obs"
    print(f"   ✓ Volatilité réalisée calculée ({n} obs)")

except Exception as e:
    vol_realisee = pd.Series(name="Volatilite_Realisee_Ann", dtype=float)
    resultats["Volatilite_Realisee_Ann"] = f"✗ {e}"
    print(f"   ✗ Erreur volatilité réalisée : {e}")


# ─────────────────────────────────────────────────────────
# 7. VARIABLES CALCULÉES DEPUIS LES PRIX S&P 500
# ─────────────────────────────────────────────────────────
print("\n📊 Variables calculées depuis les prix S&P 500...")

snp = df["SNP500_Prix"]

Rendement_Mensuel   = (snp.pct_change() * 100).rename("Rendement_Mensuel_Pct")
Momentum_12_1       = (snp.shift(1).pct_change(periods=11) * 100).rename("Momentum_12_1_Mois")
Momentum_6m         = (snp.pct_change(periods=6) * 100).rename("Momentum_6_Mois")
Momentum_3m         = (snp.pct_change(periods=3) * 100).rename("Momentum_3_Mois")
Momentum_1m         = (snp.pct_change(periods=1) * 100).rename("Momentum_1_Mois")

print(f"   ✓ Rendement mensuel      ({Rendement_Mensuel.notna().sum()} obs)")
print(f"   ✓ Momentum 12-1 mois     ({Momentum_12_1.notna().sum()} obs)")
print(f"   ✓ Momentum 6 mois        ({Momentum_6m.notna().sum()} obs)")
print(f"   ✓ Momentum 3 mois        ({Momentum_3m.notna().sum()} obs)")
print(f"   ✓ Momentum 1 mois        ({Momentum_1m.notna().sum()} obs)")


# ─────────────────────────────────────────────────────────
# 8. ASSEMBLAGE FINAL
# ─────────────────────────────────────────────────────────
print("\n🔧 Assemblage du dataset final...")

toutes_series = [
    # Rendements & Momentum (calculés localement)
    Rendement_Mensuel, Momentum_12_1, Momentum_6m, Momentum_3m, Momentum_1m,
    # Macro
    Fed_Taux, Fed_Variation, CPI_Niveau, CPI_Variation,
    Chomage, Chomage_Var, Prod_Indus, Prod_Indus_Pct,
    Ventes_Detail, Ventes_Detail_Pct,
    # Taux & Spreads
    Taux_10ans, Taux_2ans, Taux_3mois,
    Spread_10_2, Spread_10_3m, CS_IG, CS_HY, TED_Spread,
    # Risque
    VIX_Niveau, VIX_Variation, vol_realisee,
    # Valorisation
    CAPE,
    # Sentiment
    Michigan, Michigan_Var, Conf_Conso,
    # Externes
    Petrole, Petrole_Pct, EURUSD, EURUSD_Pct,
    M2, M2_Pct, Taux_Immo,
]

df_enrichi = df.copy()
for serie in toutes_series:
    if serie is not None and not serie.empty:
        serie_clean = serie.copy()
        serie_clean.index = pd.to_datetime(serie_clean.index)
        df_enrichi = df_enrichi.join(serie_clean, how="left")

# Variables dérivées calculées après fusion
if "CAPE_Shiller" in df_enrichi.columns and "Taux_10ans" in df_enrichi.columns:
    df_enrichi["Earnings_Yield"]      = (1 / df_enrichi["CAPE_Shiller"]) * 100
    df_enrichi["Prime_Risque_Actions"] = df_enrichi["Earnings_Yield"] - df_enrichi["Taux_10ans"]
    print("   ✓ Prime de risque actions calculée")

if "VIX_Niveau" in df_enrichi.columns and "Volatilite_Realisee_Ann" in df_enrichi.columns:
    df_enrichi["Variance_Risk_Premium"] = df_enrichi["VIX_Niveau"] - df_enrichi["Volatilite_Realisee_Ann"]
    print("   ✓ Variance Risk Premium calculée")

if "Taux_10ans" in df_enrichi.columns and "Taux_2ans" in df_enrichi.columns:
    df_enrichi["Spread_Calcule_10_2"] = df_enrichi["Taux_10ans"] - df_enrichi["Taux_2ans"]
    print("   ✓ Spread courbe calculé (backup)")

df_enrichi = df_enrichi.reset_index()

# ─────────────────────────────────────────────────────────
# 9. EXPORT
# ─────────────────────────────────────────────────────────
df_enrichi.to_csv(OUTPUT_FILE, sep=";", decimal=",", index=False, encoding="utf-8-sig")

# ─────────────────────────────────────────────────────────
# 10. RAPPORT FINAL
# ─────────────────────────────────────────────────────────
print("\n" + "=" * 60)
print(f"✅ EXPORT TERMINÉ : {OUTPUT_FILE}")
print(f"   {df_enrichi.shape[0]} lignes × {df_enrichi.shape[1]} colonnes")
print("=" * 60)

print("\n📋 DÉTAIL DES COLONNES :")
print(f"{'#':<4} {'Colonne':<35} {'Obs valides':>12} {'Couverture':>12}")
print("-" * 65)
for i, col in enumerate(df_enrichi.columns, 1):
    n   = df_enrichi[col].notna().sum()
    pct = round(n / len(df_enrichi) * 100)
    bar = "█" * (pct // 10) + "░" * (10 - pct // 10)
    print(f"{i:<4} {col:<35} {n:>8} obs   {bar} {pct:>3}%")

print("\n📋 RÉSUMÉ RÉCUPÉRATION FRED :")
ok  = sum(1 for v in resultats.values() if v.startswith("✓"))
err = sum(1 for v in resultats.values() if v.startswith("✗"))
print(f"   ✓ Succès  : {ok}")
print(f"   ✗ Échecs  : {err}")
if err > 0:
    print("\n   Séries en erreur :")
    for k, v in resultats.items():
        if v.startswith("✗"):
            print(f"   • {k:<35} {v}")