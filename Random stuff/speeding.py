import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from scipy.stats import linregress, f_oneway, spearmanr, zscore
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.cluster import KMeans

import statsmodels.api as sm

from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Image, Table, TableStyle
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib import colors

# =========================
# Load + Clean Data
# =========================

df = pd.read_csv("/home/ajcason/Downloads/Speeding(Sheet1).csv")

df["Name"] = df["Name"].ffill()
df = df.dropna(subset=["Speed", "Speed limit"])

# =========================
# Helper Metrics
# =========================

def regression_metrics(x, y):
    res = linregress(x, y)

    y_pred = res.intercept + res.slope * x
    residuals = y - y_pred

    return {
        "slope": res.slope,
        "intercept": res.intercept,
        "r": res.rvalue,
        "r2": res.rvalue**2,
        "p": res.pvalue,
        "mae": mean_absolute_error(y, y_pred),
        "rmse": np.sqrt(mean_squared_error(y, y_pred)),
        "residuals": residuals,
        "predicted": y_pred
    }

# =========================
# 1. Individual Analysis
# =========================

driver_results = []

for name, group in df.groupby("Name"):
    m = regression_metrics(group["Speed limit"].values, group["Speed"].values)

    driver_results.append({
        "Name": name,
        "Slope": m["slope"],
        "Intercept": m["intercept"],
        "R2": m["r2"],
        "Corr": m["r"],
        "P-value": m["p"],
        "MAE": m["mae"],
        "RMSE": m["rmse"],
        "StdSpeed": group["Speed"].std(),
        "N": len(group)
    })

driver_df = pd.DataFrame(driver_results).sort_values("R2", ascending=False)

# =========================
# 2. Global Model (OLS + Linreg)
# =========================

X = sm.add_constant(df["Speed limit"])
ols_model = sm.OLS(df["Speed"], X).fit()

global_metrics = regression_metrics(df["Speed limit"], df["Speed"])

df["Predicted"] = global_metrics["predicted"]
df["Residual"] = global_metrics["residuals"]

# =========================
# 3. Correlations
# =========================

pearson_corr = df["Speed limit"].corr(df["Speed"])
spearman_corr, spearman_p = spearmanr(df["Speed limit"], df["Speed"])

# =========================
# 4. ANOVA
# =========================

groups = [g["Speed"].values for _, g in df.groupby("Name")]
F, p = f_oneway(*groups)

# =========================
# 5. Outlier Detection
# =========================

df["Speed_z"] = zscore(df["Speed"])
outliers = df[np.abs(df["Speed_z"]) > 3]

# =========================
# 6. Clustering (behavior grouping)
# =========================

cluster_data = df[["Speed", "Speed limit"]]
kmeans = KMeans(n_clusters=3, random_state=0, n_init=10).fit(cluster_data)
df["Cluster"] = kmeans.labels_

# =========================
# 7. Plots (saved)
# =========================

# Regression plot
plt.figure()
plt.scatter(df["Speed limit"], df["Speed"])
x_line = np.linspace(df["Speed limit"].min(), df["Speed limit"].max(), 100)
y_line = global_metrics["intercept"] + global_metrics["slope"] * x_line
plt.plot(x_line, y_line)
plt.title("Global Regression")
plt.xlabel("Speed Limit")
plt.ylabel("Speed")
reg_path = "regression.png"
plt.savefig(reg_path)
plt.close()

# Residual plot
plt.figure()
plt.scatter(df["Predicted"], df["Residual"])
plt.axhline(0)
plt.title("Residuals")
plt.xlabel("Predicted")
plt.ylabel("Residual")
res_path = "residuals.png"
plt.savefig(res_path)
plt.close()

# Histogram
plt.figure()
plt.hist(df["Speed"], bins=12)
plt.title("Speed Distribution")
hist_path = "hist.png"
plt.savefig(hist_path)
plt.close()

# Boxplot per driver
plt.figure(figsize=(10,6))
df.boxplot(column="Speed", by="Name", rot=45)
plt.title("Speed by Driver")
box_path = "boxplot.png"
plt.savefig(box_path)
plt.close()

# =========================
# 8. PDF REPORT
# =========================

pdf_path = "Speeding_Analysis_Report.pdf"
doc = SimpleDocTemplate(pdf_path)
styles = getSampleStyleSheet()
content = []

def add(text, style="Normal"):
    content.append(Paragraph(str(text), styles[style]))
    content.append(Spacer(1, 10))

add("Speeding Data Analysis Report", "Title")

add("Global Regression (OLS Summary)")
add(ols_model.summary().as_text().replace("\n", "<br/>")[:2000])

add(f"Pearson correlation: {pearson_corr:.4f}")
add(f"Spearman correlation: {spearman_corr:.4f} (p={spearman_p:.4g})")

add("ANOVA Results")
add(f"F={F:.4f}, p={p:.6f}")

add("Driver Summary Table")
table_data = [driver_df.columns.tolist()] + driver_df.round(3).values.tolist()
table = Table(table_data)
table.setStyle(TableStyle([
    ("GRID", (0,0), (-1,-1), 0.5, colors.black),
    ("BACKGROUND", (0,0), (-1,0), colors.grey)
]))
content.append(table)
content.append(Spacer(1, 12))

add("Outliers detected")
add(outliers[["Name","Speed","Speed limit","Speed_z"]].to_string(index=False))

add("Regression Plot")
content.append(Image(reg_path, width=400, height=300))
content.append(Spacer(1, 10))

add("Residual Plot")
content.append(Image(res_path, width=400, height=300))

add("Histogram")
content.append(Image(hist_path, width=400, height=300))

add("Boxplot by Driver")
content.append(Image(box_path, width=400, height=300))

doc.build(content)

# =========================
# 9. Console Output (summary)
# =========================

print("Report saved to:", pdf_path)
print("\nTop driver by sensitivity:")
print(driver_df.iloc[0])

print("\nMost consistent driver:")
print(driver_df.sort_values("RMSE").iloc[0])
