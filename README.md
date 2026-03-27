# Group_5_Econometrics_Project
# **Determinants of Wages in the Manufacturing Sector in the Red River Delta: Evidence from the Labor Force Survey 2018**

## Group Members
| Name | Student ID | Role & Responsibilities | Contribution |
|------|------------|-------------------------|--------------|
| **Nguyen Phuong Ngan** | 11245914 | **Team Leader:**  |  |
| **Do Pham Ha Chi** | 11245851 |  |  |
| **Vu Tran Cat Linh** | 11245899 |  |  |
| **Phung Nhat Minh** | 11245910 |  |  |

## Project Structure
```text
Group_5_Econometrics_Project/
├── data/
│   ├── raw/
│   │   └── data.csv               # Clean data 
│   └── processed/
│       ├── data_model.RData       # Full sample
│       ├── sub1_female.RData      # Subsample 1: Female workers
│       ├── sub2_male.RData        # Subsample 2: Male workers
│       └── data_final.RData       # Output of 02_eda
│
├── scripts/
│   ├── 01_variable_coding.R       # Read data, create & encode all variables
│   ├── 02_descriptive_stats.R     # Table 1 & 2: descriptive statistics
│   ├── 03_distribution.R          # Histogram, QQ-plot, outlier detection
│   └── 04_correlation_vif.R       # Correlation matric, VIF check
│
├── figures/                       # All plots saved here (.png)
│   ├── histogram_wage.png
│   ├── correlation_matrix.png
│   └── residual_plots.png
│
├── tables/                        # All tables saved here (.csv)
│   ├── descriptive_stats.docx
│   └── regression_results.docx
│
├── Report.Rmd
│
└── REAME.md
```
## Setup project
### Step 1: Clone the repository
```bash
git clone https://github.com/phngan23 Group_5_Econometrics_Project
```

### Step 2: Open as R Project
In RStudio: File → Open Project → browse to Group_5_Econometrics_Project.Rproj

Note: Alwayls open RStudio via the `.Rproj` file, NOT by double-clicking `.R` files directly. This ensures all file paths like `data/raw/data.csv` work correctly.

### Step 3: Install required packages
Run this ONCE in the RStudio Console:
```bash
install.packages(c("tidyverse", "haven", "corrplot", "car", "lmtest", "sandwich", "stargazer"))
```

### How to Load Data
Variables created in one R sesssion do NOT carry over to the next sesson or a new script. Always start each script by loading the `.RData` file - this instantly restores all variables.

Template copy this to the top of every new script:
```bash
# ── Packages ──────────────────────────────────
library(tidyverse)

# ── Load data ─────────────────────────────────
load("data/processed/data_model.RData")   # full sample
load("data/processed/sub1_female.RData")  # female subsample
load("data/processed/sub2_male.RData")    # male subsample

# ── Quick check ───────────────────────────────
nrow(data_model)    # should be 16,470
nrow(sub1_female)   # should be 9,426
nrow(sub2_male)     # should be 7,044
```

## Variable Reference
| Variable | Type | Source | Description |
|----------|------|--------|-------------|
| `ln(wage)` | Continuour | C44A  | Log of monthly wage — dependent variable |
| educ | Ordinal (1–9) | C17 | Educational attainment level |
| age | Continuous | C5 | Age in years |
| age2 | Continuous | C5^2 | Age squared - captures diminishing returns |
| hours | Continuous | C46A | Usual weekly working hours |
| male | Dummy | C3 | 1 = Male, 0 = Female |
| cert | Dymmy | C19 | 1 = Has vocational certificate, 0 = No |
| contract | Dymmy | C36 | 1 = Indefinite-term contract, 0 = Other |
| state | Dymmy | C31 | 1 = State-owned sector, 0 = Other |
| fdi | Dymmy | C31 | 1 = Foreign-invested sector, 0 = Other |
| match | Dymmy | C51 | 1 = Working in trained field, 0 = Other |
| bhxh | Dymmy | C39 | 1 = Has social insurance, 0 = No |
| educxmatch | Continuous | C17xC31 | Interaction: education × match |

Base group for dummy variables:
- **Sector**: domestic private sector (`state=0`, `fdi=0`)
- **Gender**: female workers (`male=0`)
- **Contract**: all non-permanent contracts (`contract=0`)

## Git Workflow - Daily Routine
```bash
# BEFORE starting work — always pull first
git pull

# AFTER finishing work — add, commit, push
git add .
git commit -m "your name: brief description"
git push
```
