# Land & Sea Planning Unit Reconcilliation (LaSPUR)
-----
LaSPUR provides a framework and set of tools to help stakeholders negotiate and resolve conflicts between land and sea spatial planning. It employs an evidence-based approach to ensure that spatial management is informed by the careful consideration of ecosystem protection, economic value, and the continuity of community living spaces.

## What does LaSPUR offer?
LaSPUR provides a tool designed to help resolve conflicts in the integration of land and sea spatial planning, categorized into three components:
- Type 1: Overlapping Areas Areas where Land Spatial Plans (RTRW) and Sea Spatial Plans (RZWP3K) occupy the same geographical space.
- Type 2: Adjacent Areas Areas where land and sea planning boundaries directly meet or border one another.
- Type 3: Interdependent Linkage Areas Areas that are not directly adjacent but are linked by ecological or functional processes, where activities in one space potentially impact the other.

## How to use LaSPUR?
LaSPUR is currently implemented as an interactive Quarto Notebook. Follow the steps below to run the module.

### Step 1: Prerequisites
Ensure you have the following installed on your system:
  - R (v4.0 or higher)
  - RStudio 
  - Quarto CLI (usually bundled with modern RStudio versions).

### Step 2: Installation & Setup
Clone the repository to your local machine via terminal or Git Bash:

Run the following command:
```
# Clone the repository
git clone https://github.com/icraf-indonesia/LaSPUR.git

# Navigate into the project directory
cd LaSPUR
```

Open the `LaSPUR.Rproj` file in RStudio and install the required libraries by running this command in the R console:
```
install.packages(c("quarto", "terra", "sf", "openxlsx", "tibble", 
                   "dplyr", "landscapemetrics", "purrr", "tidyr", "stringr"))
```
### Step 3: Running the Analysis
The core analytical logic is contained within the `LaSPUR_type1.qmd` file.

- Open the Notebook: Locate and open `LaSPUR_type1.qmd` in your R editor.
- Configure Data Paths: Update the data section in the first code chunk to point to your local RTRW (Land) and RZWP3K (Sea) spatial datasets. You can download the [Example Dataset here](https://drive.google.com/drive/folders/1T8qlp-in1LfJU-07PjNwfJUgcTvCfi_H?usp=sharing) to test the module.
- Execute Chunks: Run the code blocks sequentially to process the reconciliation.
