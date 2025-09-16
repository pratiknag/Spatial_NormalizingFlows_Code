#!/bin/bash

echo "🔍 Checking environment..."

# --- Check if Python3 is installed ---
if ! command -v python3 &> /dev/null; then
    echo "❌ Python3 is not installed. Please install Python 3 before proceeding."
    exit 1
fi

# --- Check if pip is installed ---
if ! command -v pip3 &> /dev/null; then
    echo "⚠️ pip is not found. Attempting to install pip..."
    curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
    python3 get-pip.py || { echo "❌ Failed to install pip. Please install manually."; exit 1; }
    rm get-pip.py
fi

# --- Check if virtualenv is installed ---
if ! python3 -m virtualenv --version &> /dev/null; then
    echo "⚠️ virtualenv is not installed. Installing it via pip..."
    python3 -m pip install virtualenv || { echo "❌ Failed to install virtualenv."; exit 1; }
fi

# --- Create and activate virtual environment ---
echo "🔧 Creating virtual environment..."
python3 -m virtualenv env || { echo "❌ Failed to create virtual environment."; exit 1; }
python3 -m virtualenv TFv2 || { echo "❌ Failed to create virtual environment."; exit 1; }
#source env/bin/activate

# --- Install Python dependencies for pytorch venv ---
echo "📦 Installing Python dependencies..."
source env/bin/activate
pip install --upgrade pip
pip install -r requirements.txt || { echo "❌ Failed to install Python dependencies."; exit 1; }
deactivate

# --- Install Python dependencies for tensorflow venv ---
echo "📦 Installing Python dependencies..."
source TFv2/bin/activate
pip install --upgrade pip
pip install -r requirements_TFv2.txt || { echo "❌ Failed to install Python dependencies."; exit 1; }
deactivate

# --- Check if R is installed ---
if ! command -v R &> /dev/null; then
    echo "❌ R is not installed. Please install R before proceeding."
    exit 1
fi

# --- Check for R libraries ---
echo "📦 Checking required R packages..."
Rscript -e '
required_packages <- c("reticulate", "ggplot2", "dplyr", "gridExtra", "viridis", "geoR", "MASS", "devtools", "fields", "rjson", "latex2exp", "reshape2", "reticulate", "tensorflow", "keras", "dplyr", "sp", "mapdata", "oce", "rlang", "ncdf4", "geosphere", "ggmap", "sf", "argoFloats", "RColorBrewer")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
    cat("🔧 Installing missing R packages:", paste(missing, collapse=", "), "\n")
    install.packages(missing, repos="https://cloud.r-project.org")
} else {
    cat("✅ All required R packages are already installed.\n")
}
' || { echo "❌ Failed to verify/install R packages."; exit 1; }
echo "============================================================"
echo -e "\033[1;32m👉 To run the examples related to deepspat install the latest version of the deepspat package from GitHub:\033[0m"
echo -e "\033[1;34m   https://github.com/andrewzm/deepspat\033[0m"
echo "============================================================"

echo "✅ Setup complete!"
echo "To activate your virtual environment later, run:"
echo "source env/bin/activate"