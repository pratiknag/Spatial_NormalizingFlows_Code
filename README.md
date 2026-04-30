# Spatial Normalizing Flows
This repository contains the code and workflows used in our paper titled "**Normalizing flows for flexible spatial process modeling**", which combines the Normalizing flows with Gaussian processes to model flexible spatial processes.

---

## ⚙️ Installation

### ✅ Prerequisites
- Python ≥ 3.6
- R installed and available in your system path

### 📦 Setup Instructions

To clone the repository and set up the environment:

```bash
git clone https://github.com/pratiknag/Spatial_NormalizingFlows.git
cd Spatial_NormalizingFlows
bash setup.sh
````

> 💡 `setup.sh` installs all dependencies and creates a virtual environment in your current directory. Supported OS: Linux and macOS.

---

## 📈 Reproducing Results

### 🔬 Simulated Experiments: 

Run the following script to reproduce all plots and results for the simulation experiments in the paper:

```bash
bash run_simulations.sh
```

📌 Note:

* The simulation script also runs the deepspat model. Please make sure to install the `deepspat` package beforehand to ensure a smooth run.

* Results and plots for all comparative models will be saved in the `plots/` directory corresponding to each simulation setting.

* Running the script may take some time — we recommend executing it in the background.

---

### 🌐 Real-World Data Applications

The `data_scraping_test.ipynb` file is a Jupyter notebook that interactively preprocesses the required PODAAC data products for inference. Please download the data and run all notebook cells in advance so that the necessary files are generated before executing the script below.

To reproduce the Argo data results, run the following script. The resulting outputs will be saved in the `plots/` folder within the corresponding directory.

```bash
bash run_argo.sh
```

---

## 📫 Contact

For questions, feel free to reach out:

📧 **[pnag@uow.edu.au](mailto:pnag@uow.edu.au)**

---
