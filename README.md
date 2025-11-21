# Research_Project_Spatial-Analysis

# 📍 Mapping Unhealthy Diets and Predicting Adult Obesity in Greater London  
### A Spatial Data Science Project using R, OLS, Random Forests, and Policy Simulation

<p align="center">
  <img src="Screenshot 2025-11-15 121822.png" width="800">
</p>

---

## 📘 Overview  
This project investigates **diet-related health risks**, **spatial dietary inequalities**, and **adult obesity patterns** across **Greater London** using:

- An anonymised **Tesco Grocery 1.0** retail dataset  
- UK public-health indicators at the **Middle Layer Super Output Area (MSOA)** level  
- A custom-built **Unhealthiness Index (UI)** reflecting eight dietary risk variables  
- Spatial clustering (Moran’s I, LISA)  
- Machine learning models (OLS, Random Forests)  
- Policy “What-If” simulations (fruit/veg increase, ready-meal reduction)  
- Income-deprivation equity analysis  

---

## 🗂️ Repository Contents


---

## 🛠️ Method Summary

### **1. Unhealthiness Index (UI)**
A composite score based on:
- Energy from sugar  
- Energy from fat  
- Energy density  
- Sweets, soft drinks, ready-meals  
- Reverse-scaled fruit/veg  
- Reverse-scaled nutrient diversity  

Scaled 0–1 and averaged to reflect dietary risk.

---

### **2. Spatial Analysis**
- **Choropleth maps** to visualise dietary risk geography  
- **Global Moran’s I** → measures overall spatial clustering  
- **Local Moran’s I (LISA)** → identifies hotspots (High-High) and coldspots (Low-Low)

---

### **3. Predictive Modelling**
Two modelling approaches were used:

#### **Ordinary Least Squares (OLS)**
Predictors included dietary risk + socioeconomic factors:  
Income, education, deprivation, BAME percentage, population density.

#### **Random Forest Regression**
- 800 trees  
- mtry = √p  
- Variable importance measured using **IncNodePurity**  

Random Forest substantially outperformed OLS in test-set R².

---

### **4. Equity Analysis**
The diet–obesity relationship was evaluated across **deciles of income deprivation**, revealing:

- Strong diet–obesity association in *affluent* areas  
- Weak or near-zero association in *highly deprived* areas  
- Suggesting structural barriers (stress, poverty, environment) outweigh dietary improvements alone

---

### **5. Policy Simulation**
“What-If” OLS simulations estimated obesity changes under:
- **10% increase in fruit/veg purchases**  
- **10% reduction in soft-drink purchases**  
- **20% reduction in ready-made meals**

Fruit/veg increases showed the strongest reduction in predicted obesity.

---

## 🔧 Key R Packages Used


---

## 📊 Example Outputs

- Unhealthiness Index Map  
- LISA Hotspot Map  
- OLS vs Random Forest Calibration Plot  
- Deprivation Decile Equity Curve  
- Policy Scenario Simulation (fruit/veg, ready-meals)

(All images included in `/figs` folder)

---

## 🎯 Project Goals
- Identify neighbourhoods most at risk of diet-related poor health  
- Predict adult obesity using dietary and socioeconomic features  
- Analyse how deprivation influences the strength of diet–health relationships  
- Demonstrate policy impacts through simulated dietary changes  
- Provide a reproducible spatial-analytic workflow

---

## 👤 Author  
**Bathini Rohan**  
Master of Data Science  
University of Adelaide  
(A1933108@adelaide.edu.au)

---

## 📜 Licence  
MIT Licence — free to use, modify, and distribute with attribution.

---

## 📎 Notes  
- Raw Tesco Grocery 1.0 data cannot be uploaded due to licensing restrictions.  
- All code is fully reproducible using publicly available government datasets.

---

## ⭐ Acknowledgements  
- Supervisor: **Tayla Broadbridge**  
- University of Adelaide — School of Mathematical Sciences  
- Tesco Grocery 1.0 dataset (Aiello et al., 2019)  
- ONS & Public Health England for MSOA-level indicators  

