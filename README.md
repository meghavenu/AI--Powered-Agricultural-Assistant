# 🌾 YieldWise — AI-Powered Crop Yield Prediction

YieldWise is an interactive **Shiny web application** that predicts
agricultural crop yields using a **Random Forest Machine Learning model**
trained on rainfall, crop production, fertilizer usage, pesticide usage,
and state-level agricultural statistics.

The app provides:
- 📊 Interactive data visualizations  
- 🌧 Rainfall trend analysis  
- 🌾 Crop yield prediction  
- 🤖 AI Assistant powered by a local ML model (no API required!)  
- 📈 Insights and suggestions for farmers  

---

## 🚀 Features

### ✔ Machine Learning (Random Forest)
- Model trained offline using `train_rf_model.R`
- Predicts based on rainfall, crop type, state, and agronomic factors
- Fast & runs entirely on your machine — no API keys needed

### ✔ Shiny-based Web UI
- Easy dropdowns for selecting crop and state
- Interactive AI Assistant panel
- Real-time yield prediction with explanation

### ✔ Clean Architecture
