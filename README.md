# 📊 ShinyApp: Distributional Analysis with World Bank povcalnet/PIP Data

This R Shiny application allows users to explore decile-level consumption and expenditure data from the [World Bank Poverty and Inequality Platform (PIP)](https://pip.worldbank.org/) or formerly PovcalNet, and conduct basic distributional simulations to evaluate the effects of monetary transfers on inequality.

Available here: https://rikimatsumoto.shinyapps.io/shiny-2023-05-04-povcalnet/

## 🌍 Purpose

The app is designed as a lightweight analytical tool for anyone interested in understanding income or consumption distribution within countries. Users can interactively explore:

## 🔧 Features

* **Interactive Visualization**: Explore decile-level consumption/expenditure data and baseline inequality metrics (e.g., Gini coefficient).
* **Simple Policy Simulation**: Simulate the impact of a monetary transfer (e.g., universal or targeted) on income distribution and inequality.
* **Responsive Interface**: Built with R Shiny for web-based interaction and minimal technical setup.

## 📦 Data Source

All data is sourced from the World Bank's [Poverty and Inequality Platform (PIP)](https://pip.worldbank.org/), which provides harmonized household survey data for a wide range of countries and years.

## 🛠️ Installation

Clone the repository and run `app.R` in RStudio.

```bash
git clone https://github.com/yourusername/shiny-distributional-analysis.git
```

## 🖥️ Usage

Once launched, the app allows users to:

1. **Select a country and survey year**
2. **Visualize decile-level consumption data**
3. **Input a transfer amount and define the target population share**
4. **Observe how the simulated transfer affects inequality metrics**

## 📈 Example Use Cases

* Explore how a universal basic income (UBI) affects the Gini coefficient - if equal amount to all deciles, inequality does not change!
* Simulate targeted transfers to the bottom 30% of the population.

## 🧠 Limitations

* The simulation assumes perfect targeting and full absorption of transfers.
* Results are illustrative and not intended for detailed policy design.

## 📄 License

MIT License. See `LICENSE` for details.

## 🤝 Acknowledgements

* World Bank Poverty and Inequality Platform (PIP)
* R and Shiny development communities
