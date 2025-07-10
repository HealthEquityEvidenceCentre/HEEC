# Health Equity Evidence Centre

A comprehensive data analysis project examining health inequalities in primary care across England, featuring interactive visualizations and automated reporting for all Integrated Care Boards (ICBs).

## 🌐 Interactive Website

**[View the full interactive analysis →](https://healthequityevidencecentre.github.io/HEEC/)**

The website includes:
- 📊 **Interactive charts** with code folding/unfolding
- 📈 **15+ datasets** covering NHS payments, workforce, patient satisfaction, and health outcomes
- 🗺️ **ICB-level analysis** across all 42 Integrated Care Boards
- 📱 **Responsive design** that works on all devices

## 📋 What's Included

### Data Analysis
- **NHS Payments**: Practice-level payments analysis by deprivation level
- **GP Workforce**: Staffing patterns and workforce density
- **Patient Satisfaction**: GP Patient Survey results and trends
- **Health Outcomes**: Disease prevalence and quality metrics
- **Practice Characteristics**: Age structure, rurality, and practice size effects

### Interactive Tools
- **Shiny Applications**: [9 interactive apps](https://healthequityevidencecentre.github.io/HEEC/#shiny-applications) for data exploration
- **Automated Datapacks**: Standardized reports for all 42 ICBs
- **Visualization Tools**: Consistent charts and analysis across datasets

### Code & Documentation
- **Reproducible Analysis**: All code available in R/Quarto format
- **Data Processing Scripts**: Standardized data cleaning and merging
- **Documentation**: Comprehensive guides for data sources and methods

## 🚀 Quick Start

### View the Analysis
1. **Online**: Visit the [interactive website](https://healthequityevidencecentre.github.io/HEEC/)
2. **Explore Apps**: Try the [Shiny applications](https://heec.shinyapps.io/Payments_shiny/) for interactive data exploration

### Run Locally
```bash
# Clone the repository
git clone https://github.com/HealthEquityEvidenceCentre/HEEC.git
cd HEEC

# View/render the main analysis
quarto preview README.qmd    # Interactive preview
quarto render README.qmd     # Generate static files
```

### Generate ICB Reports
```bash
# Generate all ICB datapacks
cd datapacks
Rscript collate.R
Rscript -e "source('generate_all_reports.R')"
```

## 📊 Key Findings

The analysis reveals significant health inequalities in primary care:

- **Payment Disparities**: Practices serving more deprived populations receive higher per-patient payments due to increased healthcare needs
- **Workforce Patterns**: GP density varies significantly across deprivation levels and geographic regions  
- **Access Differences**: Patient satisfaction and service access vary by practice characteristics and local deprivation
- **Rural vs Urban**: Distinct patterns in healthcare delivery between rural and urban areas

## 🔧 Technology Stack

- **R**: Data processing and statistical analysis
- **Quarto**: Interactive documentation and website generation
- **Shiny**: Interactive web applications
- **ggplot2**: Data visualization
- **GitHub Pages**: Website hosting

## 📁 Repository Structure

```
├── README.qmd              # Main analysis document (source)
├── data/                   # All datasets (15+ sources)
├── analysis/               # Exploratory analysis scripts
├── shiny/                  # Interactive applications
├── datapacks/              # ICB-specific reports
└── _site/                  # Generated website files
```

## 🤝 Contributing

This project is maintained by the Health Equity Evidence Centre. For questions, suggestions, or collaboration:

- 📧 Email: [contact@heec.co.uk](mailto:contact@heec.co.uk)
- 🐛 Issues: [GitHub Issues](https://github.com/HealthEquityEvidenceCentre/HEEC/issues)
- 💡 Discussions: [GitHub Discussions](https://github.com/HealthEquityEvidenceCentre/HEEC/discussions)

## 📜 License

This project is open source and available under the [MIT License](LICENSE).

## 🏥 About HEEC

The Health Equity Evidence Centre works to reduce health inequalities through data-driven research and evidence synthesis. Learn more at [heec.co.uk](https://www.heec.co.uk/).

---

**📌 Quick Links:**
- [Interactive Website](https://healthequityevidencecentre.github.io/HEEC/) 
- [Shiny Apps](https://heec.shinyapps.io/Payments_shiny/)
- [Project Documentation](https://healthequityevidencecentre.github.io/HEEC/#datasets)
- [ICB Datapacks Guide](datapacks/README.md)