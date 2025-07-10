# ICB Datapacks

This folder contains automated datapacks for all Integrated Care Boards (ICBs) in England, providing health inequalities analysis at the local level.

## Overview

The datapacks system generates standardized reports for each ICB, containing:
- NHS payments analysis by deprivation level
- Patient satisfaction metrics
- Workforce density patterns
- Disease prevalence comparisons
- Life expectancy data
- Secondary care utilization

## File Structure

```
datapacks/
├── README.md                 # This file
├── collate.R                 # Main data processing script
├── collate_region.R          # Regional aggregation script
├── final_data.csv            # Processed ICB-level data
├── final_data_region.csv     # Regional summary data
├── ICB Reports/              # Individual ICB analysis
│   ├── slides.Rmd           # Template for ICB reports
│   └── template.pptx        # PowerPoint template
└── Region reports/           # Regional summaries
    ├── slides.Rmd           # Template for regional reports
    └── template.pptx        # PowerPoint template
```

## How to Generate ICB Reports

### Prerequisites

1. Ensure R is installed with required packages:
```r
install.packages(c("rmarkdown", "knitr", "dplyr", "ggplot2", "officer"))
```

2. Ensure the main data processing has been completed (see main README.qmd)

### Generate Individual ICB Reports

1. **Run the data collation script:**
```bash
Rscript collate.R
```
This creates `final_data.csv` with all ICB-level metrics.

2. **Generate PowerPoint slides for all ICBs:**
```bash
cd "ICB Reports"
Rscript -e "
icb_names <- unique(read.csv('../final_data.csv')$ICB.NAME)
for(icb in icb_names) {
  rmarkdown::render('slides.Rmd', 
    params = list(icb_name = icb),
    output_file = paste0('slides/', gsub('[^A-Za-z0-9 ]', '', icb), '.pptx'))
}
"
```

3. **Generate markdown reports (optional):**
```bash
Rscript -e "
icb_names <- unique(read.csv('../final_data.csv')$ICB.NAME)
for(icb in icb_names) {
  rmarkdown::render('slides.Rmd', 
    params = list(icb_name = icb),
    output_format = 'md_document',
    output_file = paste0('markdown/', gsub('[^A-Za-z0-9 ]', '', icb), '.md'))
}
"
```

### Generate Regional Reports

1. **Run the regional collation script:**
```bash
Rscript collate_region.R
```

2. **Generate regional PowerPoint slides:**
```bash
cd "Region reports"
Rscript -e "
regions <- c('East of England', 'London', 'Midlands', 'North East and Yorkshire', 
             'North West', 'South East', 'South West')
for(region in regions) {
  rmarkdown::render('slides.Rmd', 
    params = list(region_name = region),
    output_file = paste0('output/', region, '.pptx'))
}
"
```

## Output Files

After rendering, you'll find:

### ICB Reports
- **PowerPoint files**: `ICB Reports/slides/[ICB Name].pptx`
- **Markdown files**: `ICB Reports/markdown/[ICB Name].md` (if generated)
- **HTML files**: `ICB Reports/html/[ICB Name].html` (if generated)

### Regional Reports
- **PowerPoint files**: `Region reports/output/[Region Name].pptx`

## Customization

### Modifying the Template

1. **Edit PowerPoint template**: Modify `template.pptx` to change slide layouts, colors, or branding
2. **Update analysis code**: Edit `slides.Rmd` to modify charts, add metrics, or change analysis focus
3. **Adjust data processing**: Modify `collate.R` to include additional datasets or metrics

### Adding New Metrics

1. Add data processing logic to `collate.R`
2. Update the template `slides.Rmd` to include new visualizations
3. Re-run the generation process

## Troubleshooting

### Common Issues

- **Missing data**: Ensure all source datasets in `/data/` folder are complete
- **Template errors**: Check that `template.pptx` is not corrupted and follows expected format
- **Memory issues**: For large datasets, consider processing ICBs in batches

### Re-generating Specific ICBs

To regenerate a single ICB report:
```bash
cd "ICB Reports"
Rscript -e "
rmarkdown::render('slides.Rmd', 
  params = list(icb_name = 'Cornwall and the Isles of Scilly'),
  output_file = 'slides/Cornwall and the Isles of Scilly.pptx')
"
```

## Data Sources

All datapacks use the standardized datasets from the main project:
- NHS Payments to General Practice
- GP Patient Survey
- Workforce data
- Disease prevalence (QOF)
- Life expectancy
- Secondary care utilization

For detailed information about data sources and processing, see the main [README.qmd](../README.qmd).