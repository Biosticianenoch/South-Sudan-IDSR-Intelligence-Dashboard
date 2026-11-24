# South Sudan IDSR Intelligence Dashboard

### An Interactive Public Health Surveillance Monitoring and Decision-Support Platform Built with R Shiny

<p align="center">

**Live Application:** [Access the Dashboard](https://enochosen.shinyapps.io/shiny/)

**Developer:** Enock Bereka | Health Data Scientist | Epidemiologist | Biostatistician

</p>

---

## 1. Project Overview

The **South Sudan IDSR Intelligence Dashboard** is an interactive public health surveillance monitoring and analytical platform developed using R and Shiny. It is designed to demonstrate how epidemiological data science, interactive data visualization and surveillance performance monitoring can support the assessment of Integrated Disease Surveillance and Response (IDSR) activities.

The application brings together facility-level supervision records and epidemiological surveillance indicators in an interactive analytical environment. It enables users to examine surveillance functionality, reporting performance, disease alerts, outbreak investigation activities, facility supervision findings and data quality.

The dashboard demonstrates the application of data science in transforming structured public health data into actionable analytical insights for surveillance monitoring, performance assessment and public health decision support.

**Important:** This project uses a synthetic epidemiological dataset generated for demonstration, application development and analytical testing. It does not contain verified operational surveillance observations from South Sudan and must not be interpreted as representing actual disease burden or health system performance.

## 2. Live Application

Explore the deployed application:

**[Launch South Sudan IDSR Intelligence Dashboard](https://enochosen.shinyapps.io/shiny/)**

The dashboard is hosted on Posit's shinyapps.io platform.

## 3. Project Objectives

The project aims to demonstrate how an interactive surveillance analytics platform can:

- Monitor IDSR supervision activities across administrative areas and health facilities.
- Assess the functionality of facility-level disease surveillance systems.
- Examine weekly reporting and reporting timeliness.
- Monitor disease alerts and outbreak investigation activities.
- Assess the availability of essential surveillance documentation and reporting resources.
- Examine specimen transportation and feedback processes.
- Identify surveillance performance gaps requiring follow-up.
- Evaluate completeness and quality of surveillance supervision records.
- Present epidemiological surveillance indicators through interactive visualizations.

## 4. Key Dashboard Features

### 4.1 Surveillance Performance Monitoring

Provides an analytical overview of facility-level surveillance performance, including:

- IDSR functionality.
- Weekly reporting status.
- Case definition availability.
- Surveillance focal person status.
- Patient register availability.
- Zero-reporting status.
- Alert reporting timeliness.

### 4.2 Disease Alert and Outbreak Monitoring

Provides an analytical view of disease alert and response-related indicators, including:

- Alert detection.
- Alert reporting timeliness.
- Outbreak investigation status.
- Unreported AFP cases.
- Unreported measles cases.
- Unreported acute jaundice cases.
- Unreported acute flaccid paralysis cases.
- Specimen transportation status.

### 4.3 Facility Supervision Analysis

Supports examination of supervision findings and facility-level records, including:

- Facility supervision information.
- Facility type and operational status.
- Surveillance performance indicators.
- Feedback provided during supervision.
- Action plan status.
- Follow-up requirements.

### 4.4 Data Quality Assessment

Provides an analytical view of data completeness and missing information to support data quality monitoring and assessment.

### 4.5 Interactive Dashboard Filtering

Users can dynamically filter the underlying supervision records using available dashboard controls, including:

- Year.
- Quarter.
- Month.
- Week.
- State.
- District.
- Health facility.
- Supervision type.
- Priority level.
- Facility type.
- Facility status.
- Staff designation.

The visualizations respond to the selected records, allowing users to investigate surveillance indicators within different geographical, temporal and operational contexts.

**Analytical design consideration:** The dashboard separates filter dimensions from chart outcome dimensions. For example, selecting a particular state filters the underlying records without making state the comparison dimension of the surveillance indicator charts. This helps preserve meaningful multi-category comparisons after filtering.

## 5. Technology Stack

| Technology | Application |
|---|---|
| R | Statistical computing and data processing |
| Shiny | Interactive web application framework |
| shinydashboard | Dashboard layout and navigation |
| tidyverse | Data manipulation and transformation |
| lubridate | Date and time processing |
| Plotly | Interactive data visualization |
| DT | Interactive data tables |
| ggplot2 | Statistical graphics |
| scales | Data presentation and formatting |
| shinyapps.io | Application deployment and hosting |

## 6. Dataset Description

The project uses a synthetic facility-level epidemiological supervision dataset.

### Dataset characteristics

| Attribute | Description |
|---|---|
| Dataset type | Synthetic epidemiological supervision data |
| Number of records | 21,528 |
| Number of variables | 32 |
| Date coverage | January 2024 – August 2026 |
| Geographical context | South Sudan |
| Analytical unit | Supervision record |
| Format | CSV |
| Primary application | Surveillance performance monitoring |

### Key analytical variables

The dataset contains information relating to:

- Administrative location and facility characteristics.
- Supervision dates and supervision types.
- Facility operational status.
- Surveillance system functionality.
- Weekly reporting performance.
- Disease alert detection and reporting.
- Outbreak investigation.
- Disease-specific surveillance indicators.
- Specimen transportation.
- Supervision feedback and action plans.
- Follow-up requirements.
- Data completeness and quality.

### Dataset file

` south_sudan_iss_synthetic_epidemiological_dataset.csv `

The dataset is included in this repository to enable reproducibility and local execution of the application.

### Data limitations

This dataset is entirely synthetic. Its records, values and distributions are intended for demonstrating analytical workflows and dashboard functionality.

The dataset must not be used to:

- Estimate actual disease incidence or prevalence in South Sudan.
- Assess real surveillance system performance.
- Make operational decisions concerning actual health facilities.
- Infer disease outbreaks or public health emergencies.
- Represent findings as official Ministry of Health or WHO statistics.

All analytical findings generated from the dataset should be interpreted within the context of synthetic data.

## 7. Repository Structure

```text
south-sudan-idsr-intelligence-dashboard/
│
├── app.R
│
├── south_sudan_iss_synthetic_epidemiological_dataset.csv
│
├── README.md
│
└── .gitignore
```

## 8. Installation and Local Setup

You can download or clone this repository and run the dashboard locally.

### Prerequisites

Ensure you have installed:

- R (https://cran.r-project.org/)
- RStudio Desktop (https://posit.co/download/rstudio-desktop/)

### Step 1: Clone the repository

```bash
git clone https://github.com/Biosticianenoch/south-sudan-idsr-intelligence-dashboard.git
```

### Step 2: Navigate to the project directory

```bash
cd south-sudan-idsr-intelligence-dashboard
```

### Step 3: Install required R packages

Run the following in your R console:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "janitor",
  "lubridate",
  "plotly",
  "DT",
  "scales"
))
```

### Step 4: Run the application

Open the project directory in RStudio and execute:

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and click **Run App**.

The application will start locally in your default browser or RStudio viewer.

## 9. Application Deployment

The dashboard is deployed using Posit's shinyapps.io hosting service.

**Production application URL:**

https://enochosen.shinyapps.io/shiny/

To deploy your own instance, install and configure the `rsconnect` package using your shinyapps.io account credentials.

```r
install.packages("rsconnect")

library(rsconnect)

rsconnect::deployApp()
```

Deployment credentials should be configured securely and must never be committed to a public GitHub repository.

## 10. Analytical and Public Health Applications

The project demonstrates several important applications of health data science and epidemiological surveillance analytics:

**Surveillance system monitoring:** Examining facility-level surveillance functionality and reporting indicators.

**Performance measurement:** Assessing surveillance documentation, reporting processes and response-related activities.

**Disease event monitoring:** Organizing disease alert and outbreak investigation indicators into an interactive analytical environment.

**Data quality management:** Identifying missing information and evaluating completeness of supervision records.

**Decision support:** Presenting surveillance indicators in an accessible format to support analytical review and identification of potential performance gaps.

These capabilities illustrate how data-driven monitoring systems can contribute to stronger public health information systems.

## 11. Future Development

Potential future enhancements include:

- Integration with authorized real-world surveillance information systems.
- Automated data ingestion from approved electronic data collection platforms.
- Enhanced geographical visualization and spatial surveillance analytics.
- Advanced epidemiological trend analysis.
- Configurable surveillance performance thresholds.
- Automated reporting and PDF report generation.
- Role-based access control and user authentication.
- Automated alerts for surveillance performance gaps.
- Integration with machine learning models for predictive surveillance analytics.
- Expanded monitoring and evaluation indicators.

These are proposed enhancements and are not necessarily implemented in the current version.

## 12. Data Governance and Responsible Use

This project is intended for educational, analytical and demonstration purposes.

Any future integration with real health information must comply with applicable data protection legislation, institutional approvals, research ethics requirements, data-sharing agreements and health information confidentiality standards.

No identifiable patient-level information should be uploaded or published through this demonstration repository.

## 13. Author and Project Attribution

**Enock Bereka**

Health Data Scientist | Epidemiologist | Biostatistician

Founder – DataQuest Solutions

**Professional interests:**

- Health Data Science.
- Epidemiological Surveillance.
- Biostatistics.
- Bayesian Statistical Modelling.
- Machine Learning and Predictive Analytics.
- Public Health Informatics.
- Interactive Data Visualization.
- Research and Statistical Computing.

**Professional links:**

- GitHub: https://github.com/Biosticianenoch
- LinkedIn: https://www.linkedin.com/in/enock-bereka/
- Website: https://www.dataquestsolutions.co.ke/
- DataQuest Solutions: https://www.dataquestsolutions.co.ke/

## 14. License

A repository license should be selected and added before redistributing this project. Until then, all rights are reserved by the author, subject to applicable third-party software and dependency licenses.

## 15. Acknowledgement

This project demonstrates the intersection of epidemiology, biostatistics, public health surveillance and modern data science.

It reflects the potential of interactive analytical systems to improve the accessibility, interpretation and communication of public health information.

---

<p align="center">

**Developed by Enock Bereka**

*I Teach. I Analyze. I Empower Decisions.*

**DataQuest Solutions | Data, AI & Technology**

</p>
