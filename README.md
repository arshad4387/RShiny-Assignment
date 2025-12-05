DIG Trial Insights — Interactive Shiny Dashboard

A comprehensive interactive dashboard built in R Shiny to explore and visualise patient characteristics, clinical outcomes, hospitalisations, and cohort profiles from the Digitalis Investigation Group (DIG) Trial dataset.

This application provides a clean analytical interface for trial researchers, clinicians, and students to explore population-level patterns, stratified outcomes, and exploratory relationships.


The app consists of six fully interactive pages, each designed for a different analytical task:

1️⃣ Trial Outcomes & Overview
	•	Summary cards showing:
	•	Cardiovascular mortality
	•	Heart failure–related events
	•	Non-cardiovascular hospitalisations
	•	Patient demographic overview
	•	Filterable distribution plots:
	•	Treatment allocation pie chart
	•	Age distribution
	•	Gender proportions
	
2️⃣ Hospitalisations
	•	Filter patients by:
	•	Minimum hospitalisation days
	•	Hospitalisation history
	•	Number of hospitalisations
	•	Death status
	•	Interactive “Apply Filters” workflow
	•	View hospitalisation details in a sortable, searchable table

3️⃣ Clinical Characteristics
	•	Summary cards showing:
	•	Mean ejection fraction
	•	Mean heart rate
	•	% with NYHA Class III–IV
	•	Mean CHF duration
	•	% with hypertension
	•	% with diabetes
	•	% recently using digoxin
	•	Median creatinine
	•	Mean BMI
	•	Distribution plots of key clinical variables
	
4️⃣ Cohort / Baseline Characteristics
	•	Filter cohort by:
	•	Age range
	•	Treatment arm
	•	Sex
	•	Visualisations:
	•	Age distribution
	•	BMI boxplots
	•	Ejection fraction by treatment
	•	NYHA class distribution
	
5️⃣ Exploration Tool
	•	Flexible analysis tool allowing users to choose:
	•	X variable
	•	Y variable
	•	Colour grouping variable
	•	Optional smoothing line (LOESS)
	•	Interactive scatterplots (Plotly)
	•	Subset table with selected variables

6️⃣ Download & Reproducibility
	•	Download the filtered dataset as CSV
	•	View complete R session information

	

	
