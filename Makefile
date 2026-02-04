RSCRIPT=Rscript
ROOT=.

# Targets
all: results/denmark_model_summary.Rds

# Step 1: Clean CAP data
data/denmark_panel.Rda: R/0_clean_cap_data.R
	$(RSCRIPT) $(ROOT)/R/0_clean_cap_data.R

# Step 2: Clean cabinet data
data/denmark_cabinet.Rda: R/1_clean_cabinet_data.R data/denmark_panel.Rda
	$(RSCRIPT) $(ROOT)/R/1_clean_cabinet_data.R

# Step 3: Create dataset
data/input_dataset.Rda: R/2_dataset.R data/denmark_panel.Rda data/denmark_cabinet.Rda
	$(RSCRIPT) $(ROOT)/R/2_dataset.R

# Step 4: Run model
results/denmark_model_summary.Rds: R/3_run_model.R data/input_dataset.Rda
	$(RSCRIPT) $(ROOT)/R/3_run_model.R

# Step 5: Validation
results/denmark_salience.pdf results/denmark_gov_effects.pdf results/denmark_validation.pdf: R/4_analysis.R results/denmark_model_summary.Rds
	$(RSCRIPT) $(ROOT)/R/4_analysis.R

.PHONY: all clean

clean:
	rm -f data/*.Rda results/*.Rds
