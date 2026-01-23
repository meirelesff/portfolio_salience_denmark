# Denmark Portfolio Salience

This repository contains the code and data to apply the Bayesian two-latent quantities IRT model, introduced in the paper *Ministries that We Want: Parties' Agendas and Signaling of Portfolio Salience*, to estimate portfolio salience in the Danish parliament. It uses data from two main sources:

- [Comparative Agendas Project (CAP), Denmark dataset](https://www.comparativeagendas.net/project/dk)
- [WhoGov](https://politicscentre.nuffield.ox.ac.uk/whogov-dataset/c), a global database on ministers and cabinets

## Repository structure

The repository is structured as follows:

```
.
├── data           # Cleaned data files
├── R              # R scripts for data cleaning and analysis
├── raw-data 
│   ├── codebooks  # Codebooks for raw data
│   └── csv        # Raw CSV data files
├── results        # Output results (figures, tables, model outputs)
└── stan           # Stan model files
```