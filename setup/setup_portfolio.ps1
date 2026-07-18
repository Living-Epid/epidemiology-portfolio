# =====================================================
# Living-Epid Epidemiology Portfolio Setup
# Author : Taiwo Adegbola
# GitHub : https://github.com/Living-Epid
# Version: 1.0
# Purpose: Creates the top-level folder structure for the
#          Epidemiology Portfolio.
# =====================================================

Write-Host ""
Write-Host "========================================="
Write-Host " Living-Epid Portfolio Setup v1.0"
Write-Host "========================================="
Write-Host ""

# Create top-level portfolio folders

$folders = @(
    "01_Foundations",
    "02_Biostatistics",
    "03_Epidemiology",
    "04_Public_Health",
    "05_Geospatial_Analytics",
    "06_R_Toolbox",
    "07_Case_Studies",
    "08_Capstone_Projects",
    "datasets",
    "notes",
    "resources",
    "templates",
    "images"
)

foreach ($folder in $folders) {
    New-Item -ItemType Directory -Name $folder -Force | Out-Null
    Write-Host "Created: $folder"
}

Write-Host ""
Write-Host "Portfolio structure created successfully!"