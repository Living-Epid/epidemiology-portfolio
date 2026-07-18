# =====================================================
# Living-Epid Epidemiology Portfolio Setup
# Author : Taiwo Adegbola
# GitHub : https://github.com/Living-Epid
# Version: 2.0
# =====================================================

$repoRoot = Split-Path $PSScriptRoot -Parent

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

    $path = Join-Path $repoRoot $folder

    if (!(Test-Path $path)) {
        New-Item -ItemType Directory -Path $path | Out-Null
        Write-Host "Created: $folder"
    }
    else {
        Write-Host "$folder already exists."
    }

}

Write-Host ""
Write-Host "Portfolio setup complete!"