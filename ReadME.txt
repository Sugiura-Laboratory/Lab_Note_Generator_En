===========================================
README - app_Lab_Note_Generator.R (Shiny App)
Version: 1.0.0
Author: Shuichi Sugiura
Date: 2025-08-09
Code Name: Wadden
===========================================

■ Overview

This Shiny app automatically generates lab notes for psychology experiments.
When a participant ID is entered, it assigns Heartbeat Counting Task (HCT)
trial durations (e.g., 30, 45, 55 seconds) in a non-repeating order based
on a pre-prepared randomization list (`randomization_list.csv`).

The entered experiment details (participant ID, lab number, experimenter name,
start time, end time) are saved as:

- PDF or editable Rmd lab note
- UTF-8 BOM encoded CSV (no garbled text on Windows/Mac Excel)

---

■ Features

- Automatically displays HCT trial order based on participant ID
- Quick "Now" button to set start/end times instantly
- Input fields for lab number and experimenter name
- Outputs lab note as PDF or Rmd format
- CSV saved in UTF-8 BOM format to prevent character corruption on Windows/Mac Excel
- Participant ID automatically formatted to 3 digits (e.g., 001)

---

■ How to Use

1. Open `app_Lab_Note_Generator.R` in R or RStudio.

2. Run the following command to launch the app:

```r
shiny::runApp("app_Lab_Note_Generator.R")
```

3. When the GUI opens, enter the following:

- Lab number (e.g., A-101)
- Experimenter name (e.g., Shuichi Sugiura)
- Participant ID (e.g., 1 → automatically converted to 001)
- Start time (or press "Now" button)
- End time (or press "Now" button)

4. Click "Generate Report" to save:

- `ID-001-labnote.pdf` (or `.Rmd`)
- `ID-001-labnote.csv`

---

■ Example Output (CSV)

```
ID,ExperimentOrder,HCT_Order,StartTime,EndTime,LabNumber,Experimenter,Date
001,Heart rate Counting Task → Experiment → Questionnaire,"30s → 45s → 55s",2025-08-09 14:00,2025-08-09 14:30,Lab-1,Shuichi Sugiura,2025-08-09
```

---

■ File Structure

- `app_Lab_Note_Generator_En.R` : Main app (English version)
- `labnote_template_En.Rmd` : PDF/Rmd template (English version)
- `randomization_list.csv` : Sample HCT trial order list
- `ReadME.txt` : English description

---

■ Notes

- `randomization_list.csv` is a sample. Please customize it for your own experiment.
- Enter participant ID as half-width digits (e.g., 1 → automatically converted to 001).
- CSV output is in UTF-8 BOM format and will open correctly in Excel on Windows/Mac.
- This app works entirely offline. No generated data is uploaded online.

---

■ Development Info

- Version    : 1.0.0
- Code Name  : Wadden
- Last Update: 2025-08-09
- Author     : Shuichi Sugiura

---

■ Special Thanks

This program was developed while watching the live seal webcam from the WEC (World Heritage Centre Wadden Sea) located in the Netherlands.
If you're a researcher or developer feeling stressed, I highly recommend watching WEC’s livestream—it brings calm and peace of mind.
With heartfelt respect and appreciation for the work WEC does.
Note: This software is not affiliated with WEC in any way.
