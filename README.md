# Dancing_Methods
The MATLAB code (Synchrony_dyad_analysis_new2.m) performs several data processing and analysis tasks. Here's a description of the key functionalities:

1. Data Loading and Preprocessing:
•	Reads a CSV file named "file_name.csv" into a table (`messyTable`).
•	Handles missing values by treating specified values as missing.
•	Identifies rows with missing values and creates a table (`missingValuesTable`) containing those rows.
•	Standardizes missing values in `messyTable` to -99.
•	Fills missing values in `messyTable` using the "nearest" interpolation method, creating `filledTable`.

2. Data Visualization:
•	Extracts specific columns from `filledTable` for further processing (columns related to the positions of different body parts over time).
•	Plots the positions of various body parts on the y-axis over time in two subplots, providing visualizations for S1 and S2.

3. Cross-Correlation:
•	Computes and plots the cross-correlation between the positions of two body parts (`fhx2` and `ghx2`).

4. Wavelet Analysis:
•	Performs a wavelet coherence analysis between the positions of two body parts (`fhx2` and `ghx2`).
•	Computes the mean wavelet coherence and saves it to a file (`mean_wcoh`).
•	Plots the wavelet coherence with a specified sample rate.

5. Power Calculation:
•	Computes the power of the wavelet coherence and calculates the mean power.
•	Transposes and saves the mean power to a file (`mean_power`).

6. Phase Analysis:
•	Calculates the phase of the wavelet cross-spectrum and converts it to degrees.
•	Unwraps the phase for further analysis.
•	Sets a significance level (`alpha`).

7. Data Export:
•	Writes the filled and processed data to a new CSV file named "filledTable1.csv".

This script is part of a set of tools that allows the analysis of people's movements based on previously recorded videos. To perform the complete analysis, you must follow the following instructions:

1. First, capture the video in MP4 format, which will be used in the subsequent analysis (see the "Videos_examples" folder.
The Video_example_dyad.mp4 is just an example, the actual videos involve mothers and their preschool children)

2. Next, use the Openpose code that, based on the video captured in the previous step, generates an output file in JSON format, 
containing coordinates for 25 points, including head, chest, hands, feet, knees, hips, shoulders, etc. Each point is associated with three parameters:
two XY coordinates and a confidence coordinate (C) indicating the confidence level of obtaining the XY coordinates. 
Thus, each point of interest is P(x, y, c) (see JSON_files.zip in the "Results" folder).
General instructions for using Openpose can be found at the following link: https://github.com/CMU-Perceptual-Computing-Lab/openpose

3. Subsequently, convert the Openpose JSON files to a CSV file using the R code available at https://github.com/ynerpoe/Dancing_Methods/tree/main/R%20scripts

4. Once the CSV file with the 25 available points is obtained, use the tenser_test_0_2 Matlab code


