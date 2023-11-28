%{
This code combines data preprocessing, visualization, and advanced signal
processing techniques (cross-correlation, wavelet coherence, and phase
analysis) to analyze and characterize the relationships between different
body part movements over time. The results, including visualizations and 
computed metrics, are saved for further examination.

contact: noemigrinspun@gmail.com
         ynerpoe@gmail.com
%}

clc;
close all;
clear all;

% Reading the file
messyTable = readtable("file_name.csv", "TextType", "string", "TreatAsMissing", ["0", "NA"]);
summary(messyTable)

missingElements = ismissing(messyTable, {string(missing), NaN, -99});
rowsWithMissingValues = any(missingElements, 2);
missingValuesTable = messyTable(rowsWithMissingValues, :);
messyTable = standardizeMissing(messyTable, -99);
filledTable = fillmissing(messyTable, "nearest");

sampleo = 30; % (frames per second)

% Position on y0 axis S1
tm = filledTable(:, 9);
lhx = filledTable(:, 4);
lhx2 = fliplr(lhx);

% Position on y0 axis S2
tm = filledTable(:, 9);
rhx = filledTable(:, 7);
rhx2 = fliplr(rhx);

% Position on y1 axis S1
tm = filledTable(:, 9);
fhx = filledTable(:, 4);
fhx2 = fliplr(fhx);

% Position on y1 axis S2
tm = filledTable(:, 9);
ghx = filledTable(:, 7);
ghx2 = fliplr(ghx);

windowSize = 5; 
b = (1/windowSize) * ones(1, windowSize);
a = 1;

fhx2 = fhx2{:,:};
ghx2 = ghx2{:,:};
lhx2 = lhx2{:,:};
rhx2 = rhx2{:,:};
tm = tm{:,:};

figure(1)
subplot(2, 1, 1) % Position on y axis
plot(tm, lhx2)
title('Head y axis S1-S2');
xlabel('t [sec]') 
ylabel('position [cm]')  
xlim([min(tm) max(tm)])
hold on
subplot(2, 1, 1) % Position on y axis
plot(tm, rhx2) 
xlabel('t [sec]') 
ylabel('position [cm]') 

subplot(2, 1, 2) % Position on y axis
plot(tm, fhx2) 
title('Head y axis S1-S2');
xlabel('Frames') 
ylabel('position [cm]')
xlim([min(tm) max(tm)])
hold on
subplot(2, 1, 2) % Position on y axis
plot(tm, ghx2)
xlabel('Frames') 
ylabel('position [cm]') 

figure(2)
crosscorr(fhx2, ghx2)

figure(3)
scales = 1:64; % Scales for the wavelet transform
wavelet = 'morl'; % Choose a wavelet function, e.g., 'morl' (Morlet wavelet)
wcoh = wcoherence(fhx2, ghx2);
mean_wcoh = mean(wcoh(:)); % average over all time points
save mean_wcoh;

figure(4)
wcoherence(fhx2, ghx2, sampleo, 'phasedisplaythreshold', 0.5);
title('wcoherence');

% Calculate the cross wavelet coherence transform power
power = abs(wcoh).^2;
mean_power = mean(power(:)); % average over all time points
transpose(mean_power)
save mean_power

[wcoh, wcs] = wcoherence(fhx2, ghx2);

% Calculate the phase of the wavelet cross-spectrum
phase = angle(wcs);
phase_degrees = rad2deg(phase);

relative_phase = unwrap(phase);

alpha = 0.05; % Set the desired significance level

writetable(filledTable, 'filledTable1.csv')


