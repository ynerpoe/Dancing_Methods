%{
This code combines data preprocessing, visualization, and advanced signal
processing techniques (cross-correlation, wavelet coherence, and phase
analysis) to analyze and characterize the relationships between different
body part movements over time. The results, including visualizations and 
computed metrics, are saved for further examination.

contact: noemigrinspun@gmail.com
%}

clc;
close all;
clear all;

%reading xls file 
%Pos x axis hand S1
M = readtable('test_dyad51.csv');
messyTable = readtable("test_dyad51.csv","TextType","string","TreatAsMissing",["0","NA"])
summary(messyTable)
missingElements = ismissing(messyTable,{string(missing),NaN,-99, 0})
rowsWithMissingValues = any(missingElements,2)
missingValuesTable = messyTable(rowsWithMissingValues,:)
messyTable = standardizeMissing(messyTable,-99)
filledTable = fillmissing(messyTable,"nearest")

sampleo = 30; %(tasa de sampleo frames/segundo)

%pos y0 axis S1
tm = filledTable(:,1);
lhx = filledTable(:,4);
lhx2 = fliplr (lhx)

%pos y0 axis S2
tm = filledTable(:,1);
rhx = filledTable(:,7);
rhx2 = fliplr (rhx)

%Pos y1 axis S1
tm = filledTable(:,1)
fhx = filledTable(:,4)
fhx2 = fliplr (fhx)

%pos y1 axis S2
tm = filledTable(:,1)
ghx = filledTable(:,7);
ghx2 = fliplr (ghx)

windowSize = 5; 
b = (1/windowSize)*ones(1,windowSize);
a = 1;

fhx2= fhx2{:,:};
ghx2=ghx2{:,:};
lhx2=lhx2{:,:};
rhx2=rhx2{:,:};
tm=tm{:,:};

figure(1);
  subplot(2,1,1) % Pos y axis
%   plot(filledTable.Time,filledTable.y_p1,'b'); 
  plot(tm, lhx2)
  title('Head y axis S1-S2');
  xlabel('t [frames]') 
  ylabel('position [cm]')  
  %ylim([min(lhx2) max(lhx2)])
  xlim([min(tm) max(tm)])
    hold on
  subplot(2,1,1) % Pos y axis
%   plot(filledTable.Time,filledTable.y_p2,'k'); 
  plot(tm, rhx2) 
  %title('Right foot movement y axis S2');
  xlabel('t [frames]') 
  ylabel('position [cm]') 
  ylim([min(rhx2) max(rhx2)])
    
subplot(2,1,2) % Pos y axis
  plot(tm, fhx2) 
  title('Head y axis S1-S2');
  xlabel('frames') 
  ylabel('position [cm]')
  ylim([min(lhx2) max(lhx2)])
  xlim([min(tm) max(tm)])
    hold on
 subplot(2,1,2) % Pos y axis
   plot(tm, ghx2)
   xlabel('frames') 
   ylabel('position [cm]') 

%figure(2)
  crosscorr(fhx2,ghx2)

%cross correlation and wavelets coherence factors
    scales = 1:64; % Scales for the wavelet transform
    wavelet = 'morl'; % Choose a wavelet function, e.g., 'morl' (Morlet wavelet)
    wcoh = wcoherence(fhx2,ghx2);  
        
%mean_wcoh = mean(wcoh, 2); % average over all time points
        mean_wcoh = mean(wcoh(:)); % average over all time points
        save mean_wcoh;

figure(4)
        wcoherence(fhx2,ghx2,sampleo,'phasedisplaythreshold',0.5);
        title('wcoherence');
    
% Calculate the cross wavelet coherence transform power
power = abs(wcoh).^2;
mean_power = mean(power(:)); % average over all time points
transpose(mean_power)
save mean_power

%wavelet cross-spectrum to use the phase of the wavelet cross-spectrum values to identify the relative lag between the input signals.
[wcoh,wcs] = wcoherence(fhx2,ghx2);

% To calculate the phase of the wavelet cross-spectrum
phase = angle(wcs);
phase_degrees = rad2deg(phase);

relative_phase = unwrap(phase);

% Paso 3: Calcular la fase de las señales wavelet
%phase1 = angle(fhx1);
%phase2 = angle(ghx1);

% Paso 4: Calcular la fase relativa
%relative_phase = exp(1i * (fhx1 - ghx1));


% To calculate the relative phase angle
%phase_signal1 = angle(fhx1)
%phase_signal2 = angle(ghx1)

% Compute the significance level
alpha = 0.05; % Set the desired significance level
%num_iterations = 10; % Number of Monte Carlo iterations
%wcoh_thresh = significance (fhx1,ghx1, wavelet, scales, alpha, num_iterations);

writetable(filledTable,'filledTable10.csv')




