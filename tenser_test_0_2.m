%{
This MATLAB script analyzes movement data likely related to body pose 
coordinates from a CSV file. Here's a general breakdown:
**1. Initialization (Lines 1-4):**
   - Clears the command window, closes any figures, and clears variables 
for a clean workspace.
**2. Setting Parameters (Lines 6-8):**
   - Defines the sampling frequency (FS) at which the data was recorded.
   - Sets the minimum and maximum frequencies (MINF and MAXF) to be 
analyzed in the later steps.
**3. Reading Data (Lines 10-15):**
   - Reads the CSV file containing the movement data into a table named `M`.
   - Extracts frame numbers and potentially labels/names from specific
columns of the table.
**4. User Input (Lines 17-21):**
   - Prompts the user to enter a point number (index) between 0 and 24, 
corresponding to a specific data point in the file.
   - Validates the user input and displays an error message if it's 
outside the allowed range.
   - If valid, displays the label/name associated with the chosen data point.
**5. Data Processing (Lines 23-33):**
   - Extracts specific coordinates (S1 and S2) based on the chosen point number.
   - Replaces missing values in the data with zeros.
   - Converts the table data into numerical arrays for further analysis.
**6. Analysis (Lines 35-40):**
   - This part performs a Continuous Wavelet Transform (CWT) on the extracted 
coordinates using functions `cwtensor` and `genxwt` (from an external toolbox).
   - CWT helps analyze the frequency content of the movement data at different time points.
   - The script then calculates the magnitude and real/imaginary parts of 
the results, potentially for further interpretation.

NOTE: Analyzing multidimensional movement interaction with
generalized cross-wavelet transform (Toiviainen, Hartmann; 2021)

"Conceptually, the generalized cross-wavelet transform (GXWT) is based 
on the calculation of bivariate cross-wavelet transforms between all 
possible pairs of individual signal components in X and Y from a same point,
thus yielding a total of N x M cross-wavelet transforms. Subsequently, 
for each point in the time-frequency plane, the distribution of the thus 
obtained N x M cross-transform values is modelled with a bivariate normal 
distribution in the complex plane.The size and shape of the estimated 
distribution, more specifically,the variance and eccentricity thereof, 
provide an aggregate measure of the degree of time- and frequency-localized 
synchrony between the multivariate time series. In particular, a 
distribution with a large major axis and high eccentricity indicates 
strong synchrony, while a more circular distribution indicates weaker 
synchrony. On the other hand, the angle between the distribution’s major 
axis and the real axis of the complex plane provides an aggregate measure 
of the mutual phase difference between the multivariate time series."
by YnerPoe
%}

clc; close all; clear all;

FS = 30;   %frecuencia de muestreo
MINF = 0.1;
MAXF = 8;

%Lee archivo 
Archivo = "44.csv";

%Reemplaza "NA" por "NaN"
M = readtable(Archivo,"TextType","string","TreatAsMissing","NA");

% Interpolación de valores faltantes
%M = fillmissing(M, 'nearest');
 
[M_nrows,M_ncols]=size(M); 
NFrames = M(1:25:M_nrows,1);
Nombres = M(1:25,2);
NF = size(NFrames);
tiempo = NF(1,1)/FS; % t en segundos

disp('Enter 0 to 24 point number or enter 25 to compute with all points');
n = input('Point number (0 to 24)?: ');

disp('=== INICIO ===');
if n>=26      %n range 0-24
    disp('The value is above the limit')
elseif n==25
    disp('Computing with All the points');
    S1 = table2array(fillmissing(M((n+1):M_nrows,3:4),'constant',[0]));
    S2 = table2array(fillmissing(M((n+1):M_nrows,6:7),'constant',[0]));
    
    S1 = table2array(fillmissing(M((n+1):M_nrows,3:4),'linear'));
    S2 = table2array(fillmissing(M((n+1):M_nrows,6:7),'linear'));
                  
    [w1 f] = cwtensor(S1,FS,MINF,MAXF); %tensor W de S1_n
    [w2 f] = cwtensor(S2,FS,MINF,MAXF); %tensor W de S2_n 
    [xs] = genxwt(w1, w2); %xs = generalized cross spectrum (frequency x time)
    
    [mag] = abs(xs); %magnitud del vector xs
    save mag
     mean_mag = mean(mag(:)); % average over all time points
        save mean_mag;
        
    % el ángulo entre el eje mayor de la distribución y el eje real del 
    % plano complejo proporciona una medida de la diferencia de fase 
    % entre las series temporales multivariantes
    xs_real = real(xs); %parte real de z
    xs_imag = imag(xs); %parte imaginaria de z
    figure(1);
    subplot(3,1,1);
    plot(real(xs),imag(xs),".");
    axis equal
    grid on
    xlabel("Re(xs)")
    ylabel("Im(xs)")
    subplot(3,1,2);
    imagesc(xs_real);
    colorbar;
    xlabel('Frames');
    %ylabel('Frecuencia');
    title('Generalized cross-wavelet transforms: real part');
    set(gca, 'YDir', 'normal');% Asegura dirección del eje Y
    subplot(3,1,3);
    imagesc(xs_imag);
    colorbar;
    xlabel('Frames');
    %ylabel('Frecuencia');
    title('Generalized cross-wavelet transforms: imaginary part');
    set(gca, 'YDir', 'normal');
else
    disp(Nombres(n+1,1));
    % Separa el punto "n" de interés de los datos de S1 y S2 desde M 
    % Reemplaza las celdas "NaN" por "0" y convierte tabla a arreglo
    % Estos daros consideran 2 dimensiones X e Y para S1 y S2
    S1_n = table2array(fillmissing(M((n+1):25:M_nrows,3:4),'constant',[0]));
    S2_n = table2array(fillmissing(M((n+1):25:M_nrows,6:7),'constant',[0]));
    
    missingElements = ismissing(S1_n)
    rowsWithMissingValues = any(missingElements,2)
    missingValuesTable = S1_n(rowsWithMissingValues,:)

    S1_n = standardizeMissing(S1_n,0)

    %S1_n1 = fillmissing (S1_n,'nearest');
    S1_n1 = fillmissing(S1_n, 'linear');
           
    missingElements1 = ismissing(S2_n)
    rowsWithMissingValues1 = any(missingElements1,2)
    missingValuesTable1 = S2_n(rowsWithMissingValues1,:)

    S2_n = standardizeMissing(S2_n,0)
    
    %S2_n1 = fillmissing( S2_n,'nearest');
    % Interpolate missing data
    S2_n1 = fillmissing(S2_n, 'linear');
    
    %Obtain wavelet tensor w(frequency x time x channel)and Performs the GXWT
    [w1 f1] = cwtensor(S1_n1,FS,MINF,MAXF); %tensor W de S1_n
    [w2 f2] = cwtensor(S2_n1,FS,MINF,MAXF); %tensor W de S2_n 
    [xs] = genxwt(w1, w2); %xs = generalized cross spectrum (frequency x time)
    % el ángulo entre el eje mayor de la distribución y el eje real del 
    % plano complejo proporciona una medida de la diferencia de fase 
    % entre las series temporales multivariantes
    xs_real = real(xs); %parte real de z
    xs_imag = imag(xs); %parte imaginaria de z
    
    [mag] = abs(xs); %magnitud del vector xs
    save mag
     mean_mag = mean(mag(:)); % average over all time points
        save mean_mag;
        
    % Plots
    
    figure(1);
  subplot(3,1,1);
    plot(real(xs),imag(xs),".");
    axis equal
    grid on
    xlabel("Re(xs)")
    ylabel("Im(xs)")
  subplot(3,1,2);
    imagesc(xs_real);
    colorbar;
    xlabel('Frames');
    ylabel('Frecuencia');
    title('Generalized cross-wavelet transforms: real part');
    set(gca, 'YDir', 'normal');% Asegura dirección del eje Y
  subplot(3,1,3);
    imagesc(xs_imag);
    colorbar;
    xlabel('Frames');
    ylabel('Frecuencia');
    title('Generalized cross-wavelet transforms: imaginary part');
    set(gca, 'YDir', 'normal');
    
     wcoh = wcoherence(S1_n1(:,2),S2_n1(:,2),FS,'phasedisplaythreshold',0.5); 
        
%mean_wcoh = mean(wcoh, 2); % average over all time points
        mean_wcoh = mean(wcoh(:)); % average over all time points
        save mean_wcoh;

    figure(2);
    wcoherence(S1_n1(:,1),S2_n1(:,1),FS,'phasedisplaythreshold',0.5);
    title('wcoherence(Coord X del punto N: S1 y S2)');

    figure(3);
    wcoherence(S1_n1(:,2),S2_n1(:,2),FS,'phasedisplaythreshold',0.5);
    title('wcoherence(Coord Y del punto N: S1 y S2)');
end

disp('=== FIN ===');

 
