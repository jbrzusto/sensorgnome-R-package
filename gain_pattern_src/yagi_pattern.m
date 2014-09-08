%% -*- mode: octave -*-
%% 
%% theoretical one-way E-field gain patterns of yagi antennas
%% used with lotek transmitter tags
%%
%% Outputs files of gain values, at a grid of (theta, phi) values.  
%% Each row of file is at constant theta, changing phi.
%% Theta: from 0 to 360 in steps of dth.
%% Phi: from 0 to 90 in steps of dphi.
%% Slightly modified from Orfanidis' 6 element Example 22.5.3
%%
%% Ref: Sophocles Orfanidis (2008).  Electromagnetic Waves and Antennas
%%      Online textbook.  Available at:
%%      http://www.ece.rutgers.edu/~orfanidi/ewa/
%%

addpath("Orfanidis_ewa");

c = 2.997925E8;                  % speed of light
f = 166.38E6;                    % frequency of Lotek tag transmitters
lambda = c / f * 100;            % wavelength, in centimetres

%%% 5 element Yagi

L = [92.5, 87.8, 83, 82.2, 80.8];         % lengths of elements from reflector to directors, in cm
a = (0.253 * 2.54) * ones(1,5);           % diameters of wires in same elements in cm
d = cumsum([0, 31.9, 25.4, 27.7, 30]);    % spacing between elements in cm -> converted to locations

%%% scale dimensions to wavelengths

L /= lambda;    % lengths of Reflector, driven, and director elements
a /= lambda;
d /= lambda;

%% Note: units for L, a, d are wavelengths.

[I,D,Rfb] = yagi(L,a,d);		    % get current patterns
disp("Yagi-5 D, Rfb")
disp([10*log10(D), 10*log10(Rfb)])

dth  = 1;			            % step size for theta, in degrees
dphi = 1;				    % step size for phi, in degrees

gm = zeros(1 + 360 / dth, 0);		    % initial empty gain matrix

for phi=0:dphi:90                           % loop over phi
  [ge,gh,th] = gain2sg(L,d,I,360/dth,phi);  % get gain with global normalization
  gm = [gm, ge'];                           % add as column to gain the matrix
  %% dbp2(th,ge,30,40);                     % to visualize as generated
endfor

sgm = size(gm);
save -ascii yagi_5_pattern.txt sgm D gm;    % save to text file

%%% 9 element Yagi

L = [89, 84.3, 79.2, 74.1, 73.2, 72.6, 71.2, 70.5, 69.5]; % lengths of Reflector, driven, and director elements in cm
a = (0.501 * 2.54) * ones(1, 9);                          % diameters of wires in same elements in cm
d = cumsum([0, 27.3, 25.2, 31.2, 37.2, 48.5, 48.2, 52.4, 54.7]);     % spacing of elements along boom in cm -> converted to locations

%% rescale to wavelengths
L /= lambda;
a /= lambda;
d /= lambda;

[I,D,Rfb] = yagi(L,a,d);		    % get current patterns
disp("Yagi-9 D, Rfb")
disp([10*log10(D), 10*log10(Rfb)])

dth  = 1;			            % step size for theta, in degrees
dphi = 1;				    % step size for phi, in degrees

gm = zeros(1 + 360 / dth, 0);		    % initial empty gain matrix

for phi=0:dphi:90                           % loop over phi
  [ge,gh,th] = gain2sg(L,d,I,360/dth,phi);  % get gain with global normalization
  gm = [gm, ge'];                           % add as column to gain the matrix
  %% dbp2(th,ge,30,40);                     % to visualize as generated
endfor

sgm = size(gm);
save -ascii yagi_9_pattern.txt sgm D gm;    % save to text file
