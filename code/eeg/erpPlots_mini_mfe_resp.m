
% This script was created by George Buzzell for the NDC Lab EEG Training
% Workshop on 02/22. This script uses parts of the "set up" structure from
% the MADE preprocessing pipeline (Debnath, Buzzell, et. al., 2020)

clear % clear matlab workspace
clc % clear matlab command window

%% setup; run this section before any other section below

% MUST EDIT THIS
%running in "EEG_training" folder on your computer
main_dir = '/Users/kihossei/Documents/GitHub/memory-for-error-mini';

% Setting up other things

%Location of MADE and ADJUSTED-ADJUST scripts
addpath(genpath([main_dir filesep 'code' filesep 'eeg' filesep 'MADE-EEG-preprocessing-pipeline']));% enter the path of the folder in this line

%Location of "EEG
addpath(genpath([main_dir filesep 'code' filesep 'eeg' filesep 'eeglab13_4_4b']));% enter the path of the EEGLAB folder in this line

%remove path to octave functions inside matlab to prevent errors when
rmpath([main_dir filesep 'code' filesep 'eeg' filesep 'eeglab13_4_4b' filesep 'functions' filesep 'octavefunc' filesep 'signal'])

% 1. Enter the path of the folder that has the data to be analyzed
data_location = [main_dir filesep 'derivatives' filesep 'eeg' filesep 'preprocessed' filesep 'processed_data'];

% 2. Enter the path of the folder where you want to save the processed data
output_location = [main_dir filesep 'derivatives' filesep 'eeg' filesep 'eeg_erp_output'];

% 3. Enter the path of the channel location file
channel_locations = [main_dir filesep 'code' filesep 'eeg' filesep 'MADE-EEG-preprocessing-pipeline' filesep 'chan_locs_files' filesep 'CACS-128-X7-FIXED-64only.bvef'];

% enter the stimulus makers that need to be adjusted for time offset
stimulus_markers = {'S  5', 'S  6', 'S  7', 'S  8'};      % enter the stimulus makers that need to be adjusted for time offset
response_markers = {'S 11', 'S 12', 'S 21', 'S 22'};

% Read files to analyses
% Keeping filenames that have more than 4 error trials.
trialCounts = readtable([output_location filesep 'mini_mfe_trial_counts.csv']); % loads the output of trialCounts script.
trialCounts = trialCounts(trialCounts.resp_incon_error > 4, :); 
datafile_names = trialCounts.fileName;
datafile_names = datafile_names';

% Check whether EEGLAB and all necessary plugins are in Matlab path.
if exist('eeglab','file')==0
    error(['Please make sure EEGLAB is on your Matlab path. Please see EEGLAB' ...
        'wiki page for download and instalation instructions']);
end




%% Plot ERPs!!

%load the mat file that has the erps and subject list
load([output_location filesep 'mini_mfe_erpDat_resp.mat'])


%make a copy/rename the erp matrix 
allData = mini_mfe_erpDat_resp;


%load in one of the participants EEGLAB-formatted data; this is to load
%parameters needed for plotting (sampling rate, chanlocs, etc).
EEG = pop_loadset( 'filename', datafile_names{1}, 'filepath', data_location);
EEG = eeg_checkset(EEG);

%if not whole numbers already, then round EEG.times to nearest whole ms to make easier to work with
EEG.times = round(EEG.times);

%setup for baseline correcting the ERP data (always done before plotting or extracting
%erps, not done to the data previously to allow use of different baselines
%as a function of review comments)
startTime = -400; %(in ms)
endTime = -200 ; %(in ms)

%find closest values in (rounded) EEG.times to the specified start/stop
[temp,startIdx] = min(abs(EEG.times-startTime));
[temp2,endIdx] = min(abs(EEG.times-endTime));

%baseline corrections
range = startIdx:endIdx;
allBase = squeeze(mean(allData(:,:,:,range),4));
allBase = mean(allData(:,:,:,range),4);

for i=1:size(allData,4)
    newData(:,:,:,i) = allData(:,:,:,i) - allBase;
end

%% select channel(s) to plot
% 1, 2, 5, 37, 34

%clusters
 %chan = (newData(:,:,[1, 2, 5, 37, 34],:)); 
 %chan = (newData(:,:,[2, 1, 33, 34],:));
 chan = (newData(:,:,[17, 18, 49, 19, 40],:)); 



%% 

%average over selectd channels
chan = mean(chan,3);

%pull out 3 conditions of interest for all subs
resp_incon_err = chan(:,1,:,:);
resp_incon_corr = chan(:,2,:,:);
resp_con_corr = chan(:,3,:,:);


%average across subs
resp_incon_err_Mean = squeeze(mean(resp_incon_err,1));
resp_incon_corr_Mean = squeeze(mean(resp_incon_corr,1));
resp_con_corr_Mean = squeeze(mean(resp_con_corr,1)); 

%corrMean = lowpass(squeeze(mean(corr,1)),20,1000);
%errorMean = lowpass(squeeze(mean(error,1)),20,1000);

%label for plot and define colors for plot
blue = [0  0 1];
red = [1 0 0];
green = [0 1 0];

%plot the two response-related erps
figure;
hold on
%plot(EEG.times, corrMean, 'color', blue, 'LineWidth', 1.5); %'LineStyle', '--');
plot(EEG.times, resp_con_corr_Mean, 'color', green, 'LineWidth', 1.5); %'LineStyle', '--');
plot(EEG.times, resp_incon_err_Mean, 'color', blue, 'LineWidth', 1.5);
plot(EEG.times, resp_incon_corr_Mean, 'color', red, 'LineWidth', 1.5);
title(sprintf('response incong error and incong correct'), 'FontSize', 30);
legendHandle = legend('Con-Correct', 'Incon-Error','Incon-Correct');
set(legendHandle, 'box', 'off', 'FontSize', 26);
hold off;

% set parameters
plotStartTime = -1000; %(in ms)
plotEndTime = 1500 ; %(in ms)
set(gcf, 'Color', [1 1 1]);
set(gca, 'YLim', [-6 16]);
set(gca, 'XLim', [plotStartTime plotEndTime]);
set(gca, 'FontSize', 20);
set(get(gca, 'YLabel'), 'String', 'Amplitude in  \muV', 'FontSize', 26);
set(get(gca, 'XLabel'), 'String', 'Time Relative to Response (ms)', 'FontSize', 26);
set(gca, 'Box', 'off');
set(gcf, 'Position', [0 0 1440 900]);



%% Plot topos!!

%load the mat file that has the erps and subject list
load([output_location filesep 'mini_mfe_erpDat_resp.mat'])

%make a copy/rename the erp matrix 
allData = mini_mfe_erpDat_resp;

%load in one of the participants EEGLAB-formatted data; this is to load
%parameters needed for plotting (sampling rate, chanlocs, etc).
EEG = pop_loadset( 'filename', datafile_names{1}, 'filepath', data_location);
EEG = eeg_checkset(EEG);

%round EEG.times to nearest whole ms to make easier to work with
EEG.times = round(EEG.times);

%setup for baseline correcting the ERP data (always done before plotting or extracting
%erps, not done to the data previously to allow use of different baselines
%as a function of review comments)
startTime = -400; %(in ms)
endTime = -200 ; %(in ms)

%find closest values in (rounded) EEG.times to the specified start/stop
[temp,startIdx] = min(abs(EEG.times-startTime));
[temp2,endIdx] = min(abs(EEG.times-endTime));

%baseline corrections
range = startIdx:endIdx;
allBase = mean(allData(:,:,:,range),4);

for i=1:size(allData,4)
    newData(:,:,:,i) = allData(:,:,:,i) - allBase;
end

%start and end time range for component of interest
compStartTime = 200; %(in ms)
compEndTime = 300 ; %(in ms)

%find closest values in (rounded) EEG.times to the specified start/stop
[temp,compStartIdx] = min(abs(EEG.times-compStartTime));
[temp2,compEndIdx] = min(abs(EEG.times-compEndTime));

%idxs of time range to plot topo for
compRange = compStartIdx:compEndIdx;

%pull out conditions of interest for all subs, and average over time
%range of interest
condition_A = mean(newData(:,1,:,compRange),4); % incong-error
condition_B = mean(newData(:,2,:,compRange),4); % incon-corr

%average across subs
condition_A_Mean = squeeze(mean(condition_A,1)); % 
condition_B_Mean = squeeze(mean(condition_B,1)); % 

%compute difference topo
con_A_minus_B_Mean = condition_A_Mean - condition_B_Mean; %incon-corr - incong-error

%plot topos
figure
topoplot(condition_A_Mean, EEG.chanlocs, 'maplimits', [-6 6], 'electrodes', 'on', 'gridscale', 300)
set(get(gca, 'title'), 'String', 'NN; Mean Amplitude (0-100 ms)', 'FontSize', 16);

figure
topoplot(condition_B_Mean, EEG.chanlocs, 'maplimits', [-6 6], 'electrodes', 'on', 'gridscale', 300)
set(get(gca, 'title'), 'String', 'NT; Mean Amplitude (0-100 ms)', 'FontSize', 16);

figure
topoplot(con_A_minus_B_Mean, EEG.chanlocs, 'maplimits', [-4 4], 'electrodes', 'on', 'gridscale', 300, 'plotrad', .6)
set(get(gca, 'title'), 'String', 'NT Minus NN; Mean Amplitude (108-176 ms)', 'FontSize', 16);




%% Extract mean component amplitudes (for statistics)

%clear output
output = [];

%create variable names for count trials output and write to disk
outputHeader = {'id, ERN, CRN, deltaERN'};
dlmwrite(strcat('erpCore_compMeans_example_', date, '.csv'), outputHeader, 'delimiter', '', '-append');

%define electrodes average over for component
compCluster = [17 21 22]; 

%define timeRange to average over for component 
compTime = [0 100]; 

%load the mat file that has the erps and subject list
load('erpCore_erps_example.mat')

%make a copy/rename the erp matrix 
allData = erpCore_erpDat_example;

%load in one of the participants EEGLAB-formatted data; this is to load
%parameters needed for plotting (sampling rate, chanlocs, etc).
EEG = pop_loadset( 'filename', datafile_names{1}, 'filepath', data_location);
EEG = eeg_checkset(EEG);

%round EEG.times to nearest whole ms to make easier to work with
EEG.times = round(EEG.times);

%setup for baseline correcting the ERP data (always done before plotting or extracting
%erps, not done to the data previously to allow use of different baselines
%as a function of review comments)
startTime = -400; %(in ms)
endTime = -200 ; %(in ms)

%find closest values in (rounded) EEG.times to the specified start/stop
[temp,startIdx] = min(abs(EEG.times-startTime));
[temp2,endIdx] = min(abs(EEG.times-endTime));

%baseline corrections
range = startIdx:endIdx;
allBase = squeeze(mean(allData(:,:,:,range),4));
allBase = mean(allData(:,:,:,range),4);

for i=1:size(allData,4)
    newData(:,:,:,i) = allData(:,:,:,i) - allBase;
end

%start and end time range for component of interest
compStartTime = compTime(1); %(in ms)
compEndTime = compTime(2) ; %(in ms)

%find closest values in (rounded) EEG.times to the specified start/stop
[temp,compStartIdx] = min(abs(EEG.times-compStartTime));
[temp2,compEndIdx] = min(abs(EEG.times-compEndTime));

%idxs of time range to plot topo for
compRange = compStartIdx:compEndIdx;

%pull out conditions of interest for all subs, and average over time
%range of interest
resp_incon_error_avgTime = mean(newData(:,1,:,compRange),4);
resp_incon_corr_avgTime = mean(newData(:,2,:,compRange),4);

%average cluster of interest
resp_incon_error_avgTimeClust = mean(resp_incon_error_avgTime(:,:,compCluster),3);
resp_incon_corr_avgTimeClust = mean(resp_incon_corr_avgTime(:,:,compCluster),3);

%compute difference scores
resp_incon_error_avgTimeClust_diff = resp_incon_error_avgTimeClust - resp_incon_corr_avgTimeClust;

%write sub numbers to ouput
output(:,1) = subjects';

%write component means to output
output(:,2) = resp_incon_error_avgTimeClust;
output(:,3) = resp_incon_corr_avgTimeClust;
output(:,4) = resp_incon_error_avgTimeClust_diff;
        
%write component means to disk
dlmwrite(strcat('erpCore_compMeans_example_', date, '.csv'), output, 'delimiter', ',', '-append');
