

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

%% Extract mean component amplitudes (for statistics)

%clear output
output = [];

% Creating an array that stores Subjects IDs!
subjects_id = datafile_names;
new_subjects_id = zeros(length(subjects_id),1);
for ttt = 1:length(subjects_id)
    new_subjects_id(ttt,1) = str2double(extractBefore(subjects_id(ttt), '_eeg'));
end
subjects_id = new_subjects_id;


%define electrodes average over for component
compCluster = [1, 2, 34, 33]; 

%define timeRange to average over for component 
compTime = [0 100]; 

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
output(:,1) = subjects_id;

%write component means to output
output(:,2) = resp_incon_error_avgTimeClust;
output(:,3) = resp_incon_corr_avgTimeClust;
output(:,4) = resp_incon_error_avgTimeClust_diff;

output = table(output(:,1), output(:,2), output(:,3), output(:,4));
output.Properties.VariableNames = {'id','ERN','CRN','deltaERN'};

%write component means to disk
writetable(output, [output_location filesep 'mini_mfe_erpDat_ERN_compMeans.csv'])






