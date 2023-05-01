
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
output_location = [main_dir filesep 'derivatives' filesep 'eeg' filesep 'preprocessed'];

% 3. Enter the path of the channel location file
channel_locations = [main_dir filesep 'code' filesep 'eeg' filesep 'MADE-EEG-preprocessing-pipeline' filesep 'chan_locs_files' filesep 'CACS-128-X7-FIXED-64only.bvef'];

% enter the stimulus makers that need to be adjusted for time offset
stimulus_markers = {'S  5', 'S  6', 'S  7', 'S  8'};      % enter the stimulus makers that need to be adjusted for time offset
response_markers = {'S 11', 'S 12', 'S 21', 'S 22'};

% Read files to analyses
% Keeping filenames that have more than 6 error trials.
trialCounts = readtable([output_location filesep 'mini_mfe_trial_counts.csv']); % loads the output of trialCounts script.
trialCounts = trialCounts(trialCounts.stim_incon_error > 6, :); 
datafile_names = trialCounts.fileName;
datafile_names = datafile_names';

% Check whether EEGLAB and all necessary plugins are in Matlab path.
if exist('eeglab','file')==0
    error(['Please make sure EEGLAB is on your Matlab path. Please see EEGLAB' ...
        'wiki page for download and instalation instructions']);
end

% Create output folders to save data
if exist([output_location filesep 'processed_data'], 'dir') == 0
    mkdir([output_location filesep 'processed_data'])
end


%% pull erp mat file

%this list can be created automatically by additional code that searches
%the trial counts file, etc. Here, just hard coding (presumably based on
%manual inspection of trial counts file and other notes/parameters)

    
%initialize participant counter variable (used for indexing into large mat
%file that data is saved into)
pIdx = 0;

%array to store erp data (participants x conditions x channels x samples of avg epoch data) 
% Kia: samples of avg epoch data = number of sample points within each
% epoch (i.e., EEG.pnts)
mini_mfe_erpDat_probeLoc = zeros(length(datafile_names),7,64,3000);

% loop through each participant in the study
for i = 1:length(datafile_names)
    
    pIdx = pIdx + 1;

    %load in raw data that is alread in eeglab (.set) format)
    EEG = pop_loadset( 'filename', datafile_names{i}, 'filepath', data_location);
    EEG = eeg_checkset(EEG);

    %%%%%%%% Kia :
    % replacing string values in EEG.event.eventType and congruency with
    % numerical values
    for hk = 1:size(EEG.event,2)
        if (strcmp(EEG.event(hk).eventType,'NaN'))
            EEG = setfield(EEG,'event',{hk},'eventType',0); 
        elseif (strcmp(EEG.event(hk).eventType, 'stim'))
            EEG = setfield(EEG,'event',{hk},'eventType',1); 
        elseif (strcmp(EEG.event(hk).eventType, 'resp'))
            EEG = setfield(EEG,'event',{hk},'eventType',2); 
        end
        if (strcmp(EEG.event(hk).congruency, 'i'))
            EEG = setfield(EEG,'event',{hk},'congruency',0); 
        elseif (strcmp(EEG.event(hk).congruency, 'c'))
            EEG = setfield(EEG,'event',{hk},'congruency',1); 
        end
    end

    %%%%%%%%%%%% 
    %remove all the non-stim-locking markers (should have done already...)
    EEG = pop_selectevent( EEG, 'latency','-.1 <= .1','deleteevents','on');
	EEG = eeg_checkset( EEG );

    % loop through 4 conditions (combinations of event types for stim)
    for c = 1:7
        try % Kia: using try catch under this loop to make the loop continue in case of an error for a certain participant. 
            % eventType = 1 means "Stim"
            % eventType = 2 means "Resp"
            % incong = 0
            % cong = 1
            eventType = 1;
            if (c==1) % incong_error
                congruency = 0;
                accuracy   = 0;
            elseif (c==2) % incong_correct
                congruency = 0;
                accuracy   = 1;
            elseif (c==3) % cong_error
                congruency = 1;
                accuracy   = 0;
            elseif (c==4) % cong_correct
                congruency = 1;
                accuracy   = 1;
            end

            %select combintion of event types of interest based on vars above
            EEG1 = pop_selectevent( EEG, 'latency','-1<=1','eventType',eventType ,'deleteevents','on','deleteepochs','on','invertepochs','off');
            EEG1 = eeg_checkset( EEG1 );

            EEG1 = pop_selectevent( EEG1, 'latency','-1<=1','congruency',congruency,'deleteevents','on','deleteepochs','on','invertepochs','off');
            EEG1 = eeg_checkset( EEG1 );

            EEG1 = pop_selectevent( EEG1, 'latency','-1<=1','accuracy',accuracy,'deleteevents','on','deleteepochs','on','invertepochs','off');
            EEG1 = eeg_checkset( EEG1 );

            % Average across epoch dimension
            % this all Channel ERP only needs to be computed once
            % per condition
            meanEpochs = mean(EEG1.data, 3); % Kia: averages across epochs of this condition

            %store data for this condition in array
            mini_mfe_erpDat_stim(pIdx,c,:,:)= meanEpochs;
        catch ME
            % Nothing to do.
            % When an error happens. This will make the loop to continue.
            fprintf('Error for this participant: %s\n', ME.message);
        end % end try/catch
        

    %end loop through conditions
    end
 
%end loop through participants
end

%save the erps and subject list
save('mini_mfe_erpDat_stim.mat','mini_mfe_erpDat_stim', 'datafile_names')




%% Plot ERPs!!

%load the mat file that has the erps and subject list
load([output_location filesep 'mini_mfe_erpDat_stim.mat'])


%make a copy/rename the erp matrix 
allData = mini_mfe_erpDat_stim;


%load in one of the participants EEGLAB-formatted data; this is to load
%parameters needed for plotting (sampling rate, chanlocs, etc).
EEG = pop_loadset( 'filename', datafile_names{1}, 'filepath', data_location);
EEG = eeg_checkset(EEG);

%if not whole numbers already, then round EEG.times to nearest whole ms to make easier to work with
EEG.times = round(EEG.times);

%setup for baseline correcting the ERP data (always done before plotting or extracting
%erps, not done to the data previously to allow use of different baselines
%as a function of review comments)
startTime = -350; %(in ms)
endTime = -150 ; %(in ms)

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
 chan = (newData(:,:,[2, 1, 33, 34],:)); 
  chan = (newData(:,:,[50, 53, 55, 58],:)); 




%% 

%average over selectd channels
chan = mean(chan,3);

%pull out 3 conditions of interest for all subs
resp_incon_err = chan(:,1,:,:);
resp_incon_corr = chan(:,2,:,:);
resp_con_corr = chan(:,4,:,:);


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
legendHandle = legend('Error', 'Correct');
set(legendHandle, 'box', 'off', 'FontSize', 26);
hold off;

% set parameters
plotStartTime = -1000; %(in ms)
plotEndTime = 1500 ; %(in ms)
set(gcf, 'Color', [1 1 1]);
set(gca, 'YLim', [-9 6]);
set(gca, 'XLim', [plotStartTime plotEndTime]);
set(gca, 'FontSize', 20);
set(get(gca, 'YLabel'), 'String', 'Amplitude in  \muV', 'FontSize', 26);
set(get(gca, 'XLabel'), 'String', 'Time Relative to Response (ms)', 'FontSize', 26);
set(gca, 'Box', 'off');
set(gcf, 'Position', [0 0 1440 900]);



