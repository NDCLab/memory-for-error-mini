
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

    % loop through 3 conditions (combinations of event types for resp)
    for c = 1:3
        % eventType: 1 = "Stim"; 2 = "Resp"
        % congruency:  1= congruent; 0 = incongruent
        % accuracy: 1 = corect; 0 = error
        
        %selecting only response epochs
        eventType  = 2;

        if (c==1) %incon-error
            congruency = 0;
            accuracy   = 0; 
        elseif (c==2) %incon-corr
            congruency = 0;
            accuracy   = 1;
        elseif (c==3) %con-corr
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
        mini_mfe_erpDat_resp(pIdx,c,:,:)= meanEpochs;
        

    %end loop through conditions
    end
 
%end loop through participants
end

%save the erps and subject list
save([output_location filesep 'mini_mfe_erpDat_resp.mat'],'mini_mfe_erpDat_resp', 'datafile_names')



