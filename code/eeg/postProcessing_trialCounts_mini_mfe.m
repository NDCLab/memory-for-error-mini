%
%%to Run on FIU HPC%
% create a local cluster object
cluster = parcluster('local');

% start matlabpool with max workers set in the slurm file
parpool(cluster, str2num(getenv('SLURM_CPUS_ON_NODE')))


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
output_location = [main_dir filesep 'derivatives' filesep 'mini_mfe_eeg' filesep 'preprocessed'];

% 3. Enter the path of the channel location file
channel_locations = [main_dir filesep 'code' filesep 'mini_mfe_eeg' filesep 'MADE-EEG-preprocessing-pipeline' filesep 'chan_locs_files' filesep 'CACS-128-X7-FIXED-64only.bvef'];

% enter the stimulus makers that need to be adjusted for time offset
stimulus_markers = {'S  5', 'S  6', 'S  7', 'S  8'};      % enter the stimulus makers that need to be adjusted for time offset
response_markers = {'S 11', 'S 12', 'S 21', 'S 22'};

% Read files to analyses
datafile_names=dir([data_location filesep '*.set']);
datafile_names=datafile_names(~ismember({datafile_names.name},{'.', '..', '.DS_Store'}));
datafile_names={datafile_names.name};
[filepath,name,ext] = fileparts(char(datafile_names{1}));

% Check whether EEGLAB and all necessary plugins are in Matlab path.
if exist('eeglab','file')==0
    error(['Please make sure EEGLAB is on your Matlab path. Please see EEGLAB' ...
        'wiki page for download and instalation instructions']);
end

% Create output folders to save data
if exist([output_location filesep 'processed_data'], 'dir') == 0
    mkdir([output_location filesep 'processed_data'])
end


%% Count trials

% switch to output directory
cd(output_location);

for subject=1:length(datafile_names)
    
    EEG=[];
    
    fprintf('\n\n\n*** Processing subject %d (%s) ***\n\n\n', subject, datafile_names{subject});
    
    %load in raw data that is alread in eeglab (.set) format)
    EEG = pop_loadset( 'filename', datafile_names{subject}, 'filepath', data_location);
    EEG = eeg_checkset(EEG);
    
    
    %remove all the non-stim-locking markers (should have done already...)
    EEG = pop_selectevent( EEG, 'latency','-.1 <= .1','deleteevents','on');
	EEG = eeg_checkset( EEG );

    %count how many of each event type (combination of event types) of
    %interest are present   
    
    resp_incon_error = length(find( ([EEG.event.eventType] == 'resp') & ([EEG.event.congruency] == 0) & ([EEG.event.accuracy] == 0) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));             
    resp_incon_correct = length(find( ([EEG.event.eventType] == 'resp') & ([EEG.event.congruency] == 0) & ([EEG.event.accuracy] == 1) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));                         
    resp_con_correct = length(find( ([EEG.event.eventType] == 'resp') & ([EEG.event.congruency] == 1) & ([EEG.event.accuracy] == 1) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));             
    
    stim_incon_error = length(find( ([EEG.event.eventType] == 'stim') & ([EEG.event.congruency] == 0) & ([EEG.event.accuracy] == 0) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));             
    stim_incon_correct = length(find( ([EEG.event.eventType] == 'stim') & ([EEG.event.congruency] == 0) & ([EEG.event.accuracy] == 1) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));             
    stim_con_error = length(find( ([EEG.event.eventType] == 'stim') & ([EEG.event.congruency] == 1) & ([EEG.event.accuracy] == 0) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));             
    stim_con_correct = length(find( ([EEG.event.eventType] == 'stim') & ([EEG.event.congruency] == 1) & ([EEG.event.accuracy] == 1) & ([EEG.event.responded] == 1) & ([EEG.event.validRt] == 1)  ));             
    
    %Create the trial counts table for trial counts
    output = [datafile_names{subject}, resp_incon_error, resp_incon_correct, resp_con_correct, stim_incon_error, stim_incon_correct, stim_con_error, stim_con_correct];
    %counts_table=table({datafile_names{subject}}, {resp_incon_error}, {resp_incon_correct}, {resp_con_correct}, {stim_incon_error}, {stim_incon_correct}, {stim_con_error}, {stim_con_correct});
    
    %create variable names for count trials output and write to disk
    %counts_table.Properties.VariableNames = {'fileName', 'resp_incon_error', 'resp_incon_correct', 'resp_con_correct', 'stim_incon_error', 'stim_incon_correct', 'stim_con_error', 'stim_con_correct'};

    %write/append table to disk
    dlmwrite(strcat('mini_mfe_trialCounts_', date, '.csv'), output, 'delimiter', ',', '-append');
    %writetable(counts_table, [output_location filesep 'mini_mfe_trial_counts.csv'], "WriteMode", "append");
    
end

