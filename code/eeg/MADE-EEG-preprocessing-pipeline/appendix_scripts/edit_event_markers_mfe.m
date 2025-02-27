%
%%%%%% Label event markers%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This script labels the mini mfe task data. Labelling includes basic
% information about stimulus type and responses, as well as exhaustive 
% labeling of prior/next trial data.



%% event codes from task
% stimulus triggers
% Right congruent : S 5
% Left cong: S 6
% Right incong: S 7
% Left incong: S 8

%correct response: 11
%error response: 12
%technically correct response, but not the first response made: 21
%technically error response, but not the first response made: 22
    
%all stim and resp markers for the task
all_stimMarkers = {'S  5', 'S  6', 'S  7', 'S  8'};
all_respMarkers = {'S 11', 'S 12', 'S 21', 'S 22'}; 

%subsets of stim/resp markers to be used in switch statements when
%labelling
% practice_stimMarkers = {'S  1', 'S  2', 'S  3', 'S  4'}; for mini mfe
% practice triggers are not differenet from the main task triggers.
mainTask_stimMarkers = {'S  5', 'S  6', 'S  7', 'S  8'};


rightTarDir_stimMarkers = {'S  5', 'S  7'};
leftTarDir_stimMarkers = {'S  6', 'S  8'};

congruent_stimMarkers = {'S  5', 'S  6'};
incongruent_stimMarkers = {'S  7', 'S  8'};

first_RespMarkers = {'S 11', 'S 12'}; 
extra_RespMarkers = {'S 21', 'S 22'}; 

corr_RespMarkers = {'S 11'}; 
error_RespMarkers = {'S 12'}; 

%specify cutoff (in s) for how fast a valid rt can be
validRt_cutoff = .150;


%% Add labels to the event structure 
EEG = pop_editeventfield( EEG, 'indices',  strcat('1:', int2str(length(EEG.event))), ...
    'eventType','NaN', 'targetDir','NaN', 'congruency','NaN', 'responded','NaN', 'accuracy','NaN', ...
    'rt','NaN', 'validRt','NaN', 'extraResponse','NaN', 'validTrial','NaN', ...
    'prevTargetDir','NaN', 'prevCongruency','NaN', 'prevResponded','NaN', ...
    'prevAccuracy','NaN', 'prevRt','NaN', 'prevValidRt','NaN', ...
    'prevExtraResponse','NaN', 'prevValidTrial','NaN', ...
    'nextTargetDir','NaN', 'nextCongruency','NaN', 'nextResponded','NaN', 'nextAccuracy','NaN', ...
    'nextRt','NaN', 'nextValidRt','NaN', 'nextExtraResponse','NaN', ...
    'nextValidTrial','NaN');
EEG = eeg_checkset( EEG );

%note:accuracy and rt always corresponds to first response
% 'eventType' - stim, resp
% 'targetDir' - l, r
% 'congruency' - c, i
% 'responded' - 1, 0 as str
% 'accuracy' - 1, 0 as str copied from trsp
% 'rt' - 
% 'validRt' - 
% 'extraResponse' - 1, 0 as str
% 'validTrial' - 1 if single, validRt response, else 0

% 'prevTargetDir' - l, r
% 'prevCongruency' - c, i
% 'prevResponded' - 1, 0 as str
% 'prevAccuracy' - 1, 0 as str copied from trsp
% 'prevRt' - 
% 'prevValidRt' - 
% 'prevExtraResponse' - 1, 0 as str
% 'prevValidTrial' - 1 if single, validRt response, else 0

% 'nextTargetDir' - l, r
% 'nextCongruency' - c, i
% 'nextResponded' - 1, 0 as str
% 'nextAccuracy' - 1, 0 as str copied from trsp
% 'nextRt' - 
% 'nextValidRt' - 
% 'nextExtraResponse' - 1, 0 as str
% 'nextValidTrial' - 1 if single, validRt response, else 0

%% loop to label markers

% load check mini mfe eeg csv file. In this file, we have a column that
% shows almost when the first flanker block starts. We do not label
% practice trials. Using this csv file, we will exclude practice trials.


mini_mfe_eeg_check_csv = table2array(readtable('/Users/kihossei/Documents/GitHub/memory-for-error-mini/input/mini_mfe_eeg_check_without_notes.csv'));
% finding current participant id 
current_participant_id = str2double(extractBefore(EEG.setname,'_eeg-mfe_s1_r1_e1'));
% finding the index number in the mini_mfe_eeg_check_csv table . Then, we can use this idx to
% select the row that corresponds to the current participant

id_col_array = mini_mfe_eeg_check_csv(:,1); % an array that stores values from the id column

latency_col_array = mini_mfe_eeg_check_csv(:,3); % an array that stores values from the latency_main column. 
% The unit of values in latency_main is in seconds.
participant_row_idx = find(ismember(id_col_array, current_participant_id));
latency_for_the_current_participant = latency_col_array(participant_row_idx); % in seconds
% stim and resp markers after the identified time in the third column of
% the mini_mfe_eeg_check_csv file will be labeled. 



all_stimMarkers_eventNums = find(ismember({EEG.event.type},mainTask_stimMarkers));
new_all_stimMarkers_eventNums = [];
for t = all_stimMarkers_eventNums
    if ((EEG.event(t).latency/EEG.srate) > latency_for_the_current_participant) % checks to see if the chosen stim event is happening after the
        % identified latency for the current participant or not. If it is
        % after that, it means it belongs to the main flanker task and we add it
        % to new_all_stimMarkers_eventNums.
        new_all_stimMarkers_eventNums = [new_all_stimMarkers_eventNums, t];
    end
end
all_stimMarkers_eventNums = new_all_stimMarkers_eventNums;

%loop through all stim marker event numbers identified in the all_stimMarkers_eventNums vector and label 
for t = all_stimMarkers_eventNums %t = event numbers stored in all_stimMarkers_eventNums
    
    stimEventNum = t;
    
    %label that this is a stimulus event
    EEG.event(t).eventType = 'stim';
    
    %find which TRIAL # this is, useful for context labelling
    stimTrialNum = find(all_stimMarkers_eventNums==t);

    %figure out what the next Trsp EVENT # is, unless this is the last
    %trial in a block. we determine if two stimuli are from seperate blocks
    %by checking the amount of time between events. The ITI is 3.5-4 sec
    %for this task, so each trial shoulod be within <4 sec of each other
    %(we use a cutoff just above this at 5 s).
    if stimTrialNum == length(all_stimMarkers_eventNums)
        nextStimEventNum = 0;
    elseif (EEG.event(all_stimMarkers_eventNums(stimTrialNum+1)).latency -  EEG.event(all_stimMarkers_eventNums(stimTrialNum)).latency)/EEG.srate < 5
        nextStimEventNum = all_stimMarkers_eventNums(stimTrialNum+1);
    else
        nextStimEventNum = 0;
    end

    %figure out what the previous Trsp EVENT # is, unless this is the
    %first trial in a block. we determine if two stimuli are from seperate blocks
    %by checking the amount of time between events. The ITI is 3.5-4 sec
    %for this task, so each trial shoulod be within <2 sec of each other
    %(we use a cutoff just above this at 5 s).
    if stimTrialNum == 1
        prevStimEventNum = 0;
    elseif (EEG.event(all_stimMarkers_eventNums(stimTrialNum)).latency -  EEG.event(all_stimMarkers_eventNums(stimTrialNum-1)).latency)/EEG.srate < 5
        prevStimEventNum = all_stimMarkers_eventNums(stimTrialNum-1);
    else
        prevStimEventNum = 0;
    end

    
    %figure out if a response was made. if so, identify event # of
    %response marker, rt, rt validity, accuracy, presence of extra responses.
    switch EEG.event(t+1).type      
        
        case first_RespMarkers %if there was a response             
            
            EEG.event(t+1).eventType = 'resp';  
            responded = 1;   
            respEventNum = t+1;
            
            %if there was a response, then get rt of response 
            rt = (EEG.event(t+1).latency - EEG.event(t).latency)/EEG.srate;
            
            %if there was a response, then determine if rt is valid or not
            if rt > validRt_cutoff
                validRt = 1;
            else 
                validRt = 0;
            end  
                      
            %if there was a response, then get accuracy of response
            switch EEG.event(t+1).type
                case corr_RespMarkers %if correct response 
                    accuracy = 1;   
                case error_RespMarkers %if error response                
                    accuracy = 0;                 
            end
            
            %if there was a response, then look if there was at least one
            %extra response after the first response
            switch EEG.event(t+2).type
                case extra_RespMarkers %if at least one extra response  
                    extraResponse = 1;   
                otherwise %if NO response                
                    extraResponse = 0;                 
            end
            
            %deterimine if a valid trial or not (single, valid rt response)
            if responded == 1 && validRt == 1 && extraResponse == 0
                validTrial = 1;
            else
                validTrial = 0;
            end
            
        otherwise %if NO response                
            responded = 0;   
            extraResponse = 0; 
            respEventNum = 0; 
            accuracy = 0;
            rt = 0;
            validRt = 0;
            validTrial = 0;
            
    end
     
    %loop to label 1/2 events per trial
    for eventNum = [stimEventNum respEventNum]

        %if trial had no response, RespEventNum will be zero
        if eventNum ~= 0
            
            %label observation condition (nonsocial vs social)
            %switch EEG.event(t).type     
             %   case ns_stimMarkers
              %      EEG.event(eventNum).observation = 'ns';
               % case s_stimMarkers
                %    EEG.event(eventNum).observation = 's';
           % end
            
            %label stimulus target direction (right vs left)
            switch EEG.event(t).type     
                case rightTarDir_stimMarkers
                    EEG.event(eventNum).targetDir = 'r';
                case leftTarDir_stimMarkers
                    EEG.event(eventNum).targetDir = 'l';
            end
            
            %label stimulus congruency (congruent vs incongruent)
            switch EEG.event(t).type     
                case congruent_stimMarkers
                    EEG.event(eventNum).congruency = 'c';
                case incongruent_stimMarkers
                    EEG.event(eventNum).congruency = 'i';
            end

            %label whether response for this trial or not
            EEG.event(eventNum).responded = responded;
            
            %label whether there were extra responses for this trial or not
            EEG.event(eventNum).extraResponse = extraResponse;
            
            %label accuracy for this trial
            EEG.event(eventNum).accuracy = accuracy;
            
            %label rt for this trial
            EEG.event(eventNum).rt = rt;
            
            %label whether rt was valid for this trial or not
            EEG.event(eventNum).validRt = validRt;
            
            %label whether valid trial or not (single, valid rt response)
            EEG.event(eventNum).validTrial = validTrial;
            
            
            %If this is not the last trial in a block, then also label the
            %current stim event based on the next trial
            if nextStimEventNum ~= 0
          
                %figure out if a response was made on the NEXT TRIAL. if so, identify event # of
                %response marker, rt, rt validity, accuracy, presence of extra responses.
                switch EEG.event(nextStimEventNum+1).type      

                    case first_RespMarkers %if there was a response             
                        nextResponded = 1;   
                        nextRespEventNum = nextStimEventNum+1;

                        %if there was a response, then get rt of response 
                        nextRt = (EEG.event(nextStimEventNum+1).latency - EEG.event(nextStimEventNum).latency)/EEG.srate;

                        %if there was a response, then determine if rt is valid or not
                        if nextRt > validRt_cutoff
                            nextValidRt = 1;
                        else 
                            nextValidRt = 0;
                        end  

                        %if there was a response, then get accuracy of response
                        switch EEG.event(nextStimEventNum+1).type
                            case corr_RespMarkers %if correct response 
                                nextAccuracy = 1;   
                            case error_RespMarkers %if error response                
                                nextAccuracy = 0;                 
                        end

                        %if there was a response, then look if there was at least one
                        %extra response after the first response
                        switch EEG.event(nextStimEventNum+2).type
                            case extra_RespMarkers %if at least one extra response  
                                nextExtraResponse = 1;   
                            otherwise %if NO response                
                                nextExtraResponse = 0;                 
                        end
             
                        %deterimine if a valid trial or not (single, valid rt response)
                        if nextResponded == 1 && nextValidRt == 1 && nextExtraResponse == 0
                            nextValidTrial = 1;
                        else
                            nextValidTrial = 0;
                        end

                    otherwise %if NO response                
                        nextResponded = 0;   
                        nextExtraResponse = 0; 
                        nextRespEventNum = 0; 
                        nextAccuracy = 0;
                        nextRt = 0;
                        nextValidRt = 0;
                        nextValidTrial = 0;

                end %end switch to determine if response made on next trial

                %label next stimulus target direction (right vs left)
                switch EEG.event(nextStimEventNum).type     
                    case rightTarDir_stimMarkers
                        EEG.event(eventNum).nextTargetDir = 'r';
                    case leftTarDir_stimMarkers
                        EEG.event(eventNum).nextTargetDir = 'l';
                end

                %label next stimulus congruency (congruent vs incongruent)
                switch EEG.event(nextStimEventNum).type     
                    case congruent_stimMarkers
                        EEG.event(eventNum).nextCongruency = 'c';
                    case incongruent_stimMarkers
                        EEG.event(eventNum).nextCongruency = 'i';
                end

                %label this trial based on whether response on next trial or not
                EEG.event(eventNum).nextResponded = nextResponded;

                %label this trial based on whether there were extra responses on next trial or not
                EEG.event(eventNum).nextExtraResponse = nextExtraResponse;

                %label this trial based on accuracy of next trial
                EEG.event(eventNum).nextAccuracy = nextAccuracy;

                %label this trial based on rt of next trial
                EEG.event(eventNum).nextRt = nextRt;

                %label this trial based on whether rt was valid for next trial or not
                EEG.event(eventNum).nextValidRt = nextValidRt;
                
                %label this trial based on whether next trial is a valid trial or not (single, valid rt response)
                EEG.event(eventNum).nextValidTrial = nextValidTrial;
 
            end %end conditional: if nextStimEventNum ~= 0
            
            %If this is not the first trial in a block, then also label the
            %current stim event based on the prior trial
            if prevStimEventNum ~= 0
 
                %figure out if a response was made on the PREVIOUS TRIAL. if so, identify event # of
                %response marker, rt, rt validity, accuracy, presence of extra responses.
                switch EEG.event(prevStimEventNum+1).type      

                    case first_RespMarkers %if there was a response             
                        prevResponded = 1;   
                        prevRespEventNum = prevStimEventNum+1;

                        %if there was a response, then get rt of response 
                        prevRt = (EEG.event(prevStimEventNum+1).latency - EEG.event(prevStimEventNum).latency)/EEG.srate;

                        %if there was a response, then determine if rt is valid or not
                        if prevRt > validRt_cutoff
                            prevValidRt = 1;
                        else 
                            prevValidRt = 0;
                        end  

                        %if there was a response, then get accuracy of response
                        switch EEG.event(prevStimEventNum+1).type
                            case corr_RespMarkers %if correct response 
                                prevAccuracy = 1;   
                            case error_RespMarkers %if error response                
                                prevAccuracy = 0;                 
                        end

                        %if there was a response, then look if there was at least one
                        %extra response after the first response
                        switch EEG.event(prevStimEventNum+2).type
                            case extra_RespMarkers %if at least one extra response  
                                prevExtraResponse = 1;   
                            otherwise %if NO response                
                                prevExtraResponse = 0;                 
                        end
                              
                        %deterimine if a valid trial or not (single, valid rt response)
                        if prevResponded == 1 && prevValidRt == 1 && prevExtraResponse == 0
                            prevValidTrial = 1;
                        else
                            prevValidTrial = 0;
                        end

                    otherwise %if NO response                
                        prevResponded = 0;   
                        prevExtraResponse = 0; 
                        prevRespEventNum = 0; 
                        prevAccuracy = 0;
                        prevRt = 0;
                        prevValidRt = 0;
                        prevValidTrial = 0;

                end %end switch to determine if response made on previous trial            

                %label prev stimulus target direction (right vs left)
                switch EEG.event(prevStimEventNum).type     
                    case rightTarDir_stimMarkers
                        EEG.event(eventNum).prevTargetDir = 'r';
                    case leftTarDir_stimMarkers
                        EEG.event(eventNum).prevTargetDir = 'l';
                end

                %label prev stimulus congruency (congruent vs incongruent)
                switch EEG.event(prevStimEventNum).type     
                    case congruent_stimMarkers
                        EEG.event(eventNum).prevCongruency = 'c';
                    case incongruent_stimMarkers
                        EEG.event(eventNum).prevCongruency = 'i';
                end

                %label this trial based on whether response on prev trial or not
                EEG.event(eventNum).prevResponded = prevResponded;

                %label this trial based on whether there were extra responses on prev trial or not
                EEG.event(eventNum).prevExtraResponse = prevExtraResponse;

                %label this trial based on accuracy of prev trial
                EEG.event(eventNum).prevAccuracy = prevAccuracy;

                %label this trial based on rt of prev trial
                EEG.event(eventNum).prevRt = prevRt;

                %label this trial based on whether rt was valid for prev trial or not
                EEG.event(eventNum).prevValidRt = prevValidRt;
  
                %label this trial based on whether prev trial is a valid trial or not (single, valid rt response)
                EEG.event(eventNum).prevValidTrial = prevValidTrial;
                
            end %end conditional: if prevStimEventNum ~= 0

        end %end if eventNum ~=0 conditional

    end %end loop through eventNum (stim, resp) for this trial

end %end loop through all_stimMarkers_eventNums (all trials)           
