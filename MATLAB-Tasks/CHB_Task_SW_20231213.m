

%        b. Department of Experimental Psychology, Faculty of
%           Mathematics and Natural Sciences, Heinrich-Heine University,
%           Düsseldorf, Germany

% VERSION HISTORY:    > 22.09.23, Final version with VAS trials (FH) 
%                     > 14.09.23, Working Version 2 (pre-pilotting) (SM)  
%                     > 04.09.23, Working Version 1 (pre-pilotting) (FH)
%                     > 31.08.23, Skeleton_translated (SM)
%                     > NN.08.23, Skeleton (FH)

% PORT:               COM-"N", serial port (PC-TCS)

% DEVICES:            TCS-II-1, Thermal Stimulator (QST.lab, FR) (v.14.39.16) 

% INPUTS:             1. Participant ID: (CHB_Pain_(unique identifier))
%                     2. Testing Session(1-2)
%                     3. bloc: (1-12)
%                     4. Reference temperature: (individually scaled or fixed)

% BLOC DURATION:      ~ to be piloted

% LEGENDS ----------------------------------------------------------------%

% TRIAL TYPE:         1. Test
%                     2. VAS

% STIM TYPE:          1. Stronger
%                     2. Weaker

% STIMULUS CHOICE:    1. Stronger
%                     2. Weaker
%                     3. None
%=========================================================================%
%% (param)
%=========================================================================%
% PARAMETERS - SET-UP
%=========================================================================%

% UTILITY ----------------------------------------------------------------%
sca;
close all;
clearvars;

% TCS SET-UP -------------------------------------------------------------%

% Configure serial port

    TCS = serialport("COM3",115200,"Timeout",1);        % put back to "COM19" if in testing room (FH)
    write(TCS, 'F', 'char');                            % Puts TCS in "quiet mode", helps to limit information sent through port and increases reliability
    flush(TCS, 'output');

% Setting Baseline temperature / putting to baseline

    baseline_temp = 35;                                 % baseline temperature             
    baseline = sprintf( 'N%03d', baseline_temp*10 );    % converts integer into string array that will be sent to tcs
    write(TCS, baseline, 'char');                       % sends information to the serial port
    flush(TCS, 'output');

% Configuring Ramp-Up speed 

    RampUp = 75;                                         % for the MR compatible probes the max ramp is limited to 75°C/s 
    ramp_speed = [ RampUp RampUp RampUp RampUp RampUp ]; % sets the ramp speed to be equal for all (5) pelltier elements of the connected probe
    ramp = sprintf( 'V0%04d', ramp_speed(1)*10 );       
    write(TCS, ramp, 'char');                           
    flush(TCS, 'output');  

% Configuring Return-Down speed

    ReturnDown = 75;
    return_speed = [ ReturnDown ReturnDown ReturnDown ReturnDown ReturnDown ]; 
    Return = sprintf( 'R0%04d', return_speed(1)*10 );      
    write(TCS, Return, 'char');                         
    flush(TCS, 'output');

% Setting durations, changed in the process but needs to be bigger than max

    Dur = 1;                          %dummy variable 
    durations = [ Dur Dur Dur Dur Dur ]; 
    stim_dur = sprintf( 'D0%05d', durations(1)*1000 );
    write(TCS, stim_dur, 'char');                         
    flush(TCS, 'output');

% INPUT LOGS -------------------------------------------------------------%

inputs = {'subj_idx', 'session', 'Trial Sequence', 'bloc', 'reference'};
defaults = {'0000', '0', '0', '0', '54'};
information = inputdlg(inputs, '2IFC_PAIN', 2, defaults);
[subj_idx, session, trial_seq, bloc, reference] = deal(information{:});

Subj_Idx  = subj_idx;
Session = str2num(session);
Bloc = str2num(bloc);
Trial_Seq = str2num(trial_seq);
Reference = str2num(reference);
reference_temp = sprintf( 'C0%03d', Reference(1)*10 );

% OUTPUT LOGS ------------------------------------------------------------%

DefaultName = strcat('CHBpain_', Subj_Idx,'_Session_', session,'_Bloc_', bloc);
[FileName,PathName,FilterIndex] = uiputfile('*.csv','Please select path for output file',DefaultName);
cd(PathName);
outputfile = fopen(FileName,'w+'); 	
fprintf(outputfile, 'subj_idx\t session\t bloc\t reference\t trial_id\t trial_type\t stim_type\t delta\t stim_choice\t vas\t rt\n');

% TRIAL SEQUENCES --------------------------------------------------------%

num_trials = 25;       %number of trials here (26)
if Trial_Seq == 1
    if Bloc == 1                                                                                     %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [6; 5; 6; 7; 8; 8; 4; 6; 10; 4; 9; 1; 7; 4; 8; 5; 7; 8; 2; 6; 7; 9; 2; 7; 8;];
            stim_type =  [2; 1; 2; 2; 2; 2; 1; 2; 2; 1; 2; 1; 2; 1; 2; 1; 2; 2; 1; 2; 2; 2; 1; 2; 2;];
    elseif Bloc == 2                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [4; 3; 4; 8; 10; 9; 3; 8; 5; 7; 1; 3; 3; 4; 1; 5; 10; 6; 9; 9; 7; 1; 10; 6; 7;];
            stim_type =  [1; 1; 1; 2; 2; 2; 1; 2; 1; 2; 1; 1; 1; 1; 1; 1; 2; 2; 2; 2; 2; 1; 2; 2; 2;];
    elseif Bloc == 3                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [6; 4; 10; 6; 3; 7; 6; 5; 2; 9; 1; 5; 7; 3; 5; 3; 2; 4; 10; 3; 4; 3; 10; 9; 1;];
            stim_type =  [2; 1; 2; 2; 1; 2; 2; 1; 1; 2; 1; 1; 2; 1; 1; 1; 1; 1; 2; 1; 1; 1; 2; 2; 1;];
    elseif Bloc == 4                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [3; 3; 10; 10; 5; 7; 7; 9; 4; 10; 1; 6; 3; 7; 5; 1; 7; 6; 6; 1; 6; 4; 8; 2; 4;];
            stim_type =  [1; 1; 2; 2; 1; 2; 2; 2; 1; 2; 1; 2; 1; 2; 1; 1; 2; 2; 2; 1; 2; 1; 2; 1; 1;];
    elseif Bloc == 5                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [7; 7; 3; 6; 1; 3; 8; 1; 2; 1; 5; 2; 1; 8; 6; 9; 8; 10; 6; 5; 5; 9; 2; 4; 8;];
            stim_type =  [2; 2; 1; 2; 1; 1; 2; 1; 1; 1; 1; 1; 1; 2; 2; 2; 2; 2; 2; 1; 1; 2; 1; 1; 2;];
    elseif Bloc == 6                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [6; 10; 1; 1; 10; 10; 2; 10; 10; 6; 9; 3; 9; 1; 6; 8; 7; 10; 7; 5; 1; 4; 2; 7; 9;];
            stim_type =  [2; 2; 1; 1; 2; 2; 1; 2; 2; 2; 2; 1; 2; 1; 2; 2; 2; 2; 2; 1; 1; 1; 1; 2; 2;];
    elseif Bloc == 7                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [3; 3; 9; 2; 3; 4; 7; 8; 4; 2; 9; 6; 10; 5; 3; 4; 3; 10; 3; 6; 3; 7; 6; 2; 3;];
            stim_type =  [1; 1; 2; 1; 1; 1; 2; 2; 1; 1; 2; 2; 2; 1; 1; 1; 1; 2; 1; 2; 1; 2; 2; 1; 1;];
    elseif Bloc == 8                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [8; 6; 5; 2; 8; 5; 7; 9; 5; 2; 7; 2; 4; 8; 2; 8; 4; 2; 7; 2; 10; 8; 2; 1; 6;];
            stim_type =  [2; 2; 1; 1; 2; 1; 2; 2; 1; 1; 2; 1; 1; 2; 1; 2; 1; 1; 2; 1; 2; 2; 1; 1; 2;];
    elseif Bloc == 9                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [2; 10; 2; 2; 7; 2; 5; 3; 1; 5; 4; 1; 7; 9; 8; 6; 10; 10; 6; 9; 5; 2; 8; 9; 8;];
            stim_type =  [1; 2; 1; 1; 2; 1; 1; 1; 1; 1; 1; 1; 2; 2; 2; 2; 2; 2; 2; 2; 1; 1; 2; 2; 2;];
    elseif Bloc == 10                                                                                %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [5; 3; 9; 9; 10; 2; 7; 7; 2; 9; 9; 8; 4; 5; 10; 4; 5; 2; 9; 2; 4; 1; 10; 8; 8;];
            stim_type =  [1; 1; 2; 2; 2; 1; 2; 2; 1; 2; 2; 2; 1; 1; 2; 1; 1; 1; 2; 1; 1; 1; 2; 2; 2;];
    elseif Bloc == 11                                                                                %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [5; 5; 8; 5; 3; 3; 10; 4; 6; 8; 4; 6; 10; 1; 8; 1; 3; 1; 7; 6; 10; 8; 8; 9; 5;];
            stim_type =  [1; 1; 2; 1; 1; 1; 2; 1; 2; 2; 1; 2; 2; 1; 2; 1; 1; 1; 2; 2; 2; 2; 2; 2; 1;];
    elseif Bloc == 12                                                                                %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [1; 5; 1; 1; 4; 9; 1; 3; 3; 9; 5; 5; 4; 9; 4; 4; 10; 1; 9; 9; 7; 4; 2; 1; 6;];
            stim_type =  [1; 1; 1; 1; 1; 2; 1; 1; 1; 2; 1; 1; 1; 2; 1; 1; 2; 1; 2; 2; 2; 1; 1; 1; 2;];   
    end
    
elseif Trial_Seq == 2
    
    if Bloc == 1                                                                                     %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [4; 10; 1; 1; 10; 7; 1; 8; 2; 5; 4; 6; 5; 8; 8; 3; 7; 2; 9; 1; 6; 2; 3; 5; 9;];
            stim_type =  [1; 2; 1; 1; 2; 2; 1; 2; 1; 1; 1; 2; 1; 2; 2; 1; 2; 1; 2; 1; 2; 1; 1; 1; 2;];
    elseif Bloc == 2                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [1; 3; 6; 9; 10; 8; 2; 10; 6; 2; 7; 2; 1; 2; 5; 5; 4; 3; 8; 9; 6; 10; 7; 5; 9;];
            stim_type =  [1; 1; 2; 2; 2; 2; 1; 2; 2; 1; 2; 1; 1; 1; 1; 1; 1; 1; 2; 2; 2; 2; 2; 1; 2;];
    elseif Bloc == 3                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [4; 4; 8; 6; 8; 7; 7; 5; 4; 10; 2; 7; 1; 9; 6; 10; 1; 10; 8; 9; 2; 4; 5; 8; 2;];
            stim_type =  [1; 1; 2; 2; 2; 2; 2; 1; 1; 2; 1; 2; 1; 2; 2; 2; 1; 2; 2; 2; 1; 1; 1; 2; 1;];
    elseif Bloc == 4                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [1; 7; 6; 2; 10; 9; 5; 10; 8; 8; 9; 3; 4; 7; 9; 8; 5; 2; 3; 1; 5; 3; 2; 4; 2;];
            stim_type =  [1; 2; 2; 1; 2; 2; 1; 2; 2; 2; 2; 1; 1; 2; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
    elseif Bloc == 5                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [8; 10; 7; 3; 2; 2; 9; 9; 3; 3; 9; 3; 3; 3; 4; 2; 9; 10; 1; 1; 4; 6; 8; 7; 3;];
            stim_type =  [2; 2; 2; 1; 1; 1; 2; 2; 1; 1; 2; 1; 1; 1; 1; 1; 2; 2; 1; 1; 1; 2; 2; 2; 1;];
    elseif Bloc == 6                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [7; 10; 10; 3; 2; 8; 9; 2; 6; 7; 9; 5; 1; 7; 4; 1; 6; 5; 4; 4; 4; 8; 1; 8; 4;];
            stim_type =  [2; 2; 2; 1; 1; 2; 2; 1; 2; 2; 2; 1; 1; 2; 1; 1; 2; 1; 1; 1; 1; 2; 1; 2; 1;];
    elseif Bloc == 7                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [4; 10; 8; 6; 9; 8; 7; 6; 3; 7; 10; 6; 5; 6; 6; 2; 5; 3; 8; 3; 2; 4; 5; 10; 3;];
            stim_type =  [1; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; 1; 2; 2; 1; 1; 1; 2; 1; 1; 1; 1; 2; 1;];
    elseif Bloc == 8                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [1; 1; 2; 4; 5; 10; 2; 6; 6; 1; 2; 6; 3; 7; 9; 10; 4; 10; 4; 9; 2; 1; 9; 5; 8;];
            stim_type =  [1; 1; 1; 1; 1; 2; 1; 2; 2; 1; 1; 2; 1; 2; 2; 2; 1; 2; 1; 2; 1; 1; 2; 1; 2;];
    elseif Bloc == 9                                                                                 %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [2; 8; 5; 7; 1; 9; 10; 4; 1; 7; 7; 3; 5; 7; 7; 3; 5; 3; 7; 6; 1; 5; 5; 7; 6;];
            stim_type =  [1; 2; 1; 2; 1; 2; 2; 1; 1; 2; 2; 1; 1; 2; 2; 1; 1; 1; 2; 2; 1; 1; 1; 2; 2;];
    elseif Bloc == 10                                                                                %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [9; 3; 4; 10; 1; 4; 10; 9; 9; 9; 9; 3; 1; 6; 9; 4; 1; 5; 8; 3; 9; 8; 4; 8; 1;];
            stim_type =  [2; 1; 1; 2; 1; 1; 2; 2; 2; 2; 2; 1; 1; 2; 2; 1; 1; 1; 2; 1; 2; 2; 1; 2; 1;];
    elseif Bloc == 11                                                                                %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [5; 10; 9; 8; 2; 6; 5; 1; 3; 6; 7; 8; 10; 5; 4; 4; 10; 8; 4; 2; 3; 8; 3; 2; 8;];
            stim_type =  [1; 2; 2; 2; 1; 2; 1; 1; 1; 2; 2; 2; 2; 1; 1; 1; 2; 2; 1; 1; 1; 2; 1; 1; 2;];
    elseif Bloc == 12                                                                                %
            trial_type = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;];
            delta =      [1; 4; 7; 5; 5; 10; 6; 3; 6; 10; 6; 5; 7; 10; 7; 2; 10; 6; 7; 6; 9; 1; 6; 1; 7;];
            stim_type =  [1; 1; 2; 1; 1; 2; 2; 1; 2; 2; 2; 1; 2; 2; 2; 1; 2; 2; 2; 2; 2; 1; 2; 1; 2;];
    end
    
end

% FIXED TIME LIMITS ------------------------------------------------------%

max_time_choice = 2.5;     % maximum time to perform the 2AF
max_time_VAS = 5;          % time to move the slider to rate outcome period
ISI = 3;                   % interstimulus interval with stimulation at baseline
stim_time = 1;

% PTB SET UP -------------------------------------------------------------%

Screen('Preference', 'Verbosity', 3);
Screen('Preference', 'SkipSyncTests', 1);
Screen('Preference', 'VisualDebugLevel', 1);
Priority (1);
%HideCursor();                                   

% COLOR SCHEME -----------------------------------------------------------%

gray = [230 230 230];           %bg color
darkgray = [130 130 130];          %button frame color
white = [255 255 255];          %utility
black = [0 0 0];                %utility
red = [255 0 0];                %lose message / button highlighting
blue = [68 114 196];            %choice blue button
pink = [205 0 205];             %choice pink button
yellow = [255 255 0];
bgcolor = gray;
textcolor = black;

% RESPONSE INDICES -------------------------------------------------------%

KbName('UnifyKeyNames'); 
escKey = KbName('ESCAPE');                         % in case abortion by experimenter
rightKey = KbName('RightArrow');                   % response option right
leftKey = KbName('LeftArrow');                     % response option left

% SCREEN SET-UP ----------------------------------------------------------%

[win, windowRect] = Screen('OpenWindow', max(Screen('Screens')));       % second array used for the properties of the VAS
Screen('FillRect', win, bgcolor);                                       % set BG color
center = [windowRect(3)/2, windowRect(4)/2];                            % center of the screen also used for positionning of black button                                          %positionning of the choice "blue" button
Screen('Flip',win);
Screen('BlendFunction', win, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

% XY AXIS  ---------------------------------------------------------------%

offset = 300;                     % offsets position of response buttons from the x and y axis in the test trials
button_red = [0 0 200 200];
button_blue = [0 0 200 200];
button_frame = 20;
button_high = 30;

% VAS SCALE --------------------------------------------------------------%

size = 450;                  % like this creates a slider with +/- 650 pixels on each side of the center (DO NOT CHANGE) (default = 450)
value_minimum = 0;           % give the value of what the minimum of your scale should be (in our case we use a scale from 0 to 200 for pain intensity)
value_maximum = 200;         % give the value of what the maximum of your scale should represent (in our case 200, most intense pain tolerable)
pixelsPerPress = 3.5;        % here can adapt the sensitivity of the VAS cursor (can increase/decrease time at which cursor moves (pixels per keypress)
anchor_offset = 95;          % change pixel values here to allign the text anchors and offset from the horizontal bar
anchor_allign = 97;          % shifts text of middle anchor to the left
start_allign = 70;           % shifts text of start anchor to the left
end_allign = 80;             % shifts text of the end anchor to the left
number_allign = 23;          % shifts the number of the middle anchor to the left
num_start_allign = 8;        % shifts the number of the start anchor to the left  
num_end_allign = 22;         % shifts the number of the end anchor to the left 
timespent = 0;
%
[xCenter, yCenter] = RectCenter(windowRect);                          % retrieve pixel value for center point
[screenXpixels, screenYpixels] = Screen('WindowSize', win);           % absolute number of pixels on y and x axis
squareX = xCenter;                                                    % squares of initial position of cursor
squareY = yCenter;
ifi = Screen('GetFlipInterval', win);           % gets frame interval from screen needed to update position of the cursor
cursor = [0 0 7 50];                            % dimensions of the cursor


%% (trials)
%=========================================================================%
% TRIAL LOOP
%=========================================================================%

% Ready message before bloc start ----------------------------------------%

Screen('TextSize', win, 50);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, 'Bitte druecken Sie auf die Leertaste, sobald Sie bereit sind!','center','center',black);       % correct german at the end ahah
Screen('Flip',win);
KbStrokeWait;

% Countdown before bloc start --------------------------------------------%

Screen('TextSize', win, 50);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '3','center','center',black);
Screen('Flip',win);
WaitSecs(1);
Screen('TextSize', win, 50);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '2','center','center',black);
Screen('Flip',win);
WaitSecs(1);
Screen('TextSize', win, 50);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '1','center','center',black);
Screen('Flip',win);
WaitSecs(1);

for i = 1:num_trials
    
if trial_type(i) == 1

%=========================================================================%
% TEST TRIALS
%=========================================================================%

% Convertion chart, stimulation deltas -----------------------------------%

    if delta(i) == 1
        temp_steps = 0.1;
    elseif delta(i) == 2
        temp_steps = 0.2;
    elseif delta(i) == 3
        temp_steps = 0.3;
    elseif delta(i) == 4
        temp_steps = 0.4;
    elseif delta(i) == 5
        temp_steps = 0.5;
    elseif delta(i) == 6
        temp_steps = -0.1;
    elseif delta(i) == 7
        temp_steps = -0.2;
    elseif delta(i) == 8
        temp_steps = -0.3;
    elseif delta(i) == 9
        temp_steps = -0.4;
    elseif delta(i) == 10
        temp_steps = -0.5;
    end

adapt_temp = Reference + temp_steps;    % determine intensity of test stimulation
stim_temp = round(adapt_temp,1); % to round to 2 decimals, avoids issued with characters flushed to TCS
test_temp = sprintf( 'C0%03d', stim_temp(1)*10 );
    
% ISI --------------------------------------------------------------------%

Screen('TextSize', win, 70);
Screen('TextFont', win, 'Calibri');                                            
DrawFormattedText(win, '+','center','center',black);
Screen('Flip',win);
WaitSecs(ISI);

% BASELINE ---------------------------------------------------------------%

Screen('TextSize', win, 70);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!','center','center',black);
Screen('Flip',win);
WaitSecs(0.5 + (0.5 - 0).* rand(1) + 0);                    % To obtain a jittering time interval of 0.5 to 1s

% REFERENCE --------------------------------------------------------------%

Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!!! REFERENZ !!!','center','center',yellow);
Screen('Flip',win);
WaitSecs(0.5);

%TCS STIM
    write(TCS, reference_temp, 'char');    %all three arguments are needed to get to target
    write(TCS, 'L', 'char');
    flush(TCS, 'output');
%TCS STIM

Screen('TextSize', win, 100);
Screen('FillRect', win, darkgray);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!!! REFERENZ !!!','center','center',yellow);
Screen('Flip',win);
WaitSecs(stim_time);

%TCS STIM back to baseline
    write(TCS, 'A', 'char');
    flush(TCS, 'output');
%TCS STIM

% DELAY ------------------------------------------------------------------%

Screen('FillRect', win, gray);
Screen('TextSize', win, 70);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!','center','center',black);
Screen('Flip',win);
WaitSecs(2.5 + (0.5 - 0).* rand(1) + 0);                    % To obtain a jittering time interval of 2.5 to 3s

% TEST -------------------------------------------------------------------%

Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!!! TEST !!!','center','center',yellow);
Screen('Flip',win);
WaitSecs(0.5);

%TCS STIM
    write(TCS, test_temp, 'char');    %all three arguments are needed to get to target
    write(TCS, 'L', 'char');
    flush(TCS, 'output');
%TCS STIM

Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!!! TEST !!!','center','center',yellow);
Screen('Flip',win);
WaitSecs(stim_time);

%TCS STIM
    write(TCS, 'A', 'char');
    flush(TCS, 'output');
%TCS STIM
    
Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!!! TEST !!!','center','center',yellow);
Screen('Flip',win);
WaitSecs(2);   

% 2AF --------------------------------------------------------------------%

Screen('FillRect', win, gray);
resp_to_be_made = true;
Onset = GetSecs;
        
        % CHOICE INTERVAL
        
        while resp_to_be_made == true
            
            %response components
            [tmp, KeyTime, KeyCode,] = KbCheck;
            Screen('FillRect',win,bgcolor);
            
            %Red button
            positionned_red_button = CenterRectOnPointd(button_red,windowRect(3)/2-offset, windowRect(4)/2);
            Screen('FillRect', win, red, positionned_red_button);
            Screen('FrameRect', win, black, positionned_red_button, button_frame);
            %Text anchor "Stronger"
            Screen('TextSize', win, 70);
            Screen('TextFont', win, 'Helvetica');                                            
            DrawFormattedText(win, 'Staerker',windowRect(3)/2-(offset + 120),windowRect(4)/2 - 200,red);
            
            %Blue button
            positionned_blue_button = CenterRectOnPointd(button_blue,windowRect(3)/2+offset, windowRect(4)/2);
            Screen('FillRect', win, blue, positionned_blue_button);
            Screen('FrameRect', win, black, positionned_blue_button, button_frame);
            %Text anchor "Weaker"
            Screen('TextSize', win, 70);
            Screen('TextFont', win, 'Helvetica');                                            
            DrawFormattedText(win, 'Schwaecher',windowRect(3)/2+(offset - 160),windowRect(4)/2 - 200,blue);  
            
            %flipping to screen 
            Screen('Flip', win);
            timer = GetSecs;
            timespent = timer - Onset;

            if KeyCode(rightKey)    
                %response components
                resp_to_be_made = false;
                stim_choice = 1;
                choice_end = GetSecs;
                rt = choice_end - Onset;
                
                %Visual components 3: Response buttons
                %red
                Screen('FillRect', win, red, positionned_red_button);
                Screen('FrameRect', win, black, positionned_red_button,button_frame);
                %Text anchor "Stronger"
                Screen('TextSize', win, 70);
                Screen('TextFont', win, 'Helvetica');                                            
                DrawFormattedText(win, 'Staerker',windowRect(3)/2-(offset + 120),windowRect(4)/2 - 200,red);
                
                %blue
                Screen('FillRect', win, blue, positionned_blue_button);
                %Text anchor "Weaker"
                Screen('TextSize', win, 70);
                Screen('TextFont', win, 'Helvetica');                                            
                DrawFormattedText(win, 'Schwaecher',windowRect(3)/2+(offset - 160),windowRect(4)/2 - 200,blue);  Screen('FrameRect', win, yellow, positionned_blue_button,button_high);
                
                Screen('Flip', win);
                WaitSecs(0.5);

            elseif KeyCode(leftKey)
                resp_to_be_made = false;
                %response components
                stim_choice = 2;
                choice_end = GetSecs;
                rt = choice_end - Onset;
                
                %Visual components 3: Response buttons
                %red
                Screen('FillRect', win, red, positionned_red_button);
                Screen('FrameRect', win, yellow, positionned_red_button,button_high);
                %Text anchor "Stronger"
                Screen('TextSize', win, 70);
                Screen('TextFont', win, 'Helvetica');                                            
                DrawFormattedText(win, 'Staerker',windowRect(3)/2-(offset + 120),windowRect(4)/2 - 200,red);
                
                
                %blue
                %may have to adapt to size of projection
                Screen('FillRect', win, blue, positionned_blue_button);
                Screen('FrameRect', win, black, positionned_blue_button,button_frame);
                %Text anchor "Weaker"
                Screen('TextSize', win, 70);
                Screen('TextFont', win, 'Helvetica');                                            
                DrawFormattedText(win, 'Schwaecher',windowRect(3)/2+(offset - 160),windowRect(4)/2 - 200,blue);  
                
                Screen('Flip', win);
                WaitSecs(0.5);

            elseif timespent >= max_time_choice 
                resp_to_be_made = false;
                stim_choice = 3;
                choice_end = GetSecs;
                rt = max_time_choice;
                
                %Visual components 3: Response buttons
                %red 
                %may have to adapt to size of projection
                Screen('FillRect', win, red, positionned_red_button);
                Screen('FrameRect', win, yellow, positionned_red_button,button_high);
                %Text anchor "Stronger"
                Screen('TextSize', win, 70);
                Screen('TextFont', win, 'Helvetica');                                            
                DrawFormattedText(win, 'Staerker',windowRect(3)/2-(offset + 120),windowRect(4)/2 - 200,red);
                
                %blue
                %may have to adapt to size of projection
                Screen('FillRect', win, blue, positionned_blue_button);
                Screen('FrameRect', win, yellow, positionned_blue_button,button_high);
                %Text anchor "Weaker"
                Screen('TextSize', win, 70);
                Screen('TextFont', win, 'Helvetica');                                            
                DrawFormattedText(win, 'Schwaecher',windowRect(3)/2+(offset - 160),windowRect(4)/2 - 200,blue);  
                
                Screen('Flip', win);
                WaitSecs(0.5);
                
            end
vas = 0;
        end

%=========================================================================%
% VAS TRIALS
%=========================================================================%
       
elseif trial_type(i) == 2

% ISI --------------------------------------------------------------------%
    
Screen('TextSize', win, 70);
Screen('TextFont', win, 'Calibri');                                            
DrawFormattedText(win, '+','center','center',black);
Screen('Flip',win);
WaitSecs(ISI);
    
% Baseline ---------------------------------------------------------------%
    
Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!','center','center',yellow);
Screen('Flip',win);
WaitSecs(2.5 + (0.5 - 0).* rand(1) + 0);
    
%TCS STIM
    write(TCS, reference_temp, 'char');    %all three arguments are needed to get to target
    write(TCS, 'L', 'char');
    flush(TCS, 'output');
%TCS STIM

Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!','center','center',yellow);
Screen('Flip',win);
WaitSecs(stim_time);

%TCS STIM
    write(TCS, 'A', 'char');
    flush(TCS, 'output');
%TCS STIM

Screen('FillRect', win, darkgray);
Screen('TextSize', win, 100);
Screen('TextFont', win, 'Helvetica');                                            
DrawFormattedText(win, '!','center','center',yellow);
Screen('Flip',win);
WaitSecs(0.5);

Screen('FillRect', win, bgcolor);
vbl = Screen('Flip', win);          %marks a time point for onset of presentation
waitframes = 1;                     %after 1 frame starts the cursor loop
OnsetVAS = GetSecs;
timespent = 0;

    while timespent <= max_time_VAS      %max time to give response to VAS

        %%%Visual elements TEXT
        %title
        Screen('TextSize', win, 40);
        Screen('TextFont', win, 'Helvetica');
        DrawFormattedText(win, 'Empfindung einschaetzen','center', screenYpixels * 0.30, black);  

        %Anchor: "Pain Threshold" + number
        Screen('TextSize', win, 25);
        Screen('TextFont', win, 'Helvetica');
        DrawFormattedText(win, 'Schmerzschwelle', windowRect(3)/2 - anchor_allign, windowRect(4)/2 - anchor_offset, black);
        %
        Screen('TextSize', win, 25);
        Screen('TextFont', win, 'Helvetica'); 
        DrawFormattedText(win, '100',windowRect(3)/2 - number_allign, windowRect(4)/2 + anchor_offset, black);
        
        %Anchor: "No Sensation" + number
        Screen('TextSize', win, 25);
        Screen('TextFont', win, 'Helvetica');
        DrawFormattedText(win, '     keine\nEmpfindung',(windowRect(3)/2-size)-start_allign , windowRect(4)/2 - anchor_offset, black);
        %
        Screen('TextSize', win, 25);
        Screen('TextFont', win, 'Helvetica');
        DrawFormattedText(win, '0',(windowRect(3)/2-size)- num_start_allign  , windowRect(4)/2 + anchor_offset, black);
        
        %Anchor: "Strongest tolerable pain" + number
        Screen('TextSize', win, 25);
        Screen('TextFont', win, 'Helvetica');
        DrawFormattedText(win, '   staerkster\n tolerierbarer \n    Schmerz', (windowRect(3)/2+size)- end_allign , windowRect(4)/2 - anchor_offset, black);
        %
        Screen('TextSize', win, 25);
        Screen('TextFont', win, 'Helvetica');
        DrawFormattedText(win, '200',(windowRect(3)/2+size)- num_end_allign , windowRect(4)/2 + anchor_offset, black);
        
        %%%Visual elements BARS
        %bar
        horizontal_bar = [0 0 size*2 5];                                                                     
        positionned_bar = CenterRectOnPointd(horizontal_bar,windowRect(3)/2, windowRect(4)/2);
        Screen('FillRect', win, black, positionned_bar);
        %middle anchor 
        middle_anchor = [0 0 5 40]; 
        positionned_middle_anchor = CenterRectOnPointd(middle_anchor,windowRect(3)/2, windowRect(4)/2);
        Screen('FillRect', win, black, positionned_middle_anchor);
        %start anchor
        start_anchor = [0 0 5 40]; 
        positionned_start_anchor = CenterRectOnPointd(start_anchor,windowRect(3)/2 - size, windowRect(4)/2);
        Screen('FillRect', win, black, positionned_start_anchor);
        %end anchor
        end_anchor = [0 0 5 40]; 
        positionned_end_anchor = CenterRectOnPointd(end_anchor,windowRect(3)/2 + size, windowRect(4)/2);
        Screen('FillRect', win, black, positionned_end_anchor);

        % Check the keyboard to see if a button has been pressed
        [keyIsDown,secs, keyCode] = KbCheck;
        
        if keyCode(leftKey)
            squareX = squareX - pixelsPerPress;
        elseif keyCode(rightKey)
            squareX = squareX + pixelsPerPress;
        end

        % We set bounds to make sure our square doesn't go completely off of
        % the screen
        if squareX < windowRect(3)/2 - size
            squareX = windowRect(3)/2 - size;
        elseif squareX > windowRect(3)/2 + size
            squareX = windowRect(3)/2 + size;
        end

        % Center the rectangle on the centre of the screen
        positionned_cursor = CenterRectOnPointd(cursor, squareX, squareY);
        % Draw the rect to the screen
        Screen('FillRect', win, red, positionned_cursor);
        % Flip to the screen
        vbl  = Screen('Flip', win, vbl + (waitframes - 0.5) * ifi);
        final_position = squareX;

        %some values used to convert to our scale's bound
        old_min = windowRect(3)/2 - size;
        old_max = windowRect(3)/2 + size;
        new_max = value_maximum;
        new_min = value_minimum;
        VAS = (((final_position - old_min) * (new_max - new_min)) / (old_max - old_min)) + 0;   %for now displayed continuously in command
        timerVAS = GetSecs;
        timespent = timerVAS - OnsetVAS;    %used to bound response time to 5 seconds on the VAS scale
        vas = VAS;
        stim_choice = 4;
        rt = 0.0;
        temp_steps = 100.00;      
    end
   
end
        
fprintf(outputfile, '%s\t %d\t %d\t %d\t %d\t %d\t %d\t %f\t %d\t %f\t %f\n', Subj_Idx, Session, Bloc, Reference, i,  trial_type(i), stim_type(i), temp_steps, stim_choice, vas, rt);

end


%% (close)
%=========================================================================%
% CLOSING STATEMENTS
%=========================================================================%

% PTB
Priority(0);
% TCS usb port
delete(TCS);
% Outcome log
fclose(outputfile);
% Utility
sca;
clear;


