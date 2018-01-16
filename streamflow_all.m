close all
%% Import data from text file.
% Script for importing data from the following text file:


%% Initialize variables.
filename = '/net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2/gem_forecasts/05KJ001/SaMESHRunShort/BASINAVG1/MESH_output_streamflow_ts_final.csv';
image_output_path = '/net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2/gem_forecasts/05KJ001/Image/'
web_output = '/net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2/web_output/'
startRow = 2;

%% Format string for each line of text:
%   column1: text (%s)
%	column2: text (%s)
%   column3: text (%s)
%	column4: text (%s)
%   column7: double (%f)
%   column9: double (%f)
%   column11: double (%f)
%   column13: double (%f)
%   column15: double (%f)
%   column17: double (%f)
%   column19: double (%f)
%   column21: double (%f)
%   column23: double (%f)
%   column25: double (%f)
%   column27: double (%f)
%   column29: double (%f)
%   column31: double (%f)
%   column33: double (%f)
%   column35: double (%f)
%   column37: double (%f)
%   column39: double (%f)
%   column41: double (%f)
%   column43: double (%f)
%   column45: double (%f)
%   column47: double (%f)
% For more information, see the TEXTSCAN documentation.
formatSpec = '%16s%16s%16s%16s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%5s%11f%s%[^\n\r]';

%% Open the text file.
fileID = fopen(filename,'r');

%% Read columns of data according to format string.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%% Remove white space around all cell columns.
dataArray{1} = strtrim(dataArray{1});
dataArray{2} = strtrim(dataArray{2});
dataArray{3} = strtrim(dataArray{3});
dataArray{4} = strtrim(dataArray{4});

%% Close the text file.
fclose(fileID);
station_names={'05AG006','05AG001','05BN012','05CK004','05DF001',...
    '05EF001','05FE004','05GG001','05HG001','05KD003','05KJ001';...
    'Oldman Near the Mouth','SSR at Medicine Hat','Bow River near the Mouth','Red Deer River Near Bindloss',...
    'NSR at Edmonton','NSR Near Deer Creek','Battle River Near SK Boundary',...
    'NSR Near Prince Alberta','SSR Near Saskatoon', 'SR Below Tobin Lake', 'SR at The Pas'};
%% Post processing for unimportable data.
% No unimportable data rules were applied during the import, so no post
% processing code is included. To generate code which works for
% unimportable data, select unimportable cells in a file and regenerate the
% script.

%% Allocate imported array to column variable names
YEAR = dataArray{:, 1};
DAY = dataArray{:, 2};
HOUR = dataArray{:, 3};
MINS = dataArray{:, 4};
QO1 = dataArray{:, 7};
QO2 = dataArray{:, 11};
QO3 = dataArray{:, 15};
QO4 = dataArray{:, 19};
QO5 = dataArray{:, 23};
QO6 = dataArray{:, 27};
QO7 = dataArray{:, 31};
QO8 = dataArray{:, 35};
QO9 = dataArray{:, 39};
QO10 = dataArray{:, 43};
QO11 = dataArray{:, 47};

%% Clear temporary variables
clearvars filename startRow formatSpec fileID dataArray ans;
time1=datenum(str2num(cell2mat(YEAR(1))),0,str2num(cell2mat(DAY(1)))+16/24);
time2=datenum(str2num(cell2mat(YEAR(end))),0,str2num(cell2mat(DAY(end)))+15.5/24);
time=linspace(time1,time2, length(YEAR));
for i=1:length(station_names)
disp(['SaMESH Real-time Streamflow Forecast for ' station_names{2,i}])
    eval (['QQ=QO' num2str(i) ';'])
plot(time(1:end-48*2),QQ(1:end-48*2),'k-',time(end-48*2+1: end),QQ(end-48*2+1:end),'g--','linewidth',2)
legend('CaPA-hindcast','RDPS-forecast')
datetick
year=cell2mat(YEAR(end));
xlabel(['Year ( ' year(1:4) ' )'], 'Fontsize',14)
ylabel ('Streamflow (m^3/s)','Fontsize',14)
ylim([floor(min(QQ))-5  floor(max(QQ))+5])
title({'SaMESH Real-time Streamflow Forecast', ['for ' station_names{2,i}]},'Fontsize',16)
print([image_output_path date '-' station_names{1,i} '.png'],'-dpng','-r300')
print([web_output station_names{1,i} '.png'],'-dpng','-r300')

close
end
system('scp /net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2/web_output/*.* armnliu@accessdepot.cmc.ec.gc.ca:/data/web/home/armnliu/www/flood/Streamflow_Prototype/')
system('rm -f /net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2/web_output/*.*')
exit
