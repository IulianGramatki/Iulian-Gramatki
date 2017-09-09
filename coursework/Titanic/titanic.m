clear
%cd "C:\Users\Iulian\Documents\GitHub\Iulian-Gramatki\coursework\Titanic";

%read the file
f = fopen('train.csv');
[c pos] = textscan(f,'%*s',1,'Delimiter','\n');  %skip first line
C = textscan(f(pos+1:end),'%d%d%d%s%s%d%d%d%s%f%s%c','Delimiter',',');
fclose(f);