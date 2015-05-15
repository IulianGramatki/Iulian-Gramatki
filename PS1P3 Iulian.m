clear all;
clc;
close all;
rng('default');

%% initialize the array

a=zeros(17,17);
used=zeros(15,15);  % auxiliary matrix for initial generation, counts which squared have already been placed;
a(1,:)=2;   % '2' stands for unoccupied houses, '1' for black and '0' for white
a(17,:)=2;
a(:,1)=2;
a(:,17)=2;
r=randi(225,500);   % generate random staring position of unoccupied and black houses
count=0;

for i=1:500
    x=floor((r(i)+14)/15);
    y=mod(r(i),15)+1;  %transform random number into 2D coordinates
    if used(x,y)==0     %check is square if unused
        count=count+1;
        used(x,y)=1;
        if count<=5
            a(x+1,y+1)=2;    %make the square unoccupied
            unused(count)=r(i);     %remember unoccupied household for later use
        else
            a(x+1,y+1)=1;    %make the square black
        end
    end
    if count>=115
        break;
    end
end;

colormap(flipud(gray));
imagesc(a);
title('period = 0');
pause(0.3);
saveas(1,'period0.pdf');

t=45;   %number of periods
for j=1:t       %loop over periods
    for i=1:225     %loop over squares
        x=floor((i+14)/15);     
        y=mod(i,15)+1;      %transform into 2D coordinates
        if a(x+1,y+1)<2 % unoccupied squares cannot move
            move=0;     % the decision variable - how many neighbors have a different color
            if a(x,y)==1-a(x+1,y+1) %check the color of all neighbors; our square of interest has coordinates (x+1,y+1)
                move=move+1;
            end
            if a(x+1,y)==1-a(x+1,y+1) 
                move=move+1;
            end
            if a(x+2,y)==1-a(x+1,y+1) 
                move=move+1;
            end
            if a(x,y+1)==1-a(x+1,y+1) 
                move=move+1;
            end
            if a(x+2,y+1)==1-a(x+1,y+1) 
                move=move+1;
            end
            if a(x,y+2)==1-a(x+1,y+1) 
                move=move+1;
            end
            if a(x+1,y+2)==1-a(x+1,y+1) 
                move=move+1;
            end
            if a(x+2,y+2)==1-a(x+1,y+1) 
                move=move+1;
            end
            if move>=3      %start the moving process
                new=unused(1);      %pick an unoccupied house
                newx=floor((new+14)/15);
                newy=mod(new,15)+1;     %transform into 2D coordinates
                a(newx+1,newy+1)=a(x+1,y+1);       %enter new house
                a(x+1,y+1)=2;                       %leave old house
                unused=unused(2:5);   %shift the queue of unoccupied houses
                unused(5)=i;        %the square from which we moved is now unoccupied; put it last in the queue
                %{            
                colormap(flipud(gray));  (display image after every move)
                imagesc(a);
                pause(0.003);
                %}               
            end
        end
    end 
    colormap(flipud(gray));     %display image after a full period
    imagesc(a);
    title(['period = ' num2str(j)]);
    pause(0.2);
    if mod(j,15)==0
        saveas(1,['period' num2str(j) '.pdf']);
    end
end