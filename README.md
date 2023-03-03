# Purpose

Purpose of iteration 3 is to have a functional game of lights out.
Our game should have an additional feature which is letting an AI play.
The AI should be able to pick a switch without user input, besides board creation.
The board should be printed out to standard out to let the user see what the AI is doing.
The AI should complete the game in a reasonable amount of moves.

# Contents

For this project we will have 6 files:

1. lightsout-ai.lisp
2. lightsout-game.lisp
3. helper-functions.lisp
4. toggle-functions.lisp
5. README.md
6. .gitignore

# How to use

Currently running program on LispWorks Personal

**Download:** http://www.lispworks.com/downloads/index.html

Note: Will have to enter in email to download

After downloading LispWorks, download file folder on bitbucket

Once you download project folder, set it somewhere memorable

## 1st step:

Get the entire path of downloaded folder

**On mac by:**

1. right click + get info on downloaded folder
2. right click + "where" section
3. only one option which is "Copy as Pathname"
            
**On windows:**

1. Head to downloaded folder in file explorer window
2. Hold down shift key and click on folder
3. Choose "copy as path" from options window

## 2nd step:

1. Change directory in LispWorks Personal

2. Replace pathname with your own from Step 1 into step 3 command

3. Issue the command: (change-directory "/Users/markortega-ponce/Desktop/LightsOutProject/lights-out")

## 3rd step:

You can choose to play, or you can watch the AI play.

**To play: (load "lightsout-game.lisp")**
    Issue the command: (start-game) after loading the file in, will prompt for 2x3 board or 5x5 board.

**To watch AI: (load "lightsout-ai.lisp")**
    Issue the command: (ai-player) after loading the file in
    
**To test functions: (run-tests)**
    Issue this command if changing the code
    
> Note: For AI, currently only have good results with 3x3 board. 
    5x5 works sometimes (25% rate), but most of the time you'll get stack overflow error
    Sometimes you'll also get a stack overflow error when playing games repeatedly
    To fix this you'll have to close LispWorks and repeat steps 2 to 3.
    I could reduce the amount of moves the AI can make, but I run into the same issues
    with 3x3 board when running repeated games. So I think it has more to do with
    LispWorks Peronal or the recursive nature of Lisp and stack-space not being immediately available.

