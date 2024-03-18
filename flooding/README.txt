Compilation Instructions (use intellij)
1. Navigate to the "Run" menu and select "Edit Configurations"
2. In the "Run/Debug Configurations" dialog, select your Scala application configuration.
3. In the "Program arguments" field, enter the command line arguments separated by spaces.
4. Click "Ok" (NOT APPLY THAT IS VERY MISLEADING) to save configurations.

Program arguments format and value options
<size> <colors> <id> <heuristic> <algorithm> <verbose>
Size -> size of the board, has to be 5, 10, 15, 20, or 25
Colors -> the nubmer of colors in the puzzle, must be 4, 5, 6, 7, or 8
id -> the id of the board grabbed, pretty arbitrary but must be 0, 1, 2, 3, or 4
heurstic -> must be 0 or 1. 0 is the bad heuristic
algorithm -> must be 0 or 1. 0 is A*, 1 is beam stack search
verbose -> must be 0, 1, or 2. determines the number of print statements you have to look at