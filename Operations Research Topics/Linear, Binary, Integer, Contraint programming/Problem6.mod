/*********************************************
 * OPL 12.8.0.0 Model
 * Author: Chris Austin
 * Creation Date: Oct 16, 2018 
 *********************************************/
using CP;

range R = 1..5;
string locationName[R] = ["Location 1", "Location 2", "Location 3", "Location 4", "Location 5"];
string machineName[R] = ["Machine 1", "Machine 2", "Machine 3", "Machine 4", "Dummy Machine 5"];

// range of machines (including dummy) and locations
dvar int x[R] in 1..5;

// Handling Costs
// Machine 1
float z1[R] = [13,16,12,11,15];
// Machine 2
float z2[R] = [15,13,20,15,14];
// Machine 3, including big-M to eliminate machine 3 in location 3.
float z3[R] = [5,7,1000,10,6];
// Machine 4
float z4[R] = [8,5,9,11,4];
// Dummy Machine 5
float z5[R] = [0,0,0,0,0];

minimize z1[x[1]] + z2[x[2]] + z3[x[3]] + z4[x[4]] + z5[x[5]];

// Only one machine to a location.
subject to { 
    allDifferent(x);
}

execute{
  writeln(machineName[1], " should be placed at ", locationName[x[1]], " with cost ", z1[x[1]]);
  writeln(machineName[2], " should be placed at ", locationName[x[2]], " with cost ", z2[x[2]]);
  writeln(machineName[3], " should be placed at ", locationName[x[3]], " with cost ", z3[x[3]]);
  writeln(machineName[4], " should be placed at ", locationName[x[4]], " with cost ", z4[x[4]]);
  writeln(machineName[5], " should be placed at ", locationName[x[5]], " with cost ", z5[x[5]]); 
}