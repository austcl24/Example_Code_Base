/*********************************************
 * OPL 12.8.0.0 Model
 * Author: caustin
 * Creation Date: Dec 05, 2018 
 *********************************************/
// --------------------------------------------------------------------------
// Licensed Materials - Property of IBM
//
// 5725-A06 5725-A29 5724-Y48 5724-Y49 5724-Y54 5724-Y55
// Copyright IBM Corporation 1998, 2013. All Rights Reserved.
//
// Note to U.S. Government Users Restricted Rights:
// Use, duplication or disclosure restricted by GSA ADP Schedule
// Contract with IBM Corp.
// --------------------------------------------------------------------------

range R = 1..3;
dvar float x[R];

maximize
  (20 * x[1]) - (20 * x[1]^2) + (50 * x[2]) - (50 * x[2]^2) + (100 * x[3]) -
  (30 * x[3]^2) + (18 * x[1] * x[2]) + (30 * x[1] * x[3]);

subject to {
  ct1:  x[1] + x[2] + x[3] <= 60;
  ct2:  x[1] + (5 * x[2]) + x[3] <= 40;
  ct3:  x[1] >= 0;
  ct4:  x[2] >= 0;
  ct5:  x[3] >= 0;
  }

execute {
 writeln("Optimal Maximized value is: ", cplex.getObjValue());
 writeln("Value for variable 1 at optimum is: ", x[1]);
 writeln("Value for variable 2 at optimum is: ", x[2]);
 writeln("Value for variable 3 at optimum is: ", x[3]);
} 