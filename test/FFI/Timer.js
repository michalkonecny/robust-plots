"use strict";
exports.runPerformanceWith = function (failure) {
  return function (success) {
    return function (expected) {
      return function (f) {
        
          // Log start time
          var start = process.hrtime();

          // Run function
          f();

          // Log end time
          var precision = 3;
          var elapsed = (process.hrtime(start)[1] / 1000000).toFixed(precision); // Convert to milliseconds

          // Log message
          if (elapsed > expected) {
            return failure("expected completion in " + expected + "ms, actually completed in " + elapsed + "ms");
          }

          return success;
        
      }
    }
  }
};