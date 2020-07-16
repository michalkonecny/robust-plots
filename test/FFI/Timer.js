"use strict";

exports.processTime = function () {
  return process.hrtime();
};

exports.processTimeElapsedSince = function (start) {
  return function () {
    var precision = 3;
    return (process.hrtime(start)[1] / 1000000).toFixed(precision);
  };
};