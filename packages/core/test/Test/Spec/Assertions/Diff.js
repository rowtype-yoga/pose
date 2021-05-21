"use strict";
var diff2html = require("diff2html");
var unidiff = require("unidiff");

exports.getPrettyHtml = function (filename) {
  return function (a) {
    return function (b) {
      var diff = unidiff.diffAsText(b, a, {
        aname: filename,
        bname: "actual",
        context: 20,
      });
      console.log();
      console.log(diff);
      return diff2html.html(diff, {
        inputFormat: "diff",
        showFiles: true,
        matching: "lines",
        outputFormat: "side-by-side",
      });
    };
  };
};
