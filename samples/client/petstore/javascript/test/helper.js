var mockrequire = require('mockrequire');

var jquery = require('jquery');
var domino = require('domino');
var XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest;
var window = domino.createWindow();
var $ = jquery(window);
$.support.cors = true;
$.ajaxSettings.xhr = function() {
  return new XMLHttpRequest();
};

var requireWithMocks = function(path) {
  return mockrequire(path, {
    'jquery': $
  });
};

exports.requireWithMocks = requireWithMocks;
