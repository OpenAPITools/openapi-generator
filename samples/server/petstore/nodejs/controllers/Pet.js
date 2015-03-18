'use strict';

var url = require('url');


module.exports.updatePet = function updatePet (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.addPet = function addPet (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.findPetsByStatus = function findPetsByStatus (req, res, next) {
  var status = req.swagger.params['status'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.findPetsByTags = function findPetsByTags (req, res, next) {
  var tags = req.swagger.params['tags'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.getPetById = function getPetById (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.updatePetWithForm = function updatePetWithForm (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var name = req.swagger.params['name'].value;
  var status = req.swagger.params['status'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.deletePet = function deletePet (req, res, next) {
  var api_key = req.swagger.params['api_key'].value;
  var petId = req.swagger.params['petId'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};

module.exports.uploadFile = function uploadFile (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var additionalMetadata = req.swagger.params['additionalMetadata'].value;
  var file = req.swagger.params['file'].value;
  

  console.log('do some magic!');
  res.setHeader('Content-Type', 'application/json');
  res.end();
};
