'use strict';

var url = require('url');


var Pet = require('./PetService');


module.exports.updatePet = function updatePet (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  var result = Pet.updatePet(body);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.addPet = function addPet (req, res, next) {
  var body = req.swagger.params['body'].value;
  

  var result = Pet.addPet(body);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.findPetsByStatus = function findPetsByStatus (req, res, next) {
  var status = req.swagger.params['status'].value;
  

  var result = Pet.findPetsByStatus(status);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.findPetsByTags = function findPetsByTags (req, res, next) {
  var tags = req.swagger.params['tags'].value;
  

  var result = Pet.findPetsByTags(tags);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.getPetById = function getPetById (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  

  var result = Pet.getPetById(petId);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.updatePetWithForm = function updatePetWithForm (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var name = req.swagger.params['name'].value;
  var status = req.swagger.params['status'].value;
  

  var result = Pet.updatePetWithForm(petId, name, status);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.deletePet = function deletePet (req, res, next) {
  var api_key = req.swagger.params['api_key'].value;
  var petId = req.swagger.params['petId'].value;
  

  var result = Pet.deletePet(api_key, petId);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};

module.exports.uploadFile = function uploadFile (req, res, next) {
  var petId = req.swagger.params['petId'].value;
  var additionalMetadata = req.swagger.params['additionalMetadata'].value;
  var file = req.swagger.params['file'].value;
  

  var result = Pet.uploadFile(petId, additionalMetadata, file);

  if(typeof result !== 'undefined') {
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify(result || {}, null, 2));
  }
  else
    res.end();
};
