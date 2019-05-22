
const Controller = require('./Controller');
const { PetService } = require('../service/PetService');


class Pet {
  static async addPet(request, response) {
    await Controller.handleRequest(request, response, PetService.deletePet);
  }

  static async deletePet(request, response) {
    await Controller.handleRequest(request, response, PetService.deletePet);
  }

  static async findPetsByStatus(request, response) {
    await Controller.handleRequest(request, response, PetService.findPetsByStatus);
  }

  static async findPetsByTags(request, response) {
    await Controller.handleRequest(request, response, PetService.findPetsByTags);
  }

  static async getPetById(request, response) {
    await Controller.handleRequest(request, response, PetService.getPetById);
  }

  static async updatePet(request, response) {
    await Controller.handleRequest(request, response, PetService.updatePet);
  }

  static async uploadFile(request, response) {
    await Controller.handleRequest(request, response, PetService.uploadFile);
  }
}

module.exports = Pet;
