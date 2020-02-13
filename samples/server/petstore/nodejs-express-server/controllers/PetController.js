const Controller = require('./Controller');

class PetController {
  constructor(Service) {
    this.service = Service;
  }

  async addPet(request, response) {
    await Controller.handleRequest(request, response, this.service.addPet);
  }

  async deletePet(request, response) {
    await Controller.handleRequest(request, response, this.service.deletePet);
  }

  async findPetsByStatus(request, response) {
    await Controller.handleRequest(request, response, this.service.findPetsByStatus);
  }

  async findPetsByTags(request, response) {
    await Controller.handleRequest(request, response, this.service.findPetsByTags);
  }

  async getPetById(request, response) {
    await Controller.handleRequest(request, response, this.service.getPetById);
  }

  async updatePet(request, response) {
    await Controller.handleRequest(request, response, this.service.updatePet);
  }

  async updatePetWithForm(request, response) {
    await Controller.handleRequest(request, response, this.service.updatePetWithForm);
  }

  async uploadFile(request, response) {
    await Controller.handleRequest(request, response, this.service.uploadFile);
  }

}

module.exports = PetController;
