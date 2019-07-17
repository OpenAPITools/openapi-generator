class PetModel {
  constructor(photoUrls, name, id, tags, status) {
    this.photoUrls = photoUrls;
    this.name = name;
    this.id = id;
    this.tags = tags;
    this.status = status;
  }
}

module.exports = PetModel;
