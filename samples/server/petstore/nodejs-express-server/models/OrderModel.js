class OrderModel {
  constructor(id, petId, quantity, shipDate, complete, status) {
    this.id = id;
    this.petId = petId;
    this.quantity = quantity;
    this.shipDate = shipDate;
    this.complete = complete;
    this.status = status;
  }
}

module.exports = OrderModel;
