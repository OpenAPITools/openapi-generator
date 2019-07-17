class UserModel {
  constructor(firstName, lastName, password, userStatus, phone, id, email, username) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.password = password;
    this.userStatus = userStatus;
    this.phone = phone;
    this.id = id;
    this.email = email;
    this.username = username;
  }
}

module.exports = UserModel;
