package com.wordnik.petstore.model

case class User (
  /* Unique identifier for the user */
  id: Long,
  /* Unique username */
  username: String,
  /* First name of the user */
  firstName: String,
  /* Last name of the user */
  lastName: String,
  /* Email address of the user */
  email: String,
  /* Password name of the user */
  password: String,
  /* Phone number of the user */
  phone: String,
  /* User Status */
  userStatus: Int)

