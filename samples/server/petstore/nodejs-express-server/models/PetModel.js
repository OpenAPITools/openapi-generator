const tag = require('./TagModel');

class PetModel {
  constructor(photoUrls, name, id, tags, status) {
    this.photoUrls = photoUrls;
    this.name = name;
    this.id = id;
    this.tags = tags;
    this.status = status;

    if (photoUrls === undefined || photoUrls.length === 0) {
      throw new Error('photoUrls is a required parameter');
    }
    if (name === undefined || name === '') {
      throw new Error('name is a required parameter');
    }
  }

  set photoUrls(pu) {
    if (pu instanceof Array) {
      this.photoUrls = pu;
    } else {
      throw new Error('PhotoUrls must be an array');
    }
  }

  set name(n) {
    if (typeof n instanceof String || typeof n === 'string') {
      this.name = n;
    } else {
      throw new Error('Type must be a String');
    }
  }

  set id(i) {
    if (typeof i === 'number') {
      this.id = i;
    } else {
      throw new Error('id must be a number');
    }
  }

  set tags(t) {
    if (t instanceof Array) {
      if (t.every(tg => tg instanceof tag)) {
        this.tags = t;
      } else {
        throw new Error('tags elements must by of type Tag');
      }
    } else {
      throw new Error('tags must be array');
    }
  }

  set status(s) {
    if (typeof s === 'boolean') {
      this.status = s;
    } else {
      throw new Error('status must be boolean');
    }
  }
}

module.exports = PetModel;
