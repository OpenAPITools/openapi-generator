var Randomizer = {
  'intBetween': function(a, b) {
    return Math.floor(Math.random()*(b-a+1)+a);
  },
  'string': function() {
    var chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZabcdefghiklmnopqrstuvwxyz";
    var string_length = 24;
    var randomstring = '';
    for (var i=0; i<string_length; i++) {
      var rnum = Math.floor(Math.random() * chars.length);
      randomstring += chars.substring(rnum,rnum+1);
    }

    return randomstring;
  },
  'int': function() {
    return this.intBetween(0, 10000);
  },
  'long': function() {
    return this.int();
  },
  'double': function() {
    return Math.random()*1000000;
  },
  'boolean': function() {
    return this.intBetween(1,2) == 1;
  },
  '__date': function() {
    var m = this.intBetween(1, 12);
    if (m < 10) { m = '0'+m; }
    var d = this.intBetween(1, 28);
    if (d < 10) { d = '0'+d; }

    return this.intBetween(1800, 2015) + '-' +  m + '-' + d;
  },
  'date': function() {
    return this.__date() + 'T' + this.__time();
  },
  '__time': function() {
    var h = this.intBetween(0, 23);
    if (h < 10) { h = '0'+h; }
    var m = this.intBetween(0, 59);
    if (m < 10) { m = '0'+m; }
    var s = this.intBetween(0, 59);
    if (s < 10) { s = '0'+s; }

    return h + ':' + m + ':' + s + '+01:00';
  },
  'array': function(type, length) {
    var out = new Array();
    if (!length) {
      length = this.intBetween(1, 5); }
    for (var i = 0; i < length; i++) {
      out.push(this[type]()); }
    return out;
  }
};

exports.intBetween = Randomizer.intBetween;
exports.string = Randomizer.string;
exports.int = Randomizer.int;
exports.long = Randomizer.long;
exports.double = Randomizer.double;
exports.boolean = Randomizer.boolean;
exports.array = Randomizer.array;
exports.__date = Randomizer.__date;
exports.date = Randomizer.date;
exports.__time = Randomizer.__time;