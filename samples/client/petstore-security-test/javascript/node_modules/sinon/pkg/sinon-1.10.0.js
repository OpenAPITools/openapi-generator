this.sinon = (function () {
var samsam, formatio;
function define(mod, deps, fn) { if (mod == "samsam") { samsam = deps(); } else if (typeof fn === "function") { formatio = fn(samsam); } }
define.amd = {};
