exports.debug = function(str){
  return function (a) {
    console.log(str,a)
    return a
  }
}
exports.debugE = function (a) {
  return function () {
    console.log(a)
    return {}
  } 
}
exports.debugEWithTag = function (tag) {
  return function (a) {
    return function () {
      console.log(tag,a)
      return {}
    } 
  }
}