exports.filterp = function (p){
  return function (arr) {
    return arr.filter(function(a,i){
      return p(a)(i)
    })
  }
}