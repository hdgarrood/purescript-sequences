
// module Benchmark.Main

exports.randomArray = function (n) {
  return function() {
    var arr = []
    for (var i = 0; i < n; i++) {
      arr.push(Math.random())
    }
    return arr;
  }
}
