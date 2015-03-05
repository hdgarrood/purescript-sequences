
var Results = (function(window) {
  var B = require('benchmark');

  var values = [1, 10, 100, 1000, 10000, 1000000, 10000000]
  var results = []
  var benches = PS.Benchmarks.benches

  values.forEach(function(v) {
    var suite = new B.Suite
    benches.forEach(function(b) {
      suite.add(b.name, b.test(v))
    })
    suite.run()
    results.push({
      value: v,
      suite: suite
    })
  })

  console.log(JSON.stringify(results))
})(this)
