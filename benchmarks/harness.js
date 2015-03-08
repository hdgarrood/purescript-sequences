var isNode = typeof module !== 'undefined' && module.exports
var Results = []

function go() {
  var B = null
  if (isNode) {
    B = require('benchmark')
  } else {
    B = Benchmark
  }

  var values = [1000, 10000]//, 1000000, 10000000]
  var benches = PS.Benchmarks.benches

  values.forEach(function(v) {
    var suite = new B.Suite
    benches.forEach(function(b) {
      suite.add(b.name, b.test(v))
    })
    suite.run()
    Results.push({
      value: v,
      suite: suite
    })

    var message = 'Fastest at ' + v + ' is ' +
                      suite.filter('fastest').pluck('name') + '\r\n';
    if (isNode) {
      process.stderr.write(message)
    } else {
      console.log(message)
    }
  })
}

// in the browser, wait until a button is pressed
if (isNode) {
  go()
  process.stdout.write(JSON.stringify(Results))
}
