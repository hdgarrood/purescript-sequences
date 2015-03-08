var isNode = typeof module !== 'undefined' && module.exports
var Results = []
var B = null
if (isNode) {
  B = require('benchmark')
} else {
  B = Benchmark
}

function randomArray(size) {
  var arr = []
  for (var i = 0; i < size; i++) {
    arr.push(Math.random())
  }
  return arr
}

function getBenchByName(suite, name) {
  var result = null
  for (var i = 0; i < suite.length; i++) {
    if (suite[i].name === name) {
      return suite[i]
    }
  }
  throw new Error('No bench object with name ' + name +
                    ' was found in the suite.')
}

function seqBench(size) {
  var array = randomArray(size)
  var results = {}
  var suite = new B.Suite()

  suite.add("Seq", function() {
    PS.Benchmarks.insertLotsSeq(array)
  })
  suite.add("Array", function() {
    PS.Benchmarks.insertLotsArray(array)
  })
  suite.run()

  // hopefully avoid memory leaks
  var stats = {
    "Seq":   getBenchByName(suite, "Seq").stats,
    "Array": getBenchByName(suite, "Array").stats
  }
  suite = null
  array = null

  return { size: size, stats: stats }
}

function log(msg) {
  var msg2 = msg + '\r\n'
  if (isNode) {
    process.stderr.write(msg2)
  } else {
    var n = document.createTextNode(msg2)
    var el =  document.getElementById('log')
    el.appendChild(n)
  }
}

function runAllBenchmarks() {
  var values = [100, 1000, 2000, 5000, 10000, 15000, 20000, 50000]

  return values.map(function(v) {
    var r = seqBench(v)
    log('Done bench for array of size: ' + v)
    return r
  })
}

function go() {
  window.Results = runAllBenchmarks()
}

// in the browser, wait until a button is pressed
if (isNode) {
  var fs = require('fs')
  var results = runAllBenchmarks()
  fs.writeFileSync('tmp/results.json', JSON.stringify(results))
  log('Results logged to tmp/results.json')
}
