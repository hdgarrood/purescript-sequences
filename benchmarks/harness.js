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

function randomSeq(size) {
  var dict = PS.Data_Foldable.foldableArray
  return PS.Data_Sequence.toSeq(dict)(randomArray(size))
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

function takeProps(obj, props) {
  var result = {}
  for (var i = 0; i < props.length; i++) {
    var p = props[i]
    result[p] = obj[p]
  }
  return result
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
  var results = {
    size:  size,
    Seq:   getBenchByName(suite, "Seq").stats,
    Array: getBenchByName(suite, "Array").stats,
  }
  suite = null
  array = null

  return results
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

function linearSpace(start, end, step) {
  var arr = []
  for (var i = start; i <= end; i += step) {
    arr.push(i)
  }
  return arr
}

function runAllBenchmarks() {
  var values = linearSpace(1000, 50000, 1000)

  return values.map(function(v) {
    var r = seqBench(v)
    log('Done bench for array of size: ' + v)
    return r
  })
}

function go() {
  window.Results = runAllBenchmarks()
}

function makeGraphData(results) {
  var data = {Seq: [], Array: []}
  for (var i = 0; i < results.length; i++) {
    var r = results[i]
    data.Seq.push({
      n: r.size,
      runtime: r.Seq.mean,
      stdDev: r.Seq.deviation
    })
    data.Array.push({
      n: r.size,
      runtime: r.Array.mean,
      stdDev: r.Array.deviation
    })
  }
  return data
}

// in the browser, wait until a button is pressed
if (isNode) {
  var fs = require('fs')
  var results = runAllBenchmarks()
  fs.writeFileSync('tmp/results.json', JSON.stringify(results))
  fs.writeFileSync('tmp/results-graph.json', JSON.stringify(makeGraphData(results)))
  log('Results logged to tmp/results.json')
}
