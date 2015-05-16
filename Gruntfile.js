module.exports = function(grunt) {
  "use strict";

  var libFiles = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
  ]
  var testsFiles = ["tests/**/*.purs"].concat(libFiles)
  var benchmarksFiles = ["benchmarks/**/*.purs"].concat(libFiles)

  grunt.initConfig({
    libFiles: libFiles,

    clean: {
      out: ["output"],
      tests: ["tmp/tests.js"],
      benchmarks: ["tmp/benchmarks.js"]
    },

    pscMake: {
      all: { src: libFiles }
    },
    psc: {
      tests: {
        options: {
          module: "Tests",
          main: "Tests"
        },
        src: testsFiles,
        dest: "tmp/tests.js"
      },
      bench: {
        options: {
          module: "Benchmark.Main",
          main: "Benchmark.Main"
        },
        src: benchmarksFiles,
        dest: "tmp/benchmarks.js"
      }
    },
    dotPsci: {
      src: libFiles
    },
    pscDocs: {
      sequence: {
        src: "src/Data/Sequence.purs",
        dest: "docs/Data.Sequence.md"
      },
      sequenceNonEmpty: {
        src: "src/Data/Sequence/NonEmpty.purs",
        dest: "docs/Data.Sequence.NonEmpty.md"
      },
      sequenceOrdered: {
        src: "src/Data/Sequence/Ordered.purs",
        dest: "docs/Data.Sequence.Ordered.md"
      },
      fingertree: {
        src: "src/Data/FingerTree.purs",
        dest: "docs/Data.FingerTree.md"
      }
    },
    execute: {
      tests: {
        src: "tmp/tests.js"
      },
      bench: {
        src: "tmp/benchmarks.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-contrib-concat");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test",    ["clean", "psc:tests", "execute:tests"]);
  grunt.registerTask("bench",   ["clean", "psc:bench", "execute:bench"]);
  grunt.registerTask("make",    ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};
