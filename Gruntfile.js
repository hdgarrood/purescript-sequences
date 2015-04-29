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
      benchmarks: {
        options: {
          module: "Benchmarks",
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
      fingertree: {
        src: "src/Data/FingerTree.purs",
        dest: "docs/Data.FingerTree.md"
      }
    },
    concat: {
      benchmarks: {
        src: ["tmp/benchmarks.js", "benchmarks/harness.js"],
        dest: "tmp/benchmarks-all.js"
      }
    },
    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-contrib-concat");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test",    ["clean", "psc:tests", "execute"]);
  grunt.registerTask("bench",   ["clean", "psc:benchmarks", "concat:benchmarks"]);
  grunt.registerTask("make",    ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};
