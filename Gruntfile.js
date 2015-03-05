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
      tests: ["tmp/tests.js", "output"],
      benchmarks: ["tmp/benchmarks.js", "output"]
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
      fingertree: {
        src: "src/Data/FingerTree.purs",
        dest: "docs/Data.FingerTree.md"
      }
    },

    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test", ["clean:tests", "psc:tests", "execute"]);
  grunt.registerTask("benchmarks", ["clean:benchmarks", "psc:benchmarks"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};
