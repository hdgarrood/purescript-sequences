module.exports = function(grunt) {
  "use strict";

  var libFiles = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
  ]

  grunt.initConfig({
    libFiles: libFiles,

    clean: ["output"],

    pscMake: libFiles,
    dotPsci: libFiles,
    pscDocs: {
      readme: {
        src: "src/**/*.purs",
        dest: "documentation.md"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};
