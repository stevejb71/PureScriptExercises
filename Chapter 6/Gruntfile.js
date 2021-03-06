module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      all: {
        src: ["<%=srcFiles%>"],
        dest: "dist/Chapter6.js"
      }
    },
    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("default", ["psc:all", "dotPsci"]);
};