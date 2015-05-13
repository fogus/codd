module.exports = function(grunt) {
  grunt.initConfig({
    watch: {
      less: {
        files: ["css/**/*.less"],
        tasks: ["less"]
      }
    },
    less: {
      compile: {
        files: {
          "css/theme/hipster.css": "css/theme/hipster.less"
        },
        options: {
          compress: true
        }
      }
    },
    server: {
      port: 1337,
      base: "."
    }
  });

  grunt.loadNpmTasks("grunt-contrib-less");

  grunt.registerTask("default", "less");
  grunt.registerTask("run", "less server watch");
};
