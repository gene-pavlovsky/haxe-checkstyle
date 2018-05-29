module.exports = function(grunt) {
    grunt.initConfig({
        pkg: grunt.file.readJSON("package.json"),

        shell: {
            libs: {
                command: "lix download"
            }
        },

        haxe: haxeOptions(),

        zip: {
            "checkstyle.zip": [
                "src/**",
                "resources/sample-config.json", "resources/logo.png", "resources/codeclimate_pr.png",
                "haxelib.json", "run.n", "README.md", "CHANGES.md"
            ]
        }
    });

    grunt.loadNpmTasks("grunt-haxe");
    grunt.loadNpmTasks("grunt-zip");
    grunt.loadNpmTasks("grunt-shell");
    grunt.registerTask("default", ["shell", "haxe:all"]);
};

function haxeOptions() {
    return {
        all: {
            hxml: "buildAll.hxml"
        },
        build: {
            hxml: "build.hxml"
        },
        test: {
            hxml: "buildTest.hxml"
        },
        debug: {
            hxml: "buildDebug.hxml"
        },
        telemetry: {
            hxml: "buildTelemetry.hxml"
        }
    };
}