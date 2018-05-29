module.exports = function (grunt) {

	grunt.initConfig({
		pkg: grunt.file.readJSON("package.json"),

		shell: {
			libs: {
                command: "lix download"
			}
		},

		haxe: {
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
		},

		zip: {
			"checkstyle.zip": ["src/**", "resources/sample-config.json", "resources/logo.png", "haxelib.json", "run.n", "README.md"]
		}
	});

	grunt.loadNpmTasks("grunt-haxe");
	grunt.loadNpmTasks("grunt-zip");
	grunt.loadNpmTasks("grunt-shell");
	grunt.registerTask("default", ["shell", "haxe:all"]);
};