const path = require("path");

module.exports = {
	mode: "development",
	devtool: "source-map",

	entry: "./src/index.js",
	output: {
		path: path.resolve(__dirname, "www"),
		filename: "bundle.js",
		publicPath: "/"
	},

}
