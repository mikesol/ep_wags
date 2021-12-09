var path = require("path");
var HtmlWebpackPlugin = require("html-webpack-plugin");
// ngrok http --region=eu --hostname=miro-wags.eu.ngrok.io 9000
module.exports = (env) => {
	var base = {
		mode: "development",
		entry: "./src/miro-index.js",
		output: {
			path: path.resolve(__dirname, "miro"),
			filename: "miro-index.js",
			clean: true,
		},
		module: {
			rules: [
				{
					test: /\.js$/i,
					include: path.resolve(__dirname, "src"),
					use: {
						loader: "babel-loader",
						options: {
							presets: ["@babel/preset-env"],
						},
					},
				},
			],
		},
		plugins: [
			new HtmlWebpackPlugin({
				inject: "head",
				templateContent: `<!DOCTYPE html>
<html>
  <head>
		<script src="https://miro.com/app/static/sdk.1.1.js"></script>
  </head>
  <body>
  </body>
</html>`,
			}),
		],
		devServer: {
			static: {
				directory: path.join(__dirname, "miro"),
			},
			compress: true,
			port: 9000,
			watchFiles: ["output/**/*.js"],
		},
	};
	return base;
};
