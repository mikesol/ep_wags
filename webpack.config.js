var path = require("path");
var webpack = require("webpack");
module.exports = {
	// mode: env.production ? "production" : "development",
	entry: "./src/ether.js",
	output: {
		path: path.resolve(__dirname, "static/js"),
		filename: "main.js",
		library: { type: "commonjs" },
		clean: true,
	},
	resolve: {
		fallback: {
			assert: require.resolve("assert/"),
			os: require.resolve("os-browserify/browser"),
		},
	},
	externals: {
		"ep_etherpad-lite/static/js/pluginfw/hooks":
			"commonjs ep_etherpad-lite/static/js/pluginfw/hooks",
		"ep_etherpad-lite/static/js/pad": "commonjs ep_etherpad-lite/static/js/pad",
		"ep_etherpad-lite/static/js/ChatMessage":
			"commonjs ep_etherpad-lite/static/js/ChatMessage",
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
		new webpack.ProvidePlugin({
			process: "process/browser",
		}),
	],
};
