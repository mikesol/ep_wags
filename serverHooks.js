exports.padInitToolbar = function (hook_name, args) {
	var toolbar = args.toolbar;

	var wagsPlay = toolbar.button({
		command: "epWagsPlay",
		localizationId: "epWagsPlay.toolbar.toggle.title",
		class: "buttonicon buttonicon-play ep-wags-play",
	});

	toolbar.registerButton("epWagsPlay", wagsPlay);

	var wagsError = toolbar.button({
		command: "epWagsError",
		localizationId: "epWagsPlay.toolbar.toggle.title",
		class: "buttonicon ep-wags-error hide-wags-error",
	});

	toolbar.registerButton("epWagsError", wagsError);
};

var eejs = require("ep_etherpad-lite/node/eejs/");
exports.eejsBlock_embedPopup = function (hook_name, args, cb) {
	args.content = args.content + eejs.require("ep_wags/templates/errors.ejs");
	return cb();
};

exports.eejsBlock_styles = function (hook_name, args, cb) {
	args.content =
		args.content +
		eejs.require("ep_wags/templates/stylesheets.ejs");
	return cb();
};
