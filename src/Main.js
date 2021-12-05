"use strict";

var awfulHack = {
	push: function () {
		return function () {};
	},
};

exports.setErrorText_ = (text) => () => {
	const $wagsErrorMessage = $("#wagsErrorMessage");
  $wagsErrorMessage.html(text);
}

exports.sanitizeUsingRegex_ = (str) => {
	var highR = new RegExp("[\u0200-\uFFFF]+", "g");;
	var b = str.replace(highR, "");
	return b;
}

exports.postToolbarInit_ = (args) => (cb) => () => {
	var editbar = args.toolbar;
	const $editBar = $("#editbar");
	editbar.registerDropdownCommand("epWagsError", "wagsError");
	editbar.registerCommand(
		"epWagsPlay",
		cb(() =>
			$editBar
				.find(".ep-wags-error")
				.removeClass("hide-wags-error")
				.addClass("show-wags-error")
		)(() =>
			$editBar
				.find(".ep-wags-error")
				.removeClass("show-wags-error")
				.addClass("hide-wags-error")
		)(() =>
			$editBar
				.find(".ep-wags-play")
				.removeClass("buttonicon-play")
				.addClass("buttonicon-cog")
				.addClass("cog-spin")
		)(() =>
			$editBar
				.find(".ep-wags-play")
				.removeClass("buttonicon-stop")
				.addClass("buttonicon-play")
		)(() =>
			$editBar
				.find(".ep-wags-play")
				.removeClass("buttonicon-cog")
				.removeClass("cog-spin")
				.addClass("buttonicon-stop")
		)((push) => () => {
			awfulHack.push = push;
		})
	);
};

exports.getCurrentText_ = () => {
	var io = $('iframe[name="ace_outer"]');
	if (io) {
		var ii = io.contents().find('iframe[name="ace_inner"]');
		if (ii) {
			return ii
				.contents()
				.find(".ace-line")
				.map(function () {
					return $(this).text();
				})
				.get()
				.join("\n");
		} else {
			console.error("no inner ace");
		}
	} else {
		console.error("no outer ace");
	}
	return "";
};

exports.getAwfulHack_ = () => {
	return awfulHack.push;
};
