"use strict";

var awfulHack = {
	push: function () {
		return function () {};
	},
};

exports.postAceInit_ = (cb) => () => {
	const $editBar = $("#editbar");
	const $button = $("<button>")
		.addClass("buttonicon")
		.addClass("buttonicon-play")
		.on(
			"click",
			cb(() =>
				$button
					.removeClass("buttonicon-play")
					.addClass("buttonicon-cog")
					.addClass("icon-spin")
			)(() =>
				$button.removeClass("buttonicon-stop").addClass("buttonicon-play")
			)(() =>
				$button
					.removeClass("buttonicon-cog")
					.removeClass("icon-spin")
					.addClass("buttonicon-stop")
			)((push) => () => {
				awfulHack.push = push;
			})
		);
	const $li = $("<li>").append($button);

	$editBar.contents().find('[data-key="undo"]').before($li);
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
