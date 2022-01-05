"use strict";

const awfulHack = {
	push() {
		return function () {};
	},
};

const playKey = {
	push() {
		return function () {};
	},
};

exports.wireUpCtrlP_ = (push) => () => {
	document.addEventListener("keydown", function (event) {
		if (event.ctrlKey && event.key === "g") {
			push()();
		}
	});
};

exports.setErrorText_ = (text) => () => {
	const $wagsErrorMessage = $("#wagsErrorMessage");
	$wagsErrorMessage.html(text);
};

exports.sanitizeUsingRegex_ = (str) => {
	const badUCode = new RegExp("[\u00A0]+", "g");
	const out = str.replace(badUCode, "");
	return out;
};

exports.postToolbarInit_ = (args) => (cb) => () => {
	const editbar = args.toolbar;
	const $editBar = $("#editbar");
	var client = filestack.init("ArcQnSXbqT4O0OORDyYxrz");
	editbar.registerCommand("epWagsUpload", () => {
		client
			.picker({
				onFileUploadFinished: (inFile) => {
					const chat = require("ep_etherpad-lite/static/js/chat").chat;
					chat.addMessage(
						{
							text:
								'I uploaded "' +
								inFile.filename +
								'" to this url: ' +
								inFile.url,
							authorId:
								require("ep_etherpad-lite/static/js/pad").pad.getUserId(),
							time: new Date().getTime(),
							displayName: "Uploader"
						},
						true,
						false
					);
				},
			})
			.open();
	});
	editbar.registerDropdownCommand("epWagsError", "wagsError");
	var f = cb(() =>
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
	});
	// does not work in safari
	//////editbar.registerCommand("epWagsPlay", f);
	$editBar.find(".ep-wags-play").click(f);
	return f;
};

exports.getCurrentText_ = () => {
	const io = $('iframe[name="ace_outer"]');
	if (io) {
		const ii = io.contents().find('iframe[name="ace_inner"]');
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

exports.getAwfulHack_ = () => awfulHack.push;
exports.getPlayKey_ = () => playKey.push;
exports.setPlayKey_ = (push) => () => {
	playKey.push = push;
};
exports.isCtrlG = (args) => () => {
	return args.evt.ctrlKey && args.evt.key === "g";
};

//////////

const USER_ACTIVATION_EVENTS = [
	"auxclick",
	"click",
	"contextmenu",
	"dblclick",
	"keydown",
	"keyup",
	"mousedown",
	"mouseup",
	"touchend",
];

exports.setUpIosAudio = function () {
	$("#editbar")
		.find(".ep-wags-play")
		.after(
			'<audio id="wagsSilenceHack" loop        src="https://media.graphcms.com/b0IXeyJzSDCZgVRHkFHL"> Your browser does not support the <code>audio</code> element.</audio>'
		);
};

exports.startIosAudio = function () {
	document.getElementById("wagsSilenceHack").play();
};

exports.stopIosAudio = function () {
	document.getElementById("wagsSilenceHack").pause();
};
