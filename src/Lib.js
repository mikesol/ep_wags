"use strict";

const filestack = require("filestack-js");
const WaveSurfer = require("wavesurfer.js");
const Microphone = require("wavesurfer.js/dist/plugin/wavesurfer.microphone.js");
const Regions = require("wavesurfer.js/dist/plugin/wavesurfer.regions.js");
const emr = require("extendable-media-recorder");
const wavrec = require("extendable-media-recorder-wav-encoder");
const util = require("audio-buffer-utils");
const lamejs = require("lamejs");

// sort of hackish
wavrec.connect().then((c) => emr.register(c));

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

const wavToMp3 = (channels, sampleRate, left, right = null) => {
	var buffer = [];
	var mp3enc = new lamejs.Mp3Encoder(channels, sampleRate, 128);
	var remaining = left.length;
	var samplesPerFrame = 1152;

	for (var i = 0; remaining >= samplesPerFrame; i += samplesPerFrame) {
		if (!right) {
			var mono = left.subarray(i, i + samplesPerFrame);
			var mp3buf = mp3enc.encodeBuffer(mono);
		} else {
			var leftChunk = left.subarray(i, i + samplesPerFrame);
			var rightChunk = right.subarray(i, i + samplesPerFrame);
			var mp3buf = mp3enc.encodeBuffer(leftChunk, rightChunk);
		}
		if (mp3buf.length > 0) {
			buffer.push(mp3buf); //new Int8Array(mp3buf));
		}
		remaining -= samplesPerFrame;
	}
	var d = mp3enc.flush();
	if (d.length > 0) {
		buffer.push(new Int8Array(d));
	}

	var mp3Blob = new Blob(buffer, { type: "audio/mp3" });
	//var bUrl = window.URL.createObjectURL(mp3Blob);

	// send the download link to the console
	//console.log('mp3 download:', bUrl);
	return mp3Blob;
};

const audioBufferToWav = (aBuffer) => {
	let numOfChan = aBuffer.numberOfChannels,
		btwLength = aBuffer.length * numOfChan * 2 + 44,
		btwArrBuff = new ArrayBuffer(btwLength),
		btwView = new DataView(btwArrBuff),
		btwChnls = [],
		btwIndex,
		btwSample,
		btwOffset = 0,
		btwPos = 0;
	setUint32(0x46464952); // "RIFF"
	setUint32(btwLength - 8); // file length - 8
	setUint32(0x45564157); // "WAVE"
	setUint32(0x20746d66); // "fmt " chunk
	setUint32(16); // length = 16
	setUint16(1); // PCM (uncompressed)
	setUint16(numOfChan);
	setUint32(aBuffer.sampleRate);
	setUint32(aBuffer.sampleRate * 2 * numOfChan); // avg. bytes/sec
	setUint16(numOfChan * 2); // block-align
	setUint16(16); // 16-bit
	setUint32(0x61746164); // "data" - chunk
	setUint32(btwLength - btwPos - 4); // chunk length

	for (btwIndex = 0; btwIndex < aBuffer.numberOfChannels; btwIndex++)
		btwChnls.push(aBuffer.getChannelData(btwIndex));

	while (btwPos < btwLength) {
		for (btwIndex = 0; btwIndex < numOfChan; btwIndex++) {
			// interleave btwChnls
			btwSample = Math.max(-1, Math.min(1, btwChnls[btwIndex][btwOffset])); // clamp
			btwSample =
				(0.5 + btwSample < 0 ? btwSample * 32768 : btwSample * 32767) | 0; // scale to 16-bit signed int
			btwView.setInt16(btwPos, btwSample, true); // write 16-bit sample
			btwPos += 2;
		}
		btwOffset++; // next source sample
	}

	let wavHdr = lamejs.WavHeader.readHeader(new DataView(btwArrBuff));

	//Stereo
	let data = new Int16Array(btwArrBuff, wavHdr.dataOffset, wavHdr.dataLen / 2);
	let leftData = [];
	let rightData = [];
	for (let i = 0; i < data.length; i += 2) {
		leftData.push(data[i]);
		rightData.push(data[i + 1]);
	}
	var left = new Int16Array(leftData);
	var right = new Int16Array(rightData);
	var wavBlob = new Blob([btwArrBuff], { type: "audio/wav" });

	if (wavHdr.channels === 2)
		return {
			wavBlob: wavBlob,
			mp3Blob: wavToMp3(wavHdr.channels, wavHdr.sampleRate, left, right),
		};
	else if (wavHdr.channels === 1)
		return {
			wavBlob: wavBlob,
			mp3Blob: wavToMp3(wavHdr.channels, wavHdr.sampleRate, data),
		};
	else return { wavBlob: wavBlob };

	function setUint16(data) {
		btwView.setUint16(btwPos, data, true);
		btwPos += 2;
	}

	function setUint32(data) {
		btwView.setUint32(btwPos, data, true);
		btwPos += 4;
	}
};

const hooks = require("ep_etherpad-lite/static/js/pluginfw/hooks");
const pad = require("ep_etherpad-lite/static/js/pad").pad;
const ChatMessage = require("ep_etherpad-lite/static/js/ChatMessage");
const sendMessage = (text) => {
	const message = new ChatMessage(text);
	hooks.aCallAll("chatSendMessage", Object.freeze({ message })).then(() => {
		pad.collabClient.sendMessage({
			type: "CHAT_MESSAGE",
			message,
		});
	});
};
exports.postToolbarInit_ = (args) => (cb) => () => {
	const editbar = args.toolbar;
	const $editBar = $("#editbar");
	const client = filestack.init("ArcQnSXbqT4O0OORDyYxrz");
	editbar.registerCommand("epWagsUpload", () => {
		client
			.picker({
				onFileUploadFinished: (inFile) => {
					const text =
						'I uploaded "' + inFile.filename + '" to this url: ' + inFile.url;
					sendMessage(text);
				},
			})
			.open();
	});
	editbar.registerDropdownCommand("epWagsError", "wagsError");
	editbar.registerDropdownCommand("epWagsRecord", "wagsRecord");
	const makeRecorder = () => {
		$("#wavesurferJsRecorder").empty();
		return WaveSurfer.create({
			container: "#wavesurferJsRecorder",
			waveColor: "violet",
			progressColor: "purple",
			plugins: [Microphone.create()],
		});
	};
	const makeEditor = () => {
		$("#wavesurferJsRecorder").empty();
		return WaveSurfer.create({
			container: "#wavesurferJsRecorder",
			waveColor: "violet",
			progressColor: "purple",
			plugins: [Regions.create({})],
		});
	};
	const recorderFlow = () => {
		const recorderWavesurfer = makeRecorder();
		const chunks = [];
		const startMicrophone = () => {
			console.log("starting microphone");
			chunks.length = 0;
			recorderWavesurfer.microphone.on("deviceReady", function (stream) {
				const mediaRecorder = new emr.MediaRecorder(stream, {
					mimeType: "audio/wav",
				});
				//const mediaRecorder = new MediaRecorder(stream);
				mediaRecorder.ondataavailable = (event) => {
					console.log('data available');
					chunks.push(event.data);
				};
				mediaRecorder.start();
				$("#recordingstart").removeClass("show-button").addClass("hide-button");
				$("#recordingstop").removeClass("hide-button").addClass("show-button");

				const stopMicrophone = () => {
					mediaRecorder.onstop = () => {
						const blob = new Blob(chunks, { type: "audio/wav" });
						//const blob = new Blob(chunks, { type: "audio/ogg; codecs=opus" });
						const editorWavesurfer = makeEditor();
						$("#recordingstop")
							.removeClass("show-button")
							.addClass("hide-button");
						$("#recordingupload")
							.removeClass("hide-button")
							.addClass("show-button");
						$("#recordingplay")
							.removeClass("hide-button")
							.addClass("show-button");
						$("#recordingcancel")
							.removeClass("hide-button")
							.addClass("show-button");
						editorWavesurfer.on("error", function (e) {
							console.error(e);
						});
						editorWavesurfer.on("ready", function () {
							const dur = editorWavesurfer.getDuration();
							const region = editorWavesurfer.addRegion({
								color: "hsla(400, 100%, 30%, 0.5)",
								loop: true,
								start: dur * 0.1,
								end: dur * 0.9,
								minLength: 0.02,
							});
							const playLoop = () => {
								region.playLoop();
								$("#recordingplay").text("Pause").off('click').on('click',pause);
							};
							const pause = () => {
								editorWavesurfer.pause();
								$("#recordingplay").text("Play").off('click').on('click',playLoop);
							};
							const uploadRecording = () => {
								resetInterface();
								const ctx = new (window.AudioContext ||
									window.webkitAudioContext)();
								const audioURL = URL.createObjectURL(blob);
								fetch(audioURL)
									.then((b) => b.arrayBuffer())
									.then((b) => ctx.decodeAudioData(b))
									.then((buf) => {
										const newBuf = util.slice(buf, parseInt(region.start * buf.sampleRate), parseInt(region.end * buf.sampleRate));
										const output = audioBufferToWav(newBuf);
										const tokenW = {};
										const tokenM = {};
										const onRetry = (obj) => {
											console.log(
												`Retrying ${obj.location} for ${obj.filename}. Attempt ${obj.attempt} of 10.`
											);
										};
										const timeNow = new Date().getTime();
										client
											.upload(
												output.wavBlob,
												{ onRetry },
												{ filename: Math.floor(timeNow) + ".wav" },
												tokenW
											)
											.then((res) => {
												const wavText =
													"I uploaded a wav version of your recording to this url: " +
													res.url;
												sendMessage(wavText);
											});
										if (output.mp3Blob) {
											client
												.upload(
													output.mp3Blob,
													{ onRetry },
													{ filename: Math.floor(timeNow) + ".mp3" },
													tokenM
												)
												.then((res) => {
													const mp3Text =
														"I uploaded a mp3 version of your recording to this url: " +
														res.url;
													sendMessage(mp3Text);
												});
										}
									});
							};
							$("#recordingplay").off('click').on('click',playLoop);
							///-------
							const resetInterface = () => {
								editbar.toggleDropDown("epWagsRecord", () => {
									$("#recordingstart")
										.removeClass("hide-button")
										.addClass("show-button");
									$("#recordingupload")
										.removeClass("show-button")
										.addClass("hide-button");
									$("#recordingplay")
										.removeClass("show-button")
										.addClass("hide-button");
									$("#recordingcancel")
										.removeClass("show-button")
										.addClass("hide-button");
									recorderFlow();
								});
							};
							const cancelRecording = () => {
								resetInterface();
							};
							///-------
							$("#recordingupload").off('click').on('click',uploadRecording);
							$("#recordingcancel").off('click').on('click',cancelRecording);
						});
						editorWavesurfer.loadBlob(blob);
						//editorWavesurfer.load(audioURL);
					}
					mediaRecorder.stop();
					recorderWavesurfer.microphone.stop();
					recorderWavesurfer.stop();
				};
				$("#recordingstop").off('click').on('click',stopMicrophone);
			});
			recorderWavesurfer.microphone.start();
		};

		$("#recordingstart").off('click').on('click',startMicrophone);
	};

	recorderFlow();
	const f = cb(() =>
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
	$editBar.find(".ep-wags-play").off('click').on('click',f);
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
