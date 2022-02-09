exports.padInitToolbar = function (hook_name, args) {
  const toolbar = args.toolbar;

  const wagsPlay = toolbar.button({
    command: 'epWagsPlay',
    localizationId: 'epWagsPlay.toolbar.toggle.title',
    class: 'buttonicon buttonicon-play ep-wags-play',
  });

  toolbar.registerButton('epWagsPlay', wagsPlay);

  const wagsError = toolbar.button({
    command: 'epWagsError',
    localizationId: 'epWagsError.toolbar.toggle.title',
    class: 'buttonicon ep-wags-error hide-wags-error',
  });

  toolbar.registerButton('epWagsError', wagsError);

  const wagsUpload = toolbar.button({
		command: "epWagsUpload",
		localizationId: "epWagsUpload.toolbar.toggle.title",
		class: "buttonicon buttonicon-file-import",
	});

	toolbar.registerButton("epWagsUpload", wagsUpload);

  const wagsRecord = toolbar.button({
		command: "epWagsRecord",
		localizationId: "epWagsRecord.toolbar.toggle.title",
		class: "buttonicon buttonicon-microphone-alt ep-wags-record",
	});

	toolbar.registerButton("epWagsRecord", wagsRecord);

  const wagsExport = toolbar.button({
		command: "epWagsExport",
		localizationId: "wagsExport.toolbar.toggle.title",
		class: "buttonicon ep-wags-export-off",
	});

	toolbar.registerButton("epWagsExport", wagsExport);

};

const eejs = require('ep_etherpad-lite/node/eejs/');
exports.eejsBlock_editorContainerBox = function (_, args, cb) {
  args.content += eejs.require('ep_wags/templates/errors.ejs');
  args.content += eejs.require("ep_wags/templates/record.ejs");
  return cb();
};

exports.eejsBlock_styles = function (_, args, cb) {
  args.content +=
		eejs.require('ep_wags/templates/stylesheets.ejs');
  return cb();
};
