const awfulHack = {
	push() {
		return function () {};
	},
};

exports.getAwfulHack_ = () => awfulHack.push;

// making pure
exports.stripHtml = function (html) {
	let tmp = document.createElement("DIV");
	tmp.innerHTML = html;
	return tmp.textContent || tmp.innerText || "";
};

exports.isAuthorized = function () {
	return miro.isAuthorized();
};

exports.requestAuthorization = function () {
	return miro.requestAuthorization();
};

exports.allTexts = function () {
	return miro.board.widgets.get({ type: "text" });
};

exports.miroOnReady = function (selectionUpdated) {
	return function (clickCb) {
		return function (fail) {
			return function (success) {
				return function () {
					miro.onReady(() => {
						miro
							.initialize({
								extensionPoints: {
									bottomBar: {
										title: "wag!",
										svgIcon:
											'<polygon points="3,4 3,18, 16,11" fill="none" fill-rule="evenodd" stroke="currentColor" stroke-width="2"/>',
										onClick: clickCb(() => {
											/** this will show the error */
										})(() => {
											/** this will hide the error */
										})(() => {
											/** this will add a loading spinner */
										})(() => {
											/** this will add a play icon back */
										})(() => {
											/** this will pull up the stop icon */
										})((push) => () => {
											awfulHack.push = push;
										}),
									},
								},
							})
							.then(
								(r) => {
									miro.addListener("SELECTION_UPDATED", selectionUpdated);
									Promise.resolve(success(r)());
								},
								(e) => {
									fail(e)();
									Promise.reject(e);
								}
							);
					});
				};
			};
		};
	};
};
