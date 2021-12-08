'use strict';

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

describe('plays sound', function () {
  // create a new pad before each test run
  beforeEach(async function () {
    await helper.aNewPad();
  });


  it('plays the sound', async function () {
    const chrome$ = helper.padChrome$;

    // get the play button and click it
    const $playButton = chrome$('.buttonicon-play');
    $playButton.click();
  });
});
