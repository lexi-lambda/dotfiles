browser.commands.onCommand.addListener(commandName => {
  if (commandName === 'remove-current-fragment' ||
      commandName === 'scroll-to-current-fragment') {
    browser.tabs.executeScript({
      file: `/bookmarklets/${commandName}.js`
    });
  } else {
    console.error(`dotfiles: ignoring unknown command ‘${commandName}’`);
  }
});
