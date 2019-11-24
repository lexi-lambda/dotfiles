if (window.location.hash.length > 0) {
  history.pushState({}, document.title, window.location.pathname + window.location.search)
}
