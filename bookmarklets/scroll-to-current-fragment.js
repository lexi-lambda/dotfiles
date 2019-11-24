const hash = window.location.hash
if (hash.startsWith('#')) {
  const fragment = decodeURIComponent(hash.substring(1))
  const elementById = document.getElementById(fragment)
  if (elementById) {
    elementById.scrollIntoView()
  } else {
    const elementsByName = document.getElementsByName(fragment)
    if (elementsByName.length > 0) {
      elementsByName[0].scrollIntoView()
    }
  }
}
