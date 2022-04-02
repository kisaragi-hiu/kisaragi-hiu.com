function ensureString(str) {
  if (str) {
    return str.toLowerCase();
  } else {
    return "";
  }
}
function ensureArray(obj) {
  if (obj) {
    return obj.join("").toLowerCase();
  } else {
    return [""];
  }
}

function shouldInclude(params, needle) {
  let title = ensureString(params.title);
  let series = ensureString(params.series);
  let voice = ensureString(params.voice);
  let tags = ensureArray(params.tags);
  // TODO: Section should be selected in a radiobutton, not filtered
  // with the text box
  let section = ensureString(params.section);

  str = title + tags + series + voice + section;
  // "a b" -> only items including both "a" and "b" are included
  return needle.split(" ").every((x) => str.includes(x));
}

function updateSearch() {
  let input_elem = document.getElementById("search");
  let needle = input_elem.value.toLowerCase();
  for (const post of document.getElementById("posts").children) {
    let params = JSON.parse(post.getAttribute("data-params"));
    if (shouldInclude(params, needle)) {
      post.style.display = "unset";
    } else {
      post.style.display = "none";
    }
  }
}
