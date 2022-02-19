function shouldInclude(params, needle) {
  let title = params.title || "";
  title = title.toLowerCase();
  let series = params.series || "";
  series = series.toLowerCase();
  let voice = params.voice || "";
  voice = voice.toLowerCase();
  let tags = params.tags || [""];
  tags = tags.join("").toLowerCase();
  // TODO: Section should be selected in a radiobutton, not filtered
  // with the text box
  let section = params.section || "";
  section = section.toLowerCase();

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
