function shouldInclude(params, needle) {
  let title = params.title || "";
  title = title.toLowerCase();
  let series = params.series || "";
  series = series.toLowerCase();
  let tags = params.tags || [""];
  tags = tags.join("").toLowerCase();
  return (title + tags + series).includes(needle);
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
