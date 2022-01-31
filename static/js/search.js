function updateSearch() {
  let input_elem = document.getElementById("search");
  let needle = input_elem.value.toLowerCase();
  for (const post of document.getElementById("posts").children) {
    let params = JSON.parse(post.getAttribute("data-params"));
    if (params.title.toLowerCase().includes(needle)) {
      post.style.display = "unset";
    } else {
      post.style.display = "none";
    }
  }
}
