/**
 * Ensure `str` is a string.
 * @param {?string} str
 * @returns {string}
 */
function normalizeString(str) {
  if (str) {
    return str.toLowerCase();
  } else {
    return "";
  }
}
/**
 * Join `obj` into a string.
 * @param {?array} obj
 * @returns {string}
 */
function normalizeArray(obj) {
  if (obj) {
    return obj.join("").toLowerCase();
  } else {
    return "";
  }
}

/**
 * Return whether `params` should be included in the results.
 * @param {object} params - The item itself.
 * @param {string} needle - The current search string.
 * @param {array} filters - Exclude an item if it does not at least
 * match one of the filters.
 * @returns {boolean}
 */
function shouldInclude(params, needle, filters) {
  if (
    filters.length != 0 &&
    !filters.some((filter) => {
      return params[filter.type] == filter.value;
    })
  ) {
    return false;
  }
  let title = normalizeString(params.title);
  let series = normalizeString(params.series);
  let voice = normalizeString(params.voice);
  let tags = normalizeArray(params.tags);
  // TODO: Section should be selected in a radiobutton, not filtered
  // with the text box
  let section = normalizeString(params.section);

  str = title + tags + series + voice + section;
  // "a b" -> only items including both "a" and "b" are included
  return needle.split(" ").every((x) => str.includes(x));
}
function updateSearch() {
  let input_elem = document.getElementById("search");
  let filters = [];
  let elems = document
    .getElementById("filters")
    .querySelectorAll("input:checked");
  elems.forEach((x) => {
    filters.push(JSON.parse(x.getAttribute("data-filter")));
  });
  let needle = input_elem.value.toLowerCase();
  for (const post of document.getElementById("posts").children) {
    let params = JSON.parse(post.getAttribute("data-params"));
    if (shouldInclude(params, needle, filters)) {
      post.style.display = "unset";
    } else {
      post.style.display = "none";
    }
  }
}
