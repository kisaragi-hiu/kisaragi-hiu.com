/**
 * Ensure `str` is a string.
 * @param {?string} str
 * @returns {string}
 */
function normalizeString(str) {
  if (!str) return "";
  return str.toLowerCase();
}
/**
 * Join `obj` into a string.
 * @param {?array} obj
 * @returns {string}
 */
function normalizeArray(obj) {
  if (!obj) return "";
  return obj.join("").toLowerCase();
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
    filters.length !== 0 &&
    !filters.some((filter) => {
      return params[filter.type] === filter.value;
    })
  ) {
    return false;
  }
  const title = normalizeString(params.title);
  const series = normalizeString(params.series);
  const voice = normalizeString(params.voice);
  const tags = normalizeArray(params.tags);
  // TODO: Section should be selected in a radiobutton, not filtered
  // with the text box
  const section = normalizeString(params.section);

  str = title + tags + series + voice + section;
  // "a b" -> only items including both "a" and "b" are included
  return needle.split(" ").every((x) => str.includes(x));
}
function updateSearch() {
  const inputElem = document.getElementById("search");
  const filters = [];
  const filtersElem = document.getElementById("filters");
  const elems = filtersElem
    ? filtersElem.querySelectorAll("input:checked")
    : [];
  for (const x of elems) {
    filters.push(JSON.parse(x.getAttribute("data-filter")));
  }
  const needle = inputElem.value.toLowerCase();
  for (const post of document.getElementById("posts").children) {
    const params = JSON.parse(post.getAttribute("data-params"));
    if (shouldInclude(params, needle, filters)) {
      post.style.display = "unset";
    } else {
      post.style.display = "none";
    }
  }
}
