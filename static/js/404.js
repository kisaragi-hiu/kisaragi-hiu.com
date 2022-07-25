let pathname = window.location.pathname;
if (pathname.match(/\.html$/)) {
  window.location.pathname = pathname.replace(/\.html$/, "");
}
