// This is what I actually edit when I commit vercel.json.

let config = {};
function rewrite(from, to) {
  config.rewrites = config.rewrites || [];
  config.rewrites.push({
    source: from,
    destination: to,
  });
}
function redirect(from, to) {
  config.redirects = config.redirects || [];
  config.redirects.push({ source: from, destination: to });
}
function subproject(from, to) {
  rewrite(`${from}/:slug*`, `${to}/:slug*`);
}

rewrite("/static/:slug*", "/:slug*");
for (const x of ["/feed.xml", "/rss.xml", "/atom.zml", "/feeds/all.atom.xml"]) {
  rewrite(x, "/index.xml");
}
redirect("/poems", "/barren-moon");
subproject("/barren-moon", "https://barren-moon.vercel.app");
subproject("/timer", "https://timer-alpha-one.vercel.app");
subproject(
  "/list-of-plants-of-formosa",
  "https://list-of-plants-of-formosa.vercel.app"
);
subproject(
  "/tftt",
  "https://trumpet-fingerings-practice-tool-experiment.vercel.app"
);

redirect(
  "/youtube",
  "https://www.youtube.com/channel/UCl_hsqcvdX0XdgBimRQ6R3A"
);
redirect("/niconico", "https://www.nicovideo.jp/user/38995186");
redirect("/pixiv", "https://www.pixiv.net/member.php?id=14235181");
redirect("/aqi", "https://aqi.tw");

// This must come before the /blog -> / redirect
// Created on an incorrect path initially
redirect(
  "/covers/2022-07-14-大地の閾を探して",
  "/covers/20220714-大地の閾を探して"
);
// Blog -> content note migration
redirect(
  "/blog/2021-06-02-insert-key-double-key",
  "/2021-06-02-insert-key-double-key"
);
redirect(
  "/blog/2021-05-01-idsmile-trumpet-sheet",
  "/2021-05-01-idsmile-trumpet-sheet"
);
redirect("/blog/2017-09-24-pollen", "/2017-09-24-pollen");
redirect(
  "/blog/2017-09-13-discovering-racket",
  "/2017-09-13-discovering-racket"
);
redirect("/blog/2016-06-30-han-characters", "/2016-06-30-han-characters");
redirect(
  "/blog/2020-02-16-slack-export-to-org",
  "/2020-02-16-slack-export-to-org"
);
redirect("/blog/2020-10-25-ox-pollen", "/2020-10-25-ox-pollen");
redirect(
  "/blog/2020-10-31-current-infrastructure",
  "/2020-10-31-infrastructure"
);
redirect("/blog/2020-03-20-hugo", "/2021-03-20-switching-to-hugo");
redirect("/blog/2020-12-16-2020-review", "/2020-review");
redirect("/blog/2018-07-27-utau-tip-labels", "/utau-labels");
redirect(
  "/blog/2017-09-12-termux-and-linux-deploy",
  "/termux-and-linux-deploy"
);
redirect("/blog/2017-06-28-redire", "/redire-cover");
redirect("/blog/2017-03-19-new-website", "/new-website");
redirect(
  "/blog/2017-07-10-itsukauminomierubashode",
  "/itsukauminomierubashode"
);
redirect("/blog/2017-10-10-iterations", "/iterations");
redirect("/blog/2019-05-15-closure", "/closures");
redirect("/blog/2019-04-17-utau-make-file", "/utau-make-file");
redirect(
  "/blog/2017-11-26-east-asian-names-reverse-or-not",
  "/east-asian-name-reverse"
);
redirect("/blog/2017-12-22-attempt-to-explain-lambda", "/lambda");
redirect(
  "/blog/2021-02-14-test-emacs-lisp-gitlab-ci",
  "/emacs-lisp-testing-gitlab-ci"
);

// / is /blog
redirect("/blog", "/");
redirect("/posts", "/");

// old frog/pollen hybrid site URLs
redirect("/tags/category-:series*", "/series/:series*");

redirect("/projects/cangjie.el", "https://github.com/kisaragi-hiu/cangjie.el");
redirect("/projects/timetrack", "https://gitlab.com/canrylog");
redirect("/projects/yasearch", "https://gitlab.com/kisaragi-hiu/yasearch");
redirect(
  "/projects/org-inline-video-thumbnails",
  "https://github.com/kisaragi-hiu/org-inline-video-thumbnails"
);
redirect(
  "/projects/background-job.el",
  "https://github.com/kisaragi-hiu/background-job.el"
);

redirect("/dotfiles", "https://gitlab.com/kisaragi-hiu/dotfiles");
redirect("/.emacs.d", "https://gitlab.com/kisaragi-hiu/.emacs.d");

redirect("/KisaragiHiu.gpg", "/KisaragiHiu.asc");

// old urls
redirect("/categories", "/series");
// Category -> Series
redirect("/taxonomy", "/tags/taxonomy");
redirect("/category/experiment", "/tags/experiments");
redirect("/category/:series*", "/series/:series*");
redirect("/series/tutorials", "/tags/tutorials");

console.log(JSON.stringify(config));
