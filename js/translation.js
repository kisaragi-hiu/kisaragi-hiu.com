"use strict";

const bodyTag = () => document.getElementsByTagName("body")[0];

let translation = {
  setLang: lang => {
    localStorage.setItem("language", lang);
    bodyTag().className = `${localStorage.getItem("language")}`;
  },
  getLang: () => bodyTag().className,
  // initialize the local storage and set up the lang attribute
  onLoad: () => {
    // initialize the local storage
    if (localStorage.getItem("language")) {
      localStorage.setItem("language", "en");
    }
    translation.setLang(localStorage.getItem("language"));
  }
};
