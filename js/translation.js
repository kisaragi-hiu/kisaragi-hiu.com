"use strict";

const htmlTag = () => document.getElementsByTagName("html")[0];

let translation = {
  setLang: lang => {
    localStorage.setItem("language", lang);
    htmlTag().attributes["lang"] = localStorage.getItem("language");
  },
  getLang: () => htmlTag().attributes["lang"],
  // initialize the local storage and set up the lang attribute
  onLoad: () => {
    // initialize the local storage
    if (localStorage.getItem("language")) {
      localStorage.setItem("language", "en");
    }
    translation.setLang(localStorage.getItem("language"));
  }
};
