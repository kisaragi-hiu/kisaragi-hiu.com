#lang pollen
function openNav() {
    document.getElementById("site-sidebar").style.width = "◊|style/sidebar-size|";
}

function closeNav() {
    document.getElementById("site-sidebar").style.width = 0;
}

function toggleNav() {
    if (document.getElementById("site-sidebar").style.width != "◊|style/sidebar-size|") {
        // if width not at opened size, assume closed
        openNav();
    } else {
        closeNav();
    }
}
