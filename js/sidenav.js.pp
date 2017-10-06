#lang pollen
◊(define sidebar-width "10em")

function openNav() {
    document.getElementById("site-sidebar").style.width = "◊|sidebar-width|";
}

function closeNav() {
    document.getElementById("site-sidebar").style.width = 0;
}

function toggleNav() {
    if (document.getElementById("site-sidebar").style.width != "◊|sidebar-width|") {
        // if width not at opened size, assume closed
        openNav();
    } else {
        closeNav();
    }
}
