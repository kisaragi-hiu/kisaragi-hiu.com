function openNav() {
    document.getElementById("site-sidebar").style.width = "10em";
}

function closeNav() {
    document.getElementById("site-sidebar").style.width = 0;
}

function toggleNav() {
    if (document.getElementById("site-sidebar").style.width != "10em") {
        // if width not at opened size, assume closed
        openNav();
    } else {
        closeNav();
    }
}
