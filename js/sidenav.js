function openNav() {
    document.getElementById("site-sidebar").style.width = "250px";
}

function closeNav() {
    document.getElementById("site-sidebar").style.width = 0;
}

function toggleNav() {
    if (document.getElementById("site-sidebar").style.width != "250px") {
        // if width not at opened size, assume closed
        openNav();
    } else {
        closeNav();
    }
}
