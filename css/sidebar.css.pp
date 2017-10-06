#lang pollen

.sidenav {
    height: 100%;
    width: 0;
    position: fixed;
    z-index: 1;
    top: 0;
    right: 0;
    background-color: #fbfbfb;
    overflow-x: hidden;
    padding-top: 2em;
    transition: 0.3s;
}

.sidenav a {
    text-decoration: none;
    font-size: 1.3em;
    color: #333;
    display: block;
    transition: 0.2s
}

.sidelinks {
    padding-top: 1.2em;
}

.sidelinks a {
    font-family: ◊|style/font-secondary|;
    padding: 1.3em 1.5em 0 0.5em;
}

.sidenav a:hover, .offcanvas a:focus{
    color: #777;
}

.sidenav .closebtn {
    position: absolute;
    top: 0;
    right: 0.3em;
    font-size: 3em;
    margin-left: 3em;
}

@media screen and (max-height: 450px) {
    .sidenav {padding-top: 15px;}
    .sidenav a {font-size: 18px;}
}
