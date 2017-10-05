#lang pollen
◊(define highlight-color "#A868E8")
◊(define font-cjk-sans-serif '("Noto Sans CJK TC" "sourcehansans-tc" "Microsoft Jhenghei"))
◊(define font-code "'Source Code Pro', 'source-code-pro', monospace")

◊(define font-primary (font-family "EB Garamond"
                                   "Hannari"
                                   "cwTeXFangSong"
                                   ◊; make sure system serif is never used
                                   "Segoe UI"
                                   "Helvetica"
                                   font-cjk-sans-serif
                                   "sans-serif"))

◊(define font-secondary (font-family "Overpass"
                                     font-cjk-sans-serif
                                     "sans-serif"))

.sidenav {
    height: 100%;
    width: 0;
    position: fixed;
    z-index: 1;
    top: 0;
    right: 0;
    background-color: #fbfbfb;
    overflow-x: hidden;
    padding-top: 10em;
    transition: 0.5s;
}

.sidenav a {
    padding: 8px 8px 8px 32px;
    text-decoration: none;
    font-size: 25px;
    color: #333;
    display: block;
    transition: 0.2s
}

.sidenav a:hover, .offcanvas a:focus{
    color: #777;
}

.sidenav .closebtn {
    position: absolute;
    top: 0;
    right: 25px;
    font-size: 36px;
    margin-left: 50px;
}

@media screen and (max-height: 450px) {
    .sidenav {padding-top: 15px;}
    .sidenav a {font-size: 18px;}
}
