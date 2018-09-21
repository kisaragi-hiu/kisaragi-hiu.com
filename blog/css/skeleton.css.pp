#lang pollen
◊(require racket/string)
◊(define cjk-fallback
   ◊string-append{'Noto Sans CJK TC',
                  'Microsoft Jhenghei',
                  'Microsoft Yahei',
                  'Meiryo',
                  'Malgun Gothic'})
◊(define sans-serif
   ◊string-append{'Fira Sans',
                  ◊|cjk-fallback|,
                  'sans-serif'})
◊(define title-sans-serif
   ◊string-append{'Overpass',
                  ◊|cjk-fallback|,
                  'sans-serif'})
◊(define monospace
   ◊string-append{'Overpass Mono',
                  'Noto Sans Mono CJK',
                  ◊|cjk-fallback|,
                  'monospace'})
◊(define title "h1,h2,h3,h4,h5,h6,h7,.title")

/*
Modified from [Skeleton V2.0.4](www.getskeleton.com)
Copyright 2014, Dave Gamache
Free to use under the MIT license.
http://www.opensource.org/licenses/mit-license.php
12/29/2014
*/


/* Table of contents
––––––––––––––––––––––––––––––––––––––––––––––––––
- Grid
- Base Styles
- Typography
- Links
- Buttons
- Forms
- Lists
- Code
- Tables
- Spacing
- Utilities
- Clearing
- Media Queries
*/


/* Grid
–––––––––––––––––––––––––––––––––––––––––––––––––– */
.container {
  position: relative;
  width: 100%;
  max-width: 960px;
  margin: 0 auto;
  padding: 0 20px;
  box-sizing: border-box; }
.column,
.columns {
  width: 100%;
  float: left;
  box-sizing: border-box; }

/* For devices larger than 400px */
@media (min-width: 400px) {
  .container {
    width: 85%;
    padding: 0; }
}

/* For devices larger than 550px */
@media (min-width: 550px) {
  .container {
    width: 80%; }
  .column,
  .columns {
    margin-left: 4%; }
  .column:first-child,
  .columns:first-child {
    margin-left: 0; }

  .one.column,
  .one.columns                    { width: 4.66666666667%; }
  .two.columns                    { width: 13.3333333333%; }
  .three.columns                  { width: 22%;            }
  .four.columns                   { width: 30.6666666667%; }
  .five.columns                   { width: 39.3333333333%; }
  .six.columns                    { width: 48%;            }
  .seven.columns                  { width: 56.6666666667%; }
  .eight.columns                  { width: 65.3333333333%; }
  .nine.columns                   { width: 74.0%;          }
  .ten.columns                    { width: 82.6666666667%; }
  .eleven.columns                 { width: 91.3333333333%; }
  .twelve.columns                 { width: 100%; margin-left: 0; }

  .one-third.column               { width: 30.6666666667%; }
  .two-thirds.column              { width: 65.3333333333%; }

  .one-half.column                { width: 48%; }

  /* Offsets */
  .offset-by-one.column,
  .offset-by-one.columns          { margin-left: 8.66666666667%; }
  .offset-by-two.column,
  .offset-by-two.columns          { margin-left: 17.3333333333%; }
  .offset-by-three.column,
  .offset-by-three.columns        { margin-left: 26%;            }
  .offset-by-four.column,
  .offset-by-four.columns         { margin-left: 34.6666666667%; }
  .offset-by-five.column,
  .offset-by-five.columns         { margin-left: 43.3333333333%; }
  .offset-by-six.column,
  .offset-by-six.columns          { margin-left: 52%;            }
  .offset-by-seven.column,
  .offset-by-seven.columns        { margin-left: 60.6666666667%; }
  .offset-by-eight.column,
  .offset-by-eight.columns        { margin-left: 69.3333333333%; }
  .offset-by-nine.column,
  .offset-by-nine.columns         { margin-left: 78.0%;          }
  .offset-by-ten.column,
  .offset-by-ten.columns          { margin-left: 86.6666666667%; }
  .offset-by-eleven.column,
  .offset-by-eleven.columns       { margin-left: 95.3333333333%; }

  .offset-by-one-third.column,
  .offset-by-one-third.columns    { margin-left: 34.6666666667%; }
  .offset-by-two-thirds.column,
  .offset-by-two-thirds.columns   { margin-left: 69.3333333333%; }

  .offset-by-one-half.column,
  .offset-by-one-half.columns     { margin-left: 52%; }

}


/* Base Styles
–––––––––––––––––––––––––––––––––––––––––––––––––– */
/* NOTE
html is set to 62.5% so that all the REM measurements throughout Skeleton
are based on 10px sizing. So basically 1.5rem = 15px :) */
html {
  font-size: 62.5%;
  background-color: #fbfbfb; }
body {
  font-size: 1.5em; /* currently ems cause chrome bug misinterpreting rems on body element */
  line-height: 1.6;
  font-weight: 400;
  font-family: ◊|sans-serif|;
  color: #444;
  background-color: #fbfbfb;
  text-rendering: optimizeLegibility; }


/* Typography
–––––––––––––––––––––––––––––––––––––––––––––––––– */
h1, h2, h3, h4, h5, h6 {
  font-size: 2.4rem;
  line-height: 1.2;
  margin-top: 2.4rem;
  margin-bottom: 1.2rem;
  font-weight: 300;
  letter-spacing: normal; }
h1 { font-weight: 600; }
h2 { font-weight: 400; }
/* h3 {  } */
/* h4 {  } */
/* h5 {  } */
/* h6 {  } */

/* Larger than phablet */
@media (min-width: 550px) {
  h1 { font-size: 2.4rem; }
  h2 { font-size: 2.4rem; }
  h3 { font-size: 2.4rem; }
  h4 { font-size: 2.4rem; }
  h5 { font-size: 2.4rem; }
  h6 { font-size: 2.4rem; }
}

p {
  font-size: 100%;
  margin-top: 0;
  margin-bottom: 1.2rem; }
article {
  font-size: 1.8rem; }


/* Header
–––––––––––––––––––––––––––––––––––––––––––––––––– */
#topheader {
  font-size: 2.4rem;
  margin-top: 1em; }
#topheader .logo {
  font-weight: 300;
  display: flex;
  justify-content: space-between;
  margin-bottom: 0; }
#topheader img {
  max-height: 2em;
  margin-right: 0.4em; }
#topheader nav {
  display: flex; }
#topheader nav a {
  font-size: 2.1rem;
  padding-right: 0.4em;
  text-decoration: none; }

#social-links a {
  font-size: 2.2rem;
  margin-right: 0.6rem; }


/* Article / Index item
–––––––––––––––––––––––––––––––––––––––––––––––––– */
.post-header .title,
.index-year {
  font-weight: 600; }
article header .title {
  font-size: 2.4rem;
  margin-bottom: 0; }
article header .date-and-category,
article header .tags {
  font-family: ◊|monospace|;
  display: block;
  color: #888;
  margin-bottom: 0;
  margin-top: 0;
  font-size: 1.6rem; }
.index-item {
  margin-top: 1.2rem;
  margin-bottom: 2.4rem; }
◊title {
  font-family: 'Overpass', ◊|cjk-fallback|, sans-serif; }


/* Links
–––––––––––––––––––––––––––––––––––––––––––––––––– */
a,
.date-and-category a,
.tags a {
  color: #a868e8;
  -webkit-transition: all 0.4s ease;
  -moz-transition: all 0.4s ease;
  -o-transition: all 0.4s ease;
  -ms-transition: all 0.4s ease;
  transition: all 0.4s ease;
  text-decoration: none; }
a:hover {
  color: #d0a3ff; }

#topheader a, article header a, #social-links a {
  color: #444; }
#topheader a:hover, article header a:hover, #social-links a:hover {
  color: #777; }


/* Images
-------------------------------------------------- */
img {
  max-width: 100%;
  height: auto;
  -webkit-transition: all 0.6s ease;
  -moz-transition: all 0.6s ease;
  -o-transition: all 0.6s ease;
  -ms-transition: all 0.6s ease;
  transition: all 0.6s ease; }
a img:hover {
  opacity: 0.8; }
.image-caption {
  color: #444;
  font-family: ◊|monospace|;
  font-size: 1.6rem; }



/* Buttons
–––––––––––––––––––––––––––––––––––––––––––––––––– */
.button,
button,
input[type="submit"],
input[type="reset"],
input[type="button"] {
  display: inline-block;
  height: 38px;
  padding: 0 30px;
  color: #555;
  text-align: center;
  font-size: 11px;
  font-weight: 600;
  line-height: 38px;
  letter-spacing: .1rem;
  text-transform: uppercase;
  text-decoration: none;
  white-space: nowrap;
  background-color: transparent;
  border-radius: 4px;
  border: 1px solid #bbb;
  cursor: pointer;
  box-sizing: border-box; }
.button:hover,
button:hover,
input[type="submit"]:hover,
input[type="reset"]:hover,
input[type="button"]:hover,
.button:focus,
button:focus,
input[type="submit"]:focus,
input[type="reset"]:focus,
input[type="button"]:focus {
  color: #333;
  border-color: #888;
  outline: 0; }
.button.button-primary,
button.button-primary,
input[type="submit"].button-primary,
input[type="reset"].button-primary,
input[type="button"].button-primary {
  color: #FFF;
  background-color: #33C3F0;
  border-color: #33C3F0; }
.button.button-primary:hover,
button.button-primary:hover,
input[type="submit"].button-primary:hover,
input[type="reset"].button-primary:hover,
input[type="button"].button-primary:hover,
.button.button-primary:focus,
button.button-primary:focus,
input[type="submit"].button-primary:focus,
input[type="reset"].button-primary:focus,
input[type="button"].button-primary:focus {
  color: #FFF;
  background-color: #1EAEDB;
  border-color: #1EAEDB; }


/* Forms
–––––––––––––––––––––––––––––––––––––––––––––––––– */
input[type="email"],
input[type="number"],
input[type="search"],
input[type="text"],
input[type="tel"],
input[type="url"],
input[type="password"],
textarea,
select {
  height: 38px;
  padding: 6px 10px; /* The 6px vertically centers text on FF, ignored by Webkit */
  background-color: #fff;
  border: 1px solid #D1D1D1;
  border-radius: 4px;
  box-shadow: none;
  box-sizing: border-box; }
/* Removes awkward default styles on some inputs for iOS */
input[type="email"],
input[type="number"],
input[type="search"],
input[type="text"],
input[type="tel"],
input[type="url"],
input[type="password"],
textarea {
  -webkit-appearance: none;
     -moz-appearance: none;
          appearance: none; }
textarea {
  min-height: 65px;
  padding-top: 6px;
  padding-bottom: 6px; }
input[type="email"]:focus,
input[type="number"]:focus,
input[type="search"]:focus,
input[type="text"]:focus,
input[type="tel"]:focus,
input[type="url"]:focus,
input[type="password"]:focus,
textarea:focus,
select:focus {
  border: 1px solid #33C3F0;
  outline: 0; }
label,
legend {
  display: block;
  margin-bottom: .5rem;
  font-weight: 600; }
fieldset {
  padding: 0;
  border-width: 0; }
input[type="checkbox"],
input[type="radio"] {
  display: inline; }
label > .label-body {
  display: inline-block;
  margin-left: .5rem;
  font-weight: normal; }


/* Lists
–––––––––––––––––––––––––––––––––––––––––––––––––– */
ul {
  list-style: circle inside; }
ol {
  list-style: decimal inside; }
ol, ul {
  padding-left: 0;
  margin-top: 0; }
ul ul,
ul ol,
ol ol,
ol ul {
  margin: 1.5rem 0 1.5rem 3rem;
  font-size: 90%; }
li {
  margin-bottom: 1rem; }


/* Code
–––––––––––––––––––––––––––––––––––––––––––––––––– */
/* Frog doesn't seem to emit <code> though… */
code {
  padding: .2rem .5rem;
  margin: 0 .2rem;
  font-size: 90%;
  white-space: nowrap;
  background: #F1F1F1;
  border: 1px solid #E1E1E1;
  border-radius: 4px;
  overflow-x: auto; }
pre > code {
  display: block;
  padding: 1rem 1.5rem;
  overflow-x: auto;
  /* When highlighted code blocks are too wide, they wrap. Resulting in the */
  /* line numbers column's rows not lining up with the code rows. Prevent */
  /* wrapping. */
  white-space: pre;
  width: inherit; }
pre, code {
  overflow-x: auto;
  font-family: ◊|monospace|;
  color: black; }


/* Blockquote
–––––––––––––––––––––––––––––––––––––––––––––––––– */
blockquote {
  font-size: 1.8rem;
  font-style: italic;
  margin-top: 1.0rem;
  margin-bottom: 1.0rem;
  margin-left: 0;
  padding-left: 1.5rem;
  border-left: 0.5rem solid #ccc; }


/* Tables
–––––––––––––––––––––––––––––––––––––––––––––––––– */
th,
td {
  padding: 12px 15px;
  text-align: left;
  border-bottom: 1px solid #E1E1E1; }
th:first-child,
td:first-child {
  padding-left: 0; }
th:last-child,
td:last-child {
  padding-right: 0; }


/* Spacing
–––––––––––––––––––––––––––––––––––––––––––––––––– */
button,
.button {
  margin-bottom: 1rem; }
input,
textarea,
select,
fieldset {
  margin-bottom: 1.5rem; }
pre,
blockquote,
dl,
figure,
table,
p,
ul,
ol,
form {
  margin-bottom: 2.5rem; }


/* Utilities
–––––––––––––––––––––––––––––––––––––––––––––––––– */
.u-full-width {
  width: 100%;
  box-sizing: border-box; }
.u-max-full-width {
  max-width: 100%;
  box-sizing: border-box; }
.u-pull-right {
  float: right; }
.u-pull-left {
  float: left; }


/* Misc
–––––––––––––––––––––––––––––––––––––––––––––––––– */
hr {
  margin-top: 3rem;
  margin-bottom: 3.5rem;
  border-width: 0;
  border-top: 1px solid #E1E1E1; }

#top-link {
  width: 2rem;
  height: 2rem;
  border-radius: 0.5rem;
  text-align: center;
  margin-bottom: 2rem; }
.diff-new {
  color: green; }
.diff-old {
  color: red;
  text-decoration: line-through; }
.pager {
  list-style: none; }
.pager li {
  display: inline; }
.pager .next {
  float: right; }
.inline-red {
  color: red; }
.inline-green {
  color: green; }


/* Clearing
–––––––––––––––––––––––––––––––––––––––––––––––––– */

/* Self Clearing Goodness */
.container:after,
.row:after,
.u-cf {
  content: "";
  display: table;
  clear: both; }


/* Media Queries
–––––––––––––––––––––––––––––––––––––––––––––––––– */
/*
Note: The best way to structure the use of media queries is to create the queries
near the relevant code. For example, if you wanted to change the styles for buttons
on small devices, paste the mobile query code up in the buttons section and style it
there.
*/


/* Larger than mobile */
@media (min-width: 400px) {}

/* Larger than phablet (also point when grid becomes active) */
@media (min-width: 550px) {}

/* Larger than tablet */
@media (min-width: 750px) {}

/* Larger than desktop */
@media (min-width: 1000px) {}

/* Larger than Desktop HD */
@media (min-width: 1200px) {}

/* Print */
@media print {
  .no-print,
  .no-print * {
    display: none; }
}
