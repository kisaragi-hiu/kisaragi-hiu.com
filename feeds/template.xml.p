<?xml version="1.0" encoding="utf-8"?>
◊(local-require "../rkt/feed.rkt")
<feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
 <title type="text">Kisaragi Hiu: Kisaragi Hiu</title>
 <link rel="self" href="http://kisaragi-hiu.com/feeds/all.atom.xml" />
 <link href="http://kisaragi-hiu.com/index.html" />
 <id>urn:http-kisaragi-hiu-com:-index-html</id>
 <updated>2018-11-04T00:37:50Z</updated>
 ◊; first element is \n from current-pagetree call
 ◊(map xexpr->string (cadr doc))
</feed>
