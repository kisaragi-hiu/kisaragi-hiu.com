#lang pollen
◊define-meta[title]{Subscribing to the RSS / ATOM feed}

ATOM is a standard for subscribing to web content. (RSS is its predecessor.)

Think a Youtube account that you use to subscribe to Youtube channels. Each ATOM feed is like a channel, and your “RSS reader” of choice is your subscription feed. Although, unlike on Youtube, you have to manually add the URL into your reader to subscribe◊ref[].

If you don’t have a reader, ◊link["https://feedly.com/"]{Feedly} and ◊link["https://www.inoreader.com/"]{Inoreader} are pretty okay.

The feed to this blog is here: ◊link[◊(abs-global "feeds/all.atom.xml")]

◊references{
◊reftxt{Firefox used to display a summary of the feed and a button to add it to some readers when visiting a feed; however, it has been removed.}
}
