let { Peers, .. } = import "../ncl/peer.ncl" in
[
	{
		name = "tetov",
		aliases = ["Anton Tetov"],
		uri = "https://tetov.se",
	},
	{
		name = "WeetHet",
		uri = "https://bsky.app/profile/weethet.bsky.social",
	},
	{
		name = "Lord-Valen",
		uri = "https://laughing-man.xyz",
	},
] | Peers
