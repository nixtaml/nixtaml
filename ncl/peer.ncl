{
	Address = std.contract.from_predicate (std.string.is_match "^\\S+@\\S+\\.\\S+$"),
	URI = std.contract.from_predicate (std.string.is_match "[a-zA-Z][a-zA-Z\\d+\\-.]*://\\S+"),
	PGPLongKeyID = std.contract.from_predicate (std.string.is_match "\\w+/[a-fA-F0-9]{16}"),
	PGPFingerprint = std.contract.from_predicate (std.string.is_match "([a-fA-F0-9]{4} *){10}"),
	PGPKey = {
		long-key-id | PGPLongKeyID,
		fingerprint | PGPFingerprint,
	},
	Peer = {
		name
			| std.string.NonEmpty,
		aliases
			| doc "Other names you may go by"
			| Array std.string.NonEmpty
			| std.array.NonEmpty
			| optional,
		pronouns
			| Array std.string.NonEmpty
			| std.array.NonEmpty
			| optional,
		ownership
			| doc "Aspect of this work one claims ownership over"
			| Array std.string.NonEmpty
			| std.array.NonEmpty
			| optional,
		email
			| Address
			| optional,
		xmpp
			| Address
			| optional,
		uri
			| doc "Hyperlink to website"
			| URI
			| optional,
		pgp_keys
			| Array PGPKey
			| std.array.NonEmpty
			| optional,
	},
	Peers = Array Peer,
}
